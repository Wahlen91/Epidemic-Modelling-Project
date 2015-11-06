# Load packages
library("ISOweek")
library("data.table")
library("dplyr")
library("lubridate")
library("stringr")
################################################################################
# Read data
Male <- fread("Data/Male/Data.csv", skip = 1)
Female <- fread("Data/Female/Data.csv", skip = 1)

# Population https://www-genesis.destatis.de/genesis/online/data;jsessionid=F70E6AD50EA5A31585D5D88F43F4D69F.tomcat_GO_1_1?operation=abruftabelleBearbeiten&levelindex=1&levelid=1446842451376&auswahloperation=abruftabelleAuspraegungAuswaehlen&auswahlverzeichnis=ordnungsstruktur&auswahlziel=werteabruf&selectionname=12411-0006&auswahltext=%23Z-31.12.2014%2C31.12.2013%2C31.12.2012%2C31.12.2011%2C31.12.2010%2C31.12.2009%2C31.12.2008%2C31.12.2007%2C31.12.2006%2C31.12.2005%2C31.12.2004%2C31.12.2003%2C31.12.2002%2C31.12.2001&nummer=5&variable=2&name=GES&werteabruf=Werteabruf
Population <- read.csv2("data/12411-0006.csv", skip = 7, stringsAsFactors = FALSE)
################################################################################

################################################################################
parseTime <- function(string){
  # Function to parse dates of the form 2009-W45.
  #
  # Args:
  #   string: A character or factor of dates on the form 2009-W45.
  #
  # Returns:
  #   The date converted to POSIXct/POSIXt as the first day of the week.
  #   
  time <- ISOweek2date(paste0(string, "-1"))
  time <- parse_date_time(time, orders = "Y! m*! d")#, tz = "Europe/Berlin")
  return(time)
}

parseData <- function(dt, sex = NULL){
  # Function to fix data from RKI
  #
  # Args:
  #   dt: data.table of data from RKI
  #   sex: Of which sex is the data (string)
  #
  # Returns:
  #   A nice looking data.frame
  #   
  
  # Remove 'Unknown' column
  dt[, Unknown := NULL]
  
  # Fix names
  names(dt)[1] <- "Time"
  
  # Set sex
  dt$Sex <- sex
  
  # Set season
  dt$season <- gsub('-[A-z 0-9]*', "", dt$Time)
  
  # Set week
  dt$Time <- gsub("w", "W", dt$Time)
  dt$date <- parseTime(dt$Time)
  dt$week <- isoweek(dt$date)
  
  # Remove dates not "supposed" to be used
  dt <- dt %>% 
    filter(!(season < 2003 | season > 2013)) %>%
    filter(!(season == 2003 & week < 31)) %>% 
    filter(!(season == 2012 & week > 30))
  
  # Make age columns integer
  dt <- dt[, lapply(.SD, as.integer), by = .(Time, Sex, season, week, date)]
  
  # Convert to data.frame
  df <- as.data.frame(dt)
  
  # Fix age groups
  nam <- names(df)
  ind <- grep("[\\.. +]", nam)
  
  # Loop over columns and add to previous
  for(i in seq(ind[1], range(ind)[2]-2, by = 2)){
    df[, i] <- df[, i] + df[, i + 1]
  }
  
  # Also put 80+ into 70+
  df[, ncol(df)-2] <- df[, ncol(df)-2] + df[, ncol(df)]
  
  # Remove all ages we are not going to keep
  drop.index <- c(seq(ind[1] + 1,
                      range(ind)[2] - 1,
                      by = 2), ncol(df))
  df <- df[, !names(df) %in% nam[drop.index]]
  
  # Fix names
  names(df) <- gsub("\\..", "-", names(df))
  names(df) <- gsub('4$', "9", names(df))
  names(df)[ncol(df)] <- "A70+"
  
  # Return data.frame
  return(df)
}

parsePopulation <- function(df){
  # Function to fix data from Destatis and join with the data from RKI
  #
  # Args:
  #   df: data.frame of data from Destatis
  #
  # Returns:
  #   A nice looking data.frame in long format with data from RKI and Destatis
  #   
  
  # Get indexes of "bad" rows and remove them
  ind <- which(is.na(Population$Insgesamt))
  Population <- Population[-ind, ]
  
  # Don't need whole population
  Population$Insgesamt <- NULL
  
  # Make year and age work-with-able
  Population$X.1 <- gsub("\\-", "", str_sub(Population$X.1, start = 1, end = 2))
  Population$X <- str_sub(gsub("\\.", "", Population$X), start = 5)
  
  # Remove non-needed rows
  ind <- which(is.na(suppressWarnings(as.numeric(Population$X.1))))
  Population <- Population[-ind, ]
  
  # Divide by 10 and round down to get age group
  Population$Age <- floor(as.numeric(Population$X.1)/10)*10
  
  # Change 80 year olds to 70+
  Population$Age[which(Population$Age == 80)] <- 70
  
  # Sum over population by age group and year
  Population2 <- Population %>% group_by(X, Age) %>%
    summarise(Male = sum(m.nnlich), Female = sum(weiblich))
  
  # For easy left_join
  Population2$Age <- paste0("A", Population2$Age, "-", Population2$Age/10, "9")
  Population2$Age[which(Population2$Age == "A70-79")] <- "A70+"
  Population2$Age <- gsub("A[0-9]-", "A00-", Population2$Age)
  names(Population2)[1] <- c("season")
  Population2$X.1 <- NULL
  
  # Long-format for the data 
  df <- melt(df, id.vars = c("Time", "Sex", "season", "week", "date"),
             variable.name = "Age", value.name = "Cases",
             variable.factor = FALSE)
  df$Age <- as.character(df$Age)
  
  # Do the left_join
  complete.data <- suppressMessages(left_join(df, Population2))
  
  # Population column based on sex
  complete.data$Population <- NA
  ind <- which(complete.data$Sex == "Male")
  complete.data$Population[ind] = complete.data$Male[ind]
  ind <- which(complete.data$Sex == "Female")
  complete.data$Population[ind] = complete.data$Female[ind]
  
  # Remove male and female column
  complete.data <- complete.data %>% select(-Male, -Female)
  
  return(complete.data)
}
################################################################################


# Parse data
Male <- parseData(Male, sex = "Male")
Female <- parseData(Female, sex = "Female")

# Combine Male and Female data
alldata <- bind_rows(Male, Female)

# Make wide format and insert population
alldata <- parsePopulation(alldata)

#save(alldata, file = "Data/alldata.RData")
#write.csv(alldata, file = "Data/alldata.csv")

