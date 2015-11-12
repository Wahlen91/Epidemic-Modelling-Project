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
Population <-
  try(suppressWarnings(
    read.csv2("data/12411-0006.csv", skip = 7, stringsAsFactors = FALSE,
              fileEncoding = "Latin1")), silent = TRUE)

# If above did not work 
if (inherits(Population, 'try-error')) {
  Population <- read.csv2("data/12411-0006.csv", skip = 7,
                          stringsAsFactors = FALSE)
}

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
  
  # Set empty strings to 0
  dt[dt == ""] <- "0"
  
  # Remove 'Unknown' column
  dt[, Unknown := NULL]
  
  # Fix names
  names(dt)[1] <- "Time"
  
  # Set sex
  dt$Sex <- sex
  
  # Clean the Time column for parsing
  dt$Time <- gsub("w", "W", dt$Time)
  
  # Make a column with the date
  dt$date <- parseTime(dt$Time)
  
  # Get the calender week
  dt$week_calender <- isoweek(dt$date)
  
  # Get the calender year
  #dt$season_calender <- as.character(year(dt$date))
  dt$season_calender <- gsub("-[A-z 0-9]*", "", dt$Time)
  
  # Make a week column with displaced weeks (week 1 is calender week 31)
  dt$week <- dt$week_calender
  ind <- which(dt$week_calender >= 31)
  dt$week[ind] <- dt$week[ind] - 30
  dt$week[-ind] <- dt$week[-ind] + 22
  
  # Make a season column with season based on the above weeks
  dt$season <- dt$season_calender
  dt$season[-ind] <- as.character(as.numeric(dt$season[-ind]) - 1)
  
  # Add outbreak indicator
  dt$o104wk <- as.numeric(dt$season == "2010" & dt$week >= 43)
  
  # Remove dates not "supposed" to be used
  # We keep 2012 week 1 and 2 and 2002 week 51 and 52 for smoothing of population
  dt <- dt %>% 
    filter((!(season < 2003 | season > 2011)) | 
             (season == 2012 & week %in% c(1,2)) | 
             (season == 2002 & week %in% c(51,52)))
  
  # Make age columns integer
  dt <- dt[, lapply(.SD, as.integer),
           by = .(Time, Sex, season, week, date, week_calender,
                  season_calender, o104wk)]
  
  # Convert to data.frame
  df <- as.data.frame(dt)
  
  # Fix age groups
  nam <- names(df)
  ind <- grep("[\\.. +]", nam)
  
  # Loop over columns and add to previous
  for(i in seq(ind[1], range(ind)[2] - 2, by = 2)){
    df[, i] <- df[, i] + df[, i + 1]
  }
  
  # Also put 80+ into 70+
  df[, ncol(df) - 2] <- df[, ncol(df) - 2] + df[, ncol(df)]
  
  # Remove all ages we are not going to keep
  drop.index <- c(seq(ind[1] + 1,
                      range(ind)[2] - 1,
                      by = 2), ncol(df))
  df <- df[, !names(df) %in% nam[drop.index]]
  
  # Fix names
  names(df) <- gsub("\\..", "-", names(df))
  names(df) <- gsub('4$', "9", names(df))
  names(df)[ncol(df)] <- "A70+"
  
  # Randomly move cases in week 53 to week 1 or 52
  set.seed("53")
  df.week.53 <- df %>% filter(week_calender == 53)
  
  # Ugly double for loop. 
  # Loop over both seasons
  for(s in df.week.53$season) {
    # Indexes for adding the randomly selected cases to
    df.ind.1 <- which(df$season == s & df$week_calender == 1)
    df.ind.52 <- which(df$season == s & df$week_calender == 52)
    #Loop over all Age groups
    for(A in names(df.week.53)[grep("A", names(df.week.53))]) {
      # Row index from season
      ind <- which(df.week.53$season == s)
      # Pick out how many cases to distribute
      size <- df.week.53[ind, A]
      # Randomly select a part for week 1
      week1 <- rbinom(n = 1, size = size, prob = 0.5)
      # The rest for week 52
      week52 <- size - week1
      
      # Add cases
      df[df.ind.52, A] <- df[df.ind.52, A] + week52
      df[df.ind.1, A] <- df[df.ind.1, A] + week1
    }
    # Remove row with week 53
    df.ind.53 <- which(df$season == s & df$week_calender == 53)
    df <- df[-df.ind.53, ]
  }
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
  
  # Lazy fix of encoding problems
  names(Population)[3] <- "maennlich"
  
  # Sum over population by age group and year
  Population2 <- Population %>% group_by(X, Age) %>%
    summarise(Male = sum(maennlich), Female = sum(weiblich))
  
  # For easy left_join
  Population2$Age <- paste0("A", Population2$Age, "-", Population2$Age/10, "9")
  Population2$Age[which(Population2$Age == "A70-79")] <- "A70+"
  Population2$Age <- gsub("A[0-9]-", "A00-", Population2$Age)
  names(Population2)[1] <- c("season")
  Population2$X.1 <- NULL
  
  # Long-format for the data 
  df <- melt(df, id.vars = c("Time", "Sex", "season", "week", "date",
                             "week_calender", "season_calender", "o104wk"),
             variable.name = "Age", value.name = "Cases",
             variable.factor = FALSE)
  df$Age <- as.character(df$Age)
  
  # Do the left_join (joining by "season" and "Age") The population is
  # counted at end of each year which is approximately in the middle
  # of the "season" year and thus the join makes sense.
  complete.data <- suppressMessages(left_join(df, Population2))
  
  # Population column based on sex
  complete.data$Population <- NA
  ind <- which(complete.data$Sex == "Male")
  complete.data$Population[ind] = complete.data$Male[ind]
  ind <- which(complete.data$Sex == "Female")
  complete.data$Population[ind] = complete.data$Female[ind]
  
  # Remove male and female column
  complete.data <- complete.data %>% dplyr::select(c(-Male, -Female))
  
  return(complete.data)
}

MovingAvg <- function(Dat){
  # Function to smooth strata wise population based on Agegroup and Sex. 
  # Smoothing is done according to a centered five-week moving avarage.
  #
  # Args:
  #   Dat: data.frame which has the variables Sex, Age, Population and date in it. 
  #
  # Returns:
  #   The original data.frame with a new column for the smoother population.
  #    Also first two and last two weeks are removed.
  #  
  
  InnerFUN <- function(data, AgeGroup, Sexe){
    Sub.dat <- data %>% 
      filter(Age == AgeGroup & Sex == Sexe) %>%
      arrange(desc(Time))
    
    MA.Vals <- transform(Sub.dat,
                         PopSmooth = as.integer(
                           stats::filter(Sub.dat$Population, 
                                         rep(1/5, 5), sides = 2,
                                         method = "convolution")))
    return(MA.Vals)
  }
  
  # Find unique Ages and Sexes for use in InnerFUN
  Ages <- unique(Dat$Age)
  Sexes <- unique(Dat$Sex) # only contains Male and Female
  
  # Run innerFUN on all ages for both Male and Female
  Male.Vals <- lapply(
    X = Ages, Sexe = Sexes[1], data = Dat, FUN = InnerFUN
  )
  Fem.Vals <- lapply(
    X = Ages, Sexe = Sexes[2], data = Dat, FUN = InnerFUN
  )
  
  # Make one data.frame from data.frames of all combination of Sex and Age
  temp.dat <- bind_rows(c(Male.Vals, Fem.Vals))
  temp.dat <- suppressMessages(left_join(Dat, temp.dat))
  
  # Remove all the rows with NA in our smoothed pop.
  temp.dat <- temp.dat %>% dplyr::filter(PopSmooth != "NA")
  return(temp.dat)
}
################################################################################

# Parse data
Male <- parseData(Male, sex = "Male")
Female <- parseData(Female, sex = "Female")

# Combine Male and Female data
alldata <- bind_rows(Male, Female)

# Make wide format and insert population
alldata <- parsePopulation(alldata)

# Create smoothed population variable.
alldata <- MovingAvg(alldata)

#save(alldata, file = "Data/alldata.RData")
#write.csv(alldata, file = "Data/alldata.csv")

