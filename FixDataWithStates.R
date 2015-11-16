# Load packages
library("ISOweek")
library("data.table")
library("dplyr")
library("lubridate")
library("stringr")
################################################################################

readCSV <- function(file){
  # Function to read files from RKI
  #
  # Args:
  #   file: The path of the csv file
  #
  # Returns:
  #   A data.frame with the state name inserted
  #
  
  # Get state name
  name <- gsub("[A-z]*/", "", file)
  name <- gsub("[A-z]*[F M]_", "", name)
  
  # Read data
  data <- read.csv2(paste0(file, "/Data.csv"), skip = 1, fileEncoding = "UTF-16LE",
                    sep = "\t")
  
  # Insert state name
  data$state <- gsub("_", " ", name)
  return(as.data.table(data))
}

readPopCSV <- function(files){
  # Function to read the population csv files
  #
  # Args:
  #   files: The name of the csv files.
  #
  # Returns:
  #   A data.frame with population over time depending on state and age
  #
  
  # Workhorse
  inner <- function(file){
    
    # Fix the path to the file
    fpath <- paste0("Data/WithStates/", file)
    
    # Read the population data
    Population <-
      try(suppressWarnings(
        read.csv2(fpath, skip = 4, stringsAsFactors = FALSE,
                  fileEncoding = "Latin1")), silent = TRUE)
    
    # In case the above does not work
    if (inherits(Population, 'try-error')) {
      Population <- read.csv2(fpath, skip = 4,
                              stringsAsFactors = FALSE)
    }
    
    # Remove columns "Insgesamt"
    remove.ind <- which(Population[1, ] == "Insgesamt")
    Population <- Population[-1, -remove.ind]  
    
    # Insert the sex
    Population$Sex <- ifelse(length(grep("Female", file)) > 0,
                             "Female",
                             "Male")
    
    return(Population)
  }
  
  # Loop the above function over all files
  pop.df.list <- lapply(files, inner)
  
  # Join male and female data, respectively.
  pop.df.male <- suppressMessages(left_join(pop.df.list[[1]], pop.df.list[[2]]))
  pop.df.female <- suppressMessages(left_join(pop.df.list[[3]], pop.df.list[[4]]))
  
  # Join male and female data.
  Population <- suppressMessages(full_join(pop.df.male, pop.df.female))
  
  return(Population)
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
  dt[is.na(dt)] <- "0"
  
  # Remove 'Unknown' column
  if ("Unknown" %in% names(dt)) {
    dt[, Unknown := NULL]
  }
  
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
                  season_calender, o104wk, state)]
  
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
  #ind <- which(is.na(Population$Insgesamt))
  #Population <- Population[-ind, ]
  
  # Don't need whole population
  #Population$Insgesamt <- NULL
  
  # Make year and age work-with-able
  Population$X.1 <- gsub("\\-", "", str_sub(Population$X.1, start = 1, end = 2))
  Population$X <- str_sub(gsub("\\.", "", Population$X), start = 5)
  
  # Remove non-needed rows
  Population$X.1[which(Population$X.1 == "un")] <- 0
  ind <- which(is.na(suppressWarnings(as.numeric(Population$X.1))))
  Population <- Population[-ind, ]
  
  # Divide by 10 and round down to get age group
  Population$Age <- floor(as.numeric(Population$X.1)/10)*10
  
  # Change 80 year olds to 70+
  Population$Age[which(Population$Age == 80)] <- 70
  
  # Remove X.1
  Population$X.1 <- NULL
  
  Population <- 
    melt(Population, id.vars = c("X", "Age", "Sex"), variable.name = "state")
  
  Population$state <- gsub("\\.", " ", Population$state)
  
  Population$state[grep("Baden", Population$state)] <- "Baden Wurttemberg"
  
  # Sum over population by age group and year
  Population2 <- Population %>% group_by(X, Age, Sex, state) %>%
    summarise(Population = sum(as.numeric(value)))
  
  # For easy left_join
  Population2$Age <- paste0("A", Population2$Age, "-", Population2$Age/10, "9")
  Population2$Age[which(Population2$Age == "A70-79")] <- "A70+"
  Population2$Age <- gsub("A[0-9]-", "A00-", Population2$Age)
  names(Population2)[1] <- c("season")
  
  # Long-format for the data 
  df <- melt(df, id.vars = c("Time", "Sex", "season", "week", "date", "state",
                             "week_calender", "season_calender", "o104wk"),
             variable.name = "Age", value.name = "Cases",
             variable.factor = FALSE)
  df$Age <- as.character(df$Age)
  
  # Make translation of state names. Reorder to match names of states  
  translation <- cbind(unique(Population2$state),
                       unique(df$state)[c(1:7, 9, 8, 10:16)])
  
  for(i in 1:nrow(translation)){
    ind <- which(df$state == translation[i, 2])
    df$state[ind] <- translation[i, 1]
  }
  
  # Do the left_join (joining by "season" and "Age") The population is
  # counted at end of each year which is approximately in the middle
  # of the "season" year and thus the join makes sense.
  complete.data <- suppressMessages(left_join(df, Population2))
  
  # Set NA Cases to 0
  complete.data$Cases[is.na(complete.data$Cases)] <- 0
  
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
  
  InnerFUN <- function(data, AgeGroup, Sexe, State){
    Sub.dat <- data %>% 
      filter(Age == AgeGroup & Sex == Sexe & state == State) %>%
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
  States <- unique(Dat$state)
  
  # Run innerFUN on all ages for both Male and Female
  Male.Vals <-
    unlist(lapply(States, function(i)
      lapply(Ages, function(j)
        InnerFUN(data = Dat, j, Sexe = Sexes[1], State = i))),
      recursive = FALSE)
  Fem.Vals <-
    unlist(lapply(States, function(i)
      lapply(Ages, function(j)
        InnerFUN(data = Dat, j, Sexe = Sexes[2], State = i))),
      recursive = FALSE)
  
  # Make one data.frame from data.frames of all combination of Sex and Age
  temp.dat <- bind_rows(c(Male.Vals, Fem.Vals))
  temp.dat <- suppressMessages(left_join(Dat, temp.dat))
  
  # Remove all the rows with NA in our smoothed pop.
  temp.dat <- temp.dat %>% dplyr::filter(PopSmooth != "NA")
  return(temp.dat)
}
################################################################################

# Get all directories for files with norovirus counts
all.dirs <- list.dirs("Data/WithStates/Counts")[-1]

# Indexes of male and female files
is.male <- grep("M_", all.dirs)
is.female <- grep("F_", all.dirs)

# Read data
Male <- lapply(all.dirs[is.male], readCSV)
Female <- lapply(all.dirs[is.female], readCSV)

# Get directories for population files
pop.files <- list.files("Data/WithStates", pattern = "csv")

# Read population data
Population <- readPopCSV(pop.files)

# Parse data
Male <- lapply(Male, function(i) parseData(i, sex = "Male"))
Female <- lapply(Female, function(i) parseData(i, sex = "Female"))

# Combine Male and Female data
alldata <- bind_rows(c(Male, Female))

# Make wide format and insert population
alldata <- parsePopulation(alldata)

# Create smoothed population variable.
alldata <- MovingAvg(alldata)

#save(alldata, file = "Data/alldata_withStates.RData")
#write.csv(alldata, file = "Data/alldata_withStates.csv")
