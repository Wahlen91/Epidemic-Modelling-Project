# Load packages
library("ISOweek")
library("data.table")
library("dplyr")
library("lubridate")
library("stringr")
################################################################################
parseTime <- function(string) {
  # Function to parse dates of the form 2009-W45.
  #
  # Args:
  #   string: A character or factor of dates on the form 2009-W45.
  #
  # Returns:
  #   The date converted to POSIXct/POSIXt as the first day of the week.
  #
  string <- gsub("w", "W", string)
  time <- ISOweek2date(paste0(string, "-1"))
  time <-
    parse_date_time(time, orders = "Y! m*! d")#, tz = "Europe/Berlin")
  return(time)
}


# Read data
Male <- fread("Data/Male/Data.csv", skip = 1)
Female <- fread("Data/Female/Data.csv", skip = 1)
load("Data/alldata.RData")

# Set 0 obs to 0
Male[Male == ""] <- "0"
Female[Female == ""] <- "0"

# Get time
Male$time <- parseTime(Male$V1)
Female$time <- parseTime(Female$V1)

# Filter period (470 rows, more than in alldata because of week 53, not important
# for this purpose)
min.time <- min(alldata$date)
max.time <- max(alldata$date)
Male <- Male %>%
  filter(time <= max.time & time >= min.time) %>%
  as.data.frame()
Female <- Female %>%
  filter(time <= max.time & time >= min.time) %>%
  as.data.frame()

# Plot results
plot(Male$time, Male$Unknown, type = "l")
plot(Female$time, Female$Unknown, type = "l")

# Total unknown
(total.unknown <-
  sum(as.numeric(Male$Unknown) + as.numeric(Female$Unknown)))

# Total
(total <-
  sum(as.numeric(as.matrix(Male[, grep("A", names(Male))])) +
        as.numeric(as.matrix(Female[, grep("A", names(Female))]))))

# Relative
total.unknown / total
