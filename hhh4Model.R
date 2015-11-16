# Load packages
library("surveillance")
library("reshape2")

# Load data
load("Data/alldata.RData")

# Make data for g(t)
alldata$sin2 <- sin(2*pi*alldata$week/52)
alldata$sin4 <- sin(4*pi*alldata$week/52)
alldata$cos2 <- cos(2*pi*alldata$week/52)
alldata$cos4 <- cos(4*pi*alldata$week/52)

GetWideFormatVariable <- function(df, variables) {
  # Convert a data.frame into a wide format matrix with the specified variables 
  # as value variables in one data.frame each. season and week are used as 
  # time variables.
  #
  # Args:
  #   df: data.frame with the variables 'variables' and a season, week, Sex and
  #        age column.
  #   variables: character of which variables one wants
  # 
  # Returns:
  #   A matrix in wide format with 'variables' as value variables in one 
  #    matrix each without the season and week columns.
  #   
  
  # Function for use in dcast to aggregate different classes of data
  aggregateFunction <- function(x) {
    ifelse(is.character(x) | is.factor(x), I(as.character(x)), sum(x))
  }
  
  # Make a data.frame in wide format with 'variable' as the value variable
  inner <- function(variable) {
    inner.df <-
      dcast(df,
            season + week ~ Sex + Age,
            fun.aggregate = aggregateFunction,
            value.var = variable)
    return(as.matrix(inner.df[, -c(1, 2)]))
  }
  
  # Return result for as many variables as specified.
  return(lapply(variables, inner))
}

# Get data frames in wide format for variables needed in the modelling
dataframes <- GetWideFormatVariable(alldata, 
                                    c("Cases", "o104wk", "PopSmooth",
                                      "sin2", "sin4", "cos2", "cos4"))

# Fix data for use in hhh4
dat <- new("sts",
           observed = dataframes[[1]],
           freq = 52,
           state = dataframes[[2]],
           epoch = 1:nrow(dataframes[[1]]),
           population = dataframes[[3]],
           start = c(2003, 1))

# Plot the time series
#pdf("Figures/StratifiedTimeSeries.pdf", width = 12, height = 8, paper = 'special') 
plot(dat, type = observed ~ time | unit,
     ylab = "No. of cases", ylim = c(0, 2500))
#dev.off()

MakeDummy <- function(df, variable, level){
  # Make a dummy variable matrix for 'variable' with 'level' as the 
  # factor for which the dummy is supposed to be 1.
  #
  # Args:
  #   df: data.frame with 'variable' in one column.
  #   variable: which variable to make a dummy of.
  #   level: which level of variable is supposed to be 1?
  # 
  # Returns:
  #   A matrix without week and season that has one column for each age
  #    and sex group specifying the value of the dummy variable over time.
  #   
  
  # Get wide format data.frame with 'variable' as value variable
  df <- GetWideFormatVariable(df, variable)[[1]]
  
  # Check which is equal to level and make numeric
  df <- 1*(df == level)
  return(df)
}

# Get dummy variable data.frames for sex, age and season
Sex <- MakeDummy(alldata, "Sex", "Male")
Age <- lapply(
  paste0("A", 0:7, "0", c(paste0("-", 0:6, "9"), "+")), # All ages
  function(i) MakeDummy(alldata, "Age", i))
Season <- lapply(
  as.character(2003:2011),
  function(i) MakeDummy(alldata, "season", i))

# Construct initial part of the formula used in hhh4
f_S1 <- addSeason2formula(f = ~1 + fe(o104wk, unitSpecific = TRUE) + 
                            Sex,
                          S = 0, period = 52, timevar = "t")

# Loop over all ages except the the first and add to formula
for (age in paste0("A", seq(10, 70, 10), c(paste0("_", seq(19, 69, 10)), "plus"))) {
  f_S1 <- update(f_S1, as.formula(paste0("~. + ", age)))
}

# Loop over all seasons except the the first and add to formula
for (season in paste0("s", 2004:2011)) {
  f_S1 <- update(f_S1, as.formula(paste0("~. + ", season)))
}

# Loop over all seasons and periodic functions and add to formula
for (season in paste0("s", 2003:2011)) {
  for (period in c("sin2", "sin4", "cos2", "cos4")) {
    f_S1 <- update(f_S1,
                   as.formula(paste0("~ . + I(", season, "*", period, ")")))
  }
}

# For some reason the intercept dissapears so we add it
f_S1 <- as.formula(paste0("~ 1 + ", as.character(f_S1)[2]))

# Specify the model
model.control <- list(ar = list(f = ~1,
                                lag = 1),
                      end = list(f = f_S1,
                                 offset = population(dat)),
                      family = "NegBinM",
                      data = list(Sex = Sex,
                                  A00_09 = Age[[1]],
                                  A10_19 = Age[[2]],
                                  A20_29 = Age[[3]],
                                  A30_39 = Age[[4]],
                                  A40_49 = Age[[5]],
                                  A50_59 = Age[[6]],
                                  A60_69 = Age[[7]],
                                  A70plus = Age[[8]],
                                  o104wk = dataframes[[2]],
                                  sin2 = dataframes[[4]],
                                  sin4 = dataframes[[5]],
                                  cos2 = dataframes[[6]],
                                  cos4 = dataframes[[7]],
                                  s2003 = Season[[1]],
                                  s2004 = Season[[2]],
                                  s2005 = Season[[3]],
                                  s2006 = Season[[4]],
                                  s2007 = Season[[5]],
                                  s2008 = Season[[6]],
                                  s2009 = Season[[7]],
                                  s2010 = Season[[8]],
                                  s2011 = Season[[9]])
)

# Fit the model (takes a while)
hhh4Model <- hhh4(dat, control = model.control)
save(hhh4Model,file="Models/hhh4Model.RData")

# Summary of model
summary(hhh4Model, idx2Exp = TRUE, reparamPsi = TRUE)

# Plot the model
#pdf("Figures/hhh4Plot.pdf", width = 12, height = 8, paper = 'special') 
plotHHH4_fitted(hhh4Model, units = 8, ylim = c(0, 2500))
#dev.off()
