# Load packages
library("surveillance")
library("reshape2")
library("ggplot2")

# Load data
load("Data/alldata.RData")

# Make data for g(t)
alldata$sin2 <- sin(2*pi*alldata$week/52)
alldata$sin4 <- sin(4*pi*alldata$week/52)
alldata$sin6 <- sin(6*pi*alldata$week/52)
alldata$sin8 <- sin(8*pi*alldata$week/52)
alldata$sin10 <- sin(10*pi*alldata$week/52)
alldata$sin12 <- sin(12*pi*alldata$week/52)
alldata$sin14 <- sin(14*pi*alldata$week/52)
alldata$sin16 <- sin(16*pi*alldata$week/52)

alldata$cos2 <- cos(2*pi*alldata$week/52)
alldata$cos4 <- cos(4*pi*alldata$week/52)
alldata$cos6 <- cos(6*pi*alldata$week/52)
alldata$cos8 <- cos(8*pi*alldata$week/52)
alldata$cos10 <- cos(10*pi*alldata$week/52)
alldata$cos12 <- cos(12*pi*alldata$week/52)
alldata$cos14 <- cos(14*pi*alldata$week/52)
alldata$cos16 <- cos(16*pi*alldata$week/52)

# Split the outbreak weeks into before and after as done in Bernard et al (2014)
alldata$o104wk.before <-
  as.numeric(alldata$o104wk == 1 & alldata$week_calender %in% 21:23)
alldata$o104wk.after <-
  as.numeric(alldata$o104wk == 1 & !alldata$week_calender %in% 21:23)

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
                                      paste0("sin", seq(2, 16, 2)),
                                      paste0("cos", seq(2, 16, 2)), 
                                      "o104wk.before", "o104wk.after"))

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
f_S2 <- addSeason2formula(f = ~1 + fe(o104wk, unitSpecific = TRUE) + 
                            Sex,
                          S = 0, period = 52, timevar = "t")
f_S4 <- f_S6 <- f_S14 <- f_S16 <- f_S2
f_S4.ba <- addSeason2formula(f = ~1 + fe(o104wk.before, unitSpecific = TRUE) +
                               fe(o104wk.after, unitSpecific = TRUE) + Sex,
                             S = 0, period = 52, timevar = "t")

# Add one model without season interaction for cos and sin
f_S4.wo.season.int <- 
  addSeason2formula(f = ~1 + fe(o104wk, unitSpecific = TRUE) + Sex,
                             S = 2, period = 52, timevar = "t")

# Loop over all ages except the the first and add to formula
for (age in paste0("A", seq(10, 70, 10), c(paste0("_", seq(19, 69, 10)), "plus"))) {
  f_S2 <- f_S4 <- f_S6 <- f_S14 <- f_S16 <- 
    update(f_S2, as.formula(paste0("~. + ", age)))
  
  f_S4.ba <- update(f_S4.ba, as.formula(paste0("~. + ", age)))
  
  f_S4.wo.season.int <- 
    update(f_S4.wo.season.int, as.formula(paste0("~. + ", age)))
}

# Loop over all seasons except the the first and add to formula
for (season in paste0("s", 2004:2011)) {
  f_S2 <- f_S4 <- f_S6 <- f_S14 <- f_S16 <- 
    update(f_S2, as.formula(paste0("~. + ", season)))
  
  f_S4.ba <- update(f_S4.ba, as.formula(paste0("~. + ", season)))
  
  f_S4.wo.season.int <- 
    update(f_S4.wo.season.int, as.formula(paste0("~. + ", season)))
}

# Highest order cos and sinus term
nr.sincos <- c(2, 4, 6, 14, 16)
# Loop over all seasons and periodic functions and add to formula
for (season in paste0("s", 2003:2011)) {
  for (period in c(paste0("sin", seq(2, max(nr.sincos), 2)),
                   paste0("cos", seq(2, max(nr.sincos), 2)))) {
    
    # Check order of cos and sin terms being added
    order <- as.numeric(gsub("[A-z]", "", period))
    # Add only terms that are supposed to be added
    if (order <= nr.sincos[1]) {
      f_S2 <- update(f_S2,
                     as.formula(paste0("~ . + I(", season, "*", period, ")")))
    }
    if (order <= nr.sincos[2]) {
      f_S4 <- update(f_S4,
                     as.formula(paste0("~ . + I(", season, "*", period, ")")))
      f_S4.ba <- update(f_S4.ba,
                        as.formula(paste0("~ . + I(", season, "*", period, ")")))
    }
    if (order <= nr.sincos[3]) {
      f_S6 <- update(f_S6,
                     as.formula(paste0("~ . + I(", season, "*", period, ")")))
    }
    if (order <= nr.sincos[4]) {
      f_S14 <- update(f_S14,
                      as.formula(paste0("~ . + I(", season, "*", period, ")")))
    }
    if (order <= nr.sincos[5]) {
      f_S16 <- update(f_S16,
                      as.formula(paste0("~ . + I(", season, "*", period, ")")))
    }
    
  }
}

# For some reason the intercept dissapears so we add it
f_S2 <- as.formula(paste0("~ 1 + ", as.character(f_S2)[2]))
f_S4 <- as.formula(paste0("~ 1 + ", as.character(f_S4)[2]))
f_S4.ba <- as.formula(paste0("~ 1 + ", as.character(f_S4.ba)[2]))
f_S6 <- as.formula(paste0("~ 1 + ", as.character(f_S6)[2]))
f_S14 <- as.formula(paste0("~ 1 + ", as.character(f_S14)[2]))
f_S16 <- as.formula(paste0("~ 1 + ", as.character(f_S16)[2]))
f_S4.wo.season.int <- 
  as.formula(paste0("~ 1 + ", as.character(f_S4.wo.season.int)[2]))

# Specify the model

model.control2 <-
  model.control4 <- model.control6 <- model.control14 <-
  model.control16 <- model.control4.ba.wo.strat.od <-
  model.control4.ba <- model.control4.wo.season.int <-
  model.control4.wo.strat.od <- list(
    ar = list(f = ~1, lag = 1),
    end = list(f = f_S2,
               offset = population(dat)),
    family = "NegBinM",
    data = list(
      Sex = Sex,
      A00_09 = Age[[1]],
      A10_19 = Age[[2]],
      A20_29 = Age[[3]],
      A30_39 = Age[[4]],
      A40_49 = Age[[5]],
      A50_59 = Age[[6]],
      A60_69 = Age[[7]],
      A70plus = Age[[8]],
      o104wk = dataframes[[2]],
      o104wk.before = dataframes[[20]],
      o104wk.after = dataframes[[21]],
      sin2 = dataframes[[4]],
      sin4 = dataframes[[5]],
      sin6 = dataframes[[6]],
      sin8 = dataframes[[7]],
      sin10 = dataframes[[8]],
      sin12 = dataframes[[9]],
      sin14 = dataframes[[10]],
      sin16 = dataframes[[11]],
      cos2 = dataframes[[12]],
      cos4 = dataframes[[13]],
      cos6 = dataframes[[14]],
      cos8 = dataframes[[15]],
      cos10 = dataframes[[16]],
      cos12 = dataframes[[17]],
      cos14 = dataframes[[18]],
      cos16 = dataframes[[19]],
      s2003 = Season[[1]],
      s2004 = Season[[2]],
      s2005 = Season[[3]],
      s2006 = Season[[4]],
      s2007 = Season[[5]],
      s2008 = Season[[6]],
      s2009 = Season[[7]],
      s2010 = Season[[8]],
      s2011 = Season[[9]]
    )
  )
model.control4$end <- list(f = f_S4, offset = population(dat))
model.control4.ba$end <- list(f = f_S4.ba, offset = population(dat))
model.control4.ba.wo.strat.od$end <- list(f = f_S4.ba, offset = population(dat))
model.control4.ba.wo.strat.od$family <- "NegBin1"
model.control4.wo.season.int$end <- 
  list(f = f_S4.wo.season.int, offset = population(dat))
model.control4.wo.strat.od$end <- list(f = f_S4, offset = population(dat))
model.control4.wo.strat.od$family <- "NegBin1"
model.control6$end <- list(f = f_S6, offset = population(dat))
model.control14$end <- list(f = f_S14, offset = population(dat))
model.control16$end <- list(f = f_S16, offset = population(dat))

# Fit the models (takes a while)
# hhh4Model2 <- hhh4(dat, control = model.control2)
# hhh4Model4 <- hhh4(dat, control = model.control4)
# hhh4Model4.ba <- hhh4(dat, control = model.control4.ba)
# hhh4Model4.ba.wo.strat.od <- hhh4(dat, control = model.control4.ba.wo.strat.od)
# hhh4Model4.wo.season.int <- hhh4(dat, control = model.control4.wo.season.int)
# hhh4Model4.wo.strat.od <- hhh4(dat, control = model.control4.wo.strat.od)
# hhh4Model6 <- hhh4(dat, control = model.control6)
# hhh4Model14 <- hhh4(dat, control = model.control14)
# hhh4Model16 <- hhh4(dat, control = model.control16)
load("Models/hhh4Model.RData")


#save(hhh4Model2, hhh4Model4, hhh4Model4.ba, hhh4Model4.ba.wo.strat.od,
#     hhh4Model4.wo.season.int,
#     hhh4Model4.wo.strat.od, hhh4Model6, hhh4Model14, hhh4Model16,
#     file = "Models/hhh4Model.RData")

# Summary of model
#summary(hhh4Model4, idx2Exp = TRUE, reparamPsi = TRUE)

# Plot the model
#pdf("Figures/hhh4Plot.pdf", width = 12, height = 8, paper = 'special') 
#plotHHH4_fitted(hhh4Model4, units = 8, ylim = c(0, 2500))
#dev.off()


# Anscombe residuals (EXPERIMENTAL!)
# Data.frame to get model.matrix
df <- data.frame(cases = as.numeric(dataframes[[1]][-1, ]),
                 AR1 = as.numeric(dataframes[[1]][-1, ]),
                 Male = as.numeric(Sex[-1, ]),
                 Female = 1*!as.numeric(Sex[-1, ]),
                 A00_09 = as.numeric(Age[[1]][-1, ]),
                 A10_19 = as.numeric(Age[[2]][-1, ]),
                 A20_29 = as.numeric(Age[[3]][-1, ]),
                 A30_39 = as.numeric(Age[[4]][-1, ]),
                 A40_49 = as.numeric(Age[[5]][-1, ]),
                 A50_59 = as.numeric(Age[[6]][-1, ]),
                 A60_69 = as.numeric(Age[[7]][-1, ]),
                 A70plus = as.numeric(Age[[8]][-1, ]),
                 o104wk = as.numeric(dataframes[[2]][-1, ]),
                 sin2 = as.numeric(dataframes[[4]][-1, ]),
                 sin4 = as.numeric(dataframes[[5]][-1, ]),
                 cos2 = as.numeric(dataframes[[12]][-1, ]),
                 cos4 = as.numeric(dataframes[[13]][-1, ]),
                 s2003 = as.numeric(Season[[1]][-1, ]),
                 s2004 = as.numeric(Season[[2]][-1, ]),
                 s2005 = as.numeric(Season[[3]][-1, ]),
                 s2006 = as.numeric(Season[[4]][-1, ]),
                 s2007 = as.numeric(Season[[5]][-1, ]),
                 s2008 = as.numeric(Season[[6]][-1, ]),
                 s2009 = as.numeric(Season[[7]][-1, ]),
                 s2010 = as.numeric(Season[[8]][-1, ]),
                 s2011 = as.numeric(Season[[9]][-1, ])
)

# Formula for model.matrix
form <-
  ~ AR1 + 1 + Male + A10_19 + A20_29 + A30_39 + 
  A40_49 + A50_59 + A60_69 + A70plus + s2004 + s2005 + s2006 + s2007 + s2008 +
  s2009 + s2010 + s2011 + I(s2003 * sin2) + I(s2003 * sin4) + I(s2003 * cos2) +
  I(s2003 * cos4) + I(s2004 * sin2) + I(s2004 * sin4) + I(s2004 * cos2) + 
  I(s2004 * cos4) + I(s2005 * sin2) + I(s2005 * sin4) + I(s2005 * cos2) +
  I(s2005 * cos4) + I(s2006 * sin2) + I(s2006 * sin4) + I(s2006 * cos2) +
  I(s2006 * cos4) + I(s2007 * sin2) + I(s2007 * sin4) + I(s2007 * cos2) + 
  I(s2007 * cos4) + I(s2008 * sin2) + I(s2008 * sin4) + I(s2008 * cos2) + 
  I(s2008 * cos4) + I(s2009 * sin2) + I(s2009 * sin4) + I(s2009 * cos2) + 
  I(s2009 * cos4) + I(s2010 * sin2) + I(s2010 * sin4) + I(s2010 * cos2) +
  I(s2010 * cos4) + I(s2011 * sin2) + I(s2011 * sin4) + I(s2011 * cos2) + 
  I(s2011 * cos4) + I(o104wk * A00_09 * Female)  + I(o104wk * A10_19 * Female) +
  I(o104wk * A20_29 * Female) + I(o104wk * A30_39 * Female) +  I(o104wk * A40_49 * Female) + 
  I(o104wk * A50_59 * Female) + I(o104wk * A60_69 * Female) + I(o104wk * A70plus * Female) + 
  I(o104wk * A00_09 * Male)  + I(o104wk * A10_19 * Male) +
  I(o104wk * A20_29 * Male) + I(o104wk * A30_39 * Male) +  I(o104wk * A40_49 * Male) + 
  I(o104wk * A50_59 * Male) + I(o104wk * A60_69 * Male) + I(o104wk * A70plus * Male)


# Get model matrix
X <- model.matrix(form, data = df)
X <- X[, c(2, 1, 3:ncol(X))]

# Get hat matrix (THIS IS FOR OLS!)
H <- X %*% solve(t(X) %*% X) %*% t(X)

# Which model
model <- hhh4Model4.wo.strat.od

# Coefficients
beta <- coefficients(model)[-length(coefficients(model))]

# Take out observed
y <- as.numeric(model$stsObj@observed[-1, ])

# Take out fitted
mu <- as.numeric(fitted.values(model))

# Dispersion (Is this correct?)
phi <- 1 + exp(-model$coefficients["-log(overdisp)"])*mu

# Anscombe residuals
A.res <- 3/2 * (y^(2/3) * mu^(-1/6) - mu^(1/2))
A.res <- A.res/sqrt(phi * (1 - diag(H)))

# Plot the residuals
# plot(A.res)

# Get age and sex
data.for.strat <- alldata[-which(alldata$date == min(alldata$date)), ]
plot.data.anscombe <- data.frame(res = A.res,
                        Age = data.for.strat$Age,
                        Sex = data.for.strat$Sex)

# plot residuals
#pdf("Figures/AnscombeResidExperimental.pdf", width = 6, height = 4, paper = 'special')
#ggplot(data = plot.data, aes(y = res, x = 1:length(res), col = Age:Sex)) +
#  geom_point() +
#  xlab("Observation") + 
#  ylab("Anscombe residuals") +
#  theme_bw()
#dev.off()

#save(plot.data.anscombe, file = "Models/plotdfAnscombe.RData")


# One step ahead prediction
#hhh4Model4.wo.strat.od
#test <- pit(hhh4Model4.wo.strat.od)
#pred <- oneStepAhead(hhh4Model4.wo.strat.od, nrow(dat)-40, type="rolling",
#                     which.start="final", verbose=FALSE)

#model <- hhh4Model4.wo.strat.od
#plot(1:467, rowSums(model$fitted.values), type = "l")
#lines(1:467, rowSums(dat@observed[-1, ]), col = "red")


