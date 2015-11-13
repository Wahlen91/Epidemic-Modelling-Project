# Load packages
library("gamlss")

# Load data
alldata <- read.csv("Data/alldata.csv")

# Make data for g(t)
alldata$sin2 <- sin(2 * pi * alldata$week / 52)
alldata$sin4 <- sin(4 * pi * alldata$week / 52)
alldata$cos2 <- cos(2 * pi * alldata$week / 52)
alldata$cos4 <- cos(4 * pi * alldata$week / 52)

# Split the outbreak weeks into before and after as done in Bernard et al (2014)
alldata$o104wk.before <-
  as.numeric(alldata$o104wk == 1 & alldata$week_calender %in% 21:23)
alldata$o104wk.after <-
  as.numeric(alldata$o104wk == 1 & !alldata$week_calender %in% 21:23)

# Make season a factor variable
alldata$season <- as.factor(alldata$season)

# Formula for the model with age and sex stratification on dispersion param
mu.formula <-
  Cases ~ offset(log(PopSmooth)) + season + season:sin2 +
  season:cos2 + season:sin4 + season:cos4 + Age + Sex +
  Age:Sex:o104wk
sigma.formula <- ~-1 + Age:Sex

# Construct the model
model <- gamlss(
  formula = mu.formula, sigma.formula = sigma.formula,
  data = alldata,
  family = NBI(mu.link = "log", sigma.link = "log"),
  trace = FALSE
)

# Summary of model.
summary(model, type = "qr")

