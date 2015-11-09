# Load packages
library("MASS")

# Load data
load("Data/alldata.RData")
#alldata <- read.csv("Data/alldata.csv")

# Make data for g(t)
alldata$sin2 <- sin(2*pi*alldata$week/52)
alldata$sin4 <- sin(4*pi*alldata$week/52)
alldata$cos2 <- cos(2*pi*alldata$week/52)
alldata$cos4 <- cos(4*pi*alldata$week/52)

# Split the outbreak weeks into before and after as done in Bernard et al (2014)
alldata$o104wk.before <- as.numeric(alldata$o104wk == 1 & alldata$week_calender %in% 21:23)
alldata$o104wk.after <- as.numeric(alldata$o104wk == 1 & !alldata$week_calender %in% 21:23)

# Construct the model without age and sex stratification
model <- glm.nb(Cases ~ offset(log(Population)) + season + season:sin2 +
                  season:cos2 + season:sin4 + season:cos4 +
                  o104wk, data = alldata, link = "log")

# Take out the estimates and respective Wald 95% confidence intervals
est <- cbind(Estimate = model$coefficients, confint.default(model))

# Exponentiate
round(exp(est), digits = 2)

# Model with age and sex stratification
model.as <- glm.nb(Cases ~ offset(log(Population)) + season + season:sin2 +
                  season:cos2 + season:sin4 + season:cos4 + Age + Sex +
                  Age:Sex:o104wk, data = alldata, link = "log")

# Take out the estimates and respective Wald 95% confidence intervals
est.as <- cbind(Estimate = model.as$coefficients, confint.default(model.as))

# Exponentiate
round(exp(est.as), digits = 2)
