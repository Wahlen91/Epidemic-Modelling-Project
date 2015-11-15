# Load packages
library("MASS")

# Load data
load("Data/alldata.RData")
#alldata <- read.csv("Data/alldata.csv")

# Source functions
source("MakeFigResid.R")

#load("Data/alldata.RData")
alldata <- read.csv("Data/alldata.csv")

# Make data for g(t)
alldata$sin2 <- sin(2*pi*alldata$week/52)
alldata$sin4 <- sin(4*pi*alldata$week/52)
alldata$cos2 <- cos(2*pi*alldata$week/52)
alldata$cos4 <- cos(4*pi*alldata$week/52)

# Set season as factor
alldata$season < as.factor(alldata$season)

# Split the outbreak weeks into before and after as done in Bernard et al (2014)
alldata$o104wk.before <- as.numeric(alldata$o104wk == 1 & alldata$week_calender %in% 21:23)
alldata$o104wk.after <- as.numeric(alldata$o104wk == 1 & !alldata$week_calender %in% 21:23)

# Construct the model without age and sex stratification
model <- glm.nb(Cases ~ offset(log(Population)) + season + season:sin2 +
                  season:cos2 + season:sin4 + season:cos4 +
                  o104wk, data = alldata, link = "log")

################################################################################

model.Smooth <- glm.nb(Cases ~ offset(log(PopSmooth)) + season + season:sin2 +
                  season:cos2 + season:sin4 + season:cos4 +
                  o104wk, data = alldata, link = "log")
#save(model,model.Smooth,file="NegBinModel.RData")

# Take out the estimates and respective Wald 95% confidence intervals
est <- cbind(Estimate = model$coefficients, confint.default(model))

#
est.Smooth <- cbind(Estimate = model.Smooth$coefficients, confint.default(model.Smooth))
# Exponentiate
round(exp(est), digits = 2)
round(exp(est.Smooth), digits=2)

# Model with age and sex stratification
model.as <- glm.nb(Cases ~ offset(log(Population)) + season + season:sin2 +
                  season:cos2 + season:sin4 + season:cos4 + Age + Sex +
                  Age:Sex:o104wk, data = alldata, link = "log")
################################################################################
model.as.Smooth <- glm.nb(Cases ~ offset(log(PopSmooth)) + season + season:sin2 +
                            season:cos2 + season:sin4 + season:cos4 + Age + Sex +
                            Age:Sex:o104wk, data = alldata, link = "log")

# Take out the estimates and respective Wald 95% confidence intervals
est.as <- cbind(Estimate = model.as$coefficients, confint.default(model.as))
est.as.Smooth <- cbind(Estimate = model.as.Smooth$coefficients, confint.default(model.as.Smooth))
# Exponentiate
round(exp(est.as), digits = 2)
round(exp(est.as.Smooth), digits=2)

# Model with age and sex stratification and split outbreak on before and after
model.as.ba <- glm.nb(Cases ~ offset(log(Population)) + season + season:sin2 +
                        season:cos2 + season:sin4 + season:cos4 + Age + Sex +
                        Age:Sex:o104wk.before + Age:Sex:o104wk.after,
                      data = alldata, link = "log")
################################################################################
model.as.ba.Smooth <- glm.nb(Cases ~ offset(log(PopSmooth)) + season + season:sin2 +
                               season:cos2 + season:sin4 + season:cos4 + Age + Sex +
                               Age:Sex:o104wk.before + Age:Sex:o104wk.after,
                             data = alldata, link = "log")

# Take out the estimates and respective Wald 95% confidence intervals
est.as.ba <- cbind(Estimate = model.as.ba$coefficients,
                   confint.default(model.as.ba.Smooth))
est.as.ba.Smooth <- csbind(Estimate = model.as.ba.Smooth$coefficients,
                   confint.default(model.as.ba.Smooth))
# Exponentiate
round(exp(est.as.ba), digits = 2)
round(exp(est.as.ba.Smooth), digits=2)

#change between smoothed counts and non-smoothed counts.
#round(exp(est.as.ba), digits = 2)-round(exp(est.as.ba.Smooth), digits=2)

Models <-list(model.Smooth,model.as.Smooth,model.as.ba.Smooth)
temp.ff <- lapply(Models,Resid.FUN, AgeClass=alldata$Age, Sex=alldata$Sex)

Figs <- lapply(temp.ff, Plot.Resids)
#pdf("Figures/ModelResidualsModel.pdf", width=6, height = 4, paper="special")
Figs[[1]]
#dev.off()
#pdf("Figures/ModelResidualsModel_as.pdf", width=6, height = 4, paper="special")
Figs[[2]]
#dev.off()
#pdf("Figures/ModelResidualsModel_as_ba.pdf", width=6, height = 4, paper="special")
Figs[[3]]
#dev.off()
