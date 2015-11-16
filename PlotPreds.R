# Load packages
library("ggplot2")
library("dplyr")

# Load data
load("Data/alldata.RData")
load("Models/NegBinModel.RData")

# Adding the predicted cases (i.e. fitted values) to alldata
alldata$PredCases <- model.Smooth$fitted.values

# Get aggregated data, (not using PopSmooth)
alldata <- alldata %>% group_by(date, week) %>%
  summarise(Cases = sum(Cases), pop = sum(Population), 
            o104wk = max(o104wk), PredCases = sum(PredCases))

# Plot whole time series
pdf("Figures/PredictionsAndObserved.pdf", width = 6, height = 4, paper = 'special') 
ggplot(data = alldata) +
  geom_line(aes(x = date, y = Cases, color = FALSE), size = 1) +
  geom_line(aes(x = date, y = PredCases, color = TRUE), size = 1) +
  scale_color_brewer(" ",
                     labels = c("Observed counts", "Predicted counts"),
                     type = "qual", palette = 1) +
  xlab("Time (week)") + ylab("No. cases reported") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 9500)) +
  theme_bw() 
dev.off()




