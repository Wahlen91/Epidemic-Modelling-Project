# Load packages
library("ggplot2")
library("dplyr")

# Load data
load("Data/alldata.RData")

# Get aggregated data 
alldata <- alldata %>% group_by(date, week) %>%
  summarise(Cases = sum(Cases), pop = sum(Population), o104wk = max(o104wk))

# Plot whole time series
pdf("Figures/WholeTimeSeries.pdf", width=6, height=4, paper='special') 
ggplot(data = alldata, aes(x = date, y = Cases)) +
  geom_line() +
  xlab("Time (week)") + ylab("No. cases reported")+
  scale_y_continuous(expand = c(0,0), limits = c(0, 9500)) +
  theme_bw() +
  geom_vline(xintercept = as.numeric(alldata$date[which(c(0, diff(alldata$o104wk)) != 0) - c(1,0)]),
             linetype = "dashed", colour = "red")
dev.off()



