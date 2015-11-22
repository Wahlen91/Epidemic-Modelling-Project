# Load packages
library("ggplot2")
library("dplyr")

# Theme to be used with ggplot
nogrid_theme = theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.line = element_line(size = .35, color = "black"),
                     axis.text = element_text(size = 12), 
                     axis.title = element_text(size = 12, face = "bold"),
                     axis.text.x = element_text(size = 10), 
                     axis.title.x = element_text(size = 10, face = "bold"),
                     strip.background = element_blank(),
                     strip.text.x = element_text(size = 14, face = "bold"),
                     strip.text.y = element_text(size = 14, face = "bold"))

# Load data
alldata <- read.csv("Data/alldata.csv")

# Add incidence
alldata <- alldata %>% mutate(incidence = Cases*100000/PopSmooth)


# Plot cases split on Age and Sex
pdf("Figures/AgeSexCases.pdf", width=4, height=4, paper='special') 
ggplot(data = alldata, aes(x = Age, y = incidence, fill = Age)) +
  geom_boxplot(width = 0.5) + 
  facet_grid(. ~ Sex) +
  theme_classic() +
  nogrid_theme + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_brewer(type = "div", palette = 5) +
  guides(fill = FALSE) +
  ylab("Incidence (per 100,000 population)") +
  scale_y_continuous(expand = c(0, 0), lim = c(0, 40))
dev.off()

# Only Age
#pdf("Figures/AgeCases.pdf", width=6, height=4, paper='special') 
ggplot(data = alldata, aes(x = Age, y = incidence, fill = Age)) +
  geom_boxplot() + 
  theme_classic() +
  nogrid_theme + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_brewer(type = "div", palette = 5) +
  guides(fill = FALSE) +
  ylab("Incidence (per 100,000 population)") +
  scale_y_continuous(expand = c(0, 0))
#dev.off()

# Only sex
#pdf("Figures/SexCases.pdf", width=6, height=4, paper='special') 
ggplot(data = alldata, aes(x = Sex, y = incidence, fill = Sex)) +
  geom_boxplot() + 
  theme_classic() +
  nogrid_theme + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(fill = FALSE) +
  ylab("Incidence (per 100,000 population)") +
  scale_y_continuous(expand = c(0, 0))
#dev.off()
