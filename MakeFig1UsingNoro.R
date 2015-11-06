##
 # THIS ONE CAN BE USED AS A TEMPLATE. WE SHOULD DO THIS FOR THE OTHER DATA.
##

# Load packages
library("data.table")
library("ggplot2")
library("dplyr")

# Read data
noro <- fread("Data/noro-ts.csv", data.table = FALSE)

# Change y to incidence (per 100,000 population)
noro$y <- noro$y*100000/noro$pop

# Fix ymin, ymax and ymed
noro.tmp <- noro %>% filter(season != 2010) %>% group_by(time) %>%
  summarise(ymax = max(y), ymin = min(y), ymed = median(y))

# Join to whole data
noro <- left_join(noro, noro.tmp)

# Filter out and use season 2010
noro2010 <- noro %>% filter(season == 2010)

# Some annoying ugly things to get tick marks for all weeks. 
# (Sugestions for doing this nicer are very welcome)
breaks <- 1:52
ind <- which(breaks %in% c(1, 11, 21, 33, 43, 52))
labels <- vector(length = length(breaks))
labels[ind] <- paste0("W", c(31, 41, 51, 11, 21, 30))
labels[-ind] <- ""

# Do the plotting in way to many lines
# ADD LEGEND! Colours might be nice. Maybe write "Outbreak period" and
# "Sprout warning issued".
pdf("Figure1.pdf", width=6, height=4, paper='special') 
ggplot(data = noro2010, aes(x = time, y = y)) +
  geom_line() +
  geom_line(aes(y = ymed), linetype="dashed") +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, alpha = 0.01)) +
  xlab("Week of reporting") + ylab("Weekly incidence (per 100,000 population)")+
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 12, 2)) +
  theme_bw() +
  scale_x_continuous(breaks = breaks,
                     labels = labels) +
  geom_vline(xintercept = c(which(diff(noro2010$o104wk) != 0) + 1, nrow(noro2010)),
             linetype = "dashed", colour = "grey") + 
  guides(alpha = FALSE)
dev.off()
  
