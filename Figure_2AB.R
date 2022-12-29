#Load libraries. Install them with install.packages() if not found
library("ggplot2")
library("patchwork")
library("tidyverse")
library("ggpubr")
library("GGally")

#Read data
readfile <- read.csv("pilot.csv", row.names = 1,sep=";",header=F)
obs<-as.data.frame(t(readfile))
obs$Music <- as.factor(obs$Music)
obs$Week <- as.factor(obs$Week)

#Reshape
obs_l <- pivot_longer(obs, -c(Day, Week, Music), values_to = "Observations", names_to = "Behavior")


#Filter categories
exploration <- c("Standing", "Walking", "Pecking")
reaction <- c ("Approaching", "Appr_1m", "Appr_2m", "Appr_3m", "Appr_4m")
activity <- c("Hopping", "Flight", "Flying up the wall")
vocalisation <- c("Warning", "Single", "Multiple", "Leave-me-alone", "Listening")
food <- c("Drinking", "Eating", "Flying", "Food walking", "Caching")
eating_between <- c("Begging", "Chasing and begging", "Feeding", "Running away")
between <- c("Shared grooming", "Aggression", "Getting nearer")
autobehavior <- c("Sleeping", "Passive looking down", "Beak cleaning", "Grooming", "Shaking off")

obs_explo <- filter(obs_l, Behavior %in% exploration)
obs_reaction <- filter(obs_l, Behavior %in% reaction)
obs_activity <- filter(obs_l, Behavior %in% activity)
obs_vocal <- filter(obs_l, Behavior %in% vocalisation)
obs_food <- filter(obs_l, Behavior %in% food)
obs_eatb <- filter(obs_l, Behavior %in% eating_between)
obs_auto <- filter(obs_l, Behavior %in% autobehavior)

#Box graphs by week
show_tests = list(c(1,2))
themeline <- theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), axis.title.x = element_blank())
compline <- stat_compare_means(method = "wilcox.test", comparisons = show_tests)
labline <- scale_x_discrete(labels=c("No music", "Music"))

auto5 <- ggplot(obs, aes(group = Week, x = Week, y = `Shaking off`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline + labline
	
shared1 <- ggplot(obs, aes(group = Week, x = Week, y = `Shared grooming`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline + labline

explo3 <- ggplot(obs, aes(group = Week, x = Week, y = `Pecking`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline + labline
	
act2 <- ggplot(obs, aes(group = Week, x = Week, y = `Flight`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline + labline

act3 <- ggplot(obs, aes(group = Week, x = Week, y = `Flying up the wall`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline + labline

vocal5 <- ggplot(obs, aes(group = Week, x = Week, y = `Listening`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline + labline

fig2A <- ggarrange(auto5, shared1, explo3, act2, act3, vocal5, ncol =3, nrow =2)

ggsave("2A.pdf", width = 9, height = 9) 

#Filter categories
compare <- c("Day", "Week", "Music", "Flight", "Flying up the wall", "Pecking", "Shaking off", "Listening", "Shared grooming")
obs_filter <- select(obs, compare)

pdf(file ="2B.pdf", width = 9, height = 9) 
ggpairs(obs_filter, columns = 4:9, , diag = "blank", upper = list(continuous = wrap("cor", method = "spearman")))
dev.off()
