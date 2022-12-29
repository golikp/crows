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
themeline <- theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank())
compline <- stat_compare_means(method = "wilcox.test", comparisons = show_tests)

explo1 <- ggplot(obs, aes(group = Week, x = Week, y = `Standing`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline
    
explo2 <- ggplot(obs, aes(group = Week, x = Week, y = `Walking`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline

explo3 <- ggplot(obs, aes(group = Week, x = Week, y = `Pecking`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline

act1 <- ggplot(obs, aes(group = Week, x = Week, y = `Hopping`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline
    
act2 <- ggplot(obs, aes(group = Week, x = Week, y = `Flight`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline

act3 <- ggplot(obs, aes(group = Week, x = Week, y = `Flying up the wall`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline

react1 <- ggplot(obs, aes(group = Week, x = Week, y = `Approaching`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline
    
react2 <- ggplot(obs, aes(group = Week, x = Week, y = `Appr_1m`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline

react3 <- ggplot(obs, aes(group = Week, x = Week, y = `Appr_2m`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline

react4 <- ggplot(obs, aes(group = Week, x = Week, y = `Appr_3m`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline

react5 <- ggplot(obs, aes(group = Week, x = Week, y = `Appr_4m`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline

vocal1 <- ggplot(obs, aes(group = Week, x = Week, y = `Warning`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline
    
vocal2 <- ggplot(obs, aes(group = Week, x = Week, y = `Single`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline

vocal3 <- ggplot(obs, aes(group = Week, x = Week, y = `Multiple`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline

vocal4 <- ggplot(obs, aes(group = Week, x = Week, y = `Leave-me-alone`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline

vocal5 <- ggplot(obs, aes(group = Week, x = Week, y = `Listening`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline
	
food1 <- ggplot(obs, aes(group = Week, x = Week, y = `Drinking`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline
    
food2 <- ggplot(obs, aes(group = Week, x = Week, y = `Eating`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline

food3 <- ggplot(obs, aes(group = Week, x = Week, y = `Flying`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline

food4 <- ggplot(obs, aes(group = Week, x = Week, y = `Food walking`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline

food5 <- ggplot(obs, aes(group = Week, x = Week, y = `Caching`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline
	
auto1 <- ggplot(obs, aes(group = Week, x = Week, y = `Sleeping`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline
    
auto2 <- ggplot(obs, aes(group = Week, x = Week, y = `Passive looking down`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline

auto3 <- ggplot(obs, aes(group = Week, x = Week, y = `Beak cleaning`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline

auto4 <- ggplot(obs, aes(group = Week, x = Week, y = `Grooming`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline

auto5 <- ggplot(obs, aes(group = Week, x = Week, y = `Shaking off`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline

shared1 <- ggplot(obs, aes(group = Week, x = Week, y = `Shared grooming`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline
    
shared2 <- ggplot(obs, aes(group = Week, x = Week, y = `Aggression`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline

shared3 <- ggplot(obs, aes(group = Week, x = Week, y = `Getting nearer`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline

shared_eat1 <- ggplot(obs, aes(group = Week, x = Week, y = `Begging`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline
    
shared_eat2 <- ggplot(obs, aes(group = Week, x = Week, y = `Chasing and begging`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline
	
shared_eat3 <- ggplot(obs, aes(group = Week, x = Week, y = `Feeding`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline

shared_eat4 <- ggplot(obs, aes(group = Week, x = Week, y = `Running away`, fill = Music)) + 
	geom_point(aes(color = Music), size = 2, shape = 21, show.legend = FALSE) +
	stat_summary(aes(color = Music), fun = median, fun.min = median, fun.max = median, geom = "crossbar", width = 1, show.legend = FALSE) +
	stat_boxplot(aes(color = Music), geom = "errorbar", width = 0.75, show.legend = FALSE) +
	compline + theme_bw() + themeline


act_fig <- act1 + act2 + act3
ggsave("activity.pdf", width = 9, height = 5) 

react_fig <- react1 + react2 + react3 + react4 + react5
ggsave("reaction.pdf", width = 9, height = 10) 

vocal_fig <- vocal1 + vocal2 + vocal3 + vocal4 + vocal5
ggsave("vocalisation.pdf", width = 9, height = 10) 

food_fig <- food1 + food2 + food3 + food4 + food5
ggsave("food.pdf", width = 9, height = 10) 

auto_fig <- auto1 + auto2 + auto3 + auto4 + auto5
ggsave("autobehavior.pdf", width = 9, height = 10)

explo_fig <- explo1 + explo2 +explo3
ggsave("exploration.pdf", width = 9, height = 5) 

shared_fig <- shared1 + shared2 + shared3
ggsave("shared.pdf", width = 9, height = 5) 

shared_eat_fig <- shared_eat1 + shared_eat2 + shared_eat3 + shared_eat4
ggsave("shared_food.pdf", width = 9, height = 10)

