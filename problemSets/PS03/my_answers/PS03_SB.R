#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/incumbents_subset.csv")

#########################################
# PROBLEM 1
#########################################
library(ggplot2)
library(stargazer)

summary(inc.sub$difflog)
summary(inc.sub$voteshare)
plot(inc.sub$difflog)
plot(inc.sub$voteshare)
hist(inc.sub$difflog)
hist(inc.sub$voteshare)

scatter <- ggplot(data = inc.sub, 
                  mapping = aes (x = difflog,
                                 y = voteshare),
                                size = sec_enrol) + 
  geom_point()  +
  labs(x = "Campaign Spending (log)",
       y = "Vote Share") +
  theme_classic() +
  theme(legend.box.background = element_rect(size = 0.1),
        legend.position = c(0.85, 0.85))
ggsave("scatter.png", width = 10, height = 6)
scatter

m1 <- lm(voteshare ~ difflog, data= inc.sub)
stargazer(m1, type = "latex")
?stargazer

#######QUESTION 2###################
scatter2 <- ggplot(data = inc.sub, 
                   mapping = aes(x = difflog, 
                                 y = voteshare)) + 
  geom_point(aes()) +
  geom_smooth(method = "lm", se = TRUE) + 
  labs(x = "Campaign Spending (log)",
       y = "Vote Share") +
  theme_classic() +
  theme(legend.box.background = element_rect(size = 0.1),
        legend.position = c(0.85, 0.85))
ggsave("scatter2.png", width = 10, height = 6)
scatter2

#######QUESTION 3#################
res1 <- residuals(m1)
res1


#################################
#PROBLEM1
summary(inc.sub$presvote)
h2 <- hist(inc.sub$presvote)

m2 <- lm(presvote ~ difflog, data = inc.sub)
stargazer(m2, type="latex")

#######QUESTION2###############
scatter3 <- ggplot(data = inc.sub, 
                   mapping = aes(x = difflog, 
                                 y = presvote)) + 
  geom_point(aes()) +
  geom_smooth(method = "lm", se = TRUE) + 
  labs(x = "Campaign Spending (log)",
       y = "Vote Share of Pres. Cand.") +
  theme_classic() +
  theme(legend.box.background = element_rect(size = 0.1),
        legend.position = c(0.85, 0.85))
ggsave("scatter3.png", width = 10, height = 6)
scatter3

#######QUESTION 3#################
res2 <- residuals(m2)
res2

##################################
#PROBLEM 3
##################################
#Question 1
m3 <- lm(voteshare ~ presvote, data=inc.sub)
stargazer(m3, type="latex")

#Question 3
scatter4 <- ggplot(data = inc.sub, 
                   mapping = aes(x = presvote, 
                                 y = voteshare)) + 
  geom_point(aes()) +
  geom_smooth(method = "lm", se = TRUE) + 
  labs(x = "Vote Share of Pres. Cand.",
       y = "Vote Share Incument Party") +
  theme_classic() +
  theme(legend.box.background = element_rect(size = 0.1),
        legend.position = c(0.85, 0.85))
ggsave("scatter4.png", width = 10, height = 6)
scatter4

###################################☺
#PROBLEM 4
##################################
#Question 1
m5 <- lm(res1 ~ res2)
stargazer(m5, type = "latex")

scatter5 <- ggplot(mapping = aes(x = res2, y = res1)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Residuals Q2", y = "Residuals Q1") +
  theme_classic() +
  theme(legend.box.background = element_rect(size = 0.1),
        legend.position = c(0.85, 0.85))
ggsave("scatter5.png", width = 10, height = 6)
scatter5

######################################
#PROBLEM 5
######################################
#Question 1
m6 <- lm(voteshare ~ difflog + presvote, data = inc.sub)
stargazer (m6, type = "latex")
