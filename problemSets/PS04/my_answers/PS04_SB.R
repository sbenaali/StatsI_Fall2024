####################
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
library(stargazer)

install.packages("car")
library(car)
data(Prestige)

#Question 1
#a
help(Prestige)
summary(Prestige$type)
Prestige$professional <- ifelse(!is.na(Prestige$type) & Prestige$type == "prof", 1, 
                            ifelse(!is.na(Prestige$type), 0, NA))
Prestige <- Prestige[!is.na(Prestige$professional), ]
summary(Prestige$professional)
is(Prestige$professional)
Prestige$professional <- factor(Prestige$professional)
summary(Prestige$professional)

#b
m1 <- lm(prestige ~ income + professional + income*professional, data = Prestige)
stargazer(m1, type = "latex")



#Question 2
#a
coefficient <- 0.042       
se <- 0.016               
t_statistic <- coefficient / se
t_statistic

df <- 128
p_value <- 2 * pt(-abs(t_statistic), df)
p_value

#b
se2 <- 0.013
t_statistic2 <- coefficient / se2
t_statistic2

p_value2 <- 2 * pt(-abs(t_statistic2), df)
p_value2
