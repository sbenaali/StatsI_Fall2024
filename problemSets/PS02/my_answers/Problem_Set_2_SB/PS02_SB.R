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

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

# I'm building the given table
experiment <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, byrow = TRUE)
rownames(experiment) <- c("Upper class", "Lower class")
colnames(experiment) <- c("Not Stopped", "Bribe requested", "Stopped/given warning")
experiment

# I now calculate the expected frequencies
row_total <- rowSums(experiment) # I sum up the total rows
col_total <- colSums(experiment) # I sum up the total cols
total <- sum(experiment) # Sum up everything

f <- outer(row_total, col_total) / total
f

# Now that I have the expected frequencies, I calculate the chi-squared
chi_squared <- sum((experiment - f)^2 / f)
chi_squared

# Now I calculate the degrees of freedom
pchi <- (nrow(experiment) - 1) * (ncol(experiment) - 1)

# And now the p-value
p_value <- pchisq(chi_squared, pchi, lower.tail = FALSE)
p_value

# Calculate standardised residuals (how far are what we observe from what we expect)
stand_residuals <- (experiment - f) / sqrt(f)
stand_residuals

#####################
# Problem 2
#####################

library(stargazer)
village <- "https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv"
village <- read.csv(village)

# Bivariate regression

m1 <- lm(water ~ reserved, data = village)
summary(m1)
stargazer(m1, type = "latex")
