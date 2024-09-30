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

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

### Code
n <- length (na.omit (y))
t90 <- qt((1 - 0.90) / 2, df = n - 1, lower.tail = FALSE)
sample_mean <- mean(y, na.rm = TRUE )
sample_sd <- sd (y, na.rm = TRUE )
lower_90 <- sample_mean - (t90 * (sample_sd/sqrt(n)))
upper_90 <- sample_mean + (t90 * (sample_sd/sqrt(n)))
confint90 <- c ( lower_90 , upper_90)
confint90

### Results are: 93.95993 102.92007
### 2nd Question
n
y
t.test(y, mu = 100, alternative = "greater")


#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)

### Exploring the "expenditure" dataset
View(expenditure)
head(expenditure)
str(expenditure)
summary(expenditure)

### 1st Question
pdf("plot_1.pdf")
plot(expenditure$X1,
     expenditure$Y,
     xlab="Per capita personal income in state",
     ylab="Per capita expenditure on housing assistance")
text(1400, 110, sprintf("Correlation=%s", round(cor(expenditure$Y,expenditure$X1), 4)))
dev.off()

pdf("plot_2.pdf")
plot(expenditure$X2,
     expenditure$Y,
     xlab="Number of financially insecure residents (per 100k)",
     ylab="Per capita expenditure on housing assistance")
text(200, 120, sprintf("Correlation=%s", round(cor(expenditure$Y,expenditure$X2), 4)))
dev.off()

pdf("plot_3.pdf")
plot(expenditure$X3,
     expenditure$Y,
     xlab="Number of people residing in urban areas (per 1k)",
     ylab="Per capita expenditure on housing assistance")
text(450, 120, sprintf("Correlation=%s", round(cor(expenditure$Y,expenditure$X3), 4)))
dev.off()

pdf("plot_4.pdf")
plot(expenditure$X1,
     expenditure$X2,
     xlab="Per capita personal income in state",
     ylab="Number of financially insecure residents (per 100k)")
text(1300, 500, sprintf("Correlation=%s", round(cor(expenditure$X1,expenditure$X2), 4)))
dev.off()

pdf("plot_5.pdf")
plot(expenditure$X1,
     expenditure$X3,
     xlab="Per capita personal income in state",
     ylab="Number of people residing in urban areas (per 1k)")
text(1300, 800, sprintf("Correlation=%s", round(cor(expenditure$X1,expenditure$X3), 4)))
dev.off()

pdf("plot_6.pdf")
plot(expenditure$X2,
     expenditure$X3,
     xlab="Number of financially insecure residents (per 100k)",
     ylab="Number of people residing in urban areas (per 1k)")
text(250, 800, sprintf("Correlation=%s", round(cor(expenditure$X2,expenditure$X3), 4)))
dev.off()

# We create a matrix to better visualise the correlations between variables
library(corrplot)

library(GGally)
pdf("plot_7.pdf")
ex_mat <- data.frame(expenditure$Y, expenditure$X1, expenditure$X2, expenditure$X3)
cor_matrix <- cor(ex_mat)
cor_matrix
ggpairs(ex_mat)
dev.off()

library(ggplot2)

ggplot(expenditure, aes(x = X3, y = Y, label = STATE)) +
  geom_point() +
  geom_text(aes(label=STATE), hjust=0, vjust=0, size = 2.5) +
  labs(x = "Number of people residing in urban areas (per 1k)",
       y = "Per capita expenditure on housing assistance",
       title = "The relationship between num. of people residing in urban areas and housing assistance")

### 2nd Question
pdf("plot_8.pdf")
boxplot(Y ~ Region, 
        data = expenditure,
        xlab = "Region", 
        ylab = "Per capita expenditure on housing assistance",
        col = "lightblue")
dev.off()

### 3rd Question
pdf("PLOT_9.pdf")
plot(expenditure$X1,
     expenditure$Y,     
     col=expenditure$Region,
     xlab="Per capita personal income in state",
     ylab="Per capita expenditure on housing assistance in state")

legend("topleft",
       legend=c("South", "Northeast", "North Central", "West"),
       col=c("green", "black","red", "blue"),
       pch=1) # Marker type (1 is default)
dev.off()

# Or
expenditure$Region_names <- factor(expenditure$Region,
                             levels = c(1, 2, 3, 4),
                             labels = c("Northeast", "North Central", "South", "West"))

region_colors <- c("Northeast" = "blue", 
                   "North Central" = "green", 
                   "South" = "red", 
                   "West" = "orange")

pdf("plot_10.pdf")
ggplot(expenditure, aes(x = X1, y = Y, color = Region_names, shape = Region_names)) +
  geom_point(size = 2) +
  geom_text(aes(label = STATE), hjust = 0, vjust = 0, size = 2.5) +
  scale_color_manual(values = region_colors) +
  scale_shape_manual(values = c(16, 17, 18, 19)) +
  labs(x = "Per capita personal income in state",
       y = "Per capita expenditure on housing assistance in state",
       color = "Region",
       shape = "Region") +
  theme_minimal()
dev.off()
