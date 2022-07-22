# ===============================================================
# RESAMPLING STATISTICS AND BOOTSTRAPPING
# ===============================================================
library(coin)
library(lmPerm)
library(dplyr)
library(tidyr)
library(ggplot2)

#===============================================================
# Independent two-sample and k-sample tests
# library(coin)
score <- c(40, 57, 45, 55, 58, 57, 64, 55, 62, 65)
treatment <- factor(c(rep("A",5), rep("B",5)))
mydata <- data.frame(treatment, score)

# checking the regular t test
t.test(score~treatment, data=mydata, var.equal=FALSE)


oneway_test(score~treatment, data=mydata, distribution= "exact")
oneway.test(score~treatment, data=mydata, var.equal = FALSE)

# ================================================================
# Bootstrapping
#=================================================================

# making botstrap sampling of mtcars data

rsq <- function(formula, data, indices) {
    d <- data[indices,]
    fit <- lm(formula, data=d)
    return(summary(fit)$r.square)
}

library(boot)
set.seed(1234)
results <- boot(data=mtcars, statistic=rsq,
                R=1000, formula=mpg~wt+disp)

print(results)

plot(results)














