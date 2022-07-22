setwd("F:/R_in_Action")
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
#######################
# Regression
# Imorting the dataset of "women" from base R
df <- women

# there is only two variables, height & weight
# regression model to be fit to predict weight from given height

fit <- lm(weight ~ height, data=df)
# checking the fit
summary(fit)

g1 <- ggplot(df, aes(height, weight)) + 
    geom_point(colour = "blue") + 
    geom_smooth(method = "lm", colour = "red", se= F) + theme_light()
g1
# plotting the residuals
plot(fit, which = 1)
# plot suggested there is a polynomial term


# checking R-sqr manually
SSY <- var(df$weight) 

SSR <- var(fit$residuals)

my_Rsqr <- 1- SSR/SSY

# NEW lm fit with polynomial

fit2 <- lm(weight ~ height + I(height^2), data=df)

df$fit2val <- fitted(fit2)
g2 <- ggplot(df, aes(height, weight)) + 
    geom_point(colour = "blue", size=2.5) 
g2 <- g2 + geom_line(aes(height, fit2val), color = "red", size=1)
g2 <- g2 + theme_light()
g2

plot(fit2, which = 1)    

 

SSR2 <- var(fit2$residuals)

my_Rsqr2 <- 1- SSR2/SSY

y_pred <- predict(fit2, newdata = df)

df$fit2pred <- y_pred


# create test and train set

splitter <- createDataPartition(df$height, p=.50, list = F)
df_train <- df[splitter, c("height", "weight")]
df_test <- df[-splitter, c("height", "weight")]

fit_train <- lm(weight ~ height + I(height^2), data = df_train)

y_pred <- predict(fit_train, newdata = df_test)

df_test$predval <- y_pred

g3 <- ggplot(data= df_test, aes(height, weight)) +
    geom_point(color = "red", size = 2) + 
    geom_line(aes(height, predval), color = "blue", size=1.1) + 
    theme_minimal()

g3

predict(fit_train, newdata = data.frame(height = 67))

############################################################
#=========================================================
# MULTIPLE LINEAR REGRESSION
## ========================================================

states <- as.data.frame(state.x77[,c("Murder", "Frost", "Population",
                                     "Illiteracy", "Income")])

# fitting the linear model with multiple predictors
fit <- lm(Murder ~ Frost + Population + Illiteracy + Income,
          data=states)

summary(fit)

# ----------------------------------------------
## Regression with mtcars data
# -------------------------------------
fit <- lm(mpg ~ hp + wt + hp:wt, data=mtcars)

summary(fit)

df <- mtcars[, colnames(mtcars) %in% names(fit$coefficients)]

df$hpwt <- df$hp * df$wt

df$mpg <- mtcars$mpg

df <- df[, c("mpg", "hp", "wt", "hpwt")]

fit2 <- lm(mpg ~ . , data = df)
summary(fit2)

library(effects)

plot(effect("hp:wt", fit, ,list(wt=c(2.2,3.2,4.2))), multiline=TRUE)


## ===================================================#####
## Regression diagnostics ##
##=================================================######
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)

confint(fit)

se2 <- fit$coefficients[2,1]/fit$coefficients[2,2]

library(GGally)
ggpairs(data=states, 
        lower = list(continuous = wrap("smooth", method= "lm", se = F, color="blue")))

par(mfrow = c(2,2))
plot(fit)

# from data set women
fit <- lm(weight ~ height, data=women)
par(mfrow=c(2,2))
plot(fit)

fit2 <- lm(weight ~ height + I(height^2), data=women)
par(mfrow=c(2,2))
plot(fit2)

newfit <- lm(weight~ height + I(height^2), data=women[-c(13,15),])
par(mfrow=c(2,2))
plot(newfit)

# using the car library for regression diagonistics
library(car)
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)


crPlots(fit)

fit2 <- lm(Murder ~ Population + Illiteracy + Income, data=states)
fit3 <- lm(Frost ~ Population + Illiteracy + Income, data=states)

par(mfrow=c(1,1))
plot(fit2$residuals, fit3$residuals)

##============================================================
## Comparing Models
##============================================================
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])

# Model 1
fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
# model 2
fit2 <- lm(Murder ~ Population + Illiteracy, data=states)

anova(fit2, fit1)
# result
#    Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1     47 289.25                           
# 2     45 289.17  2  0.078505 0.0061 0.9939
# this result means - addition of income and frost is insignificant

# checking the cross validation using caret

fit_caret <- train(Murder ~ Population + Illiteracy,
                   data = states,
                   method = "lm")

set.seed(111)
kcv <- trainControl(method = "cv", number = 5)
fit_caret_cv <- train(Murder ~ Population + Illiteracy,
                      data = states,
                      trControl = kcv,
                      method = "lm")




