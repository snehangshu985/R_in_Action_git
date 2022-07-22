setwd("F:/R_in_Action")
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)

# ==============================================================
# Fitting ANOVA models
# ==============================================================
# One-way ANOVA
library(multcomp)
df <- cholesterol
table(df$trt)
df %>% group_by(trt) %>% summarise(std = sd(response))

fit_anova <- aov(response ~ trt, data = df)
summary(fit_anova)
# fitting the linear model
fit_lm1 <- lm(response ~ trt - 1, data = df)
fit_lm2 <- lm(response ~ trt, data = df)
fit_

fit_lm1$coefficients
fit_lm2$coefficients

# group_effect <- fit_lm$coefficients
# 
# group_effect_int <- confint(fit_lm)
# 
# df_lm <- cbind(group_effect, group_effect_int)
# 
# colnames(df_lm) <- c("y", "y_min", "y_max")
# 
# df_lm <- as.data.frame(df_lm)
# 
# df_lm$y_minus <- df_lm$y_min - df_lm$y 
# 
# df_lm$y_plus <- df_lm$y_max - df_lm$y
# df_lm <- df_lm[, c("y", "y_minus", "y_plus")]
# 
# df_lm$y <- cumsum(df_lm$y)
# df_lm$y_min <- df_lm$y + df_lm$y_minus
# df_lm$y_max <- df_lm$y + df_lm$y_plus
# 
# df_lm <- df_lm[, c("y", "y_min", "y_max")]     
#     
# df_lm$myFactor <- rownames(df_lm)
# 
# rownames(df_lm) <- NULL
# df_lm$myFactor <- as.factor(df_lm$myFactor)
# 
# ggplot(df_lm, aes(myFactor,  y, group = 1)) + geom_point() + geom_line() + 
#     geom_ribbon(aes(ymin = y_min, ymax= y_max), fill = "grey", alpha = 0.5)


# ==============================================================================

mydf <- ToothGrowth

dfA <- mydf %>% group_by(dose, supp) %>% 
    summarise(Avg_grow = mean(len), Std = sd(len),Count = n())

ggplot(data = dfA, aes(dose, Avg_grow)) + geom_line(aes(color = supp))

ggplot(mydf, aes(factor(dose), len)) + 
    geom_boxplot(aes(color = factor(dose)), show.legend = F)
    
ggplot(mydf, aes(supp, len)) + 
    geom_boxplot(aes(color = supp), show.legend = F)


#=====================================================================
# ANOVA as regression
# ====================================================================
library(multcomp)
df <- cholesterol
fit_aov <- aov(response ~ trt, data=df)
summary(fit_aov)

fit_lm <- lm(response ~ trt, data=df)
summary(fit_lm)

fit_lm2 <- lm(response ~ trt -1, data=df)
summary(fit_lm2)

















    