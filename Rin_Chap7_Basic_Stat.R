# Loadint the required library
library(dplyr)
library(tidyr)
library(ggplot2)
library(Hmisc)
library(psych)

# setting the working Directory
setwd("F:/R_in_Action")

# the required darta set

df <- mtcars
df2 <- df[myvars]
myvars <- c("mpg", "hp", "wt")
# Doing basic EDA
summary(df[myvars])

# creating function for skewness and kurtosis

mystats <- function(x, na.omit=FALSE){
    if (na.omit)
        x <- x[!is.na(x)]
    m <- mean(x)
    n <- length(x)
    s <- sd(x)
    skew <- sum((x-m)^3/s^3)/n
    kurt <- sum((x-m)^4/s^4)/n - 3
    return(c(n=n, mean=m, stdev=s, skew=skew, kurtosis=kurt))
}




# calculate the skewness and kurtosis
sapply(df[myvars], mystats)

# descriptive statistics by Hmisc and psych package
Hmisc::describe(df[,myvars])


t(Hmisc::describe(df[,myvars]))

# group and view summary stats
df %>% group_by(am) %>% summarise(mean)

aggregate(df, by = list(am=df$am), mean, sd)

# make contigency table
#install.packages("vcd")
library(vcd)

df3 <- Arthritis # data for arithmatic treatments

# checking improvement vs treatment(original vs placebo)
mytable <- table(df3$Treatment, df3$Improved)
mytable2 <- addmargins(mytable)
# giving proportion table with margin

marginalTable <- addmargins(prop.table(mytable))

chisq.test(mytable)

# checking improvement vs gender

mytable2 <- table(df3$Sex, df3$Improved)

marginalTable2 <- addmargins(prop.table(mytable2))

chisq.test(mytable2)


round(marginalTable2, 2)
# taking only the actual treated data
df4 <- filter(df3, Treatment=="Treated")
# creating the contigency table
mytable3 <- table(df4$Sex, df4$Improved)
# taking the chi sqr indpendence test
# H_null is there is no relation between Gender and effect of treatment

chisq.test(mytable3)
# the p-value is 0.1884
# the test fail to reject the null hypothesis
# There is no relation between gender and treatment outcome
# they are independent
chisq.test(mytable2)


#############################################

# Correlations

# Data set is 
df5 <- as.data.frame(state.x77)
sd_value <- lapply(df5, sd)
cv_matrix <- cov(df5)
cv_matrix <- as.data.frame(cv_matrix)
cor_matrix <- as.data.frame(round(cor(df5), 2))
cor_matrix2 <- matrix(nrow = length(sd_value), ncol=length(sd_value))

for(i in 1:length(sd_value)){
    for(j in 1:length(sd_value)){
        cor_matrix2[i,j] = cv_matrix[i,j]/(sd_value[[i]]*sd_value[[j]])
        
        
    }
}

cor_matrix2 <- round(cor_matrix2, 2)

cor_matrix2 <- as.data.frame(cor_matrix2)

library(GGally)
ggpairs(df5)

########################################

# Hypothesis testing using t.test

library(MASS)
# testing the US crime data from MASS package
# we are interested in testing probability of imprisonment 
# whether related with southern or non southern states in US


sothern0 <- UScrime %>% filter(So == 0) # filtering the non southern states

sothern1 <- UScrime %>% filter(So == 1) # the southern states
# testing the mean of probability of two groups
test1 <- t.test(sothern0$Prob, sothern1$Prob)

# the same result can be obtain using the below code
# without filtering out the groups
test2 <- t.test(Prob~So, data = UScrime)

test1$p.value
test2$p.value

# mathermatical way to the test
x0_bar <- mean(sothern0$Prob)
x1_bar <- mean(sothern1$Prob)
diff_in_mean <- x1_bar - x0_bar 

sesqr <- var(sothern0$Prob)/length(sothern0$Prob) +  var(sothern1$Prob)/length(sothern1$Prob)

se <- sqrt(sesqr) 

my_qt <- diff_in_mean/se

my_pval <- 2*pt(my_qt, lower.tail = F, df= 25)    

test1$p.value


    