library(dplyr)
library(tidyr)

Student <- c("John Davis", "Angela Williams", "Bullwinkle Moose",
             "David Jones", "Janice Markhammer", "Cheryl Cushing",
             "Reuven Ytzrhak", "Greg Knox", "Joel England",
             "Mary Rayburn")
Math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
Science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
English <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)

df <- data.frame(Student, Math, Science, English)

df <- df %>% 
    separate(Student, into = c("Firstname", "Lastname") ,sep = " ") %>% 
    arrange(Lastname, Firstname)


df <- df %>% mutate(score = (scale(Math) + scale(Science) + scale(English))/3)



score_breaks <- quantile(df$score, probs = c(.2,.4,.6,.8))
score_breaks <- c(-Inf, score_breaks, +Inf)

grade <- cut(df$score, breaks = score_breaks, 
             labels = c("F", "D", "C", "B", "A"))

df$grade <- grade
