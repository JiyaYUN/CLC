remove(list = ls())

# Load Data
Mark <- read.csv("D:/Simon.UR/Fall B/Analytics Design and Application/evals_experiment.csv")
Student <- read.csv("D:/Simon.UR/Fall B/Analytics Design and Application/students_experiment.csv")
Mark <- merge(Mark, Student, by="student_id")

summary(Mark)
unique(Mark$district)

for (i in 1:nrow(Mark)) {
     if ("St. Paul" %in% Mark$district[i]) {
          Mark$EducationalProgram[i] <- "Current"
     } else if ("Beaverton" %in% Mark$district[i]) {
          Mark$EducationalProgram[i] <- "Current"
     } else if ("Lake Oswego" %in% Mark$district[i]) {
          Mark$EducationalProgram[i] <- "A"
     } else if ("Ridgefield" %in% Mark$district[i]) {
          Mark$EducationalProgram[i] <- "A"
     } else if ("Camas" %in% Mark$district[i]) {
          Mark$EducationalProgram[i] <- "B"
     } else if ("Sherwood" %in% Mark$district[i]) {
          Mark$EducationalProgram[i] <- "B"
     } else if ("Riverdale" %in% Mark$district[i]) {
          Mark$EducationalProgram[i] <- "C"
     } else {
          Mark$EducationalProgram[i] <- "C"
     }
}

library(dplyr)

MarkDiff <- function(data) {
     data <- data %>%
          mutate(
               score_reading = ifelse(lag(student_id) == student_id, score_reading - lag(score_reading), NA),
               score_writing = ifelse(lag(student_id) == student_id, score_writing - lag(score_writing), NA),
               score_mathNoCalc = ifelse(lag(student_id) == student_id, score_mathNoCalc - lag(score_mathNoCalc), NA),
               score_mathCalc = ifelse(lag(student_id) == student_id, score_mathCalc - lag(score_mathCalc), NA),
               score_total = score_reading + score_writing + score_mathNoCalc + score_mathCalc
          )
     
     return(data)
}
Mark <- MarkDiff(Mark)
Mark <- Mark %>%
     filter(program != "intake")

summary(aov(score_total ~ EducationalProgram, data = Mark))

boxplot(score_total ~ EducationalProgram, data = Mark,
        col = c("lightgreen", "lightblue", "orange", "red"),
        main = "ANOVA Boxplot of Diffscore in 3 Educational Program")



Mark$EducationalProgram <- factor(Mark$EducationalProgram)
Mark$EducationalProgram <- relevel(Mark$EducationalProgram, ref = "Current")

#total score difference
summary(lm(score_total ~ EducationalProgram, data = Mark))

#reading score difference
summary(lm(score_reading ~ EducationalProgram, data = Mark))

#writing score difference
summary(lm(score_writing ~ EducationalProgram, data = Mark))

#mathNoCalc  score difference
summary(lm(score_mathNoCalc ~ EducationalProgram, data = Mark))

#mathCalc score difference
summary(lm(score_mathCalc ~ EducationalProgram, data = Mark))

