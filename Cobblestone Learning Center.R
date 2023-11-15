remove(list=ls())
library(dplyr)

# Load data
Mark <- read.csv("D:/Simon.UR/Fall B/Analytics Design and Application/evals_data.csv")
StudentInfo <- read.csv("D:/Simon.UR/Fall B/Analytics Design and Application/student_data.csv")

# Data cleaning
StudentInfo <- StudentInfo[,1:2]
Mark$date <- as.Date(Mark$date, format = "%m/%d/%Y")

# Combine district columns
Mark <- merge(Mark, StudentInfo, by="student_id")

#Total Score
Mark$TotalScore <- Mark$score_reading + Mark$score_writing + Mark$score_mathNoCalc + Mark$score_mathCalc

#Intake
Intake <- Mark[Mark$program == "intake",]
summary(Intake)

hist(Intake$TotalScore, main = "Histogram of TotalScore", xlab = "TotalScore")
boxplot(Intake$TotalScore, main = "Boxplot of TotalScore", ylab = "TotalScore")
Intake <- Intake[!(Intake$TotalScore %in% boxplot.stats(Intake$TotalScore)$out), ]
write.csv(Intake, "D:/Simon.UR/Fall B/Analytics Design and Application/Intake.csv")

#Mark difference
MarkDiff <- function(data) {
     data <- data %>%
          arrange(student_id, date) %>%
          mutate(
               diff_reading = ifelse(lag(student_id) == student_id, score_reading - lag(score_reading), NA),
               diff_writing = ifelse(lag(student_id) == student_id, score_writing - lag(score_writing), NA),
               diff_mathNoCalc = ifelse(lag(student_id) == student_id, score_mathNoCalc - lag(score_mathNoCalc), NA),
               diff_mathCalc = ifelse(lag(student_id) == student_id, score_mathCalc - lag(score_mathCalc), NA),
               diff_total_score = diff_reading+diff_writing+diff_mathNoCalc+diff_mathCalc
          )
     return(data)
}

Mark <- MarkDiff(Mark)
Mark <- Mark[complete.cases(Mark),]

boxplot(Mark$diff_total_score, main = "Boxplot of TotalScore", ylab = "DiffTotalScore")
Mark <- Mark[!(Mark$diff_total_score %in% boxplot.stats(Mark$diff_total_score)$out), ]
write.csv(Mark, "D:/Simon.UR/Fall B/Analytics Design and Application/MarkDiffTotal.csv")


# Sub-district student info
Beaveton <- StudentInfo[StudentInfo$district == "Beaveton", ]
Sherwood <- StudentInfo[StudentInfo$district == "Sherwood", ]
LakeOswego <- StudentInfo[StudentInfo$district == "Lake Oswego", ]
StPaul <- StudentInfo[StudentInfo$district == "St. Paul", ]
Wilsonville <- StudentInfo[StudentInfo$district == "Wilsonville", ]
Camas <- StudentInfo[StudentInfo$district == "Camas", ]
Riverdale <- StudentInfo[StudentInfo$district == "Riverdale", ]
Ridgefield <- StudentInfo[StudentInfo$district == "Ridgefield", ]

skills <- subset(MarkDiff, MarkDIff$program=="skills", TRUE)
refresh <- subset(MarkDiff, MarkDIff$program=="refresh", TRUE)

unique(Mark$program)
