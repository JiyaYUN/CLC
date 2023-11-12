remove(list=ls())
library(dplyr)

# Load data
Mark <- read.csv("D:/Simon.UR/Fall B/Analytics Design and Application/evals_data.csv")
StudentInfo <- read.csv("D:/Simon.UR/Fall B/Analytics Design and Application/student_data.csv")

# Data cleaning
StudentInfo <- StudentInfo[,1:2]

# Combine district columns
Mark <- merge(Mark, StudentInfo, by="student_id")

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
