remove(list=ls())
#Load data
Mark <- read.csv("D:/Simon.UR/Fall B/Analytics Design and Application/evals_data.csv")
StudentInfo <-read.csv("D:/Simon.UR/Fall B/Analytics Design and Application/student_data.csv")
#Data cleaning
StudentInfo <- StudentInfo[,1:2]
#combine district columns
Mark <- merge(Mark, StudentInfo, by="student_id")

#sub-district student info
Beaveton <- StudentInfo[StudentInfo$district == "Beaveton", ]
Sherwood <- StudentInfo[StudentInfo$district == "Sherwood", ]
LakeOswego <- StudentInfo[StudentInfo$district == "Lake Oswego", ]
StPaul <- StudentInfo[StudentInfo$district == "St. Paul", ]
Wilsonville <- StudentInfo[StudentInfo$district == "Wilsonville", ]
Camas <- StudentInfo[StudentInfo$district == "Camas", ]
Riverdale <- StudentInfo[StudentInfo$district == "Riverdale", ]
Ridgefield <- StudentInfo[StudentInfo$district == "Ridgefield", ]

#Refresh
RefreshOnline <- Mark[Mark$program == "refresh" & Mark$location == "online",]
RefreshCenter <- Mark[Mark$program == "refresh" & Mark$location == "center",]



#Siqi cleaning data for tutoring
calculate_differences <- function(data) {
    data <- data %>%
        arrange(student_id, date) %>%
        mutate(
            diff_reading = ifelse(lag(student_id) == student_id, score_reading - lag(score_reading), NA),
            diff_writing = ifelse(lag(student_id) == student_id, score_writing - lag(score_writing), NA),
            diff_mathNoCalc = ifelse(lag(student_id) == student_id, score_mathNoCalc - lag(score_mathNoCalc), NA),
            diff_mathCalc = ifelse(lag(student_id) == student_id, score_mathCalc - lag(score_mathCalc), NA)
        )
    return(data)
}
data_with_diffs <- calculate_differences(data)
head(data_with_diffs)
tutoring_data <- subset(data_with_diffs, data_with_diffs$program=="tutoring", TRUE)

