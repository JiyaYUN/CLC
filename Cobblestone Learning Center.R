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

