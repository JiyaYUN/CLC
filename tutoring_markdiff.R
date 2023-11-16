install.packages("utils")
library(utils)
MarkDiffTotal <- read.csv("~/Desktop/MarkDiffTotal.csv")
   View(MarkDiffTotal)
tutoring_data <- MarkDiffTotal[MarkDiffTotal$program == "tutoring", ]
average_diff_reading <- mean(tutoring_data$diff_reading, na.rm = TRUE)
average_diff_writing <- mean(tutoring_data$diff_writing, na.rm = TRUE)
average_diff_mathNoCalc <- mean(tutoring_data$diff_mathNoCalc, na.rm = TRUE)
average_diff_mathCalc <- mean(tutoring_data$diff_mathCalc, na.rm = TRUE)
 
 average_diffs <- c(AverageDiffReading = average_diff_reading,
                                            AverageDiffWriting = average_diff_writing,
                                            AverageDiffMathNoCalc = average_diff_mathNoCalc,
                                            AverageDiffMathCalc = average_diff_mathCalc)
 

print(average_diffs)