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
skills_data <- subset(data_with_diffs, data_with_diffs$program=="skills", TRUE)