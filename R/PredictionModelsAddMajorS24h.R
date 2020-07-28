#' Create major surgery < 24 hours function
#' 
#' This function create a new column in the data set for major surgery within 24 hours.
#'
#' Author: Celina Lindgren
#' @param study_data The study data frame. 
#' @export
PredictionModelsAddMajorS24h <- function(study_data) {

    ## Change to character
    study_data$nomesco <- as.character(study_data$nomesco)
    study_data$snomed <- as.character(study_data$snomed)

    ## Create column for major surgery, please enter surgery codes considered major here. Set column to TRUE if major surgery.
    study_data <- study_data %>% 
        dplyr::mutate(true_major = grepl("446816008|446683008|446115005|178674000|
                                          275093002|272300006|112777008|57470004|
                                          8476009|35340001|23036009|439756000|
                                          274457001|74011006|73231008|67319007|
                                          56413001|36777000|74770008", study_data$snomed)) 
    ## Paste dos and tos
    study_data <- study_data %>% 
        dplyr::mutate(date_time_surgery = paste(dos, tos)) %>% 
        dplyr::mutate(date_time_ed = paste(doar, toar))

    ## Change  date_time_surgery to POSIXct object
    study_data$date_time_surgery <- as.POSIXct(study_data$date_time_surgery, format = "%Y-%m-%d %H:%M")
    study_data$date_time_ed <- as.POSIXct(study_data$date_time_ed, format = "%Y-%m-%d %H:%M")

    ## Create column with time to surgery
    study_data <- study_data %>% 
        dplyr::mutate(time_to_surgery = difftime(date_time_surgery, date_time_ed, units = "hours"))

    ## Create column for major surgery < 24 h
    study_data <- study_data %>% 
        dplyr::mutate(majors24h = ifelse(true_major == "TRUE" & time_to_surgery <= 24, 1, 0))

    ## Fix missing values
    study_data$s <- as.character(study_data$s) # change s to character 
    study_data$s <- with(study_data, ifelse(dos == 0 | tos == 0, 0, s)) # if dos or tos is 0, s should also be 0
    study_data$majors24h <- with(study_data, ifelse(is.na(s), NA, study_data$majors24)) # if NA in s, majors24 should also be NA

    ## Drop irrelevant columns 
    study_data$s <- NULL
    study_data$dos <- NULL
    study_data$tos <- NULL
    study_data$doar <- NULL
    study_data$toar <- NULL
    study_data$doi <- NULL
    study_data$toi <- NULL
    study_data$snomed <- NULL
    study_data$nomesco <- NULL    
    study_data$true_major <- NULL
    study_data$time_to_surgery <- NULL
    study_data$date_time_ed <- NULL
    study_data$date_time_surgery <- NULL

    return(study_data)
}
