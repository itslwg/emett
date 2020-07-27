#' Add iss15 function
#' 
#' This function uses iss to create a binary column of iss > 15
#'
#' Author: Celina Lindgren
#' @param study_data The study dataframe.
#' @export
PredictionModelsAddISS <- function(study_data) {
    ## Add column iss15 for alla observations with iss > 15
    study_data <- study_data %>% 
        dplyr::mutate(iss15 = ifelse(iss >= 15, 1, 0))
    
    ## Fix missing values
    study_data$iss15 <- with(study_data, ifelse(is.na(iss), NA, study_data$iss15)) # if missing in iss, also missing in iss15
   
    ## Drop iss column
    study_data$iss <- NULL

    return (study_data)
}
