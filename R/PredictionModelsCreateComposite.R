#' Create composite outcome function
#' 
#' This function creates a new binary variable composite outcome
#'
#' Author: Celina Lindgren
#' @param study_data The study data frame.
#' @export
PredictionModelsCreateComposite <- function(study_data) {

    study_data <- study_data %>%
        dplyr::mutate(composite = factor(ifelse(icu48h == 1 | s24h == "Yes" | majors24h == 1 | iss15 == 1, 1, 0)))

    return (study_data)
}
