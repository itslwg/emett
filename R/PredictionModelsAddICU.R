#' Add ICU outcome function
#'
#' Author: Celina Lindgren
#' @param study_data study data as a data frame
#' @importFrom magrittr "%>%"
#' @export
PredictionModelsAddICU <- function(study_data) {
    ## Create new columns, ed_admit and icu_admit for combined time and date of arrivial
    study_data <- study_data %>%
        dplyr::mutate(ed_admit = paste0(doar, toar)) %>%
        dplyr::mutate(icu_admit = paste0(daicu, taicu))
    ## Transform columns into POSIXct object to handle as dates
    study_data$ed_admit <- as.POSIXct(study_data$ed_admit, format = "%Y-%m-%d %H:%M")
    study_data$icu_admit <- as.POSIXct(study_data$icu_admit, format = "%Y-%m-%d %H:%M")
    ## Create final columns with time to ICU admission and binary outcome if admitted < 48 h
    study_data <- study_data %>%
        dplyr::mutate(time_to_icu = difftime(icu_admit, ed_admit, units = "hours")) %>%
        dplyr::mutate(icu48h = difftime(icu_admit, ed_admit, units = "hours"))
    ## Change all who were admitted within 48h to 1, and the rest to 0
    study_data$icu48h <- ifelse(study_data$icu48h <= 48, 1, 0)
    ## Convert NA values in icu48h to those of daicu/taicu
    study_data$icu48h <- with(study_data, ifelse(daicu == "0" | taicu == "0", 0, icu48h ))
    study_data$icu48h <- with(study_data, ifelse(daicu == "NA" | taicu == "NA", NA, icu48h))
    ## Drop date and time variables, keep doar toar for major surgery function
    study_data <- within(study_data, rm("daicu", "taicu", "ed_admit", "icu_admit", "time_to_icu"))
    return(study_data)
}
