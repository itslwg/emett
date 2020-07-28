#' AddTimeBetweenInjuryAndArrival
#'
#' Calculates the time between injury and arrival based on doi, toi, doar and toar. Add the difference to the study sample and remove the original time variables.
#' @param study.sample Data Frame. The study sample. No default.
#' @param data.dictionary List. The data dictionary. No default.
#' @export
AddTimeBetweenInjuryAndArrival <- function(study.sample, data.dictionary) {
    ## Error handling
    if (!is.data.frame(study.sample))
        stop("Study data has to be a data frame")
    ## Define required time variables
    datetime.list <- list(doi.toi = c("doi", "toi"),
                          doar.toar = c("doar", "toar"))
    required.variables <- unlist(datetime.list)
    ## Error handling
    if (!all(required.variables %in% colnames(study.sample)))
        stop("All required variables are not in study data")
    ## Modify date and time formats in data dictionary
    data.dictionary[required.variables] <- lapply(data.dictionary[required.variables], function(x) {
        x$f <- gsub("\"", "", x$f)
        return(x)
    })
    ## Reformat date and time data
    datetime.data <- lapply(datetime.list, function(dt) {
        date <- study.sample[, dt[1]] # Get current date data
        time <- study.sample[, dt[2]] # And time data
        date.format <- data.dictionary[[dt[1]]]$f # Get date format
        time.format <- data.dictionary[[dt[2]]]$f # And time format
        ## Format as datetime objects
        data <- as.POSIXct(strptime(paste(date, time),
                                    format = paste(date.format, time.format),
                                    tz = "Asia/Kolkata")) 
        return(data)
    })
    ## Calculate delay and put back in study data
    study.sample$delay <- with(datetime.data, as.numeric(difftime(doar.toar, doi.toi, units = "mins")))
    return(study.sample)
}
