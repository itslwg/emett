#' Setup
#'
#' Setup the parameters for modelling, the study description and Initialize the study
#' @export
Setup <- function() {
    library(devtools)
    devtools::load_all()
    n.bootstrap.samples <- 10
    n.partitions <- 3
    study.name <- "SuperLearner vs Clinicians"
    authors <- c("Martin Gerdin Wärnberg", "Ludvig Wärnberg Gerdin")
    description <- "A comparison of triage conducted by clinicians and the ensemble algorithm SuperLearner."
    relevant.variables <- c("age", "moi", "sex", "mot", "tran", "s30d", "egcs", "mgcs", "vgcs", "avpu", "hr",
                            "sbp", "dbp", "spo2", "rr", "tc", "ic", "doar", "toar", "doi", "toi", "s24h", "hd",
                            "taicu", "daicu", "tos", "dos", "nomeco", "snomed", "s", "iss")
    ## Initialize a bengaltiger study, i.e. create a directory structure and study template
    ## bengaltiger::Init(study.name = study.name, authors = authors, description = description,
    ##                   functions.to.include = c("ImportStudyData", "FetchAndSetData"))
    data <- FetchAndSetData(relevant.variables)
    
}
