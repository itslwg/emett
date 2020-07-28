#' Collapse mechanism of injury
#'
#' Takes the main data as input and generates a new mechanism of injury variable with overall levels instead of codes.
#' @param study.sample Data frame. The study sample. No default.
#' @export
CollapseMechanismOfInjury <- function(study.sample) {
    ## Test that study data if a data frame, stop if not
    if (!is.data.frame(study.sample)) stop("Argument study.sample is not a data frame.")
    ## Transport accidents are V01-V99, falls are W00-W19, burns are X00-X19, other
    ## external causes of accidental injury are W20-W99 and X20-X59, intentional
    ## self-harm is X60-X84, assault is X85-Y09, events of undetermined intent is
    ## Y10-Y34, and legal intervention is Y35-Y36
    ## List of lists, in which each sub-list holds two vectors. The first vector is
    ## called codes and has the codes used to define a certain mechanism and the
    ## second is called level and has the name of that mechanism
    below.ten <- paste0(0, 0:9)
    moi.list <- list(ta = list(codes = paste0("V", c(below.ten, 10:99)),
                               level = "Transportation accident"),
                     falls = list(codes = paste0("W", c(below.ten, 10:19)),
                                  level = "Fall"),
                     burns = list(codes = paste0("X", c(below.ten, 10:19)),
                                  level = "Burn"),
                     oeca = list(codes = c(paste0("W", 20:99), paste0("X", 20:59)),
                                 level = "Other external cause of accidental injury"),
                     ish = list(codes = paste0("X", 60:84),
                                level = "Intentional self harm"),
                     assault = list(codes = c(paste0("X", 85:99), paste0("Y", below.ten)),
                                    level = "Assault"),
                     ui = list(codes = paste0("Y", 10:34),
                               level = "Event of undetermined intent"),
                     li = list(codes = paste0("Y", 35:36),
                               level  = "Legal intervention"))
    new.moi <- as.character(study.sample$moi) # make a new character vector of moi
    ## Replace codes with levels in the new copy
    for (m in moi.list)
        new.moi[grep(paste0(m$codes, collapse = "|"), study.sample$moi)] <- m$level
    ## Replace old moi with new, factor moi in dataset
    study.sample$moi <- as.factor(new.moi)
    return (study.sample)
}
