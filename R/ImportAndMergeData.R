#' ImportAndMergeData
#'
#' Import both the ISS data and coded data, merges and returns the analysis dataset.
#' @export
ImportAndMergeData <- function() {
    ## Error handling
    dfs.nms <- c("sample_iss_18012020.csv", "ttris_nomesco_snomed.csv")
    dfs <- lapply(setNames(dfs.nms, nm = c("iss", "coded")), function(nm) {
        df <- bengaltiger::ImportStudyData(data.path = "../data/",
                                           data.file.name = nm)
        return (unique(df))
    })
    dfs$iss <- dfs$iss %>%
        dplyr::select("pid", "centre", "iss")
    m <- dplyr::inner_join(dfs$iss, dfs$coded)

    return (m)
}
