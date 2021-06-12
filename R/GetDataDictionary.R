#' GetDataDictionary
#'
#' Reads the data dictionary from a .csv file and returns it as a list
#' @param data.path The data.path to the data dictionary. Defaults to "extdata/".
#' @param file_name The name of the data dictionary file. Defaults to "data_dictionary.csv".
#' @export
GetDataDictionary <- function(data.path = "./extdata/", file.name = "data_dictionary.csv") {
    ## Error handling
    if (!bengaltiger::IsLength1(data.path) | !bengaltiger::IsLength1(file.name))
        stop("Arguments data.path and file.name must be of type character.")
    ## Paste data.path and filename to form a full data path
    fp <- paste0(data.path, file.name)
    ## Get data dictionary
    data.dictionary <- data.table::fread(fp, data.table = FALSE)
    ## Modify data dictionary column names
    data.dictionary.names <- names(data.dictionary) # Store current dictionary names
    new.names <- unlist(lapply(data.dictionary.names,
                               function(x) unlist(strsplit(x, "\\(|\\)"))[2])) # Get short names
    names(data.dictionary) <- new.names # Apply names
    ## Make a list of the data dictionary
    data.dictionary.list <- lapply(setNames(nm = data.dictionary$vn),
                                   function(x) as.list(data.dictionary[data.dictionary$vn == x, ]))
    return(data.dictionary.list)
}
