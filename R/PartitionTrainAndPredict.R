#' Partition Train And Predict
#'
#' Partitions the study sample, fits the model and makes predictions with SuperLearner.
#' @param study.sample Data frame. The study.sample. No default.
#' @param outcome.variable.name Character vector of length 1. The name of the outcome variable of interest. Defaults to "s30d".
#' @param n.partitions Numeric vector of length 1. The number of partitions to create with PartitionSample. Accepted values are 2 or 3. If 2, a train and test set is created. If 3, train, validation, and test sets are created - the models is fitted on the training set, optimal breaks is gridsearched on the validation set, and the model is tested on the test set. Defaults to 2. 
#' @param models.names Character vector. The model names to stack in SuperLearner. Defaults to c("SL.gam", "SL.randomForest", "SL.nnet, SL.xgboost", "SL.svm")
#' @param save.to.results Logical. If TRUE SuperLearner predictions, outcome and tc in each partition is saved to the Results list. Defaults to TRUE.
#' @param verbose Logical. If TRUE the modelling process is printed to console. Defaults to FALSE.
#' @param return.samples Logical vector of length 1. If TRUE the list of samples partitioned from the study.sample is returned. Defaults to TRUE.
#' @param use.fitted.sl Logical vector of length 1. If TRUE the file. Default/No default. 
#' @export
PartitionTrainAndPredict <- function(study.sample,
                                     outcome.variable.name = "s30d",
                                     model.names = c("SL.randomForest"),
                                     n.partitions = 2,
                                     save.to.results = TRUE,
                                     boot = FALSE,
                                     verbose = FALSE,
                                     return.samples = TRUE,
                                     use.fitted.sl = FALSE){
    ## Error handling
    if (!is.data.frame(study.sample))
        stop ("data must be of type data frame")
    if (!bengaltiger::IsLength1(outcome.variable.name) | !is.character(outcome.variable.name))
        stop ("outome.variable.name must be of a character vector of length 1")     
    ## Partition the sample, and return the separate partitions, the corresponding outcome
    ## for both sets and tc in both sets
    partitions <- PartitionSample(study.sample = study.sample,
                                  outcome.variable.name = outcome.variable.name,
                                  n.partitions = n.partitions)
    fitted.sl <- ""
    sl.object.file <- paste0("SuperLearner_", outcome.variable.name, ".rds")
    ## Fit the model to the training data
    if (use.fitted.sl) {
        if (file.exists(sl.object.file)) {
            if (verbose)
                message(paste0("Fetching ", sl.object.file, "..."))
            fitted.sl <- readRDS(sl.object.file)
        } else {
            if (verbose) {
                message(paste("No", sl.object.file, "object have been saved to disk. Ignoring use.fitted.sl."))
                message("Fitting SuperLearner...")
            }
            fitted.sl <- with(partitions, SuperLearner::SuperLearner(Y = train$y, X = train$x,
                                                                     family = binomial(),
                                                                     SL.library = model.names,
                                                                     method = "method.AUC",
                                                                     verbose = FALSE))  
        }
    } else {
        message("Fitting SuperLearner...")        
        fitted.sl <- with(partitions, SuperLearner::SuperLearner(Y = train$y, X = train$x,
                                                                 family = binomial(),
                                                                 SL.library = model.names,
                                                                 method = "method.AUC",
                                                                 verbose = FALSE))
    }
    if (!boot | !use.fitted.sl) {
        saveRDS(fitted.sl, file = sl.object.file)
        if (verbose)
            message(paste0("SuperLearner object saved to disk as ", sl.object.file, "..."))
    }
    ## Extract training sets
    train.validation <- partitions[-grep("test", names(partitions))]
    con.list.labels <- paste0("con.model.", names(train.validation))
    ## Make predictions on the validation set
    predictions <- lapply(setNames(train.validation, nm = con.list.labels),
                          function (partition.list) predict(object = fitted.sl,
                                                            newdata = partition.list$x,
                                                            onlySL = TRUE)$pred)
    label <- ifelse(n.partitions == 2, "train", "validation")
    if (verbose)
        message(paste("Finding optimal breaks for continuous probabilities on the", label, "set..."))
    ## Gridsearch the optimal cut-points for the predicted probabilities on
    ## the appropriate set
    optimal.breaks <- GridsearchBreaks(predictions = predictions[grepl(label, con.list.labels)][[1]], 
                                       outcome.vector = partitions[[label]]$y)
    full.training.list <- list(y = unlist(lapply(train.validation, "[[", "y")),
                               x = do.call(rbind, lapply(train.validation, "[[", "x")))
    if (n.partitions == 3)
        ## Train the model once again. Now on both the training and validation sets
        fitted.sl <- with(full.training.list, SuperLearner::SuperLearner(Y = y, X = x, family = binomial(),
                                                                         SL.library = model.names,
                                                                         method = "method.AUC",
                                                                         verbose = FALSE))
    ## Make predictions on the test set
    predictions$con.model.test <- predict(object = fitted.sl,
                                          newdata = partitions$test$x,
                                          onlySL = TRUE)$pred
    ## Bin predictions made on the test set using the optimal cut-points
    cut.list.labels <- paste0("cut.model.", c("train", "validation", "test"))
    binned.predictions <- lapply(setNames(predictions, nm = cut.list.labels),
                                 function (preds) as.numeric(cut(x = preds, breaks = c(-Inf, optimal.breaks, Inf),
                                                                 labels = c("Green", "Yellow", "Orange", "Red"),
                                                                 include.lowest = TRUE)))
    NewLabelsAndNumeric <- function(label) {
        new.labels <- paste0(label, ".", names(partitions))
        new.list <- lapply(setNames(partitions, nm = new.labels),
                           function (partition.list) as.numeric(partition.list[[label]]))
        return (new.list)
    }
    return.object <- list(predictions.list = c(predictions, binned.predictions,
                                               NewLabelsAndNumeric("y"),
                                               NewLabelsAndNumeric("tc")))
    if (return.samples) {
        return.object$samples <- lapply(partitions, "[[", "x")
    }
    ## Save the predictions, outcome and clinicians tc in each partition to the results list
    if (save.to.results) {
        for (i in seq_along(return.object))
            bengaltiger::SaveToResults(return.object[[i]], names(return.object)[i])
    }
    return (return.object)
}
