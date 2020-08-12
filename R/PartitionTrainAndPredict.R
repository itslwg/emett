#' Partition Train And Predict
#'
#' Partitions the study sample, fits the model and makes predictions with SuperLearner.
#' @param study.sample Data frame. The study.sample. No default.
#' @param outcome.variable.name Character vector of length 1. The name of the outcome variable of interest. Defaults to "s30d".
#' @param models.names Character vector. The model names to stack in SuperLearner. Defaults to c("SL.gam", "SL.randomForest", "SL.nnet, SL.xgboost", "SL.svm")
#' @param n.partitions Numeric vector of length 1. The number of partitions to create with PartitionSample. Accepted values are 2 or 3. If 2, a train and test set is created. If 3, train, validation, and test sets are created - the models is fitted on the training set, optimal breaks is gridsearched on the validation set, and the model is tested on the test set. Defaults to 2.
#' @param save.sample.predictions Logical. If TRUE SuperLearner predictions, outcome and tc in each partition is saved to the results list. Defaults to TRUE.
#' @param boot.sample Logical vector of length 1. If TRUE run is treated as a bootstrap sample, meaning e.g. thatthe SuperLearner object is not saved to disk. Defaults to FALSE 
#' @param verbose Logical. If TRUE the modelling process is printed to console. Defaults to FALSE.
#' @param return.samples Logical vector of length 1. If TRUE the list of samples partitioned from the study.sample is returned. Defaults to TRUE.
#' @param use.fitted.sl Logical vector of length 1. If TRUE the file. Default/No default. 
#' @export
PartitionTrainAndPredict <- function(study.sample,
                                     outcome.variable.name = "s30d",
                                     model.names = c("SL.randomForest", "SL.nnet"),
                                     n.partitions = 2,
                                     save.sample.predictions = TRUE,
                                     boot.sample = FALSE,
                                     verbose = FALSE,
                                     save.samples = TRUE,
                                     use.fitted.sl = FALSE){
    ## Error handling
    if (!is.data.frame(study.sample))
        stop ("data must be of type data frame")
    if (!bengaltiger::IsLength1(outcome.variable.name) | !is.character(outcome.variable.name))
        stop ("outome.variable.name must be of a character vector of length 1")
    if (!(n.partitions %in% c(2,3)))
        stop ("Argument n.partitions must be either 2 or 3.")
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
            message(paste0("Argument use.fitted.sl is TRUE and SuperLearner_", outcome.variable.name, " exists.", " Skipping initial fitting and using ", sl.object.file, "..."))
            fitted.sl <- readRDS(sl.object.file)
        } else {
            if (verbose) {
                message(paste("No", sl.object.file, "object have been saved to disk. Ignoring use.fitted.sl."))
                message("Fitting SuperLearner to training set...")
            }
            fitted.sl <- with(partitions, SuperLearner::SuperLearner(Y = train$y, X = train$x,
                                                                     family = binomial(),
                                                                     SL.library = model.names,
                                                                     method = "method.AUC",
                                                                     verbose = FALSE))  
        }
    } else {
        message("Fitting SuperLearner on the training set...")        
        fitted.sl <- with(partitions, SuperLearner::SuperLearner(Y = train$y, X = train$x,
                                                                 family = binomial(),
                                                                 SL.library = model.names,
                                                                 method = "method.AUC",
                                                                 verbose = FALSE))
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
    ## Gridsearch the optimal cut-points for the predicted probabilities on
    ## the appropriate set
    optimal.breaks <- ""
    if (use.fitted.sl) {
        results.breaks <- readRDS("results.Rds")[[paste0(outcome.variable.name, ".optimal.breaks")]]
        if (is.null(results.breaks)) {
            if (verbose)
                message(paste("Finding optimal breaks for continuous probabilities on the", label, "set..."))
            optimal.breaks <- GridsearchBreaks(predictions = predictions[grepl(label, con.list.labels)][[1]], 
                                               outcome.vector = partitions[[label]]$y)
            suppressMessages({
                bengaltiger::SaveToResults(optimal.breaks, paste0(outcome.variable.name, ".optimal.breaks"))
            })
        } else {
            message("Parameter use.fitted.sl is set to True, and results file contain optimal.breaks element. Using those as breaks for binning continous predictions...")
            optimal.breaks <- results.breaks
        }  
    } else if (!use.fitted.sl || boot.sample) {
        if (verbose)
            message(paste("Finding optimal breaks for continuous probabilities on the", label, "set..."))
        optimal.breaks <- GridsearchBreaks(predictions = predictions[grepl(label, con.list.labels)][[1]], 
                                           outcome.vector = partitions[[label]]$y)
    }
    full.training.list <- list(y = unlist(lapply(train.validation, "[[", "y")),
                               x = do.call(rbind, lapply(train.validation, "[[", "x")))
    if (n.partitions == 3) {
    	if (use.fitted.sl) {
    	    if (file.exists(sl.object.file)) {
                message(paste0("Argument use.fitted.sl is TRUE and SuperLearner_", outcome.variable.name, " exists.", " Skipping refitting to training + validation set and using", sl.object.file, "..."))
    	        fitted.sl <- readRDS(sl.object.file)
    	    } else {
                message(paste0("Argument use.fitted.sl is TRUE, but SuperLearner_", outcome.variable.name, " doe not exist.", " Re-fitting SuperLearner to training + validation set..."))
    	        fitted.sl <- with(full.training.list, SuperLearner::SuperLearner(Y = y, X = x,
    	                                                                 family = binomial(),
    	                                                                 SL.library = model.names,
    	                                                                 method = "method.AUC",
    	                                                                 verbose = FALSE))
                saveRDS(fitted.sl, file = sl.object.file)
                if (verbose)
                    message(paste0("SuperLearner object saved to disk as ", sl.object.file, "..."))
    	    }
    	} else {
            if (verbose)
                message("Re-fitting SuperLearner on full training + validation set...")
    	    fitted.sl <- with(full.training.list, SuperLearner::SuperLearner(Y = y, X = x,
                                                                             family = binomial(),
                                                                             SL.library = model.names,
                                                                             method = "method.AUC",
                                                                             verbose = FALSE))
            if (!boot.sample) {
                saveRDS(fitted.sl, file = sl.object.file)
                if (verbose)
                    message(paste0("SuperLearner object saved to disk as ", sl.object.file, "..."))   
            }
    	}
    }
    ## Make predictions on the test set
    predictions$con.model.test <- predict(object = fitted.sl, newdata = partitions$test$x, onlySL = TRUE)$pred
    ## Bin predictions made on the test set using the optimal cut-points
    cut.list.labels <- paste0("cut.model.", c("train", "validation", "test"))
    binned.predictions <- lapply(setNames(predictions, nm = cut.list.labels), function (preds) {
        as.numeric(
            cut(x = preds,
                breaks = c(-Inf, optimal.breaks, Inf),
                labels = c("Green", "Yellow", "Orange", "Red"),
                include.lowest = TRUE)
        )  
    })
    NewLabelsAndNumeric <- function(label) {
        new.labels <- paste0(label, ".", names(partitions))
        new.list <- lapply(setNames(partitions, nm = new.labels),
                           function (partition.list) as.numeric(partition.list[[label]]))
        return (new.list)
    }
    return.object <- list()
    return.object$predictions.list <- c(predictions, binned.predictions,
                                        NewLabelsAndNumeric("y"),
                                        NewLabelsAndNumeric("tc"))
    if (save.samples) {
        return.object$samples <- lapply(partitions, "[[", "x")
    }
    ## Save the predictions, outcome and clinicians tc in each partition to the results list
    if (save.sample.predictions) {
        suppressMessages({
            bengaltiger::SaveToResults(return.object, paste0(outcome.variable.name, ".results"))
        })
    }

    return (return.object)
}
