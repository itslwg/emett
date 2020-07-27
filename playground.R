## This is a file for testing
## Source all functions (remove when turned into package)
library(devtools)
devtools::load_all()
print_trace_back <- function() {
    print(rlang::trace_back(bottom = sys.frame(-1)))
}
options(error = print_trace_back)
## Set parameters that are default in make.study
n.bootstrap.samples <- 10
n.partitions <- 3
## Get data dictionary
data.dictionary <- GetDataDictionary()
## Define study name, authors, description of the study, and relevant variables
study.name <- "SuperLearner vs Clinicians"
authors <- c("Martin Gerdin Wärnberg", "Ludvig Wärnberg Gerdin")
description <- "A comparison of triage conducted by clinicians and the ensemble algorithm SuperLearner."
relevant.variables <- c("age", "moi", "sex", "mot", "tran", "s30d", "egcs", "mgcs", "vgcs", "avpu", "hr",
                        "sbp", "dbp", "spo2", "rr", "tc", "ic", "doar", "toar", "doi", "toi", "s24h", "hd",
                        "taicu", "daicu", "tos", "dos", "nomesco", "snomed", "s", "iss")
pretty.names <- c("SuperLearner", "Random Forest")
## Import study data
study.sample <- ImportAndMergeData()
study.sample <- PrepareStudySample(study.sample = study.sample[, relevant.variables],
                                   data.dictionary = data.dictionary)
## Create bengaltiger study sample (Not included the correct inclusion.criteria functions)
inclusion.criteria <- c(OnlyInformedConsent, bengaltiger::OnlyAdults, OnlyPatientsWithCompleteOutcome)
study.sample <- bengaltiger::CreateStudySample(study.data = study.sample, complete.cases = FALSE,
                                               relevant.variables = names(study.sample),
                                               inclusion.criteria = inclusion.criteria)
flowchart <- bengaltiger::CreateFlowchart(compile.flowchart=TRUE)
## Prepare the study sample, i.e. redefine levels of factors using the data dictionary and set missing
study.sample <- CollapseMechanismOfInjury(study.sample = study.sample)
study.sample <- SetToOutcome(study.sample)
## Add additional features and group/collapse mechanism of injury
study.sample <- AddAdditionalVariables(study.sample, data.dictionary)
## Remove informed conxsent variable from study sample
study.sample[, "ic"] <- NULL
## Create sample characteristics
sample.characteristics.table <- SampleCharactersticsWrapper(study.sample, data.dictionary,
                                                            save.to.disk = TRUE, file.format = "pdf")
## Transform features to dummy variables
study.sample <- ToDummyVariables(study.sample = study.sample)
## Add missing indicator variables
study.sample <- AddMissingIndicatorVariables(study.sample = study.sample)
## Do median imputation, i.e. impute continuous with median and factors with mode
study.sample <- DoMedianImputation(study.sample = study.sample)
## Partition sample, train, tune cut-points, and predict on a hold-out sample
predictions.list <- PartitionTrainAndPredict(study.sample = study.sample,
                                             save.to.results = TRUE,
                                             n.partitions = n.partitions,
                                             boot = FALSE,
                                             verbose = TRUE
                                        #sample=TRUE
                                             )
## Generate point estimates and bootstrap estimates; Save estimates to results
## separately
statistics <- BootstrapStatistics(f = ComputeAucAndNri, data = study.sample,
                                  R = n.bootstrap.samples, parallel = "multicore", ncpus = 4,
                                  save.to.results = FALSE, log = TRUE, boot = TRUE,
                                  verbose = TRUE, return.samples = FALSE,
                                  n.partitions = n.partitions)
## bengaltiger::SaveToResults(output.object = statistics, object.name = "statistics")
results <- readRDS("results.Rds")
## Create ROC-plots
CreateRocPlot(results$predictions.list, device = "pdf")
## Create Mortality plot
CreateMortalityPlot(results$predictions.list, device = "pdf")
## Create table of estimates
estimates.with.ci <- ComputeConfidenceIntervals(boot.object = results$statistics, type = "basic")
CreateEstimatesTable(estimates.with.ci, table.name = "estimates.table",
                     footnote = "latex", escape = FALSE, booktabs = TRUE,
                     file.format = "rmd")
## Create classification tables, i.e. reclassification tables
tables <- CreateClassificationTables(results$predictions.list)
## Create coefficients table, i.e. measures for each individual model in SuperLearner
CreateCoefficientsTable(sample=results$samples$test, outcomes=results$predictions.list$y.test,
                        pretty.model.nms = pretty.names,
                        booktabs = TRUE)
## Create classifier plots, i.e. roc plots for all different classifiers
CreateClassifierPlots(sample=results$samples$test,
                      outcomes=results$predictions.list$y.test,
                      pretty.model.nms = pretty.names,
                      device = "pdf")
