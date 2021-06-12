## This is a file for testing
## Source all functions (remove when turned into package)
library(devtools)
devtools::load_all()
print_trace_back <- function() {
    print(rlang::trace_back(bottom = sys.frame(-1)))
}
options(error = print_trace_back)
## Set parameters that are default in make.study
n.bootstrap.samples <- 3
n.partitions <- 3
study.variables <- c("age", "moi", "sex", "mot", "tran", "s30d", "egcs", "mgcs", "vgcs", "avpu", "hr",
                     "sbp", "dbp", "spo2", "rr", "tc", "ic", "doar", "toar", "doi", "toi", "s24h", "hd",
                     "taicu", "daicu", "tos", "dos", "nomesco", "snomed", "s", "iss")
model.names <- c("SL.xgboost", "SL.glmnet")
## Define helper for predictions
NewLabelsAndNumeric <- function(label) {
    new.labels <- paste0(label, ".", names(partitions))
    new.list <- lapply(setNames(partitions, nm = new.labels),
                       function (partition.list) as.numeric(partition.list[[label]]))
    return (new.list)
}
s30d.settings = list(outcome.label="s30d", variables.to.drop=c("s24h", "composite"))
study.sample.s30d <- PrepareSample(study.variables, s30d.settings)

## Do modelling on 30-day mortality
variables.to.drop <- s30d.settings$variables.to.drop
outcome.variable.name <- s30d.settings$outcome.label
study.sample.s30d <- study.sample.s30d[
  , !grepl(paste0(variables.to.drop, collapse="|"), names(study.sample.s30d))
]
s30d.results <- PartitionTrainAndPredict(
    study.sample=study.sample.s30d,
    outcome.variable.name="s30d",
    cvControl=list(stratifyCV=TRUE, V=2),  ## Increase V in full run
    sample=TRUE, ## Remove in full run
    model.names=model.names,
    return.partitions=TRUE,
    save.sample.predictions=TRUE,
    n.partitions=n.partitions,
    boot.sample=FALSE,
    use.fitted.sl=FALSE,
    verbose=TRUE
)
## Generate point estimates and bootstrap estimates; Save estimates to results
## separately
s30d.statistics <- BootstrapStatistics(
    f = ComputeAucAndNri,
    data=study.sample.s30d,
    outcome.variable.name="s30d",
    model.names=model.names,
    R=n.bootstrap.samples,
    parallel="multicore",
    ncpus=4,
    save.sample.predictions=FALSE,
    log=TRUE,
    boot.sample=TRUE,
    use.fitted.sl=FALSE,
    verbose=TRUE,
    n.partitions=n.partitions,
    clean.start=FALSE
)
bengaltiger::SaveToResults(output.object = s30d.statistics, object.name = "s30d.statistics")
## Visuzalize results 
viz.settings.s30d <- list(
    triage.comparison.labels=c("Survived", "Died"),
    superlearner.object.path="./SuperLearner_s30d.rds"
)
## ROC-curve for ensemble
CreateRocPlot(
    s30d.results$predictions.list,
    file.name="s30d.roc.plot",
    device = "pdf"
)
## Compare the event levels in triage categories with clinicians
CreateTriageComparisonPlot(
    s30d.results$predictions.list,
    outcome.variable.name="s30d",
    file.name="s30d.triage.comparison.plot",
    pretty.labels=viz.settings.s30d$triage.comparison.labels,
    device = "pdf"
)
## Create coefficients table, i.e. measures for each individual model in SuperLearner
coeff.risk.table <- CreateCoefficientsTable(
    sample=s30d.results$samples$test,
    outcomes=s30d.results$predictions.list$y.test
)
bengaltiger::SaveToResults(coeff.risk.table, "s30d.coeff.risk.table")
## Create table of estimates
s30d.estimates <- ComputeConfidenceIntervals(
    boot.object = s30d.statistics,
    type = "basic"
)
estimates.table <- CreateEstimatesTable(
    s30d.estimates
)
## Illustrate NRI for s30d
VisualiseReclassification(
    s30d.results$predictions.list,
    outcome.variable.name="s30d",
    outcome.labels=viz.settings.s30d$triage.comparison.labels
)
## Create classification tables, i.e. reclassification tables
classification.tables <- CreateClassificationTables(s30d.results$predictions.list)
bengaltiger::SaveToResults(classification.tables, "s30d.classification.tables")

## Do modelling on the composite outcome
composite.settings = list(outcome.label="composite", variables.to.drop=c("s30d", "s24h", "composite_missing"))
study.sample.composite <- PrepareSample(study.variables, composite.settings)
## Partition sample, train SuperLearner, gridsearch breaks, predict on test
variables.to.drop <- composite.settings$variables.to.drop
outcome.variable.name <- composite.settings$outcome.label
study.sample.composite <- study.sample.composite[
  , !grepl(paste0(variables.to.drop, collapse="|"), names(study.sample.composite))
]
composite.results <- PartitionTrainAndPredict(
    study.sample=study.sample.composite,
    outcome.variable.name="composite",
    cvControl=list(stratifyCV=TRUE, V=2),  ## Increase V in full run
    sample=TRUE, ## Remove in full run
    model.names=model.names,
    return.partitions=TRUE,
    save.sample.predictions=TRUE,
    n.partitions=n.partitions,
    boot.sample=FALSE,
    use.fitted.sl=FALSE,
    verbose=TRUE
)
## Generate point estimates and bootstrap estimates; Save estimates to results
## separately
composite.statistics <- BootstrapStatistics(
    f = ComputeAucAndNri,
    data=study.sample.composite,
    model.names=model.names,
    outcome.variable.name = "composite",
    R=n.bootstrap.samples,
    parallel="multicore",
    ncpus=4,
    save.sample.predictions=FALSE,
    log=TRUE,
    boot.sample=TRUE,
    use.fitted.sl=FALSE,
    verbose=TRUE,
    return.partitions=FALSE,
    n.partitions=n.partitions,
    clean.start=FALSE
)
bengaltiger::SaveToResults(output.object = composite.statistics, object.name = "composite.statistics")
## bengaltiger::SaveToResults(output.object = statistics, object.name = "statistics")
## Visuzalize results
results <- readRDS("results.Rds")
composite.results <- results$composite.results
composite.statistics <- results$composite.statistics
viz.settings.composite <- list(
    triage.comparison.labels=c("Non-event", "Event"),
    superlearner.object.path="./SuperLearner_composite.rds"
)
## ROC-curve
CreateRocPlot(
    composite.results$predictions.list,
    file.name="composite.roc.plot",
    device = "pdf"
)
## Compare the event levels in triage categories with clinicians
CreateTriageComparisonPlot(
    composite.results$predictions.list,
    outcome.variable.name="composite",
    file.name="composite.triage.comparison.plot",
    pretty.labels=viz.settings.composite$triage.comparison.labels,
    device = "pdf"
)
## Create coefficients table, i.e. measures for each individual model in SuperLearner
coeff.risk.table <- CreateCoefficientsTable(
    sample=composite.results$samples$test,
    outcomes=composite.results$predictions.list$y.test,
    superlearner.object.path = viz.settings.composite$superlearner.object.path
)
bengaltiger::SaveToResults(coeff.risk.table, "coeff.risk.table.composite")
## Create table of estimates
composite.estimates <- ComputeConfidenceIntervals(
    boot.object = composite.statistics,
    type = "basic"
)
estimates.table <- CreateEstimatesTable(
    composite.estimates
)
## Illustrate NRI for composite
VisualiseReclassification(composite.results$predictions.list, "composite")
## Create classification tables, i.e. reclassification tables
classification.tables <- CreateClassificationTables(composite.results$predictions.list)
bengaltiger::SaveToResults(classification.tables, "composite.classification.tables")
## ROC-curve for individual classifiers
CreateClassifierPlots(
    samples = list(s30d.results = results$s30d.results$samples$test,
                   composite.results = results$composite.results$samples$test),
    outcomes = list(s30d.results = results$s30d.results$predictions.list$y.test,
                    composite.results = results$composite.results$predictions.list$y.test)
)
