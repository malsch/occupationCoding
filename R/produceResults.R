#' Produces summaries of predictive performance
#'
#' Produces \code{\link{accuracy}}, \code{\link{plotReliabilityDiagram}}, \code{\link{sharpness}}, \code{\link{logLoss}}, \code{\link{plotTruePositivesVsFalsePositives}}, and \code{\link{plotAgreementRateVsProductionRate}}.
#'
#' @param occupationalPredictions a data.table created with a \code{\link{expandPredictionResults}}-function from this package.
#' @param k how many top k categories to aggregate over?
#' @param n Number of unique persons in test data (may be larger than the number of persons in occupationalPredictionsAmongTopK if for some persons no codes were suggested)
#' @param num.codes Number of allowed categories in classification
#'
#' @seealso \code{\link{calcAccurateAmongTopK}}
#'
#' @return a data.table
#' @import data.table
#' @export
#'
#' @examples
#' # set up data
#' data(occupations)
#' allowed.codes <- c("71402", "71403", "63302", "83112", "83124", "83131", "83132", "83193", "83194", "-0004", "-0030")
#' allowed.codes.titles <- c("Office clerks and secretaries (without specialisation)-skilled tasks", "Office clerks and secretaries (without specialisation)-complex tasks", "Gastronomy occupations (without specialisation)-skilled tasks",
#'  "Occupations in child care and child-rearing-skilled tasks", "Occupations in social work and social pedagogics-highly complex tasks", "Pedagogic specialists in social care work and special needs education-unskilled/semiskilled tasks", "Pedagogic specialists in social care work and special needs education-skilled tasks", "Supervisors in education and social work, and of pedagogic specialists in social care work", "Managers in education and social work, and of pedagogic specialists in social care work",
#'  "Not precise enough for coding", "Student assistants")
#' proc.occupations <- removeFaultyAndUncodableAnswers_And_PrepareForAnalysis(occupations, colNames = c("orig_answer", "orig_code"), allowed.codes, allowed.codes.titles)
#'
#' ## split sample
#' set.seed(3451345)
#' n.test <- 50
#' group <- sample(c(rep("test", n.test), rep("training", nrow(proc.occupations) - n.test)))
#' splitted.data <- split(proc.occupations, group)
#'
#' # train model and make predictions
#' model <- trainLogisticRegressionWithPenalization(splitted.data$train, preprocessing = list(stopwords = tm::stopwords("de"), stemming = "de", countWords = FALSE), tuning = list(alpha = 0.05, maxit = 50^5, nlambda = 100, thresh = 1e-5))
#' res <- predictLogisticRegressionWithPenalization(model, splitted.data$test)
#'
#' # expand to contain more categories than the initial ones
#' res.proc1 <- expandPredictionResults(res, allowed.codes = c("12345", allowed.codes), method.name = "glmnet1")
#'
#' # aggregate over top k categories
#' calcAccurateAmongTopK(res.proc1, k = 1)[,mean(acc)]
#' calcAccurateAmongTopK(res.proc1, k = 5)[,mean(acc)]
#'
#' # we can use different methods to create a combined dataset. This is how to run the subsequent analysis functions only once.
#' res.proc2 <- expandPredictionResults(res, allowed.codes = c("12345", allowed.codes), method.name = "glmnet2")
#' res.proc <- rbind(res.proc1, res.proc2); class(res.proc) <- c(class(res.proc), "occupationalPredictionsComplete")
#'
#' produceResults(res.proc, k = 1, n = n.test, num.codes = length(allowed.codes) + 1)
produceResults <- function(occupationalPredictions, k = 1, n, num.codes) {

  if (!("occupationalPredictionsComplete" %in% class(occupationalPredictions))){
    stop("'occupationalPredictionsComplete' needs to have class 'occupationalPredictionsComplete' (=constructed with a expandPredictionResults method).")
  }

  no.individuals <- occupationalPredictions[, length(unique(id))]
  print(paste("Number of individuals:", no.individuals))
  print(paste("Expected Number of rows in dataset:", no.individuals*num.codes, "=", no.individuals, "Individuals *", num.codes, "=", "Codes"))
  print(paste("Observed Number of rows in dataset:"))

  print(occupationalPredictions[, .N, by = method.name])

  accurateAmong <- calcAccurateAmongTopK(occupationalPredictions, k = k)

  print("## Agreement Rate (at 100% production rate):")
  print(accuracy(accurateAmong, n = n))

  print("## Plot Reliability Diagram")
  print(plotReliabilityDiagram(occupationalPredictions, k = k, num.codes = num.codes))

  print("## Sharpness:")
  print(sharpness(occupationalPredictions))

  print("## Log2 loss:")
  print(logLoss(occupationalPredictions))

  print("## Plot True Positives vs False Positives")
  print(plotTruePositivesVsFalsePositives(accurateAmong))

  print("## Plot Agreement Rate vs Production Rate")
  print(plotAgreementRateVsProductionRate(accurateAmong, n = n, yintercept = 0.85))

  # calculate AUC (I don't like ROC because the number of positive conditions (used in the tpr/recall) is not meaningful as a denominator here)
  # library(verification)
  # calcAccurateAmongTopK(occupationalPredictions, k = k)[, verification::roc.area(acc, pred.prob), by = sim.name] # used in the paper

  # ###########################
  # # idea: absolute number in a category should be similar to the predicted number (aggregates per category). But the results are a bit strange.. so include in the paper?
  #
  # # calculate residuals
  # res.complete[, acc := pred.code == true.code]
  # res.complete[, residual := acc - pred.prob] # y_{n_fk} - \hat{p}_{n_fk}
  #
  # # this should be exactly 1 for all ids
  # summary(res.complete[sim.name == "osa1111 = 0" & pred.code != "-9999", sum(pred.prob), by = id])
  # res.complete[sim.name == "osa1111 = 0" & pred.code != "-9999", sum(residual), by = list(pred.code, sim.name)][order(V1), sum(V1)]
  # # this should be 1064 -> our numbers are up to 1.8 off.
  # res.complete[sim.name == "osa1111 = 0" & pred.code != "-9999", sum(pred.prob), by = id][,sum(V1)]
  #
  # pdf("Z:/Eigene Dateien/in work/Algorithms for Occupation Coding/paper/graphs/category_residuals.pdf")
  # res.complete[sim.name == "osa1111 = 0" & pred.code != "-9999", sum(residual), by = list(pred.code, sim.name)][order(V1), plot(V1, ylab = "Residual")]
  # dev.off()
  #
  # pdf("Z:/Eigene Dateien/in work/Algorithms for Occupation Coding/paper/graphs/category_residuals2.pdf")
  # plot(res.complete[sim.name == "osa1111 = 0" & pred.code != "-9999", list(residual = sum(residual)), by = list(pred.code, sim.name)][order(residual), ][residual < -5 | residual > 5, list(pred.code = as.factor(pred.code), by = residual)], las = 3, ylab = "Residual")
  # abline(h = 0)
  # dev.off()
  # res[sim.name == "osa1111 = 0", list(sum(pred.prob)), by = pred.code][order(V1)]
  #
  # res.complete[sim.name == "osa1111 = 0" & pred.code != "-9999", list(residual = sum(residual)), by = list(pred.code, sim.name)][residual > -0.5 & residual < 0.5][, .N, by = residual]
  #
  # res.complete[sim.name == "osa1111 = 0" & pred.code != "-9999", list(residual = sum(residual)), by = list(pred.code, sim.name)][residual > -0.5 & residual < 0.5, list(.N, mean(residual))]
  # turtle[code == 71402, .N]
  # res.complete[sim.name == "osa1111 = 0" & pred.code == "71402", list(sum(pred.prob), sum(residual))]
  #
}
