#' Sharpness
#'
#' Calculate Sharpness \eqn{\log_2 loss = \frac{1}{N} \sum_n entropy_n} and standard error \eqn{\sqrt{\frac{1}{N(N-1)} \sum_n (entropy_n - sharpness)^2}} with \eqn{entropy_n = - \sum_k p_{nk} \log_2 p_{nk}}
#'
#' Sharpness is zero (optimal) if a single category is predicted with probability 1. It is maximal if all categories have equal probability \eqn{p = \frac{1}{K}}
#'
#' Note: What should be done if a predicted probability is zero? \eqn{0 \times log(0)} is not defined but necessary to calculate sharpness. We set {0 \times log(0) = 0}. This also means we exclude observations from our analysis if all probabilities are predicted as zero. An alternative could be to set such zeros to \eqn{1/k)}, which would lead to very different sharpness.
#'
#' @param occupationalPredictions a data.table created with a \code{\link{expandPredictionResults}}-function from this package.
#'
#' @seealso \code{\link{plotReliabilityDiagram}}, \code{link{logLoss}}
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
#' # we can use different methods to create a combined dataset. This is how to run the subsequent analysis functions only once.
#' res.proc2 <- expandPredictionResults(res, allowed.codes = c("12345", allowed.codes), method.name = "glmnet2")
#' res.proc <- rbind(res.proc1, res.proc2); class(res.proc) <- c(class(res.proc), "occupationalPredictionsComplete")
#'
#' sharpness(res.proc)
sharpness <- function(occupationalPredictions) {

  if (!("occupationalPredictionsComplete" %in% class(occupationalPredictions))){
    stop("'occupationalPredictionsComplete' needs to have class 'occupationalPredictionsComplete' (=constructed with a expandPredictionResults method).")
  }

  # we set 0 * log_2 0 = 0  (implemented as pred.prob > 0)
  occupationalPredictions[pred.prob > 0, list(entropy = sum(-pred.prob * log2(pred.prob))), by = list(id, method.name)][, list(.N, sharpness = mean(entropy), se = sqrt(var(entropy) / .N)), by = method.name]
  # res.complete[, list(entropy = sum(-pred.prob * log2(pred.prob))), by = list(id, sim.name)][, list(sharpness = mean(entropy), se = sqrt(var(entropy) / .N)), by = sim.name]

}
