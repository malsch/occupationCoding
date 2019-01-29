#' Log loss
#'
#' Calculate log loss \eqn{\log_2 loss = \frac{1}{N} \sum_n \log_2 loss_n} and standard error \eqn{\sqrt{\frac{1}{N(N-1)} \sum_n (\log_2 loss_n - \log_2 loss)^2}} with \eqn{loss_n = \sum_k -y_{nk} \log_2 p_{nk}}
#'
#' log loss is the average probability of true categories that actually realized.
#'
#' @param occupationalPredictions a data.table created with a \code{\link{expandPredictionResults}}-function from this package.
#'
#' @seealso \code{\link{sharpness}}
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
#' logLoss(res.proc)
logLoss <- function(occupationalPredictions) {

  if (!("occupationalPredictionsComplete" %in% class(occupationalPredictions))){
    stop("'occupationalPredictionsComplete' needs to have class 'occupationalPredictionsComplete' (=constructed with a expandPredictionResults method).")
  }

  # calculate mean log loss +- standard errors
  res <- occupationalPredictions[code == pred.code, list(.N, logscore = -mean(log2(pred.prob)), se = sqrt(var(log2(pred.prob))/.N)), by = method.name]

  if (any(res[, is.infinite(logscore)])) {
    res[, type := "all observations (true log score)"]

    res2 <- occupationalPredictions[code == pred.code & pred.prob > 0, list(.N, logscore = -mean(log2(pred.prob)), se = sqrt(var(log2(pred.prob))/.N), type = "observations with pred.prob = 0 of true category excluded"), by = method.name]
    return(rbind(res, res2))
  } else {
    return(res)
  }

  # calculate mean log loss
  # check that no other category is predicted with maximal probability that is lower than pred.prob (result should all be -9999)
#  res[, .SD[acc == TRUE | pred.code == "-9999"][which.min(pred.prob)], by = list(id, sim.name)][, .N, by = pred.code]
  # an example how the so found probabilities look like
  # summary(res[, .SD[acc == TRUE | pred.code == "-9999"][which.max(pred.prob)], by = list(id, sim.name)][sim.name == "substring", list(pred.prob, log(pred.prob), log2(pred.prob))])
  # if so, we may calculate the mean log loss (+- standard errors)
#  res[, .SD[acc == TRUE | pred.code == "-9999"][which.max(pred.prob)], by = list(id, sim.name)][, list(.N, logscore = -mean(log2(pred.prob)), se = sqrt(var(log2(pred.prob))/.N)), by = sim.name]

}
