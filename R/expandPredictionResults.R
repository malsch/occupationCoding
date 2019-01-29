#' Expands predicted datasets to contain all allowed codes
#'
#' Start with a data.table of class 'occupationalPredictions' (for each combination of pred.code and answer one prediction) and expand it to contain all allowed codes.
#'
#' The problem solved here is this: Most algorithms do not provide codes for all categories from the classification, because this would require that the categories are in the training data. This function expands the dataset and predicts some very small probabilities (or 0) for classification codes that the training algorithm found impossible to predict.
#'
#' @param data a data.table created with a \code{predict}-function from this package.
#' @param allowed.codes a character vector of all allowed codes.
#' @param method.name the name how the method shall be called.
#'
#' @seealso \code{\link{produceResults}}
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
#' expandPredictionResults(res, allowed.codes, method.name = "Logistic Regression")
expandPredictionResults <- function(occupationalPredictions, allowed.codes, method.name) {

  if (!("occupationalPredictions" %in% class(occupationalPredictions))){
    stop("'occupationalPredictions' needs to have class 'occupationalPredictions' (=constructed with a predict method).")
  }

  # 1. construct with CJ an expanded data.table that contains one row for every combination of id and allowed.code. 2. Merge with original data to expand that dataset
  res.complete <- merge(occupationalPredictions[, list(id, pred.code, ans, pred.prob, code, among.suggested.code = TRUE)], CJ(id = unique(occupationalPredictions[, id]), pred.code = allowed.codes), by = c("id", "pred.code"), all.y = TRUE)

  # special case: in some prediction datasets a code "-9999" was predicted that is a placeholder for all other codes. Use this probability
  if (occupationalPredictions[, any(pred.code == "-9999")]) {
    res.complete <- merge(res.complete, occupationalPredictions[pred.code == "-9999", list(pred.prob.insert = pred.prob, id, true.code.insert = code)], by = "id", all.x = TRUE)
  } else {
    res.complete <- merge(res.complete, occupationalPredictions[pred.code == min(pred.code), list(pred.prob.insert = 0, id, true.code.insert = code)], by = "id", all.x = TRUE)
  }

  # fill new rows with appropriate values
  res.complete[is.na(pred.prob), pred.prob := pred.prob.insert]
  res.complete[is.na(code), code := true.code.insert]
  res.complete[, true.code.insert := NULL]
  res.complete[, pred.prob.insert := NULL]

  # an indicator that tells us if the original algorithm came up with this code (impossible.code = FALSE)
  res.complete[is.na(among.suggested.code), among.suggested.code := FALSE]

  res.complete[, method.name := method.name]

  class(res.complete) <- c(class(res.complete), "occupationalPredictionsComplete")
  return(res.complete)
}
