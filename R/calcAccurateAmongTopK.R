#' Calculate aggregate properties for top k predicted categories
#'
#' Start with a data.table of class 'occupationalPredictions' (for each combination of pred.code and answer one prediction) and calulate if one of the top k entries is accurate.
#'
#' \code{num.suggested} and \code{general.among.top5} is currently not used. Relates to situations if the prediction algorithm does not provide all codes.
#'
#' @param occupationalPredictions a data.table created with a \code{\link{expandPredictionResults}}-function from this package.
#' @param k how many top k categories to aggregate over?
#'
#' @seealso ...
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
#' calcAccurateAmongTopK(res.proc, k = 5)[,mean(acc), by = method.name]
#' # res[, calcAccurateAmongTopK(.SD, k = 5), by = method][,mean(acc), by = method]
calcAccurateAmongTopK <- function(occupationalPredictions, k = 1) {

  if (!("occupationalPredictionsComplete" %in% class(occupationalPredictions))){
    stop("'occupationalPredictionsComplete' needs to have class 'occupationalPredictionsComplete' (=constructed with a expandPredictionResults method).")
  }

  # determine if a pred.code is accurate (acc) and get the (at most) k most probable pred.codes per id
  # only codes if they were probable in the first place (among.suggested.code == TRUE)
  occupationalPredictions2 <- occupationalPredictions[among.suggested.code == TRUE, head(.SD[order(sample(1:.N, .N))][order(pred.prob, decreasing = TRUE), list(ans, pred.code, pred.prob, code, acc = code == pred.code, among.suggested.code)], k), by = list(id, method.name)]

  if (k == 1) {
    randomChoices <- merge(occupationalPredictions, occupationalPredictions2, by = c("pred.prob", "id", "method.name"))[, .N, by = list(method.name, id)][N > 2]

    if (nrow(randomChoices) > 0) {
      warning("Some ids have more than one most probable code (see above). Random choice made (or lowest pred.code selected?)")
      print(randomChoices)
    }
  }

  # calculate key statistics (pred.code: set of predicted codes among top k, pred.prob: sum of probabilities, acc: number of correct predictions, num.suggested: number (1-k), general.among.top5: indicator for any non-included code),
  return(occupationalPredictions2[, list(pred.code = paste(pred.code, collapse = ", "), pred.prob = sum(pred.prob), acc = sum(acc), num.suggested.codes = sum(!among.suggested.code), num.suggested = .N, general.among.top5 = "-9999" %in% pred.code), by = list(id, ans, code, method.name)])
}
