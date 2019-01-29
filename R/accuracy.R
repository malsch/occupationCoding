#' Accuracy
#'
#' Calculate accuracy \eqn{p = \frac{1}{n} \sum acc} and standard errors \eqn{\sqrt{\frac{1}{n} * p * (1-p)}}.
#'
#' Note that this function also works if \code{occupationalPredictionsAmongTopK} contains less than \code{n} individuals.
#'
#' @param occupationalPredictionsAmongTopK a data table created with \code{\link{calcAccurateAmongTopK}}.
#' @param n Number of unique persons in test data (may be larger than the number of persons in occupationalPredictionsAmongTopK if for some persons no codes were suggested)
#'
#' @return a data.table with columns N (number of unique IDs in occupationalPredictionsAmongTopK), acc (\code{sum(acc) / n}), se (standard error of acc), mean.in.subset.N (\code{mean(acc)}), and count.pred.prob0 (\code{sum(pred.prob == 0)})
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
#' calcAccurateAmongTopK(res.proc, k = 5)[,mean(acc), by = method.name]
#' accuracy(calcAccurateAmongTopK(res.proc, k = 5), n = 50)
#' accuracy(calcAccurateAmongTopK(res.proc, k = 1), n = 50)
accuracy <- function(occupationalPredictionsAmongTopK, n) {

  occupationalPredictionsAmongTopK[, list(.N, acc = sum(acc) / n, se = sqrt(sum(acc) * (n - sum(acc)) / n^3), mean.in.subset.N = mean(acc), count.pred.prob0 = sum(pred.prob == 0)), by = method.name] # se = sqrt(1/n * p * (1-p)) = sqrt(n.true * n.false / n^3)

}
