#' Predict codes using an extreme gradient boosted tree model
#'
#' Function does the same preprocessing as in \code{\link{trainXgboost}} and calls the xgboost predict-function.
#'
#' @param model the output created from \code{\link{trainLogisticRegressionWithPenalization}}
#' @param newdata eiter a data.table created with \code{\link{removeFaultyAndUncodableAnswers_And_PrepareForAnalysis}} or a character vector
#'
#' @seealso \code{\link{trainXgboost}}, \code{\link[xgboost]{predict.xgb.Booster}}
#'
#' @return a data.table of class \code{occupationalPredictions} that contains predicted probabilities \code{pred.prob} for every combination of \code{ans} and \code{pred.code}. pred.code may not cover the full set of possible codes.
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
#' # train model and make predictions. The following parameters are chosen for speed, not for good model performance.
#' model <- trainXgboost(proc.occupations, allowed.codes = allowed.codes, testCases = group == "test", returnPredictions = FALSE,
#'                       preprocessing = list(stopwords = tm::stopwords("de"), stemming = "de", countWords = FALSE),
#'                       tuning = list(eta = 0.5, lambda = 1e-4, alpha = 0,
#'                                     max.depth = 20, gamma = 0.6,
#'                                     min_child_weight = 0, max_delta_step = 1,
#'                                     subsample = 0.75, colsample_bytree = 1, colsample_bylevel=1,
#'                                     nrounds= 3, early_stopping_rounds = 1,
#'                                     early.stopping.max.diff = n.test / 100, early.stopping.precision.digits = 3,
#'                                     nthread = 8, verbose=1)
#'                       )
#'
#' predictXgboost(model, c("test", "HIWI", "Hilfswissenschaftler"))
#' res <- predictXgboost(model, splitted.data$test)
#'
#' # look at most probable answer from each id
#' res[, .SD[which.max(pred.prob), list(ans, true.code = code, pred.code, acc = code == pred.code)], by = id]
#' res[, .SD[which.max(pred.prob), list(ans, true.code = code, pred.code, acc = code == pred.code)], by = id][, mean(acc)] # calculate accurac of predictions
#'
#' # for further analysis we usually require further processing:
#' produceResults(expandPredictionResults(res, allowed.codes, method.name = "xgboost"), k = 1, n = n.test, num.codes = length(allowed.codes))
#'
#'
#' #' #######################################################
#' ## RUN A GRID SEARCH (takes some time).. Not really. We start with a default configurations and vary it in all directions. This is our grid.
#' ## Note that we actually use trainXgboost to make predictions.
#' \donttest{
#' nthreads <- 1 # you may want to increase this if you have more than one processor
#'
#' model.grid <- data.table(expand.grid(stopwords = FALSE, stemming = "de", strPreprocessing = TRUE, countWords = TRUE,
#' nrounds = 40, early_stopping_rounds = 1, early.stopping.max.diff = n.test / 100, early.stopping.precision.digits = 3,
#' eta = 0.5, max_delta_step = 1,
#' lambda = 1e-4, alpha = 0,
#' max.depth = 20, gamma = 0.6, min_child_weight = 0,
#' subsample = 0.75, colsample_bytree = 1, colsample_bylevel = 1))
#'
#'  model.grid <- model.grid[rep(1, 17)]
#'  model.grid[1, max.depth := 10]
#'  model.grid[2, max.depth := 30]
#'  model.grid[3, gamma := 0.3]
#'  model.grid[4, gamma := 0.9]
#'  model.grid[5, lambda := 0]
#'  model.grid[6, lambda := 1]
#'  model.grid[7, alpha := 0.2]
#'  model.grid[8, min_child_weight := 0.01]
#'  model.grid[9, subsample := 0.5]
#'  model.grid[10, subsample := 1]
#'  model.grid[11, colsample_bytree := 0.6]
#'  model.grid[12, colsample_bylevel := 0.6]
#'  model.grid[13, max_delta_step := 0.33]
#'  model.grid[14, max_delta_step := 3]
#'  model.grid[15, eta := 0.25]
#'  model.grid[16, eta := 1]
#'
#'  # Do grid search (only circle over the first 6 rows to save time in this demonstration)
#'  for (i in 1:6) {
#'    res.proc <- trainXgboost(proc.occupations, allowed.codes = allowed.codes, testCases = group == "test", returnPredictions = TRUE,
#'                             preprocessing = list(stopwords = if (model.grid[i, stopwords]) tm::stopwords("de") else character(0),
#'                                                  stemming = if (model.grid[i, stemming == "de"]) "de" else NULL,
#'                                                  countWords = model.grid[i, countWords]),
#'                             tuning = list(nrounds=model.grid[i, nrounds],
#'                                           eta = model.grid[i, eta],
#'                                           lambda = model.grid[i, lambda],
#'                                           alpha = model.grid[i, alpha],
#'                                           max_delta_step = model.grid[i, max_delta_step],
#'                                           max.depth = model.grid[i, max.depth],
#'                                           gamma = model.grid[i, gamma],
#'                                           min_child_weight = model.grid[i, min_child_weight],
#'                                           subsample = model.grid[i, subsample],
#'                                           colsample_bytree = model.grid[i, colsample_bytree],
#'                                           colsample_bylevel=model.grid[i, colsample_bylevel],
#'                                           early_stopping_rounds = model.grid[i, early_stopping_rounds],
#'                                           early.stopping.max.diff = model.grid[i, early.stopping.max.diff],
#'                                           early.stopping.precision.digits = model.grid[i, early.stopping.precision.digits],
#'                                           nthread = nthreads, verbose=1))
#'
#'    res.proc2 <- expandPredictionResults(res.proc, allowed.codes = allowed.codes, method.name = "Xgboost")
#'
#'    ac <- accuracy(calcAccurateAmongTopK(res.proc2, k = 1), n = nrow(splitted.data$test))
#'    ll <- logLoss(res.proc2)
#'    sh <- sharpness(res.proc2)
#'
#'    model.grid[i, acc := ac[, acc]]
#'    model.grid[i, acc.se := ac[, se]]
#'    model.grid[i, acc.N := ac[, N]]
#'    model.grid[i, acc.prob0 := ac[, count.pred.prob0]]
#'    model.grid[i, loss.full := ll[1, logscore]]
#'    model.grid[i, loss.full.se := ll[1, se]]
#'    model.grid[i, loss.full.N := ll[1, N]]
#'    model.grid[i, loss.sub := ll[2, logscore]]
#'    model.grid[i, loss.sub.se := ll[2, se]]
#'    model.grid[i, loss.sub.N := ll[2, N]]
#'    model.grid[i, sharp := sh[, sharpness]]
#'    model.grid[i, sharp.se := sh[, se]]
#'    model.grid[i, sharp.N := sh[, N]]
#'    model.grid[i, nrounds.used := which.min(attr(res.proc, "evaluation_log")[[3]])]
#'    model.grid[i, sum.pred.prob1 := res.proc2[, .SD[which.max(pred.prob), pred.prob], by = id][, sum(V1)]]
#'    model.grid[i, train.log := paste(round(attr(res.proc, "evaluation_log")$train_Logloss / log(2), 2), collapse = ", ")]
#'    model.grid[i, eval.log := paste(round(attr(res.proc, "evaluation_log")$eval_Logloss / log(2), 2), collapse = ", ")]
#' }
#'
#'  model.grid[, stopping.reason := "logloss minimized / constant"]
#'  model.grid[grepl("Inf", eval.log), stopping.reason := "sum of predicted probabilities is too large"]
#'  model.grid[nrounds.used == 40, stopping.reason := "max. number of rounds reached"]
#'
#' # look at the interesting lines
#' model.grid[1:6, list(max.depth, gamma, lambda, acc, pred.acc = sum.pred.prob1 / n.test, loss.full, sharp)]
#'
#' }
predictXgboost <- function(model, newdata, add.impossible.codes = NULL) {

  # get character vector depending on type of input
  if ("occupationData" %in% class(newdata)) {
    ans <- newdata[, ans]
    if (exists("id", newdata)) {
      id <- newdata[, id]
    } else {
      id <- 1:length(ans)
    }
  }
  if (is.character(newdata)) {
    ans <- newdata
    id <- 1:length(ans)
  }

  # preprocessing
  preprocessing <- model$preprocessing

  ansP <- stringPreprocessing(ans)

  # prepare text for efficient computation -> transform to sparse matrix
  matrix <- asDocumentTermMatrix(ansP, vect.vocab = model$vect.vocab,
                                 stopwords = preprocessing$stopwords,
                                 stemming = preprocessing$stemming,
                                 type = "dgCMatrix")$dtm

  # include feature for number of words
  if (preprocessing$countWords) {
    ans_freq <- sapply(strsplit(ansP, " "), length)
    matrix <- cbind(matrix, ans_freq)
  }

  # include feature for origin
  if (!is.null(model$coding_index)) {
    matrix <- cbind(matrix, origin = 0)
  }

  if (is.null(model$trainingEmptyColumns)) {
    dTest <- xgboost::xgb.DMatrix(data = matrix)
  } else {
    # if we excluded some columns in the training data (because they are only available in the test data used for early stopping), we must exclude the same columns in the test data.
    dTest <- xgboost::xgb.DMatrix(data = matrix[, !model$trainingEmptyColumns])
  }

  # make predictions and bring them in a nice format
  predProb <- data.table(id = rep(id, each = nrow(model$outcome_dictionary)),
                         ans = rep(ans, each = nrow(model$outcome_dictionary)),
                         pred.code = rep(model$outcome_dictionary$cat, times = length(ans)),
                         pred.prob = predict(model, dTest))

  # add additional columns from new data to the result
  if ("occupationData" %in% class(newdata)) {

    predProb <- merge(predProb, newdata, by = c("id", "ans"))
    class(predProb) <- c(class(predProb), "occupationalPredictions")
  }

  return(predProb)
}
