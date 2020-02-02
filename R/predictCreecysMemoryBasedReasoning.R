#' Predict codes with Creecys Memory-based reaoning model
#'
#' Function does the same preprocessing as in \code{\link{trainCreecysMemoryBasedReasoning}} and predicts codes with a modified \code{k}-nearest-neighbor approach.
#'
#' @param model the output created from \code{\link{trainCreecysMemoryBasedReasoning}}
#' @param newdata eiter a data.table created with \code{\link{removeFaultyAndUncodableAnswers_And_PrepareForAnalysis}} or a character vector
#' @param tuning a list with elements
#' \describe{
#'   \item{k.neighbors}{Number of nearest neighbors to use.}
#'   \item{metric}{\code{metric} determines how to calculate 'nearness'. Setting \code{metric == MAX} is not recommended. See Creecy et al. for their reasoning and testing of different metrics.}
#' }
#'
#' @seealso
#' \code{\link{trainCreecysMemoryBasedReasoning}}
#'
#' Creecy, R. H., Masand, B. M., Smith, S. J., Waltz, D. L. (1992). Trading MIPS and Memory for Knowledge Engineering. Comm. ACM 35(8). pp. 48--65.
#'
#' @return a data.table that provides a confidence score for the most likely category. Unlike other prediction functions in this package, no probabilities for all categories are provided, which makes post-processing a bit more difficult. See examples.
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
#' memModel <- trainCreecysMemoryBasedReasoning(splitted.data$training,
#'                                              preprocessing = list(stopwords = character(0), stemming = NULL, strPreprocessing = TRUE, removePunct = FALSE))
#' testi <- predictCreecysMemoryBasedReasoning(memModel, c("test", "HIWI", "Hilfswissenschaftler"), tuning = list(k.neighbors = 12, metric = c("SUM")))
#' testi; testi
#'
#' resMem <- predictCreecysMemoryBasedReasoning(memModel, splitted.data$test, tuning = list(k.neighbors = 12, metric = c("SUM")))
#'
#' # Analyize predictions
#' accuracy(resMem[, list(pred.code, pred.prob = confidence.score, acc = pred.code == code, num.suggested.codes = 1, num.suggested = 1, general.among.top5 = pred.code == "-9999", method.name = "Creecy.Sum.12")], n = n.test)
#' plotTruePredictionsVsFalsePredictions(resMem[, list(pred.code, pred.prob = confidence.score, acc = pred.code == code, num.suggested.codes = 1, num.suggested = 1, general.among.top5 = pred.code == "-9999", method.name = "Creecy.Sum.12")])
#' plotAgreementRateVsProductionRate(resMem[, list(pred.code, pred.prob = confidence.score, acc = pred.code == code, num.suggested.codes = 1, num.suggested = 1, general.among.top5 = pred.code == "-9999", method.name = "Creecy.Sum.12")], n = n.test, yintercept = 0.85)
#'
#' #' #######################################################
#' ## RUN A GRID SEARCH (takes some time)
#' \donttest{
#' # create a grid of all tuning combinations to try
#'  model.grid <- data.table(expand.grid(stopwords = c(TRUE, FALSE), stemming = c(FALSE, "de"), metric = c("SUM", "ERROR"), k.neighbors = c(2, 10, 17)))
#'
#'  # Do grid search
#'  for (i in 1:nrow(model.grid)) {
#'
#'    res.model <- trainCreecysMemoryBasedReasoning(splitted.data$training, preprocessing = list(stopwords = if (model.grid[i, stopwords]) tm::stopwords("de") else character(0),
#'                                                                                               stemming = if (model.grid[i, stemming == "de"]) "de" else NULL,
#'                                                                                               strPreprocessing = TRUE,
#'                                                                                               removePunct = FALSE))
#'
#'    res.proc <- predictCreecysMemoryBasedReasoning(res.model, splitted.data$test,
#'                                                   tuning = list(k.neighbors = model.grid[i, k.neighbors],
#'                                                                 metric = model.grid[i, metric]))
#'
#'    ac <- accuracy(res.proc[, list(pred.code, pred.prob = confidence.score, acc = pred.code == code, num.suggested.codes = 1, num.suggested = 1, general.among.top5 = pred.code == "-9999", method.name = "Creecy.Sum.12")], n = nrow(splitted.data$test))
#'
#'    model.grid[i, acc := ac[, acc]]
#'    model.grid[i, acc.se := ac[, se]]
#'    model.grid[i, acc.N := ac[, N]]
#'    model.grid[i, acc.prob0 := ac[, count.pred.prob0]]
#'  }
#'
#' model.grid[order(metric, k.neighbors, stemming)]
#' }
predictCreecysMemoryBasedReasoning <- function(model, newdata, tuning = list(k.neighbors = 12, metric = c("SUM", "ERROR", "MAX"))) {

  # get character vector depending on type of input
  if ("occupationData" %in% class(newdata)) {
    ans <- newdata[, ans]
  }
  if (is.character(newdata)) {
    ans <- newdata
  }

  ##########################
  # preprocessing
  preprocessing <- model$preprocessing

  if (preprocessing$removePunct) {
    ans <- tm::removePunctuation(ans)
  }

  if (preprocessing$strPreprocessing) {
    ans <- stringPreprocessing(ans)
  }

  # prepare text for efficient computation -> transform to sparse matrix
  dtmToPredict <- asDocumentTermMatrix(ans, vect.vocab = model$vect.vocab,
                                       stopwords = preprocessing$stopwords,
                                       stemming = preprocessing$stemming,
                                       type = "dgTMatrix")$dtm

  # create features (every word and every combination of 2 words is a feature, neglecting the word order: "single word fields along with all pairwise conjunctions")
  DT.test <- data.table(id.test = dtmToPredict@i + 1, words = dtmToPredict@Dimnames[[2]][dtmToPredict@j + 1])[, .SD[order(words)], by = id.test] # [, .SD[order(words)], by = id.test] creates alphabetical ordering, possibly not needed
  create_features <- function(words) {
    combined_words <- outer(words, words, FUN = paste, sep = ".")
    c(words, combined_words[upper.tri(combined_words)]) # exploiting alphabetic ordering
  }
  DT.test <- DT.test[,.SD[,list(feature = create_features(words))], keyby = id.test]


  ###
  # find matching features
  merged.features <- merge(DT.test, model$training.prepared, by = "feature", all.x = TRUE, all.y = FALSE, allow.cartesian=TRUE)

  ### calculate nearness between training and test cases,
  # select the k nearest training cases
  # and calculate confidence score of the best code
  calculate_confidence_score <- function(a) if (length(a) > 1) a[1] / (a[1] + a[2]) else 1 # or a[1] in the else condition?
  if (tuning$metric == "SUM") {
    nearness <- merged.features[!is.na(id.training), list(nearness = sum(cross.category.feature.weight)), by = list(id.test, id.training, pred.code = code)][, head(.SD[order(nearness, decreasing = TRUE)], n = tuning$k.neighbors), by = id.test]
    res <- nearness[, list(score = sum(nearness)), by = list(id.test, pred.code)]
    res[, confidence.score := .SD[,calculate_confidence_score(sort(score, decreasing = TRUE))], by = id.test]
    res <- res[, .SD[which.max(score), list(pred.code, confidence.score)], by = id.test]
  }
  if (tuning$metric == "ERROR") {
    nearness <- merged.features[!is.na(id.training), list(nearness = 1 - prod(1 - p.code.given.feature)), by = list(id.test, id.training, pred.code = code)][, head(.SD[order(nearness, decreasing = TRUE)], n = tuning$k.neighbors), by = id.test]
    res <- nearness[, list(score = sum(nearness)), by = list(id.test, pred.code)]
    res[, confidence.score := .SD[,calculate_confidence_score(sort(score, decreasing = TRUE))], by = id.test]
    res <- res[, .SD[which.max(score), list(pred.code, confidence.score)], by = id.test]
  }
  if (tuning$metric == "MAX") { # 1 nearest neighbor
    res <- merged.features[,.SD[which.max(p.code.given.feature), list(pred.code = code, confidence.score = p.code.given.feature)], by = id.test]
  }

  ### prepare result
  # include results if no matches are found
  res <- merge(res, data.table(id.test = 1:length(ans)), all = TRUE, by = "id.test")
  res[is.na(pred.code), pred.code := "-9999"]
  res[is.na(confidence.score), confidence.score := 0]
  ###
  # include original answer
  res[, ans := ans[id.test]]

  # add additional columns from new data to the result
  if ("occupationData" %in% class(newdata)) {
    for (i in seq_along(names(newdata))) {
      if (names(newdata)[i] != "ans") {
        set(res, i = NULL, j = names(newdata)[i], value = newdata[, i, with = FALSE])
      }
    }

    # class(res) <- c(class(res), "occupationalPredictions") Only if we had probabilities for more than one category. One could adapt the above function to deliver this.
  }

  return(res)
}
