#' From multiple prediction methods, select the prediction method for each id which returns highest probability
#'
#' Start with a data.table of class 'occupationalPredictionsComplete' (for each combination of pred.code and answer one prediction, we also should have this for multiple prediction methods), calculate for each id the probability of the top k categories, and select for each id the prediction method which returns highest probability. The so-found method is then used for this id and is called \code{new.method.name}.
#'
#' The problem solved here is this: \code{\link{trainXgboost}} is good for most answers and for interactions. But xgboost fails if a keyword was misspelled or a job title is in the alphabetic dictionary but not in the training data. In those cases we would like to use a prediction method from \code{\link{trainSimilarityBasedReasoning}} which will return higher probabilities.
#'
#' @param occupationalPredictions a data.table created with the \code{\link{expandPredictionResults}}-function from this package. Actually, the utility of this function is only if we \code{rbind} several such data.tables together (see example).
#' @param combined.methods a character vector of methods to select from. We will only use the subset of rows from \code{occupationalPredictions} with \code{method.name 'in' combined.methods} (same names as assigned in \code{\link{expandPredictionResults}}).
#' @param new.method.name the name how the highest-probability-method shall be called.
#' @param k Calculate probability over \code{k} most probable categories.
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
#' attr(splitted.data$training, "classification")$code <- attr(proc.occupations, "classification")$code
#'
#' ####### train models
#' # first model uses dist.type = wordwise and some other recommended settings (n.draws could be higher)
#' simBasedModel <- trainSimilarityBasedReasoning(data = splitted.data$training,
#'                               coding_index_w_codes = coding_index_excerpt,
#'                               coding_index_without_codes = frequent_phrases,
#'                               preprocessing = list(stopwords = tm::stopwords("de"), stemming = NULL, strPreprocessing = TRUE, removePunct = FALSE),
#'                               dist.type = "wordwise",
#'                               dist.control = list(method = "osa", weight = c(d = 1, i = 1, s = 1, t = 1)),
#'                               threshold = c(max = 3, use = 1), simulation.control = list(n.draws = 50, check.normality = FALSE),
#'                               tmp_folder = "similarityTables")
#'
#' res1 <- expandPredictionResults(predictSimilarityBasedReasoning(simBasedModel, splitted.data$test), allowed.codes, method.name = "WordwiseSimilarityOsa1111")
#'
#' # second model uses dist.type = substring and some other recommended settings (n.draws could be higher)
#' simBasedModel <- trainSimilarityBasedReasoning(data = splitted.data$training,
#'                               coding_index_w_codes = coding_index_excerpt,
#'                               coding_index_without_codes = frequent_phrases,
#'                               preprocessing = list(stopwords = NULL, stemming = NULL, strPreprocessing = TRUE, removePunct = FALSE),
#'                               dist.type = "substring",
#'                               dist.control = list(method = "substring", weight = numeric()),
#'                               threshold = c(0, 0), simulation.control = list(n.draws = 50, check.normality = FALSE),
#'                               tmp_folder = "similarityTables")
#'
#' res2 <- expandPredictionResults(predictSimilarityBasedReasoning(simBasedModel, splitted.data$test, parallel = TRUE), allowed.codes, method.name = "substringSimilarity")
#'
#' # third model uses dist.type = fulltext and some other recommended settings (n.draws could be higher)
#' simBasedModel <- trainSimilarityBasedReasoning(data = proc.occupations,
#'                               coding_index_w_codes = coding_index_excerpt,
#'                               coding_index_without_codes = frequent_phrases,
#'                               preprocessing = list(stopwords = NULL, stemming = NULL, strPreprocessing = TRUE, removePunct = FALSE),
#'                               dist.type = "fulltext",
#'                               dist.control = list(method = "osa", weight = c(d = 1, i = 1, s = 1, t = 1)),
#'                               threshold = c(max = 3, use = 1), simulation.control = list(n.draws = 50, check.normality = FALSE),
#'                               tmp_folder = "similarityTables")
#' res3 <- expandPredictionResults(predictSimilarityBasedReasoning(simBasedModel, splitted.data$test), allowed.codes, method.name = "FulltextSimilarityOsa1111")
#'
#' res.combined <- rbind(res1, res2, res3); class(res.combined) <- class(res1)
#'
#' res.max <- selectMaxProbMethod(res.combined, combined.methods = c("WordwiseSimilarityOsa1111", "substringSimilarity"), k = 1, new.method.name = "maxProbAmong1")
#' res.combined <- rbind(res.combined, res.max); class(res.combined) <- class(res1)
#' produceResults(res.combined, k = 1, n = n.test, num.codes = length(allowed.codes))
selectMaxProbMethod <- function(occupationalPredictions, combined.methods = c("xgboost", "SimilarityBasedSubstring", "SimilarityBasedWordwise"), k = 1, new.method.name = "maxProbAmong1") {

  if (!("occupationalPredictionsComplete" %in% class(occupationalPredictions))){
    stop("'occupationalPredictions' needs to have class 'occupationalPredictionsComplete' (=constructed with ?expandPredictionResults).")
  }

  methods.not.mentioned.in.occupationalPredictions <- setdiff(combined.methods, occupationalPredictions[, unique(method.name)])
  if (length(methods.not.mentioned.in.occupationalPredictions) > 0)
    stop(paste("Some methods from combined.methods are not available in occupationalPredictions:", methods.not.mentioned.in.occupationalPredictions, "Please update combined.methods."))

  select.max.prob.method <- calcAccurateAmongTopK(occupationalPredictions[method.name %in% combined.methods], k = k)
  select.max.prob.method <- merge(select.max.prob.method[, .SD[which.max(pred.prob), .(method.name)], by = id], occupationalPredictions, by = c("id", "method.name"))
  select.max.prob.method[, method.name := new.method.name]

  class(select.max.prob.method) <- class(occupationalPredictions)
  return(select.max.prob.method)
}
