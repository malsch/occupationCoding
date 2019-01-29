#' Train Creecys Memory-based reaoning model
#'
#' The function does some preprocessing and calculates the importance of various features.
#'
#' @param data a data.table created with \code{\link{removeFaultyAndUncodableAnswers_And_PrepareForAnalysis}}
#' @param preprocessing a list with elements
#' \describe{
#'   \item{stopwords}{a character vector, use \code{tm::stopwords("de")} for German stopwords.}
#'   \item{stemming}{\code{NULL} for no stemming and \code{"de"} for stemming using the German porter stemmer.}
#'   \item{strPreprocessing}{\code{TRUE} if \code{\link{stringPreprocessing}} shall be used.}
#'   \item{removePunct}{\code{TRUE} if \code{\link[tm]{removePunctuation}} shall be used.}
#' }
#'
#' @seealso
#' \code{\link{predictCreecysMemoryBasedReasoning}}
#'
#' Creecy, R. H., Masand, B. M., Smith, S. J., Waltz, D. L. (1992). Trading MIPS and Memory for Knowledge Engineering. Comm. ACM 35(8). pp. 48--65.
#'
#' @return a processed feature matrix to be used in \code{\link{predictCreecysMemoryBasedReasoning}}
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
#' # Recommended configuration (and commonly used in this package)
#' memModel <- trainCreecysMemoryBasedReasoning(proc.occupations,
#'                  preprocessing = list(stopwords = character(0), stemming = NULL, strPreprocessing = TRUE, removePunct = FALSE))
trainCreecysMemoryBasedReasoning <- function(data,
                                       preprocessing = list(stopwords = character(0), stemming = NULL, strPreprocessing = TRUE, removePunct = TRUE)) {

  codes <- data[,code]

  # preprocessing
  if (preprocessing$removePunct) {
    ans <- data[, tm::removePunctuation(ans)]
  } else {
    ans <- data[, ans]
  }

  if (preprocessing$strPreprocessing) {
    ans <- stringPreprocessing(ans)
  }

  # prepare text for efficient computation -> transform to sparse matrix
  matrix <- asDocumentTermMatrix(ans, vect.vocab = NULL,
                                 stopwords = preprocessing$stopwords,
                                 stemming = preprocessing$stemming,
                                 type = "dgTMatrix")

  # create features (every word and every combination of 2 words is a feature, neglecting the word order: "single word fields along with all pairwise conjunctions")
  DT.wordwise <- data.table(id.training = matrix$dtm@i + 1, words = matrix$dtm@Dimnames[[2]][matrix$dtm@j + 1])[, .SD[order(words)], by = id.training] # [, .SD[order(words)], by = id.training] creates alphabetical ordering, possibly not needed
  create_features <- function(words) {
    combined_words <- outer(words, words, FUN = paste, sep = ".")
    c(words, combined_words[upper.tri(combined_words)]) # exploiting alphabetic ordering
  }
  DT.wordwise <- DT.wordwise[,.SD[,list(feature = create_features(words))], keyby = id.training]
  # DT.wordwise[id.training == 8]
  # save for each feature the assigned code
  training.prepared <- data.table(id.training = 1:length(ans), code = codes, key = "id.training")[DT.wordwise]

  # calculate feature importance
  training.prepared[, feature_freq := .N, by = feature]
  # "per category feature importance", P(C_i | f_k)
  training.prepared[, p.code.given.feature := .N / feature_freq, by = list(feature, code)]
  # "cross category" feature importance Weight f_k = sum P(C_i | f_k) ^2
  training.prepared[, cross.category.feature.weight := .SD[!duplicated(code), sum(p.code.given.feature^2)], by = feature]

  return(list(training.prepared = training.prepared, vect.vocab = matrix$vect.vocab, preprocessing = preprocessing))
}
