#' Wordwise Similarity Table with Coding index
#'
#' Calculate string similarity on a word basis between \code{unique.string} and \code{(coding_index_w_codes, coding_index_without_codes)}.
#'
#' Special function for similarity-based reasoning: creates WORDWISE!!!!! distance data with osa-method c(d = 1, i = 1, s = 1, t = 1)
#' --> allows to correct 1 letter in a single word and matches this word with the dictionary.
#' This means: unique.string is split wordwise and that word is used for matchning which has lowest osa-distance (all in case of a tie)
#' example:
#' "KUESTER and HAUSMEISTER" has distance 0 to both dictString.title HAUSMEISTER and KUESTER. Because the word HAUSMEISTER has minimal distance, another dictString.title HAUMEISTER, which has dist = 1 is not included.
#'
#' @param unique.string a character vector (usually unique(answer))
#' @param coding_index_w_codes a data.table with columns "title" and "Code".
#' @param coding_index_without_codes a character vector of additional titles
#' @param preprocessing a list with elements
#' \describe{
#'   \item{stopwords}{a character vector, use \code{tm::stopwords("de")} for German stopwords.}
#'   \item{stemming}{\code{NULL} for no stemming and \code{"de"} for stemming using the German porter stemmer.}
#'   \item{strPreprocessing}{\code{TRUE} if \code{\link{stringPreprocessing}} shall be used.}
#'   \item{removePunct}{\code{TRUE} if \code{\link[tm]{removePunctuation}} shall be used.}
#' }
#' @param dist.control a list that will be passed to \code{\link[stringdist]{stringdistmatrix}}. Currently only two elements are implemented:
#' \describe{
#'   \item{method}{Method for distance calculation.}
#'   \item{weight}{For method='osa' or 'dl'.}
#' }
#' @param threshold All entries with distance above this threshold will be removed from the result
#'
#' @seealso \code{\link{trainSimilarityBasedReasoning}}, \code{\link{createSimilarityTableStringdist}}, \code{\link{createSimilarityTableSubstring}}
#'
#' @return a list with elements
#' \describe{
#'   \item{dist_table_w_code}{a data.table with colummns \code{intString}, \code{dictString.title}, \code{dictString.Code}, \code{dist}}
#'   \item{dist_table_without_code}{\code{NULL} or a data.table with colummns \code{intString}, \code{dictString}, \code{dist}}
#'   \item{vect_vocab}{see \code{link{asDocumentTermMatrix}}}
#' }
#'
#' @examples
#' ## Prepare coding index
#' # write female titles beneath the male title
#' coding_index <- rbind(coding_index_excerpt[, list(title = bezMale, Code)],
#'                       coding_index_excerpt[, list(title = bezFemale, Code)])
#' # standardize titles from the coding index
#' coding_index <- coding_index[,title := stringPreprocessing(title)]
#' # drop duplicate lines, might be suboptimal because we keep each title and its associated code only a single time. This means we delete duplicates and the associated, possibly relevant codes.
#' coding_index <- coding_index[!duplicated(title)]
#'
#' (x <- c("Abgeordneter", "Abgeordneter", "Abgeordnete", "abgeordnet", "abgeordnet zu xxx", "FSJ", "FSJ2", "Industriemechaniker", "Dipl.-Ing. - Agrarwirtschaft (Landwirtschaft)"))
#' createSimilarityTableWordwiseStringdist(unique.string = stringPreprocessing(x),
#'                                         coding_index_w_codes = coding_index,
#'                                          coding_index_without_codes = frequent_phrases,
#'                                          preprocessing = list(stopwords = tm::stopwords("de"), stemming = NULL, strPreprocessing = TRUE, removePunct = FALSE),
#'                                          dist.control = list(method = "osa", weight = c(d = 1, i = 1, s = 1, t = 1)),
#'                                          threshold = 1)
createSimilarityTableWordwiseStringdist <- function(unique.string,
                                                    coding_index_w_codes,
                                                    coding_index_without_codes,
                                                    preprocessing,
                                                    dist.control = list(method = "osa", weight = c(d = 1, i = 1, s = 1, t = 1)),
                                                    threshold = 1) {

  # prepare text for efficient computation -> transform to sparse matrix
  matrix <- asDocumentTermMatrix(unique.string, vect.vocab = NULL,
                                 stopwords = preprocessing$stopwords,
                                 stemming = preprocessing$stemming,
                                 type = "dgTMatrix")

  distmat <- stringdist::stringdistmatrix(coding_index_w_codes[, title], toupper(matrix$dtm@Dimnames[[2]]), method = dist.control$method, weight = dist.control$weight) # calculate distances between each word in matrix1 and the coding_index_w_codes
  distmat.table <- data.table(word.id = which(distmat <= threshold, arr.ind = TRUE)[, 2], dictString = coding_index_w_codes[, list(title, Code)][which(distmat <= threshold, arr.ind = TRUE)[, 1]], dist = distmat[which(distmat <= threshold, arr.ind = TRUE)])
  distmat.table <- merge(distmat.table, data.table(intString = unique.string[matrix$dtm@i + 1], word.id = matrix$dtm@j + 1), by = "word.id", allow.cartesian=TRUE) # we have now columns for each unique.string, word, dictString, dist
  # now keep all words that have at least one dictString with minimal distance
  distmat.table[, dist2 := min(dist), by = list(intString, word.id)]
  dist_table_w_code <- distmat.table[, .SD[dist == dist2, list(dictString.title, dictString.Code, dist)], by = intString]

  dist_table_without_code <- NULL
  # do the same for coding_index_without_codes
  if (length(coding_index_without_codes) > 2) {
    distmat <- stringdist::stringdistmatrix(coding_index_without_codes, toupper(matrix$dtm@Dimnames[[2]]), method = dist.control$method, weight = dist.control$weight) # calculate distances between each word in matrix1 and the coding_index
    distmat.table <- data.table(word.id = which(distmat <= threshold, arr.ind = TRUE)[, 2], dictString = coding_index_without_codes[which(distmat <= threshold, arr.ind = TRUE)[, 1]], dist = distmat[which(distmat <= threshold, arr.ind = TRUE)])
    distmat.table <- merge(distmat.table, data.table(intString = unique.string[matrix$dtm@i + 1], word.id = matrix$dtm@j + 1), by = "word.id", allow.cartesian=TRUE) # we have now columns for each unique.string, word, dictString, dist
    # now keep all words that have at least one dictString with minimal distance
    distmat.table[, dist2 := min(dist), by = list(intString, word.id)]
    dist_table_without_code <- distmat.table[, .SD[dist == dist2, list(dictString, dist)], by = intString]
  }

  return(list(dist_table_w_code = dist_table_w_code, dist_table_without_code = dist_table_without_code, vect_vocab = matrix$vect.vocab))

}
