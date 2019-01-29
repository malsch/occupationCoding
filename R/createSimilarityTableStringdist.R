#' Similarity Table with Coding index
#'
#' Calculate string similarity between \code{unique.string} and \code{(coding_index_w_codes, coding_index_without_codes)}.
#'
#' Special function for similarity-based reasoning: creates distance data with osa-method c(d = 1, i = 1, s = 1, t = 1)
#' dist == 0: strings in dict and data are identical
#  dist == 1: exactly one deletion, insertion, substitution, or transposition
#  dist == 2: exactly two deletion, insertion, substitution, or transposition, the majority is a mismatch with dist == 2
#'
#' @param unique.string a character vector (usually unique(answer))
#' @param coding_index_w_codes a data.table with columns "title" and "Code".
#' @param coding_index_without_codes a character vector of additional titles
#' @param dist.control a list that will be passed to \code{\link[stringdist]{stringdistmatrix}}. Currently only two elements are implemented:
#' \describe{
#'   \item{method}{Method for distance calculation.}
#'   \item{weight}{For method='osa' or 'dl'.}
#' }
#' @param threshold All entries with distance above this threshold will be removed from the result
#'
#' @seealso \code{\link{trainSimilarityBasedReasoning}}, \code{\link{createSimilarityTableWordwiseStringdist}}, \code{\link{createSimilarityTableSubstring}}
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
#' (x <- c("Abgeordneter", "Abgeordneter", "Abgeordnete", "abgeordnet", "FSJ", "FSJ2", "Industriemechaniker", "Dipl.-Ing. - Agrarwirtschaft (Landwirtschaft)"))
#' createSimilarityTableStringdist(unique.string = stringPreprocessing(x),
#'                                 coding_index_w_codes = coding_index,
#'                                 coding_index_without_codes = frequent_phrases,
#'                                 dist.control = list(method = "osa", weight = c(d = 1, i = 1, s = 1, t = 1)),
#'                                 threshold = 3)
createSimilarityTableStringdist <- function(unique.string,
                                            coding_index_w_codes,
                                            coding_index_without_codes,
                                            dist.control = list(method = "osa", weight = c(d = 1, i = 1, s = 1, t = 1)),
                                            threshold = 3) {

  distmat <- stringdist::stringdistmatrix(coding_index_w_codes[, title], unique.string, method = dist.control$method, weight = dist.control$weight)
  dist_table_w_code <- data.table(intString = unique.string[which(distmat <= threshold, arr.ind = TRUE)[, 2]], dictString = coding_index_w_codes[, list(title, Code)][which(distmat <= threshold, arr.ind = TRUE)[, 1]], dist = distmat[which(distmat <= threshold, arr.ind = TRUE)])

  dist_table_without_code <- NULL
  # do the same for coding_index_without_codes
  if (length(coding_index_without_codes) > 2) {
    distmat <- stringdist::stringdistmatrix(coding_index_without_codes, unique.string, method = dist.control$method, weight = dist.control$weight)
    dist_table_without_code <- data.table(intString = unique.string[which(distmat <= threshold, arr.ind = TRUE)[, 2]], dictString = coding_index_without_codes[which(distmat <= threshold, arr.ind = TRUE)[, 1]], dist = distmat[which(distmat <= threshold, arr.ind = TRUE)])
  }

  return(list(dist_table_w_code = dist_table_w_code, dist_table_without_code = dist_table_without_code, vect_vocab = NULL))

}
