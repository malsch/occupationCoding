#' Similarity Table with Coding index
#'
#' Calculate SUBSTRING similarity between \code{unique.string} and \code{(coding_index_w_codes, coding_index_without_codes)}. unique.string and coding_index are similar if coding_index is a substring of unique.string.
#'
#' Special function for similarity-based reasoning: creates distance data with substring-method
#'
#' @param unique.string a character vector (usually unique(answer))
#' @param coding_index_w_codes a data.table with columns "title" and "Code".
#' @param coding_index_without_codes a character vector of additional titles
#'
#' @seealso \code{\link{trainSimilarityBasedReasoning}}, \code{\link{createSimilarityTableWordwiseStringdist}}, \code{\link{createSimilarityTableStringdist}}, \code{\link{createSimilarityTableStringdist}}
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
#' createSimilarityTableSubstring(unique.string = stringPreprocessing(x),
#'                                coding_index_w_codes = coding_index,
#'                                coding_index_without_codes = frequent_phrases)
createSimilarityTableSubstring <- function(unique.string,
                                           coding_index_w_codes,
                                           coding_index_without_codes) {


  # create distance data where dictionary string is substring from survey string
  #####################################################
  # find all indices of survey strings where coding_index[i,] is a substring
  distmat.ind <- lapply(1:nrow(coding_index_w_codes), FUN = function(i) grep(coding_index_w_codes[i, title], unique.string))
  # bring them in data.table format
  length.ind <- sapply(distmat.ind, length)
  distmat.ind <- data.table(dict = rep(1:nrow(coding_index_w_codes), times = length.ind), u.string = unlist(distmat.ind))
  # get strings instead of indices
  dist_table_w_code <- data.table(intString = unique.string[distmat.ind[, u.string]], dictString = coding_index_w_codes[, list(title, Code)][distmat.ind[, dict]], dist = 0L)

  dist_table_without_code <- NULL
  # do the same for coding_index_without_codes
  if (length(coding_index_without_codes) > 2) {
    distmat.ind <- lapply(1:length(coding_index_without_codes), FUN = function(i) grep(coding_index_without_codes[i], unique.string))
    # bring them in data.table format
    length.ind <- sapply(distmat.ind, length)
    distmat.ind <- data.table(dict = rep(1:length(coding_index_without_codes), times = length.ind), u.string = unlist(distmat.ind))
    # get strings instead of indices
    dist_table_without_code <- data.table(intString = unique.string[distmat.ind[, u.string]], dictString = coding_index_without_codes[distmat.ind[, dict]], dist = 0L)
  }

  return(list(dist_table_w_code = dist_table_w_code, dist_table_without_code = dist_table_without_code, vect_vocab = NULL))
}
