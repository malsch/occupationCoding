#' Code answers with a coding index
#'
#' Look up the correct code in a coding index. We often find no code, 1 code or even more than one possible code this way.
#'
#' @param newdata either a data.table created with \code{\link{removeFaultyAndUncodableAnswers_And_PrepareForAnalysis}} or a character vector.
#' @param coding_index a data.table as created with function \code{\link{prepare_German_coding_index_Gesamtberufsliste_der_BA}}
#' @param include.substrings (default: \code{FALSE}). If \code{FALSE}, a match is found if, after preprocessing, the entry from the coding index and the string-element are exactly identical. If TRUE (Attention: THIS IS SLOW!!), a match is found if, after preprocessing, the entry from the coding index is a substring of the string-element.
#' @param max.count.categories (default: \code{Inf}). Should we search the whole coding index (default) or should we exclude entries with large \code{count_categories}, an indicator of job title ambiguity? Only entries in the coding index with \code{count_categories \eqn{\le} max.count.categories} are searched.
#'
#' @seealso
#' \code{\link{predictSimilarityBasedReasoning}}
#'
#' @return a data.table with columns id, ans, and pred.code (format is not comparable to other formats in this package.)
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
#' # recommended default
#' res <- predictWithCodingIndex(proc.occupations,
#'                               coding_index = coding_index_excerpt,
#'                               include.substrings = FALSE,
#'                               max.count.categories = Inf)
#'
#' # playing around with the parameters to obtain other results
#' res <- predictWithCodingIndex(proc.occupations,
#'                               coding_index = coding_index_excerpt,
#'                               include.substrings = TRUE,
#'                               max.count.categories = 15)
#'
#' #################################
#' # Analysis: Standard functions from this package won't work here.
#' # Absolute numbers: either nothing is predicted (nPredictedCodes = NA), or 1 or more cods are predicted
#' res[ , .N, by = list(nPredictedCodes = 1 + nchar(pred.code) %/% 6 )]
#' # Relative Numbers
#' res[ , .N / res[, .N], by = list(nPredictedCodes = 1 + nchar(pred.code) %/% 6 )]
#' # Agreement rate among answers where only a single code was predicted
#' res[nchar(pred.code) == 5, mean(pred.code == code)]
predictWithCodingIndex <- function(newdata, coding_index,
                                   include.substrings = FALSE, max.count.categories = Inf) {

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

  ###########################################
  ###### prepare coding index ######

  # write female titles beneath the male titles
  coding_index <- rbind(coding_index[, list(title = bezMale, Code, count_categories)],
                        coding_index[, list(title = bezFemale, Code, count_categories)])
  # drop duplicate lines
  coding_index <- coding_index[,.N, by = list(title, Code, count_categories)
                               ][,list(title, Code, count_categories)]

  # standardize titles from the coding index
  coding_index <- coding_index[,title := stringPreprocessing(title)]


  ###########################################
  ####### do coding with the dictionary #####
  string.proc <- stringPreprocessing(ans)
  if (!include.substrings) {
    # substrings are not considered as matches
    res <- sapply(string.proc, function(str) {
      kldb_code <- coding_index[title == str & count_categories <= max.count.categories,
                                paste(unique(Code), collapse = ",")]
      if (kldb_code == "" | length(kldb_code) == 0) kldb_code <- NA
      return(kldb_code)
    })
  } else {
    # substrings are considered as matches

    # loop through coding_index$titles
    coding_index_matches <- sapply(coding_index$title, grepl, string.proc)
    # loop through string.proc
    res <- apply(coding_index_matches, 1, function(vec) coding_index[vec & count_categories <= max.count.categories, paste(unique(Code), collapse = ",")])
    res[res == "" | length(res) == 0] <- NA
  }

  res <- data.table(id, ans, pred.code = res)

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
