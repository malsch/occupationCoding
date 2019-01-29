#' Data Preparation
#'
#' Prepare data (i.e. columns 'id', 'ans', and 'code' are appended to the dataset - only these columns will be used later) and remove answer that we cannot use (i.e. answers that have non-ASCII characters after preprocessing and answers that are at most one character long). During data preparation you should make sure that nothing important is lost here.
#'
#' The 2010 German classification is available at \url{https://www.klassifikationsserver.de/}.
#'
#' @param answers a character vector of answers
#' @param codes a vector of classification codes having the same length as \code{answers}. Will be transformed to character.
#' @param allowed.codes a vector of allowed codes from the classification.
#' @param allowed.codes.titles Labels for \code{allowed.codes}. Should have the same length as \code{allowed.codes}.
#'
#' @seealso \code{\link{createDescriptives}}
#'
#' @return a data.table with attributes \code{classification} and \code{overview_tally}
#' @import data.table
#' @export
#'
#' @examples
#' occupations <- data.table(answers = c("LEITER VERTRIEB", "Kfz-Schlossermeister", "Aushilfe im Hotel(Bereich Housekeeping)"),
#'                    codes = c("61194", "25213", "63221"))
#' (allowed.codes <- c("11101", "61194", "25213", "63221", "..."))
#' (allowed.codes.titles <- c("Berufe in der Landwirtschaft (ohne Spezialisierung) - Helfer-/Anlernt\xe4tigkeiten", "Berufe in der Kraftfahrzeugtechnik - komplexe Spezialistent\xe4tigkeiten", "F\xfchrungskräfte - Einkauf und Vertrieb", "Berufe im Hotelservice - Helfer-/Anlernt\xe4tigkeiten", "many more category labels from the classification"))
#' removeFaultyAndUncodableAnswers_And_PrepareForAnalysis(occupations, colNames = c("answers", "codes"), allowed.codes, allowed.codes.titles)
#'
#' data(occupations)
#' allowed.codes <- c("71402", "71403", "63302", "83112", "83124", "83131", "83132", "83193", "83194", "-0004", "-0030")
#' allowed.codes.titles <- c("Office clerks and secretaries (without specialisation)-skilled tasks", "Office clerks and secretaries (without specialisation)-complex tasks", "Gastronomy occupations (without specialisation)-skilled tasks",
#'  "Occupations in child care and child-rearing-skilled tasks", "Occupations in social work and social pedagogics-highly complex tasks", "Pedagogic specialists in social care work and special needs education-unskilled/semiskilled tasks", "Pedagogic specialists in social care work and special needs education-skilled tasks", "Supervisors in education and social work, and of pedagogic specialists in social care work", "Managers in education and social work, and of pedagogic specialists in social care work",
#'  "Not precise enough for coding", "Student assistants")
#' removeFaultyAndUncodableAnswers_And_PrepareForAnalysis(occupations, colNames = c("orig_answer", "orig_code"), allowed.codes, allowed.codes.titles)
#'
#' ## we could also paste both answers together
#' occupations[, answer_combined := paste(orig_answer, orig_answer2)]
#' removeFaultyAndUncodableAnswers_And_PrepareForAnalysis(occupations, colNames = c("answer_combined", "orig_code"), allowed.codes, allowed.codes.titles)
removeFaultyAndUncodableAnswers_And_PrepareForAnalysis <- function(data, colNames = c("answer", "code"), allowed.codes, allowed.codes.titles = 1:length(allowed.codes)) {

  # the classification contains all allowed codes
  classification <- data.table(code = allowed.codes, title = allowed.codes.titles)

  # insert 2 new columns into data, bases on colNames
  res <- copy(data)
  res[, "ans" := as.character(get(colNames[1]))]
  res[, "code" := as.character(get(colNames[2]))]

  # provide some additional info about removed answers
  overview_tally_preparation <- list(N_verbatims_start = nrow(res))

  ## remove non-ASCII answers
  overview_tally_preparation$N_ASCII_removed <- res[,sum(!tau::is.ascii(stringPreprocessing(ans)))]
  res <- res[tau::is.ascii(stringPreprocessing(ans)),]

  ## remove answers with at most one character
  overview_tally_preparation$N_short_verbatims <- res[nchar(stringPreprocessing(ans)) <= 1, .N]
  res <- res[nchar(stringPreprocessing(ans)) > 1, ]

  ## remove answers and codes if the code is not allowed
  overview_tally_preparation$N_Codes_removed <- res[!(code %in% allowed.codes), .N]
  res <- res[(code %in% allowed.codes)]

  # set ids
  res[, id := 1:.N]
  setkey(res, id)

  attr(res, "classification") <- classification
  attr(res, "overview_tally") <- overview_tally_preparation
  class(res) <- c(class(res), "occupationData")

  cat("Number of cases at start:", overview_tally_preparation$N_verbatims_start, "\n")
  cat("Number of cases with non-ASCII characters (removed):", overview_tally_preparation$N_ASCII_removed, "\n")
  cat("Number of cases with codes that are not allowed (removed):", overview_tally_preparation$N_Codes_removed, " (check with 'data[!(code %in% allowed.codes), table(code)]')\n")
  cat("Number of cases with at most one character (removed):", overview_tally_preparation$N_short_verbatims, "\n")
  cat("Number of remaining cases:", res[, .N], "\n")

  return(res)
}
