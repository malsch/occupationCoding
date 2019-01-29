#' Describe occupational data
#'
#' Outputs the following descriptives: frequencies, mean, median, max of number of words. Number of codes used and frequencies for special codes
#'
#' @param data eiter a data.table created with \code{\link{removeFaultyAndUncodableAnswers_And_PrepareForAnalysis}} or a character vector
#' @param noninformative a charactor vector: if an answer is in \code{noninformative}, it is excluded
#'
#' @return Side effects only: a plot and text
#' @import data.table
#' @export
#'
#' @examples
#' data <- data.table(answers = c("LEITER VERTRIEB", "Kfz-Schlossermeister", "Aushilfe im Hotel(Bereich Housekeeping)", "Studentische Hilfskraft"),
#'                    codes = c("61194", "25213", "63221", "-0001"))
#' (allowed.codes <- c("11101", "61194", "25213", "63221", "-0001"))
#' (allowed.codes.titles <- c("Berufe in der Landwirtschaft (ohne Spezialisierung) - Helfer-/Anlernt\xe4tigkeiten", "Berufe in der Kraftfahrzeugtechnik - komplexe Spezialistent\xe4tigkeiten", "F\xfchrungskräfte - Einkauf und Vertrieb", "Berufe im Hotelservice - Helfer-/Anlernt\xe4tigkeiten", "a negative label (used for categories like 'student assistant' that are not in the official classification)"))
#' data <- removeFaultyAndUncodableAnswers_And_PrepareForAnalysis(data, colNames = c("answers", "codes"), allowed.codes, allowed.codes.titles)
#' createDescriptives(data)
#'
#' (answer <- c("LEITER VERTRIEB", "Kfz-Schlossermeister", "Aushilfe im Hotel(Bereich Housekeeping)"))
#' createDescriptives(answer)
#'
#' data(occupations)
#' allowed.codes <- c("71402", "71403", "63302", "83112", "83124", "83131", "83132", "83193", "83194", "-0004", "-0030")
#' allowed.codes.titles <- c("Office clerks and secretaries (without specialisation)-skilled tasks", "Office clerks and secretaries (without specialisation)-complex tasks",
#'  "Gastronomy occupations (without specialisation)-skilled tasks", "Occupations in child care and child-rearing-skilled tasks", "Occupations in social work and social pedagogics-highly complex tasks", "Pedagogic specialists in social care work and special needs education-unskilled/semiskilled tasks", "Pedagogic specialists in social care work and special needs education-skilled tasks", "Supervisors in education and social work, and of pedagogic specialists in social care work", "Managers in education and social work, and of pedagogic specialists in social care work",
#'  "Not precise enough for coding", "Student assistants")
#' occupations <- removeFaultyAndUncodableAnswers_And_PrepareForAnalysis(occupations, colNames = c("orig_answer", "orig_code"), allowed.codes, allowed.codes.titles)
#' createDescriptives(occupations)
createDescriptives <- function (data, noninformative = c("")) {
  if ("occupationData" %in% class(data)) {
    ans <- data[, gsub(" {2,}", " ", ans)] # remove multiple (>2) empty space
  }
  if (is.character(data)) {
    ans <- gsub(" {2,}", " ", data)
  }

  ans <- ans[!(ans %in% noninformative)]
  cat("Number of informative answers:", length(ans), "\n")

  # answer length
  ans_freq <- sapply(strsplit(ans, " "), length)
  plot(table(ans_freq))
  cat("Frequency of Number of Words:\n")
  print(table(ans_freq))
  cat("\n")
  cat("Mean Number of Words: ", mean(ans_freq), "\n")
  cat("Median Number of Words: ", median(ans_freq), "\n")
  cat("Maximal Number of Words: ", max(ans_freq), "\n")

  if (is.data.table(data)) {
    cat("Number of categories (with code > 0) from classification used: ", data[code > 0 & code %in% attr(data, "classification")[, code], .N, by = code][, .N], "\n")
    cat("Frequency of categories (wih code < 0) from classification:\n")
    print(data[code < 0 & code %in% attr(data, "classification")[, code], table(code)])
  }
}
