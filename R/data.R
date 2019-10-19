#' A selection of 250 coded occupational answers
#'
#' A dataset containing 250 selected verbal answers and their associated codes from the 2010 German classification of Occupation. This dataset is not meant to be representative, but only used to demonstrate the functionality of the package. We anticipate that accurate occupation coding will be more difficult with real data compared to this toy example.
#'
#' @format A data frame with 250 rows and 3 variables:
#' \describe{
#'   \item{orig_answer}{verbal answer given to the first question about occupation}
#'   \item{orig_answer2}{verbal answer given to the second question about occupation (not used (much) in this package but it illustrates that this and several other variables are used by human coders)}
#'   \item{orig_code}{5-digit codes from the German classification of Occupation. Negative codes are not part of the official classification but were used for coding. -0030 refers to student assistants, -0004 means that the job description was not precise enough for coding.}
#' }
#' @source
#' The following codes were hand-selected and double-checked to ensure anonymity: 71402 (50 cases), 71403 (20 cases), 63302 (20 cases), 83112 (25 cases), 83124 (25 cases), 83131 (20 cases), 83132 (20 cases), 83193 (15 cases), 83194 (5 cases), -0004 (25 cases), -0030 (25 cases)
#'
#' Antoni, M., Drasch, K., Kleinert, C., Matthes, B., Ruland, M. and Trahms, A. (2010). Arbeiten und Lernen im Wandel * Teil 1: Überblick über die Studie, FDZ-Methodenreport 05/2010, Forschungsdatenzentrum der Bundesagentur fur Arbeit im Institut für Arbeitsmarkt- und Berufsforschung, Nuremberg.
#'
#' Drasch, K., Matthes, B., Munz, M., Paulus, W. and Valentin, M.-A. (2012). Arbeiten und Lernen im Wandel * Teil V: Die Codierung der offenen Angaben zur beruflichen Tätigkeit, Ausbildung und Branche, FDZ-Methodenreport 04/2012, Forschungsdatenzentrum der Bundesagentur für Arbeit im Institut für Arbeitsmarkt- und Berufsforschung, Nuremberg.
#'
#' @encoding UTF-8
"occupations"

#' An excerpt from the Gesamtberufsliste der BA
#'
#' 90 selected job titles from the \code{Gesamtberufsliste der BA}. This dataset is not complete and only used to demonstrate the functionality of the package. We recommend that users build their own coding index. \code{\link{prepare_German_coding_index_Gesamtberufsliste_der_BA}} can be used to build a German coding index.
#'
#' @format A data frame with 90 rows and 5 variables:
#' \describe{
#'   \item{Berufsbenennungen}{neutral short title}
#'   \item{bezMale}{male long title}
#'   \item{bezFemale}{female long title}
#'   \item{Code}{Code from the German Classification of Occupations (KldB 2010)}
#'   \item{count_categories}{An indicator of job title ambiguity. Only used within function \code{\link{predictWithCodingIndex}}}
#' }
#' @source
#' Bundesagentur für Arbeit (2019). Gesamtberufsliste der Bundesagentur für Arbeit. Stand: 03.01.2019. The \code{Gesamtberufsliste der BA} is available at \url{https://download-portal.arbeitsagentur.de/files/}.
#'
#' The function \code{\link{prepare_German_coding_index_Gesamtberufsliste_der_BA}} was used to process the downloaded \code{.xlsx}-file and prepare this coding index. The resulting coding index has 27853 rows. We selected 90 rows that are related to the \code{\link{occupations}} dataset.
#'
#' @encoding UTF-8
"coding_index_excerpt"

#' Some job titles and job descriptions 
#'
#' There exist some job titles/descriptions that are not part of the \code{Gesamtberufsliste_der_BA} (see \code{\link{prepare_German_coding_index_Gesamtberufsliste_der_BA}}), possibly because they are too general or because they are misspelled. They are still common answers that should not be ignored. We use these answers within the function \code{\link{trainSimilarityBasedReasoning}}.
#' 
#' As a general rule, we included some of the most frequent verbal answers (after \code{\link{stringPreprocessing}}) if they have more than three characters and have no empty spaces. Creating predictions for these answers would be impossible if only the job titles from the Gesamtberufsliste were used only.
#'
#' @format A character vector with 701 elements
#'
#' @encoding UTF-8
"frequent_phrases"
