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

#' 2010 German Classification of Occupations (KldB 2010)
#'
#' This file contains the labels for all 1286 five-digit categories (Berufsgattungen) from the 2010 German Classification of Occupations.
#' 
#' Five additional categories were added for coding purposes: (-0030 = Studentische Hilfskraft, -0004 = Berufsbeschreibung zu unpräzise/nicht kodierbar, -0006 = Mehrere Jobs, -0012 = Arbeiter/in (nicht näher spezifiziert), -0019 = Freiwilligendienst, FSJ, Zivildienst)
#' 
#' @format A data frame with 2 variables:
#' \describe{
#'   \item{code}{5-digit number (Berufsgattung)}
#'   \item{title}{Category label}
#' }
#' @source
#' Federal Employment Agency (2011). Klassifikation der Berufe 2010, Bundesagentur für Arbeit, Nuremberg.
#' 
#' @encoding UTF-8
"kldb2010PlusFive"

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

#' Anonymized training data (substring similarity) to be used with Similarity-based Reasoning
#'
#' This aggregated (anonymized) training data is to be used within the \code{\link{trainSimilarityBasedReasoning2}}-function (\code{dist.type = "substring"}), see the documentation therein. It allows the coding of German language occuptions into the German Classification of Occupations (KldB 2010).
#'
#' @format A data.table with 39242 rows and 3 variables:
#' \describe{
#'   \item{dictString}{Job titles (and similars). They were either taken from the \code{Gesamtberufsliste_der_BA} or from \code{\link{frequent_phrases}}}
#'   \item{survCode}{5-digit codes from the survey data}
#'   \item{N}{Frequency of how often a survey text identical or similar to \code{dictString} was coded as \code{survCode} (using substring-similarity)}
#' }
#'
#' Substring-similarity means that, to be counted, the survey response must contain the text in dictString, i.e., dictString must be identical or shorter than the original text provided by the respondent.
#'
#' @seealso
#' See \code{\link{trainSimilarityBasedReasoning2}}, for which this data set was created, and \code{\link{surveyCountsWordwiseSimilarity}}, which has been created the same way but uses a different metric to calculate string similarities.
#'
#' @source
#' Data from the following surveys were pooled:
#'
#' Antoni, M., Drasch, K., Kleinert, C., Matthes, B., Ruland, M. and Trahms, A. (2010): Arbeiten und Lernen im Wandel * Teil 1: Überblick über die Studie, FDZ-Methodenreport 05/2010, Forschungsdatenzentrum der Bundesagentur für Arbeit im Institut für Arbeitsmarkt- und Berufsforschung, Nuremberg.
#'
#' Rohrbach-Schmidt, D., Hall, A. (2013): BIBB/BAuA Employment Survey 2012, BIBB-FDZ Data and Methodological Reports Nr. 1/2013. Version 4.1, Federal Institute for Vocational Education and Training (Research Data Centre), Bonn.
#'
#' Lange, C., Finger, J., Allen, J., Born, S., Hoebel, J., Kuhnert, R., Müters, S., Thelen, J., Schmich, P., Varga, M., von der Lippe, E., Wetzstein, M., Ziese, T. (2017): Implementation of the European Health Interview Survey (EHIS) into the German Health Update (GEDA), Archives of Public Health, 75, 1–14.
#'
#' Hoffmann, R., Lange, M., Butschalowsky, H., Houben, R., Schmich, P., Allen, J., Kuhnert, R., Schaffrath Rosario, A., Gößwald, A. (2018): KiGGS Wave 2 Cross-Sectional Study—Participant Acquisition, Response Rates and Representativeness, Journal of Health Monitoring, 3, 78–91. (only wave 2)
#'
#' Trappmann, M., Beste, J., Bethmann, A., Müller, G. (2013): The PASS Panel Survey after Six Waves, Journal for Labour Market Research, 46, 275–281. (only wave 10)
#'
#' Job titles were taken from the following publication:
#'
#' Bundesagentur für Arbeit (2019). Gesamtberufsliste der Bundesagentur für Arbeit. Stand: 03.01.2019. The \code{Gesamtberufsliste der BA} is available at \link{https://download-portal.arbeitsagentur.de/files/}.
#'
#' @examples
#' ## what follows is the source code used to create this data set
#' ##
#' 
#' # load toy example data
#' data(occupations) # toy example, the five data sets cited above were used instead
#' # In addition to codes from ther 2010 German Classification of Occupations, our data make use of the following special codes:
#' (special_codes <- c("-0004" = "genaue Kodierung nicht möglich", "-0006" = "Multiple jobs", "-0012" = "Blue-colar worker", "-0030" = "Student employee/assistant, work placement student, research assistant", "-0019" = "Federal volunteer service, voluntary social year (FSJ), civil service"))
#' data(coding_index_excerpt) # toy example, the Gesamtberufsliste was used instead. After running ?prepare_German_coding_index_Gesamtberufsliste_der_BA, our version of the coding index had 27853 entries.
#' data(frequent_phrases)
#'
#' # prepare coding index for our purposes
#' coding_index <- coding_index_excerpt[!(Berufsbenennungen %in% c("Bundeskanzler/in", "Bundespräsident/in", "Admiral", "General"))] # remove very rare occupations that might violate privacy regulations
#' coding_index_w_codes <- rbind(coding_index[, list(title = bezMale, Code)], coding_index[, list(title = bezFemale, Code)])
#' coding_index_w_codes <- coding_index_w_codes[,title := stringPreprocessing(title)]
#' coding_index_w_codes <- coding_index_w_codes[!duplicated(title)]
#'
#' # prepare the training data (special codes were harmonized in advance)
#' training_data <- occupations[, .(answer = stringPreprocessing(orig_answer), code = orig_code)]
#'
#' # trick to save time: do this only once for each unique string and merge later
#' similarityTableSubstring <- createSimilarityTableSubstring(unique.string = unique(training_data$answer),
#'                                                            coding_index_w_codes = coding_index_w_codes,
#'                                                            coding_index_without_codes = frequent_phrases)
#' similarityTableSubstring2 <- rbind(similarityTableSubstring$dist_table_w_code[, .(intString, dictString = dictString.title)],
#'                                    similarityTableSubstring$dist_table_without_code[, .(intString, dictString)])
#' surveyCountsSubstringSimilarity_toyExample <- merge(training_data[, .(answer, survCode = code)], similarityTableSubstring2, by.x = "answer", by.y = "intString", allow.cartesian = TRUE)[, .N, by = list(dictString, survCode)][order(dictString)]
#'
#' @encoding UTF-8
"surveyCountsSubstringSimilarity"

#' Anonymized training data (wordwise similarity) to be used with Similarity-based Reasoning
#'
#' This aggregated (anonymized) training data is to be used within the \code{\link{trainSimilarityBasedReasoning2}}-function (\code{dist.type = "wordwise", dist.control = list(method = "osa", weight = c(d = 1, i = 1, s = 1, t = 1)), threshold = c(max = NA, use = 1))}, see the documentation therein. It allows the coding of German language occuptions into the German Classification of Occupations (KldB 2010).
#'
#' @format A data.table with 26710 rows and 3 variables:
#' \describe{
#'   \item{dictString}{Job titles (and similars). They were either taken from the \code{Gesamtberufsliste_der_BA} or from \code{\link{frequent_phrases}}}
#'   \item{survCode}{5-digit codes from the survey data}
#'   \item{N}{Frequency of how often a survey text identical or similar to \code{dictString} was coded as \code{survCode} (using wordwise-similarity)}
#' }
#'
#' Wordwise-similarity means that, to be counted, the verbal survey answer must be similar to dictString, more specifically, dictString must be identical with any one word in the survey response (a difference by at most one character is allowed to account for spelling errors).
#'
#' @seealso
#' See \code{\link{trainSimilarityBasedReasoning2}}, for which this data set was created, and \code{\link{surveyCountsSubstringSimilarity}}, which has been created the same way but uses a different metric to calculate string similarities.
#'
#' @source
#' Data from the following surveys were pooled:
#'
#' Antoni, M., Drasch, K., Kleinert, C., Matthes, B., Ruland, M. and Trahms, A. (2010): Arbeiten und Lernen im Wandel * Teil 1: Überblick über die Studie, FDZ-Methodenreport 05/2010, Forschungsdatenzentrum der Bundesagentur für Arbeit im Institut für Arbeitsmarkt- und Berufsforschung, Nuremberg.
#'
#' Rohrbach-Schmidt, D., Hall, A. (2013): BIBB/BAuA Employment Survey 2012, BIBB-FDZ Data and Methodological Reports Nr. 1/2013. Version 4.1, Federal Institute for Vocational Education and Training (Research Data Centre), Bonn.
#'
#' Lange, C., Finger, J., Allen, J., Born, S., Hoebel, J., Kuhnert, R., Müters, S., Thelen, J., Schmich, P., Varga, M., von der Lippe, E., Wetzstein, M., Ziese, T. (2017): Implementation of the European Health Interview Survey (EHIS) into the German Health Update (GEDA), Archives of Public Health, 75, 1–14.
#'
#' Hoffmann, R., Lange, M., Butschalowsky, H., Houben, R., Schmich, P., Allen, J., Kuhnert, R., Schaffrath Rosario, A., Gößwald, A. (2018): KiGGS Wave 2 Cross-Sectional Study—Participant Acquisition, Response Rates and Representativeness, Journal of Health Monitoring, 3, 78–91. (only wave 2)
#'
#' Trappmann, M., Beste, J., Bethmann, A., Müller, G. (2013): The PASS Panel Survey after Six Waves, Journal for Labour Market Research, 46, 275–281. (only wave 10)
#'
#' Job titles were taken from the following publication:
#'
#' Bundesagentur für Arbeit (2019). Gesamtberufsliste der Bundesagentur für Arbeit. Stand: 03.01.2019. The \code{Gesamtberufsliste der BA} is available at \link{https://download-portal.arbeitsagentur.de/files/}.
#'
#' @examples
#' ## what follows is the source code used to create this data set
#' ##
#' 
#' # load toy example data
#' data(occupations) # toy example, the five data sets cited above were used instead
#' # In addition to codes from ther 2010 German Classification of Occupations, our data make use of the following special codes:
#' (special_codes <- c("-0004" = "genaue Kodierung nicht möglich", "-0006" = "Multiple jobs", "-0012" = "Blue-colar worker", "-0030" = "Student employee/assistant, work placement student, research assistant", "-0019" = "Federal volunteer service, voluntary social year (FSJ), civil service"))
#' data(coding_index_excerpt) # toy example, the Gesamtberufsliste was used instead. After running ?prepare_German_coding_index_Gesamtberufsliste_der_BA, our version of the coding index had 27853 entries.
#' data(frequent_phrases)
#'
#' # prepare coding index for our purposes
#' coding_index <- coding_index_excerpt[!(Berufsbenennungen %in% c("Bundeskanzler/in", "Bundespräsident/in", "Admiral", "General"))] # remove very rare occupations that might violate privacy regulations
#' coding_index_w_codes <- rbind(coding_index[, list(title = bezMale, Code)], coding_index[, list(title = bezFemale, Code)])
#' coding_index_w_codes <- coding_index_w_codes[,title := stringPreprocessing(title)]
#' coding_index_w_codes <- coding_index_w_codes[!duplicated(title)]
#'
#' # prepare the training data (special codes were harmonized in advance)
#' training_data <- occupations[, .(answer = stringPreprocessing(orig_answer), code = orig_code)]
#'
#' # trick to save time: do this only once for each unique string and merge later
#' similarityTableWordwise <- createSimilarityTableWordwiseStringdist(unique.string = unique(training_data$answer),
#'                                                                    coding_index_w_codes = coding_index_w_codes,
#'                                                                    coding_index_without_codes = occupationCoding::frequent_phrases,
#'                                                                    preprocessing = list(stopwords = NULL, stemming = NULL, strPreprocessing = TRUE, removePunct = FALSE),
#'                                                                    dist.control = list(method = "osa", weight = c(d = 1, i = 1, s = 1, t = 1)),
#'                                                                    threshold = 1)
#' similarityTableWordwise2 <- rbind(similarityTableWordwise$dist_table_w_code[, .(intString, dictString = dictString.title)],
#'                                   similarityTableWordwise$dist_table_without_code[, .(intString, dictString)])
#' surveyCountsWordwiseSimilarity_toyExample <- merge(training_data[, .(answer, survCode = code)], similarityTableWordwise2, by.x = "answer", by.y = "intString", allow.cartesian = TRUE)[, .N, by = list(dictString, survCode)][order(dictString)]
#' @encoding UTF-8
"surveyCountsWordwiseSimilarity"
