#' Prepares the Gesamtberufsliste der BA to be used with this package
#'
#' To use this function go to \link{https://download-portal.arbeitsagentur.de/files/} and download the file \code{Gesamtberufsliste_der_BA.xlsx}. This function prepares a coding index from the Excel file, useful for other functions in this package.
#'
#' Part of the German Classification of Occupation (KldB 2010) is an alphabetic directory of job titles, available from \link{https://statistik.arbeitsagentur.de/Navigation/Statistik/Grundlagen/Klassifikation-der-Berufe/KldB2010/Systematik-Verzeichnisse/Systematik-Verzeichnisse-Nav.html}. Almost 28.000 job titles are listed in the alphabetic directory along with their associated codes. However, using the alphabetic directory for automatic coding is difficult because it consists of neutral job titles (e.g. Digital-Media-Designer/in) only. They are neutral because the '/' divides the male and female spelling. However, people will never use neutral titles that contain a '/' when they describe their own job. An alternative is the \code{Gesamtberufsliste_der_BA.xlsx}, which contains separate male and female job titles next to the neutral title. The Gesamtberufsliste is used for yearly updates of the alphabetic directory, entailing that both documents are mostly identical.
#'
#' This functions loads the \code{Gesamtberufsliste_der_BA.xlsx} into memory. It corrects some errors in the Gesamtberufsliste, may remove a few entries, renames some column names, calculates an indicator of job title ambiguity, and, most importantly, it removes parentheses and text in between. The resulting data.table can be used with the function \code{\link{predictWithCodingIndex}} for coding. If coded training data from earlier studies is available, \code{\link{trainSimilarityBasedReasoning}} is another option for using this coding index.
#'
#' With \code{count.categories = TRUE} an indicator of job title ambiguity is calculated. See the source code for examples what exactly is being calculated. This indicator is only needed if one wishes to change the default behaviour in \code{\link{predictWithCodingIndex}} by setting \code{max.count.categories}. This parameter allows to exclude ambiguous job titles (those with large values in count_categories) from the coding index.
#'
#' @param path_to_file path to downloaded file
#' @param count.categories (default: \code{FALSE}). The default sets the column \code{count_categories} to 0, avoiding lengthy computations. With \code{count.categories = TRUE} an indicator of job title ambiguity is calculated. See below.
#'
#' @seealso
#' \code{\link{coding_index_excerpt}}, \code{\link{predictWithCodingIndex}}, \code{\link{trainSimilarityBasedReasoning}}
#'
#' @return a data.table with columns Berufsbenennungen, bezMale, and bezFemale, Code, and count_categories.
#' @import data.table
#' @export
#'
#' @examples
#' \dontrun{
#' # Illustrative results from this function (if you have not downloaded the file yet).
#' View(coding_index_excerpt)
#'
#' # point path_to_file to your local file
#' path_to_file <- ".../Gesamtberufsliste_der_BA.xlsx"
#' coding_index <- prepare_German_coding_index_Gesamtberufsliste_der_BA(path_to_file, count.categories = FALSE)
#' }
prepare_German_coding_index_Gesamtberufsliste_der_BA <- function(path_to_file, count.categories = FALSE) {

  gesamtberufsliste <- data.table(readxl::read_excel(path_to_file, sheet = 2, skip = 4, col_names = c("DKZ-ID", "Codenummer", "Zustand", "Bezeichnung_neutral", "Bezeichnung_maennlich", "Bezeichnung_weiblich")))

  # correct spelling errors
  gesamtberufsliste[Bezeichnung_neutral == "Fellverarbeiterhelfer/n", Bezeichnung_neutral := "Fellverarbeiterhelfer/in"]
  gesamtberufsliste[Bezeichnung_neutral == "Facharzt/-ärztin - Innere Medizin u. Hämatolog. u. Onkologie" & Bezeichnung_maennlich == "f", Bezeichnung_maennlich := "Facharzt für Innere Medizin und Hämatologie und Onkologie"]

  # some male/female job titles are missing -> insert them
  gesamtberufsliste[is.na(Bezeichnung_maennlich) & is.na(Bezeichnung_weiblich) & Bezeichnung_neutral == "Ballerina/Ballerino", c("Bezeichnung_maennlich", "Bezeichnung_weiblich") := list("Ballerino", "Ballerina")]
  gesamtberufsliste[is.na(Bezeichnung_maennlich) & is.na(Bezeichnung_weiblich) & Bezeichnung_neutral == "Deichvogt/Deichvögtin", c("Bezeichnung_maennlich", "Bezeichnung_weiblich") := list("Deichvogt", "Deichvögtin")]
  gesamtberufsliste[is.na(Bezeichnung_maennlich) & is.na(Bezeichnung_weiblich) & Bezeichnung_neutral == "Laufschlosser/-in", c("Bezeichnung_maennlich", "Bezeichnung_weiblich") := list("Laufschlosser", "Laufschlosserin")]
  gesamtberufsliste[is.na(Bezeichnung_maennlich) & is.na(Bezeichnung_weiblich) & Bezeichnung_neutral == "Prüfwart/-wartin", c("Bezeichnung_maennlich", "Bezeichnung_weiblich") := list("Prüfwart", "Prüfwartin")]
  # if there is no '/' in the neutral title, we can simply insert that neutral title (although it may be just the male or the female title)
  gesamtberufsliste[is.na(Bezeichnung_maennlich) & is.na(Bezeichnung_weiblich) & !grepl("/", Bezeichnung_neutral), c("Bezeichnung_maennlich", "Bezeichnung_weiblich") := list(Bezeichnung_neutral, Bezeichnung_neutral)]

  # if we still have titles without male/female text -> remove them
  # gesamtberufsliste[(is.na(Bezeichnung_maennlich)  | nchar(Bezeichnung_maennlich) < 3) & (is.na(Bezeichnung_weiblich)  | nchar(Bezeichnung_weiblich) < 3)]
  gesamtberufsliste <- gesamtberufsliste[!((is.na(Bezeichnung_maennlich)  | nchar(Bezeichnung_maennlich) < 3) & (is.na(Bezeichnung_weiblich) | nchar(Bezeichnung_weiblich) < 3))]

  ## remove text in parenthesis. Examples: Abgleicher/in (Elektro) and Abgleicher/in (Nachrichtentechnik) -> Abgleicher/in
  # gesamtberufsliste[,Bezeichnung_neutral_ohne := gsub(" \\(.*\\)", "", Bezeichnung_neutral)]
  gesamtberufsliste[,bezMale := gsub(" \\(.*\\)", "", Bezeichnung_maennlich)]
  gesamtberufsliste[,bezFemale := gsub(" \\(.*\\)", "", Bezeichnung_weiblich)]

  ## extract Code
  gesamtberufsliste[, Code := substr(Codenummer, 3, 7)]

  # remove superfluous columns (we could also delete Bezeichnung_neutral)
  gesamtberufsliste[, `DKZ-ID` := NULL]
  gesamtberufsliste[, Codenummer := NULL]
  gesamtberufsliste[, Bezeichnung_maennlich := NULL]
  gesamtberufsliste[, Bezeichnung_weiblich := NULL]
  # gesamtberufsliste[, Bezeichnung_neutral_ohne := NULL]
  gesamtberufsliste[, Zustand := NULL]

  if (count.categories) {
    #############
    # add another column `count_categories` (only used within function predictWithCodingIndex if max.count.categories < Inf)
    #############

    # Suppose we try coding a given job title and search the coding for all entries that have this job title as a substring: How many different codes would the coding index suggest?
    # An example for the following:
    # gesamtberufsliste[grepl("Augenoptiker", bezMale, fixed = TRUE) | grepl("Augenoptiker", bezFemale, fixed = TRUE) | grepl("Augenoptikerin", bezMale, fixed = TRUE) | grepl("Augenoptikerin", bezFemale, fixed = TRUE), .N, by = Code]
    # -> unique codings are only possible if a job title (in male or female form) is not a substring of another job title (in male or female form), e.g. Augenoptiker/in has multiple codes because Augenoptiker/in (staatlich geprüft).......82523, Augenoptikermeister/in...82593 and Augenoptiktechniker/in..82523 have different codes

    # standarize texts to get more matchtes
    gesamtberufsliste[,bezMaleL := tolower(bezMale)]
    gesamtberufsliste[,bezMaleL := gsub("-", "",bezMaleL)] # e.g. koch/köchin should also match Chefkoch/-köchin
    gesamtberufsliste[,bezFemaleL := tolower(bezFemale)]
    gesamtberufsliste[,bezFemaleL := gsub("-", "",bezFemaleL)] # e.g. koch/köchin should also match Chefkoch/-köchin

    # helper function -> with some simple job titles we observe many different categories -> how many
    getNumCategories <- function(male.title, female.title) {
      gesamtberufsliste[grepl(male.title, bezMaleL, fixed = TRUE) | grepl(male.title, bezFemaleL, fixed = TRUE) | grepl(female.title, bezMaleL, fixed = TRUE) | grepl(female.title, bezFemaleL, fixed = TRUE), .N, by = Code][, .N]
    }

    # an example for this -> with some simple job titles we observe many different categories -> how many
    getNumCategories2 <- function(male.title, female.title) {
      gesamtberufsliste[grepl(male.title, bezMaleL, fixed = TRUE) | grepl(male.title, bezFemaleL, fixed = TRUE) | grepl(female.title, bezMaleL, fixed = TRUE) | grepl(female.title, bezFemaleL, fixed = TRUE)]
    }
    # getNumCategories2("koch", "köchin")
    # getNumCategories2("koch", "köchin")[,.N, by = Code]

    # Suppose we try coding a given job title and search the coding for all entries that have this job title as a substring: How many different codes would the coding index suggest?
    gesamtberufsliste[, count_categories:= mapply(getNumCategories, bezMaleL, bezFemaleL)]

    # clean up
    gesamtberufsliste[,bezFemaleL := NULL]
    gesamtberufsliste[,bezMaleL := NULL]
  } else {
    gesamtberufsliste[, count_categories:= 0L]
  }

  ################
  # clean up
  # some job titles do not have a male/female form. To avoid that we need to deal with empty cells ("-"), insert the existing form instead.
  # This is a cheap hack that works because we will delete duplicate entries in one of the next steps.
  gesamtberufsliste[bezFemale == "-" | is.na(bezFemale), bezFemale := bezMale]
  gesamtberufsliste[bezMale == "-" | is.na(bezMale), bezMale := bezFemale]

  setnames(gesamtberufsliste, "Bezeichnung_neutral", "Berufsbenennungen")
  gesamtberufsliste
}
