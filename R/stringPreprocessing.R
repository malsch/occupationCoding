#' Preprocess German occupational text
#'
#' Function replaces some common characters / character sequences (e.g., Ä, Ü, "DIPL.-ING.") with their uppercase equivalents and removes punctuation, empty spaces and the word "Diplom".
#'
#' \code{\link{charToRaw}} helps to find UTF-8 characters.
#'
#' @encoding UTF-8
#' @param verbatim a character vector.
#' @param lang (default de) Everything else will throw an error.
#'
#' @return the same character vector after processing
#' @export
#'
#' @examples
#' (x <- c("Verkauf von B\xfcchern, Schreibwaren", "Fach\xe4rzin f\xfcr Kinder- und Jugendmedizin im \xf6ffentlichen Gesundheitswesen", "Industriemechaniker", "Dipl.-Ing. - Agrarwirtschaft (Landwirtschaft)"))
#' stringPreprocessing(x)
stringPreprocessing <- function(verbatim, lang = "de") {
  if(!is.character(verbatim))
    stop("Character Input required for function stringPreprocessing")
  if (lang != "de")
    stop("This function was programmed for German characters and German occupational titles")
  verbatim <- toupper(verbatim)
  verbatim <- gsub(intToUtf8(0xc4), "AE", verbatim, fixed=TRUE)
  verbatim <- gsub(intToUtf8(0xd6), "OE", verbatim, fixed=TRUE)
  verbatim <- gsub(intToUtf8(0xdc), "UE", verbatim, fixed=TRUE)
  verbatim <- gsub(intToUtf8(0xdf), "SS", verbatim, fixed=TRUE)
  verbatim <- gsub(intToUtf8(0xc2), "A", verbatim, fixed=TRUE)
  verbatim <- gsub(intToUtf8(0xc9), "E", verbatim, fixed=TRUE)
  verbatim <- gsub(intToUtf8(0xca), "E", verbatim, fixed=TRUE)
  verbatim <- gsub(intToUtf8(0xcd), "I", verbatim, fixed=TRUE)
  verbatim <- gsub(intToUtf8(0xce), "I", verbatim, fixed=TRUE)
  verbatim <- gsub(intToUtf8(0xd4), "O", verbatim, fixed=TRUE)
  verbatim <- gsub("/", " ", verbatim , fixed=TRUE)
  verbatim <- gsub("+", " ", verbatim , fixed=TRUE)
  verbatim <- gsub("-", " ", verbatim , fixed=TRUE)
  verbatim <- gsub(")", " ", verbatim , fixed=TRUE)
  verbatim <- gsub("(", " ", verbatim , fixed=TRUE)
  verbatim <- gsub("\u20ac", "EURO", verbatim, fixed=TRUE) # no solution with intToUtf8
  verbatim <- gsub("\u0080", "EURO", verbatim, fixed = TRUE) # there must be a solution with intToUtf8
  # verbatim <- gsub(" [A-Za-z]{1,3}\\.", "", verbatim) # Abkürzungen mit max. 3 Buchstaben vor einem Punkt werden gelöscht
  verbatim <- gsub("DIPL.-ING.", "DIPLOMINGENIEUR", verbatim , fixed=TRUE) # this abbreviation is often used in the coding index
  verbatim <- gsub("ING.", "INGENIEUR", verbatim , fixed=TRUE) # also sometimes used
  verbatim <- gsub("DIPL.", "DIPLOM", verbatim , fixed=TRUE) # this abbreviation is often used in the coding index
  verbatim <- gsub("DIPLOM", "DIPLOM ", verbatim , fixed=TRUE) # better to think of DIPLOM as a separate word
  verbatim <- tm::removePunctuation(verbatim)
  verbatim <- stringr::str_trim(verbatim)
  verbatim <- gsub(" {2,}", " ", verbatim) # in case we have at least two double spaces, replace it with a single one
#  verbatim <- gsub("  ", " ", verbatim , fixed=TRUE) # in case we have multiple double spaces
#  verbatim <- gsub("  ", " ", verbatim , fixed=TRUE) # in case we have multiple double spaces
#  verbatim <- gsub("  ", " ", verbatim , fixed=TRUE) # in case we have multiple double spaces
#  verbatim <- gsub( " *\\(.*?\\) *", "", verbatim) # would it make sense to remove text in parenthesis completely?
  verbatim <- gsub("DIPLOM ","",verbatim)
  verbatim <- gsub("DIPL ","",verbatim)
  return(verbatim)
}
