#' Document-Term Matrix
#'
#' Constructs a document-term matrix.
#'
#' @param input a character vector.
#' @param vect.vocab a vocabulary created with \code{\link[text2vec:vectorizers]{vocab_vectorizer}}. If \code{NULL}, the vocabulary is created from the input. See example for a typical use case.
#' @param stopwords character vector of stopwords to exclude when creating the vocabulary. \code{tm::stopwords("de")} provides German stopwords.
#' @param stemming \code{NULL} for no stemming and \code{"de"} for stemming using the German porter stemmer.
#' @param type character, one of c("dgCMatrix", "dgTMatrix", "lda_c") taken from \code{\link[text2vec]{create_dtm}}. \code{dgCMatrix} are useful for glmnet; \code{dgTMatrix} matrix refers to sparse matrices in triplet form, i.e. positions of all non-zero values are stored (easier to work with, but non-unique).
#'
#' @return A list with two elements
#' \describe{
#'   \item{dtm}{a sparse document-term-matrix, depending on the \code{type}-parameter}
#'   \item{vect.vocab}{a vocabulary that can be inserted as \code{vect.vocab} to build a document term matrix on new data with the same vocabulary.}
#' }
#'
#' @seealso
#' \url{http://text2vec.org/vectorization.html} for details on the implementation used here,
#' another implementation \code{\link[tm]{TermDocumentMatrix}} is slower
#'
#' @export
#'
#' @examples
#' x <- c("Verkauf von Schreibwaren", "Verkauf", "Schreibwaren")
#' asDocumentTermMatrix(x)
#' asDocumentTermMatrix(x, type = "dgTMatrix")
#' asDocumentTermMatrix(x, stopwords = tm::stopwords("de"))
#'
#' (x <- c("Verkauf von B\xfcchern, Schreibwaren", "Fach\xe4rzin f\xfcr Kinder- und Jugendmedizin im \xf6ffentlichen Gesundheitswesen", "Industriemechaniker", "Dipl.-Ing. - Agrarwirtschaft (Landwirtschaft)"))
#' x <- stringPreprocessing(x)
#' dtm <- asDocumentTermMatrix(x, stemming = "de")
#' print(dtm$dtm)
#' dimnames(dtm$dtm)[[2]]
#'
#' # use the newly created vocab_vectorizer
#'(x <- stringPreprocessing(c("WILL NOT SHOW UP", "Verkauf von B\xfcchern, Schreibwaren", "Fach\xe4rzin f\xfcr Kinder- und Jugendmedizin")))
#' asDocumentTermMatrix(x, vect.vocab = dtm$vect.vocab, stopwords = character(0), stemming = "de")$dtm
asDocumentTermMatrix <- function(input, vect.vocab = NULL,
                                 stopwords = character(0),
                                 stemming = NULL,
                                 type = c("dgCMatrix", "dgTMatrix", "lda_c")) {

  # tokenize with or without stemming?
  if (!is.null(stemming)) {
    if (!requireNamespace("SnowballC", quietly = TRUE)) {
      stop("Package \"SnowballC\" needed if you want to do stemming. Please install it.",
           call. = FALSE)
    }
    tok_fun <-function(x) {
      tokens <- text2vec::word_tokenizer(x)
      lapply(tokens, SnowballC::wordStem, language = stemming)
    }
  } else {
    tok_fun <- text2vec::word_tokenizer
  }

  # prep_fun = toupper
  it_train = text2vec::itoken(tolower(input),
                    # preprocessor = prep_fun,
                    tokenizer = tok_fun,
                    progressbar = FALSE)
  if (is.null(vect.vocab)) {
    vocab = text2vec::create_vocabulary(it_train, stopwords = tolower(stopwords))
    vect.vocab = text2vec::vocab_vectorizer(vocab)
  }
  input.dtm = text2vec::create_dtm(it_train, vect.vocab, type = type)
  return(list(dtm = input.dtm, vect.vocab = vect.vocab))
}
