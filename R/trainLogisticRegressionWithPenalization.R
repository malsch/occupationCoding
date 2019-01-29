#' Train a logistic regression model with penalization
#'
#' Function does some preprocessing and calls glmnet for a logistic regression model
#'
#' Setting \code{tuning$alpha = 0} (Ridge Penalty) seems to be most stable.
#'
#' In our experience, \code{glmnet} often returns a warning like \code{3: from glmnet Fortran code (error code -72); Convergence for 72th lambda value not reached after maxit=100000 iterations; solutions for larger lambdas returned}. To solve this issue, we can increase \code{maxit} to try more iterations or we can decrease the threshold \code{thresh}.
#'
#' @param data a data.table created with \code{\link{removeFaultyAndUncodableAnswers_And_PrepareForAnalysis}}
#' @param preprocessing a list with elements
#' \describe{
#'   \item{stopwords}{a character vector, use \code{tm::stopwords("de")} for German stopwords.}
#'   \item{stemming}{\code{NULL} for no stemming and \code{"de"} for stemming using the German porter stemmer.}
#'   \item{countWords}{Set to TRUE if the predictor matrix should contain a column for answer length.}
#' }
#' @param tuning a list with elements that will be passed to \code{\link[glmnet]{glmnet}}
#'
#' @seealso \code{\link{predictLogisticRegressionWithPenalization}}, \code{\link[glmnet]{glmnet}}
#'
#' @return a logistic regression model. Commands from \code{\link[glmnet]{glmnet}} should work.
#' @import data.table
#' @import text2vec
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
#' # Recommended configuration
#' trainLogisticRegressionWithPenalization(proc.occupations,
#'                  preprocessing = list(stopwords = character(0), stemming = "de", countWords = FALSE),
#'                  tuning = list(alpha = 0.05, maxit = 10^6, nlambda = 100, thresh = 1e-7))
#'
#' # Other possibility
#' trainLogisticRegressionWithPenalization(proc.occupations,
#'                  preprocessing = list(stopwords = tm::stopwords("de"), stemming = NULL, countWords = TRUE),
#'                  tuning = list(alpha = 0.05, maxit = 10^6, nlambda = 100, thresh = 1e-7))
trainLogisticRegressionWithPenalization <- function(data,
                                                  preprocessing = list(stopwords = character(0), stemming = NULL, countWords = FALSE),
                                                  tuning = list(alpha = 0.05, maxit = 10^5, nlambda = 100, thresh = 1e-07)) {

  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("Package \"glmnet\" needed if you want to use this function. Please install it.",
         call. = FALSE)
  }

  # remove seldom codes
  seldom.codes <- data[, .N, by = code][N <= 1, code]
  if (length(seldom.codes) > 0) {
    warning(paste("This algorithm requires that every outcome code appears at least twice in the training data. The following codes were removed:", paste(seldom.codes, collapse = ", ")))

    data <- data[!(code %in% seldom.codes)]
  }

  # and prepare codes
  outcome <- as.factor(data[,code])

  # preprocessing
  ans <- data[, stringPreprocessing(ans)]

  # prepare text for efficient computation -> transform to sparse matrix
  matrix <- asDocumentTermMatrix(ans, vect.vocab = NULL,
                                 stopwords = preprocessing$stopwords,
                                 stemming = preprocessing$stemming,
                                 type = "dgCMatrix")

  # include feature for number of words
  if (preprocessing$countWords) {
    ans_freq <- sapply(strsplit(ans, " "), length)
    matrix$dtm <- cbind(matrix$dtm, ans_freq)
  }

  # estimate model
  fit <- glmnet::glmnet(matrix$dtm, outcome, family="multinomial", alpha = tuning$alpha, maxit = tuning$maxit, nlambda = tuning$nlambda, thresh = tuning$thresh)

  # and save preprocessing
  fit$vect.vocab <- matrix$vect.vocab
  fit$preprocessing <- preprocessing
  fit$seldom.codes <- seldom.codes
  return(fit)
}
