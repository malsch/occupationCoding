#' Cosine Similarity
#'
#' Calculate cosine similarity between every row in \code{matrix1} and every row in \code{matrix2}.
#'
#' Cosine similarity is a measure of similarity between two vectors \eqn{x} and \eqn{y} that measures the cosine of the angle between them. Since we consider positive vectors, its maximal value is 1 if both vectors are identical and its minimal value is 0 if \eqn{x \times y = 0}.
#'
#' The definition is: \eqn{similarity = (x \times y) / (||x|| \times ||y||) = (\sum_i x_i \times y_i) / (\sqrt{(\sum_i x_i^2)} \times \sqrt{(\sum_i y_i^2)})}
#'
#' @param matrix1 a matrix of type \code{dgCMatrix}.
#' @param matrix2 a matrix of type \code{dgCMatrix}.
#'
#' @seealso \code{\link[Matrix]{Matrix}}
#'
#' @return A \code{dgCMatrix} where element \code{A[index1, index2]} is the cosine similarity between \code{matrix1[index1,]} and \code{matrix2[index2,]}.
#' @export
#'
#' @examples
#' x <- c("Verkauf von Schreibwaren", "Verkauf", "Schreibwaren", "Industriemechaniker", "NOTINDOCUMENTTERMMATRIX")
#' (y <- c("Verkauf von B\xfcchern, Schreibwaren", "Fach\xe4rzin f\xfcr Kinder- und Jugendmedizin im \xf6ffentlichen Gesundheitswesen", "Industriemechaniker", "Dipl.-Ing. - Agrarwirtschaft (Landwirtschaft)"))
#'
#' tok_fun = text2vec::word_tokenizer
#' it_train = text2vec::itoken(tolower(y), tokenizer = tok_fun, progressbar = FALSE)
#' vocab = text2vec::create_vocabulary(it_train)
#' vect.vocab = text2vec::vocab_vectorizer(vocab)
#'
#' matrix1 <- asDocumentTermMatrix(x, vect.vocab = vect.vocab)$dtm
#' matrix2 <- asDocumentTermMatrix(y, vect.vocab = vect.vocab)$dtm
#'
#' cosineSimilarity(matrix1, matrix1)
#' cosineSimilarity(matrix1, matrix2)
cosineSimilarity <- function(matrix1, matrix2) {

  if (class(matrix1) != "dgCMatrix") stop("matrix1 needs to have class dgCMatrix")
  if (class(matrix2) != "dgCMatrix") stop("matrix2 needs to have class dgCMatrix")

  if (matrix1@Dim[2] != matrix2@Dim[2]) stop("matrix1 and matrix2 need to have identical numbers of columns.")

  similarity <- Matrix::tcrossprod(matrix1, matrix2) / outer(sqrt(Matrix::rowSums(matrix1)), sqrt(Matrix::rowSums(matrix2)))
  similarity@x[is.na(similarity@x)] <- 0 # if a row in either matrix1 or matrix2 is all zero, the denominator is 0 and similarity is NA -> replace with zero.

  return(similarity)
}
