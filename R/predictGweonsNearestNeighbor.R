#' Predict codes with Gweons Nearest Neighbor Method
#'
#' Function does the same preprocessing as in \code{\link{trainGweonsNearestNeighbor}} and predicts codes with a modified 1-nearest-neighbor approach.
#'
#' @param model the output created from \code{\link{trainGweonsNearestNeighbor}}
#' @param newdata eiter a data.table created with \code{\link{removeFaultyAndUncodableAnswers_And_PrepareForAnalysis}} or a character vector
#' @param tuning a list with element
#' \describe{
#'   \item{nearest.neighbors.multiplier}{defaults to 0.1. Gweon et al. (2017) show that 0.1 is a better choice than 0 but the exact value is a bit arbitrary.}
#' }
#'
#' @seealso
#' \code{\link{trainGweonsNearestNeighbor}}
#'
#' Gweon, H.; Schonlau, M., Kaczmirek, L., Blohm, M., Steiner, S. (2017). Three Methods for Occupation Coding Based on Statistical Learning. Journal of Official Statistics 33(1), pp. 101--122
#'
#' This function is based on \url{https://github.com/hgweon/occupation-coding/blob/master/Modified_NN.r}. Considerable speed improvements were implemented.
#'
#' @return a data.table of class \code{occupationalPredictions} that contains predicted probabilities \code{pred.prob} for every combination of \code{ans} and \code{pred.code}. pred.code may not cover the full set of possible codes. If all predicted codes have probability 0, these predictions are removed and we instead insert \code{pred.code := "-9999"} with \code{pred.prob = 1/num.allowed.codes}.
#' @import data.table
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
#' ## split sample
#' set.seed(3451345)
#' n.test <- 50
#' group <- sample(c(rep("test", n.test), rep("training", nrow(proc.occupations) - n.test)))
#' splitted.data <- split(proc.occupations, group)
#'
#' # train model and make predictions
#' model <- trainGweonsNearestNeighbor(splitted.data$train,
#'                                     preprocessing = list(stopwords = tm::stopwords("de"), stemming = "de", strPreprocessing = TRUE, removePunct = FALSE))
#' predictGweonsNearestNeighbor(model, c("test", "HIWI", "Hilfswissenschaftler"))
#' res <- predictGweonsNearestNeighbor(model, splitted.data$test)
#'
#' # look at most probable answer from each id
#' res[, .SD[which.max(pred.prob), list(ans, true.code = code, pred.code, acc = code == pred.code)], by = id]
#' res[, .SD[which.max(pred.prob), list(ans, true.code = code, pred.code, acc = code == pred.code)], by = id][, mean(acc)] # calculate accurac of predictions
#'
#' # for further analysis we usually require further processing:
#' produceResults(expandPredictionResults(res, allowed.codes, method.name = "GweonsNearestNeighbor"), k = 1, n = n.test, num.codes = length(allowed.codes))
predictGweonsNearestNeighbor <- function(model, newdata, tuning = list(nearest.neighbors.multiplier = 0.1)) {

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

  dtmTraining <- model$matrix
  codeTraining <- model$code

  ##########################
  # preprocessing
  preprocessing <- model$preprocessing

  if (preprocessing$removePunct) {
    ans <- tm::removePunctuation(ans)
  }

  if (preprocessing$strPreprocessing) {
    ans <- stringPreprocessing(ans)
  }

  # prepare text for efficient computation -> transform to sparse matrix
  dtmToPredict <- asDocumentTermMatrix(ans, vect.vocab = model$vect.vocab,
                                       stopwords = preprocessing$stopwords,
                                       stemming = preprocessing$stemming,
                                       type = "dgCMatrix")$dtm

  # Modified NN
  # copied from https://github.com/hgweon/occupation-coding/blob/master/Modified_NN.r
  f_modified_nn <- function(X_tr, X_tr_code, X_ts) {
    # X_tr <- as.matrix(train_data[,-1])
    # changed, because test data usually do not have known codes
    # X_ts <- as.matrix(test_data[,-1])
    # X_ts <- test_data

    nr <- nrow(X_tr)
    ns <- nrow(X_ts)

    # cosine similarity
    # RAM errors occur with large data. Probably at this place:
    similarity <- cosineSimilarity(X_ts, X_tr)

    # uncomment slow way of calculating cosine similarity
    # similarity <- matrix(0,nrow=ns, ncol=nr)
    max_sim <- numeric(ns)
    tr_label <- names(table(X_tr_code))
    pred_freq <- matrix(0,nrow=ns,ncol=length(tr_label))
    for(i in 1:ns)
    {
      ind <- which(X_ts[i,]==1)
      if(length(ind)>0 & sum(X_tr[,ind])>0)
      {
        #      for(j in 1:nr)
        #      {
        #        which(X_ts[i,]==1)
        #        which(X_tr[j,]*X_ts[i,]>0)
        #        numer <- sum(X_tr[j,]*X_ts[i,])
        #        A <- sqrt(sum(X_tr[j,]))
        #        B <- sqrt(sum(X_ts[i,]))
        #        if(A>0 & B>0 ) similarity[i,j] <- numer/(A*B)
        #      }
        max_sim[i] <- max(similarity[i,])
        ind <- which(similarity[i,]==max_sim[i])
        result <- table(X_tr_code[ind])
        for(j in 1:length(result))
        {
          ind <- which(names(result[j])==tr_label)
          pred_freq[i,ind] <- result[j]
        }
      }
    }

    # calculate relative frequencies how often code c is among those codes that have highest cosine similarity
    freq_sum <- apply(pred_freq,1,sum)
    # old code
    # pred_prob <- matrix(0,nrow=ns,ncol=length(tr_label))
    # for(i in 1:ns)
    # {
    #   if(freq_sum[i]>0) pred_prob[i,] <- pred_freq[i,]/freq_sum[i]
    # }
    # new code (faster)
    freq_sum[freq_sum == 0] <- 1 # replace 0 with 1 to avoid NAs in the following
    pred_prob <- sweep(pred_freq, 1, freq_sum, FUN = "/")

    colnames(pred_freq) <- tr_label
    colnames(pred_prob) <- tr_label
    out <- list(frequency=pred_freq, probability=pred_prob, max_similarity=max_sim)
    return(out)
  }

  # don't count number of word appearances but make this a 0-1 indicator of appearance
  dtmTraining@x[dtmTraining@x > 1] <- 1
  dtmToPredict@x[dtmToPredict@x > 1] <- 1

  # calculate absolute/relative frequencies and max_similarity
  res <- f_modified_nn(X_tr = dtmTraining, X_tr_code = codeTraining, X_ts = dtmToPredict)

  # NN-3 from Creecy equals p(c_i | x) * s(x) * (K(x)/K(x) + 0.1)
  max_freq <- apply(res$frequency, 1, sum) # K(x) = number of nearest neighbors in the training data
  if (length(max_freq) == 1) {
    predProb <- res$max_similarity * max_freq / (max_freq + tuning$nearest.neighbors.multiplier) * res$probability
  } else {
    predProb <- diag(res$max_similarity * max_freq / (max_freq + tuning$nearest.neighbors.multiplier)) %*% res$probability
  }

  # and bring them in a nice format
  predProb2 <- data.table(id = rep(id, times = dim(predProb)[2]),
                          ans = rep(ans, times = dim(predProb)[2]),
                          pred.code = rep(attr(predProb, "dimnames")[[2]], each = length(ans)),
                          pred.prob = as.vector(predProb))

  # append a pred.code = "-9999" for to each id. If nothing was predicted, set pred.prob to 1/num.allowed.codes
  idsWithoutPredictions <- predProb2[, list(sumP = sum(pred.prob), pred.code = "-9999"), by = list(id, ans)]
  idsWithoutPredictions[sumP == 0, pred.prob :=  1/model$num.allowed.codes]
  idsWithoutPredictions[sumP > 0, pred.prob := 0]
  idsWithoutPredictions[, sumP := NULL]
  predProb2 <- rbind(predProb2, idsWithoutPredictions) # danger zone: I am not sure if idsWithoutPredictions is always in the same order as newdata


  # add additional columns from new data to the result
  if ("occupationData" %in% class(newdata)) {
    for (i in seq_along(names(newdata))) {
      if (names(newdata)[i] != "ans" & names(newdata)[i] != "id") {
        set(predProb2, i = NULL, j = names(newdata)[i], value = unlist(rep(newdata[, i, with = FALSE], times = dim(predProb)[2] + 1))) # +1 because we added the row for -9999
      }
    }


    # if (length(idsWithoutPredictions)) {
    #   warning(paste(length(idsWithoutPredictions), " cases are dissimilar from all training observations. (pred.code = min, pred.prob = 0) is for these cases replaced with (pred.code = \"-9999\", pred.prob = 1/", model$num.allowed.codes, ") and all other pred.codes are removed."))
    #   smallest.code <- min(rep(attr(predProb, "dimnames")[[2]])) # select one code to be replaced with -9999
    #   predProb2[id %in% idsWithoutPredictions & pred.code == smallest.code, pred.prob := 1/model$num.allowed.codes]
    #   predProb2[id %in% idsWithoutPredictions & pred.code == smallest.code, pred.code := "-9999"]
    #   predProb2 <- predProb2[!(id %in% idsWithoutPredictions) | (id %in% idsWithoutPredictions & pred.code == "-9999")]
    # }

    class(predProb2) <- c(class(predProb2), "occupationalPredictions")
  }

  # only keep rows if pred.prob > 0. Insert pred.prob for all others later (using the -9999 category)
  predProb2 <- predProb2[pred.code == "-9999" | pred.prob > 0]

  return(predProb2)
}
