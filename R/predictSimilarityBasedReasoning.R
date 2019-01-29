#' Predict codes using a Similarity Based Probability Model
#'
#' Function does the same preprocessing as in \code{\link{trainSimilarityBasedReasoning}} and makes probability predictions. If the verbal answer is not similar to any entry from the coding index, it just predicts all codes to have probability \code{1/model$num.allowed.codes}.
#'
#' @param model the output created from \code{\link{trainSimilarityBasedReasoning}}.
#' @param newdata eiter a data.table created with \code{\link{removeFaultyAndUncodableAnswers_And_PrepareForAnalysis}} or a character vector.
#' @param parallel If \code{model$dist.type == "substring"} we may set this TRUE to use parallel computing. It has not been tested yet if this makes predictions faster.
#'
#' @seealso \code{\link{trainSimilarityBasedReasoning}}
#'
#' @return a data.table of class \code{occupationalPredictions} that contains predicted probabilities \code{pred.prob} for every combination of \code{ans} and \code{pred.code}. pred.code may not cover the full set of possible codes, but will contain a code "-9999" for every individual that provides a probability for all categories missing.
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
#' ## split sample
#' set.seed(3451345)
#' n.test <- 50
#' group <- sample(c(rep("test", n.test), rep("training", nrow(proc.occupations) - n.test)))
#' splitted.data <- split(proc.occupations, group)
#' attr(splitted.data$training, "classification")$code <- attr(proc.occupations, "classification")$code
#'
#' ####### train models
#' # first model uses dist.type = wordwise and some other recommended settings (n.draws could be higher)
#' simBasedModel <- trainSimilarityBasedReasoning(data = splitted.data$training,
#'                               coding_index_w_codes = coding_index_excerpt,
#'                               coding_index_without_codes = frequent_phrases,
#'                               preprocessing = list(stopwords = tm::stopwords("de"), stemming = NULL, strPreprocessing = TRUE, removePunct = FALSE),
#'                               dist.type = "wordwise",
#'                               dist.control = list(method = "osa", weight = c(d = 1, i = 1, s = 1, t = 1)),
#'                               threshold = c(max = 3, use = 1), simulation.control = list(n.draws = 50, check.normality = FALSE),
#'                               tmp_folder = "similarityTables")
#'
#' res <- predictSimilarityBasedReasoning(simBasedModel, splitted.data$test)
#'
#' # look at most probable answer from each id
#' res[, .SD[which.max(pred.prob), list(ans, true.code = code, pred.code, acc = code == pred.code)], by = id]
#' res[, .SD[which.max(pred.prob), list(ans, true.code = code, pred.code, acc = code == pred.code)], by = id][, mean(acc)] # calculate aggrement rate
#'
#' # for further analysis we usually require further processing:
#' produceResults(expandPredictionResults(res, allowed.codes, method.name = "WordwiseSimilarityOsa1111"), k = 1, n = n.test, num.codes = length(allowed.codes))
#'
#' # second model uses dist.type = substring and some other recommended settings (n.draws could be higher)
#' simBasedModel <- trainSimilarityBasedReasoning(data = splitted.data$training,
#'                               coding_index_w_codes = coding_index_excerpt,
#'                               coding_index_without_codes = frequent_phrases,
#'                               preprocessing = list(stopwords = NULL, stemming = NULL, strPreprocessing = TRUE, removePunct = FALSE),
#'                               dist.type = "substring",
#'                               dist.control = list(method = "substring", weight = numeric()),
#'                               threshold = c(0, 0), simulation.control = list(n.draws = 50, check.normality = FALSE),
#'                               tmp_folder = "similarityTables")
#'
#' res <- predictSimilarityBasedReasoning(simBasedModel, splitted.data$test)
#' res <- predictSimilarityBasedReasoning(simBasedModel, splitted.data$test, parallel = TRUE) # if method = substring was used during training, we can speed up the predictions
#' produceResults(expandPredictionResults(res, allowed.codes, method.name = "substringSimilarity"), k = 1, n = n.test, num.codes = length(allowed.codes))
#'
#'
#' # third model uses dist.type = fulltext and some other recommended settings (n.draws could be higher)
#' simBasedModel <- trainSimilarityBasedReasoning(data = proc.occupations,
#'                               coding_index_w_codes = coding_index_excerpt,
#'                               coding_index_without_codes = frequent_phrases,
#'                               preprocessing = list(stopwords = NULL, stemming = NULL, strPreprocessing = TRUE, removePunct = FALSE),
#'                               dist.type = "fulltext",
#'                               dist.control = list(method = "osa", weight = c(d = 1, i = 1, s = 1, t = 1)),
#'                               threshold = c(max = 3, use = 1), simulation.control = list(n.draws = 50, check.normality = FALSE),
#'                               tmp_folder = "similarityTables")
#' res <- predictSimilarityBasedReasoning(simBasedModel, splitted.data$test)
#' produceResults(expandPredictionResults(res, allowed.codes, method.name = "FulltextSimilarityOsa1111"), k = 1, n = n.test, num.codes = length(allowed.codes))
#'
#' #######################################################
#' ## RUN A GRID SEARCH (takes some time)
#' \donttest{
#' # create a grid of all tuning combinations to try
#' model.grid <- data.table(expand.grid(stopwords = FALSE, stemming = FALSE, strPreprocessing = TRUE, removePunct = FALSE,
#'                                      dist.type = c("wordwise", "fulltext"), # dist.control = list(method = "osa", weight = c(d = 1, i = 1, s = 1, t = 1)
#'                                      threshold = c(0, 2),
#'                                      n.draws = c(5, 30), stringsAsFactors = FALSE))
#' model.grid <- rbind(model.grid, data.table(expand.grid(stopwords = FALSE, stemming = FALSE, strPreprocessing = TRUE, removePunct = FALSE,
#'                                                        dist.type = "substring",
#'                                                        threshold = FALSE,
#'                                                        n.draws = 30, stringsAsFactors = FALSE)))
#'
#'
#' for (i in 1:nrow(model.grid)) { #
#'   simBasedModel <- trainSimilarityBasedReasoning(data = splitted.data$training,
#'                                                  coding_index_w_codes = coding_index_excerpt,
#'                                                  coding_index_without_codes = frequent_phrases,
#'                                                  preprocessing = list(stopwords = if (model.grid[i, stopwords]) tm::stopwords("de") else character(0),
#'                                                                       stemming = if (model.grid[i, stemming == "de"]) "de" else NULL,
#'                                                                       strPreprocessing = model.grid[i, strPreprocessing],
#'                                                                       removePunct = model.grid[i, removePunct]),
#'                                                 dist.type = model.grid[i, dist.type],
#'                                                  dist.control = list(method = "osa", weight = c(d = 1, i = 1, s = 1, t = 1)),
#'                                                  threshold = c(max = 3, use = model.grid[i, threshold]), simulation.control = list(n.draws = model.grid[i, n.draws], check.normality = FALSE),
#'                                                  tmp_folder = "similarityTables")
#'
#'   if (model.grid[i, dist.type] == "substring") {
#'    # parallelization is only implemented (and helpful if dist.type = substring)
#'     res.proc2 <- expandPredictionResults(predictSimilarityBasedReasoning(simBasedModel, splitted.data$test, parallel = TRUE), allowed.codes = allowed.codes, method.name = "SimilarityBased")
#'   } else {
#'     res.proc2 <- expandPredictionResults(predictSimilarityBasedReasoning(simBasedModel, splitted.data$test, parallel = FALSE), allowed.codes = allowed.codes, method.name = "SimilarityBased")
#'   }
#'
#'   ac <- accuracy(calcAccurateAmongTopK(res.proc2, k = 1), n = nrow(splitted.data$test))
#'   ll <- logLoss(res.proc2)
#'   sh <- sharpness(res.proc2)
#'
#'   model.grid[i, acc := ac[, acc]]
#'   model.grid[i, acc.se := ac[, se]]
#'   model.grid[i, acc.N := ac[, N]]
#'   model.grid[i, acc.prob0 := ac[, count.pred.prob0]]
#'   model.grid[i, loss.full := ll[1, logscore]]
#'   model.grid[i, loss.full.se := ll[1, se]]
#'   model.grid[i, loss.full.N := ll[1, N]]
#'   model.grid[i, loss.sub := ll[2, logscore]]
#'   model.grid[i, loss.sub.se := ll[2, se]]
#'   model.grid[i, loss.sub.N := ll[2, N]]
#'   model.grid[i, sharp := sh[, sharpness]]
#'   model.grid[i, sharp.se := sh[, se]]
#'   model.grid[i, sharp.N := sh[, N]]
#'   model.grid[i, sum.pred.prob1 := res.proc2[, .SD[which.max(pred.prob), pred.prob], by = id][, sum(V1)]]
#' }
#'
#' model.grid[order(dist.type, threshold, n.draws)]
#' }
predictSimilarityBasedReasoning <- function(model, newdata, parallel = FALSE) {

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

  if (parallel & model$dist.type == "substring") {
    if (!requireNamespace("parallel", quietly = TRUE)) {
      stop("Package \"parallelC\" needed if you want to do parallel processing with multiple cores. Please install it.",
           call. = FALSE)
    }
    cl <- parallel::makeCluster(parallel::detectCores() - 1)
  }



  ##########################
  # preprocessing
  preprocessing <- model$preprocessing

  if (preprocessing$removePunct) {
    ans <- tm::removePunctuation(ans)
  }

  if (preprocessing$strPreprocessing) {
    ans <- stringPreprocessing(ans)
  }

  # input is one answer and one similarity function
  make.probability.predictions.for.single.answer <- function(model.prob.query, category.prob, K) {
    # aggregiere rules: und berechne Wahrschneinlichkeiten getrennt nach Code

    # if there is no similar cell, return all categories ("-9999") with probability 1/No. categories
    if (model.prob.query[,.N] == 0) return(data.table(pred.code = "-9999", pred.prob = 1/K))

    # calculate p(C_f = c | y)
    model.prob.query[, model.prob := string.prob / sum(string.prob)]

    # create the result data table
    # simple idea merge(category.prob, model.prob.query, by = c("dist", "string"), allow.cartesian=TRUE), BUT:
    # If one would calculate list(pred.prob = sum(mean.theta * model.prob)), by = list(id, code) at this point, we would make an error: when a particular rule does not suggest a category we still need to take the prior probabilities unobserved.mean.theta into account
    # -> add those prior probabilities to the predictions table (though the difference may well be neglectible)
    # first pick the subset of rows from category.prob that are covered by cells similar to the query
    category.prob.subset <- merge(model.prob.query, category.prob, by = c("dist", "string"), all.x = TRUE)[,list(dist, string, code, mean.theta)]
    # DT needs a row for every combination of (string) and (subset of codes that was assigned to any of the strings)
    # we also include a special extra code here: -9999 represents any code that has no training cases in the cell -> unobserved.mean.theta will be inserted here
    predictions2 <- unique(CJ(string = model.prob.query[, string], code = c("-9999", category.prob.subset[, unique(code)]))) # CJ returns a cross product, unique(CJ(...)) is used to avoid that we have rows for each dist, string, combination
    # get unobserved.mean.theta and model.prob from model.prob.query
    predictions2 <- merge(predictions2, model.prob.query, by = c("string"), all.x = TRUE, allow.cartesian = TRUE) # allow cartesian means that we might get duplicate rows if a string is in dist = official and in dist = self-created
    # get mean.theta from category.prob
    predictions2 <- merge(predictions2, category.prob.subset, by = c("dist", "string", "code"), all.x = TRUE)

    # insert mean theta for categories that were not found via a specific rule
    predictions2[is.na(mean.theta), mean.theta := unobserved.mean.theta]

    # now we have everything in place to make the predictions
    predictions2 <- predictions2[, list(pred.prob = sum(mean.theta * model.prob)), by = list(pred.code = code)]
    return(predictions2)
    # see code from make.probability.predictions for a few more calculations
  }


  if (model$dist.type != "wordwise") {
    res <- lapply(ans, function(singleAns) {

      switch(model$dist.type,
             # model$dist.type == "substring"
             substring = {
               if (parallel) {
                 ind <- which(parallel::parSapply(cl, model$prediction.datasets$modelProb[, string], grepl, singleAns, fixed = TRUE))
               } else {
                 ind <- which(sapply(model$prediction.datasets$modelProb[, string], grepl, singleAns, fixed = TRUE))
               }
               make.probability.predictions.for.single.answer(model$prediction.datasets$modelProb[ind],
                                                              model$prediction.datasets$categoryProb,
                                                              K = model$num.allowed.codes)
             },

             # model$dist.type == "fulltext"
             fulltext = {
               ind <- which(stringdist::stringdist(model$prediction.datasets$modelProb[, string], singleAns, method = model$dist.control$method, weight = model$dist.control$weight) <= model$threshold[2])
               make.probability.predictions.for.single.answer(model$prediction.datasets$modelProb[ind],
                                                                       model$prediction.datasets$categoryProb,
                                                                       K = model$num.allowed.codes)
             }
      )
    })

    # assign IDs and bind everything together
    for (idn in seq_along(res)) {
      res[[idn]][, id := id[idn]]
    }
    res <- rbindlist(res)

    if (parallel & model$dist.type == "substring") parallel::stopCluster(cl)

  } else { # model$dist.type == "wordwise"

    matrix <- asDocumentTermMatrix(ans, vect.vocab = NULL,
                                   stopwords = model$preprocessing$stopwords,
                                   stemming = model$preprocessing$stemming,
                                   type = "dgTMatrix")

    distmat <- stringdist::stringdistmatrix(model$prediction.datasets$modelProb[, string], toupper(matrix$dtm@Dimnames[[2]]), method = model$dist.control$method, weight = model$dist.control$weight) # calculate distances between each word in matrix1 and the coding_index
    distmat.table <- data.table(word.id = which(distmat <= model$threshold[2], arr.ind = TRUE)[, 2], dictStringInd = which(distmat <= model$threshold[2], arr.ind = TRUE)[, 1], dist = distmat[which(distmat <= model$threshold[2], arr.ind = TRUE)])
    distmat.table <- merge(distmat.table, data.table(id = id[matrix$dtm@i + 1], intString = ans[matrix$dtm@i + 1], word.id = matrix$dtm@j + 1), by = "word.id", allow.cartesian=TRUE) # we have now columns for each unique.string, word, dictString, dist
    # now keep all words that have at least one dictString with minimal distance
    distmat.table[, dist2 := min(dist), by = list(intString, word.id, id)] # not sure if it is good to calculate this by word. Alternative: calc dist2 from all words. And the online implementation is again different
    dist_table_without_code <- unique(distmat.table[, .SD[dist == dist2, list(dictStringInd, dist)], by = list(id, intString)]) # unique removes duplicate dictStringInd that may be linked to multiple words


    res <- dist_table_without_code[, .SD[, make.probability.predictions.for.single.answer(model$prediction.datasets$modelProb[dictStringInd],
                                                                                   model$prediction.datasets$categoryProb,
                                                                                   K = model$num.allowed.codes)],
                            by = id]

    # add "nothing predicted" if there is no wordwise match for an id
    additionalIDs <- setdiff(id, res[, id])
    if (length(additionalIDs) > 0) res <- rbind(res, data.table(id = additionalIDs, pred.code = "-9999", pred.prob = 1/model$num.allowed.codes))

  }

  # add additional columns from new data to the result
  if ("occupationData" %in% class(newdata)) {
    res <- merge(res, newdata, by = "id")
    class(res) <- c(class(res), "occupationalPredictions")
    return(res)
  } else {
    return(merge(res, data.table(id = id, ans = newdata), by = "id"))
  }

}
