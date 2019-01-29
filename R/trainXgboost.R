#' Train an extreme gradient boosted tree model
#'
#' Function does some preprocessing and calls xgboost to train gradient boosted trees.
#'
#' See run_algorithms.R for some comments about tuning.
#'
#' @param data a data.table created with \code{\link{removeFaultyAndUncodableAnswers_And_PrepareForAnalysis}}
#' @param allowed.codes a vector containg all the labels of the codes (even those that are not in the data are possible.)
#' @param testCases If \code{NULL} (default) the model is trained as usual. If \code{testCases} is a logical vector equal to the length of \code{data}, the function splits data into a separate evaluation set (the elements with \code{testCases == TRUE}), prints missclassification error of training and evaluation dataset and returns the predictions from the evaluation dataset.
#' @param returnPredictions (only used if testCases are given.) If TRUE, a data.table with predictions for all testCases is returned. Otherwise the xgboost model is returned which can be used for diagnostics but not inside \code{\link{predictXgboost}} because the term-document matrix will be calculated in a different way.
#' @param preprocessing a list with elements
#' \describe{
#'   \item{stopwords}{a character vector, use \code{tm::stopwords("de")} for German stopwords.}
#'   \item{stemming}{\code{NULL} for no stemming and \code{"de"} for stemming using the German porter stemmer.}
#'   \item{countWords}{Set to TRUE if the predictor matrix should contain a column for answer length.}
#' }
#' @param tuning a list with elements that will be passed to \code{\link[xgboost]{xgb.train}} except for two parameters
#' \describe{
#'   \item{early.stopping.max.diff}{If the sum of probabilities of the most probable category is by \code{early.stopping.max.diff} cases larger than the observed number of cases correctly predicted, return Infinity, a value that will support early stopping. Idea behind this: Once the sum of predicted probabilities becomes too large, it is unusual (impossible) to decrease it again. Thus, we have overfitting.}
#'   \item{early.stopping.precision.digits}{Logloss is rounded with \code{early.stopping.precision.digits}. This leads to early stopping if improvements in logloss are actually to small.}
#' }
#'
#' @seealso \code{\link{predictXgboost}}, \code{\link[xgboost]{xgb.train}}
#'
#' @return If \code{testCases = NULL} (default) a xgboost model to be used with \code{\link{predictXgboost}}.
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
#' group <- sample(c(rep("test", n.test), rep("train", nrow(proc.occupations)-n.test)))
#' ##### Tune pararameters with verbose=1 output. We split the data into training and evaluation set of size n.test = 50
#' n.test <- 50
#'
#' # output test dataset with 'returnPredictions = TRUE
#' eval.dataset <- trainXgboost(proc.occupations, allowed.codes = allowed.codes, testCases = group == "test", returnPredictions = TRUE,
#'                       preprocessing = list(stopwords = tm::stopwords("de"), stemming = "de", countWords = FALSE),
#'                       tuning = list(eta = 0.5, lambda = 1e-4, alpha = 0,
#'                                     max.depth = 20, gamma = 0.6,
#'                                     min_child_weight = 0, max_delta_step = 1,
#'                                     subsample = 0.75, colsample_bytree = 1, colsample_bylevel=1,
#'                                     nrounds= 3, early_stopping_rounds = 1,
#'                                     early.stopping.max.diff = n.test / 100, early.stopping.precision.digits = 3,
#'                                     nthread = 8, verbose=1)
#'                       )
#' eval.dataset[, .SD[which.max(pred.prob), list(ans, true.code = code, pred.code, acc = code == pred.code)], by = id][, mean(acc)]
#' produceResults(expandPredictionResults(eval.dataset, allowed.codes = allowed.codes, method.name = "xgboost"), k = 1, n = n.test, num.codes = length(allowed.codes))
#'
#' # same as before but output the model
#' XGboostModel <- trainXgboost(proc.occupations, allowed.codes = allowed.codes, testCases = group == "test", returnPredictions = FALSE,
#'                       preprocessing = list(stopwords = tm::stopwords("de"), stemming = "de", countWords = FALSE),
#'                       tuning = list(eta = 0.5, lambda = 1e-4, alpha = 0,
#'                                     max.depth = 20, gamma = 0.6,
#'                                     min_child_weight = 0, max_delta_step = 1,
#'                                     subsample = 0.75, colsample_bytree = 1, colsample_bylevel=1,
#'                                     nrounds= 3, early_stopping_rounds = 1,
#'                                     early.stopping.max.diff = n.test / 100, early.stopping.precision.digits = 3,
#'                                     nthread = 8, verbose=1)
#'                       )
#'
#' # same as before, but without test data and without early stopping (not recommended because results can be worse)
#' XGboostModel <- trainXgboost(proc.occupations, allowed.codes = allowed.codes, testCases = NULL, returnPredictions = FALSE,
#'                       preprocessing = list(stopwords = tm::stopwords("de"), stemming = "de", countWords = FALSE),
#'                       tuning = list(eta = 0.5, lambda = 1e-4, alpha = 0,
#'                                     max.depth = 20, gamma = 0.6,
#'                                     min_child_weight = 0, max_delta_step = 1,
#'                                     subsample = 0.75, colsample_bytree = 1, colsample_bylevel=1,
#'                                     nrounds= 3, early_stopping_rounds = NULL,
#'                                     early.stopping.max.diff = n.test / 100, early.stopping.precision.digits = 3,
#'                                     nthread = 8, verbose=0)
#'                       )
trainXgboost <- function(data, allowed.codes, testCases = NULL, returnPredictions = FALSE,
                         preprocessing = list(stopwords = character(0), stemming = "de", countWords = TRUE),
                         tuning = list(eta = 0.5, lambda = 1e-4, alpha = 0,
                                       max.depth = 20, gamma = 0.6,
                                       min_child_weight = 0, max_delta_step = 1,
                                       subsample = 0.75, colsample_bytree = 1, colsample_bylevel=1,
                                       nrounds=40, early_stopping_rounds=1,
                                       early.stopping.max.diff = sum(testCases) / 100, early.stopping.precision.digits = 3,
                                       nthread = 1, verbose=1)) {


  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop("Package \"xgboost\" needed if you want to use this function. Please install it.",
         call. = FALSE)
  }

  # and prepare codes
  outcome <- factor(as.character(data[,code]), levels = allowed.codes)
  outcome_dictionary <- data.table(num = 0:(nlevels(outcome) - 1), cat = levels(outcome))

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

  if (is.null(testCases)) { # standard if we want a model
    dtrain <- xgboost::xgb.DMatrix(
      data=matrix$dtm,
      label=as.numeric(outcome) - 1)

    trainingEmptyColumns <- FALSE # no columns from predictor matrix will be excluded during prediction
    watchlist <- list() # not used
  } else {
    ### Use the following lines to see live evaluation
    ### remove columns from predictor matrix if they are all zero in the training data
    trainingEmptyColumns <- Matrix::colSums(matrix$dtm[!testCases, ]) == 0
    dtrain <- xgboost::xgb.DMatrix(
      data=matrix$dtm[!testCases, !trainingEmptyColumns],
      label=as.numeric(outcome[!testCases]) - 1)

    dTest <- xgboost::xgb.DMatrix(data = matrix$dtm[testCases, !trainingEmptyColumns],
                         label=as.numeric(outcome[testCases]) - 1)

    watchlist <- list(train = dtrain, eval = dTest)
  }

  # user defined evaluation function, return a pair metric_name, result
  # NOTE: when you do customized loss function, the default prediction value is margin (not sure if this is correct)
  # this may make buildin evalution metric not function properly
  # for example, we are doing logistic loss, the prediction is score before logistic transformation
  # the buildin evaluation error assumes input is after logistic transformation
  # Take this in mind when you use the customization, and maybe you need write customized evaluation function
  # IDEA: we start with small probabilities that increase with the number of rounds. This means in the beginning we expect to predict more correct categories than what sum(probabilities) would suggest. Stop early if the probabilities grow too large (and the model overfits)
  evalerror <- function(preds, dtrain) {
    labels <- xgboost::getinfo(dtrain, "label")
    pred2 <- matrix(preds, nrow = nrow(outcome_dictionary), byrow = FALSE) # one row per person
    n <- ncol(pred2)
    pred.cat <- apply(pred2, 2, which.max) # for each n, the index of the category with maximal probability
    num.categories.expected <- sum(pred2[cbind(pred.cat, 1:n )])
    num.categories.correct.predicted <- sum(labels == (pred.cat - 1))
    logloss <- -mean(log(pred2[cbind(labels+1, 1:n)]))
    err <- num.categories.correct.predicted - num.categories.expected
    # we would like to stop as soon as err < -tuning$early.stopping.max.diff (results become oversharp) or if logloss actually increases. Workaround to achieve this:
    err <- if (err < -tuning$early.stopping.max.diff) Inf else logloss
    return(list(metric = paste("Logloss:", round(logloss, 4), "N.predict:", num.categories.correct.predicted, "(Acc:", round(num.categories.correct.predicted/n, 4), ")-N.expect:", round(num.categories.expected, 2)), value = round(as.numeric(err), tuning$early.stopping.precision.digits))) # if many errors in a row are smaller than -5 this seems like a good point to stop running more rounds
  }

  # estimate model
  fit <- xgboost::xgb.train(params = list( # see https://xgboost.readthedocs.io/en/latest//parameter.html
      objective="multi:softprob", # "multi:softmax"
      # eval_metric = "merror", eval_metric = "mlogloss", # the latter will be used for early stopping.
      eval_metric = evalerror,
      num_class = nrow(outcome_dictionary),
      booster="gbtree",
      eta=tuning$eta, # Control the learning rate, related to nrounds
      lambda=tuning$lambda, # shrinks weights of each leaf (i.e. values that will be passed through a sigmoid) towards zero with a quadratic penalty
      alpha=tuning$alpha, # shrinks weights of each leaf (i.e. values that will be passed through a sigmoid) towards zero with a absolute penalty
      tree.method = "exact", # because we have 0-1 variables, this should not impede speed to much. Looks like the time is still the same (and the results also)
      max.depth=tuning$max.depth, # Maximum depth of the tree, we'd probably like that every possible job title can be picked up by a tree, so this is related to the number of meaningful job titles a tree can find.
      subsample=tuning$subsample, # subsample ratio of the training instance, Rational: 1 seems reasonable. In every round a tree should be able to pick up the category for every word. 0.9 was a bit worse/or maybe just slower. However, the difference between mlogloss for eval and training becomes smaller.
      colsample_bytree=tuning$colsample_bytree, # subsample ratio of columns when constructing each tree. rational is the same as before. Likewise, 0.9 makes this a bit worse.
      colsample_bylevel = tuning$colsample_bylevel, # even a value at 0.4 did not change the results by much. usually not used because one could achieve the same with the above options?
      gamma = tuning$gamma, # highly important, 0.9 looks quite good for PASS. minimum loss reduction required to make a further partition, closely related to "gain" in the model dump, meaningful words have large gains.
      min_child_weight = tuning$min_child_weight, #  should probably be 0, when large the algorithm will stop early "stop trying to split once you reach a certain degree of purity in a node and your model can fit it" (see https://stats.stackexchange.com/questions/317073/explanation-of-min-child-weight-in-xgboost-algorithm)
      max_delta_step = tuning$max_delta_step), # very meaningful, 8 is too large, 0 too low. supposed to be helpful for the estimation of probabilities with imbalanced datasets with logistic regression, see https://xgboost.readthedocs.io/en/latest/how_to/param_tuning.html
    data=dtrain,
    nthread= tuning$nthread, # number of CPU threads
    nrounds= tuning$nrounds, # 80, # number of trees
    early_stopping_rounds = tuning$early_stopping_rounds, # early stopping if the performance keeps getting worse consecutively for k rounds
    verbose=tuning$verbose, # 1 # do not show partial info
    nfold=1, # number of CV folds
    watchlist = watchlist,
    # feval=rmpse, # custom evaluation metric
    maximize=FALSE # the lower the evaluation score the better
  )

  ##  some diagnostics to understand xgboost
  # plot the first of nrounds trees
  # feature.names <- matrix$dtm@Dimnames[[2]][!trainingEmptyColumns]
  # (dt <- xgboost::xgb.model.dt.tree(feature.names, fit))
  # xgboost::xgb.plot.tree(feature_names = feature.names, model = fit, n_first_tree = 0) # shows only the first tree (the one to predict "-0004")
  # xgboost::xgb.plot.tree(feature_names = feature.names, model = fit, n_first_tree = 1) # shows only the first two trees (that predict "-0004" and "01104")
  ## xgboost::xgb.importance(feature.names, model = fit)
  # write.table(data.table(featureid = 0:(length(feature.names)-1), featurename = feature.names, type = "q"), file = "feature_map.txt", quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
  #
  #
  # xgboost::xgb.dump(model = fit, fname = "test.txt", fmap = "feature_map.txt", with_stats = TRUE)
  #
  # How to match feature names of splits that are following a current 'Yes' branch:
  # merge(dt, dt[, .(ID, Y.Feature=Feature)], by.x='Yes', by.y='ID', all.x=TRUE)[order(Tree,Node)]
  # xgboost::xgb.plot.deepness(fit)


  if (!is.null(testCases) & returnPredictions == TRUE) {
    res <- data.table(id = rep(1:length(ans[testCases]), each = nrow(outcome_dictionary)),
                      ans = rep(ans[testCases], each = nrow(outcome_dictionary)),
                      code = rep(outcome[testCases], each = nrow(outcome_dictionary)),
                      pred.code = rep(outcome_dictionary$cat, times = length(ans[testCases])),
                      pred.prob = predict(fit, dTest, ntreelimit = fit$best_ntreelimit)) # use optimal number of iterations (as measured by mlogloss) to make predictions (one might also want to use the last iterations because it is not so clear what is better. Differences are usually small.)
    class(res) <- c(class(res), "occupationalPredictions")
    attr(res, "evaluation_log") <- fit$evaluation_log
    return(res)
  }


  # and save preprocessing
  fit$outcome_dictionary <- outcome_dictionary
  fit$vect.vocab <- matrix$vect.vocab
  fit$trainingEmptyColumns <- trainingEmptyColumns # in case we used a training process with test data and early stopping
  fit$preprocessing <- preprocessing
  return(fit)
}
