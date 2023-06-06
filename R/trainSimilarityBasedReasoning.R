#' Train Similarity Based Probability Model
#'
#' For each entry in the coding index, look up how answers that are similar to the coding index were coded in training data and calculate probabilities.
#'
#' @param data a data.table created with \code{\link{removeFaultyAndUncodableAnswers_And_PrepareForAnalysis}}
#' @param coding_index_w_codes a data.table with columns
#' \describe{
#'   \item{bezMale}{a character vector, contains masculine job titles from the coding index.}
#'   \item{bezFemale}{a character vector, contains feminine job titles from the coding index.}
#'   \item{Code}{a character vector with associated classification codes.}
#' }
#' @param coding_index_without_codes a preprocessed character vector, meant for \code{\link{frequent_phrases}}
#' @param preprocessing a list with elements
#' \describe{
#'   \item{stopwords}{a character vector, use \code{tm::stopwords("de")} for German stopwords. Only used if \code{dist.type = "wordwise"}.}
#'   \item{stemming}{\code{NULL} for no stemming and \code{"de"} for stemming using the German porter stemmer. Do not use unless the job titles in \code{coding_index_w_codes} were stemmed.}
#'   \item{strPreprocessing}{\code{TRUE} if \code{\link{stringPreprocessing}} shall be used.}
#'   \item{removePunct}{\code{TRUE} if \code{\link[tm]{removePunctuation}} shall be used.}
#' }
#' @param dist.type How to calculate similarity between entries from both coding_indices and verbal answers from the survey? Three options are currently supported. Since we use the \code{\link[stringdist]{stringdist}}-function excessively, one could easily extend the functionality of this procedure to other distance metrics.
#' \describe{
#'   \item{dist.type = "fulltext"}{Uses the \code{\link[stringdist]{stringdist}}-function directly after preprocessing to calculate distances. (the simplest approach but least useful.)}
#'   \item{dist.type = "substring"}{An entry from the coding index and a verbal answer are similar if the entry from the coding index is a substring of the verbal answer.}
#'   \item{dist.type = "wordwise"}{After preprocessing, split the verbal answer into words. Then calculate for each word separately the the similarity with entries from the coding index, using \code{\link[stringdist]{stringdist}}. Not the complete verbal answer but only the words (0 or more) that have highest similarity are then used to determine similarity with entries from the coding index.}
#' }
#' @param dist.control If \code{dist.type = "fulltext"} or \code{dist.type = "wordwise"} the entries from this list will be passed to \code{\link[stringdist]{stringdist}}. Currently only two possible entries are supported (method = "osa", weight = c(d = 1, i = 1, s = 1, t = 1) is recommended), but one could easily extend the functionality.
#' @param threshold A numeric vector with two elements. If \code{dist.type = "fulltext"} or \code{dist.type = "wordwise"}, the threshold determines up to which distance a verbal answer and an entry from the coding index are similar. The second number actually gets used. The first number is only used to speed up similarity calculations. It should be identical or larger than the second number. 
#' @param simulation.control a list with two components,
#' \describe{
#'   \item{n.draws}{Number of draws from the posterior distribution to determine posterior predictive probabilities. The larger, the more precise the results will be.}
#'   \item{check.normality}{We would like that the hyperprior distribution is normal. Set check.normality to TRUE to do some diagnostics about this.}
#' }
#' @param tmp_folder The name of a folder where the algorithm will store the results from similarity calculations. Use any folder you like. Links between verbal answers and coding indices are only stored if the distance is \code{<= threshold[1]}
#'
#' @seealso
#' See \code{\link{predictSimilarityBasedReasoning}} for more examples and recommended settings. See \code{\link{trainSimilarityBasedReasoning2}} for the same functionality, but using aggregated (anonymized!) training data. German training data are available.
#'
#' \code{\link{createSimilarityTableWordwiseStringdist}}, \code{\link{createSimilarityTableSubstring}}, \code{\link{createSimilarityTableStringdist}} for implementations of the different \code{dist.type}. \code{\link{frequent_phrases}} is a character vector with frequent German answers.
#'
#' Schierholz, Malte (2019): New methods for job and occupation classification. Dissertation, Mannheim. \url{https://madoc.bib.uni-mannheim.de/50617/}, pp. 206-208 and p. 268, pp. 308-320
#'
#' @return a list with components
#' \describe{
#'   \item{prediction.datasets$modelProb}{Contains all entries from the coding index. dist = "official" if the entry stems from coding_index_w_codes and dist = selfcreated if the entry stems from coding_index_without_codes. \code{string.prob} is used for weighting purposes (model averaging) if a new verbal answer is similar to multiple strings. \code{unobserved.mean.theta} gives a probability (usually very low) for any category that was not observed in the training data together with this string.}
#'   \item{prediction.datasets$categoryProb}{\code{mean.theta} is the probability for \code{code} given that an incoming verbal answer is similar to \code{string}. Only available if this code was at least a single time observed with this string (Use \code{unobserved.mean.theta} otherwise).}
#'   \item{num.allowed.codes}{Number of categories in the classification.}
#'   \item{preprocessing}{The input parameter stored to replicate preprocessing with incoming data.}
#'   \item{dist.type}{The input parameter stored to replicate distance calculations with incoming data.}
#'   \item{dist.control}{The input parameter stored to replicate distance calculations with incoming data.}
#'   \item{threshold}{The input parameter stored to replicate distance calculations with incoming data.}
#'   \item{simulation.control}{The input parameter.}
#' }
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
#' # train model
#' simBasedModel <- trainSimilarityBasedReasoning(data = proc.occupations,
#'                               coding_index_w_codes = coding_index_excerpt,
#'                               coding_index_without_codes = frequent_phrases,
#'                               preprocessing = list(stopwords = tm::stopwords("de"), stemming = NULL, strPreprocessing = TRUE, removePunct = FALSE),
#'                               dist.type = "wordwise",
#'                               dist.control = list(method = "osa", weight = c(d = 1, i = 1, s = 1, t = 1)),
#'                               threshold = c(max = 3, use = 1), simulation.control = list(n.draws = 50, check.normality = FALSE),
#'                               tmp_folder = "similarityTables")
trainSimilarityBasedReasoning <- function(data,
                                          coding_index_w_codes,
                                          coding_index_without_codes,
                                          preprocessing = list(stopwords = tm::stopwords("de"), stemming = NULL, strPreprocessing = TRUE, removePunct = FALSE),
                                          dist.type = c("wordwise", "substring", "fulltext"),
                                          dist.control = list(method = "osa", weight = c(d = 1, i = 1, s = 1, t = 1)),
                                          threshold = c(max = 3, use = 1),
                                          simulation.control = list(n.draws = 250, check.normality = FALSE),
                                          tmp_folder = "similarityTables") {

  # codes <- data[,code]
  num.allowed.codes <- length(attr(data, "classification")$code)
  dataCopy <- copy(data[, list(ans, code, id)])

  if (num.allowed.codes < 3) stop("Number of allowed codes is too low: ", num.allowed.codes)
  if (!(tmp_folder %in% list.dirs(full.names = FALSE))) stop ("Please make sure the following folder exists: ", tmp_folder)
  
  if (is.na(threshold[1])) stop("threshold[1] is NA. Please enter a value (default: 3).")

  ####
  # Prepare coding index
  # write female titles beneath the male title
  coding_index_w_codes <- rbind(coding_index_w_codes[, list(title = bezMale, Code)],
                                coding_index_w_codes[, list(title = bezFemale, Code)])
  # standardize titles from the coding index
  coding_index_w_codes <- coding_index_w_codes[,title := stringPreprocessing(title)]
  # drop duplicate lines, might be suboptimal because we keep each title and its associated code only a single time. This means we delete duplicates and the associated, possibly relevant codes.
  coding_index_w_codes <- coding_index_w_codes[!duplicated(title)]
    # we will need this column later to merge with survey data (that may also have answers coded which are not predicted by the dictionary)
  coding_index_w_codes[, dict.predicted.category := TRUE]

  ###
  # preprocessing
  if (preprocessing$removePunct) {
    dataCopy <- dataCopy[, ans := tm::removePunctuation(ans)]
  }

  if (preprocessing$strPreprocessing) {
    dataCopy <- dataCopy[, ans := stringPreprocessing(ans)]
  }

  # 2 strategies to save time:
  # - work with unique(ans)
  # - save matching results and load them as needed

  # second strategy
  tmp.file.name <- sprintf("%s_%s_max%s.RData", dist.type, paste(unlist(dist.control), collapse = "_"), threshold[1])
  if (tmp.file.name %in% list.files(tmp_folder)) {
    load(file = paste(tmp_folder, tmp.file.name, sep = "/"))

    # get intString that were processed previously
    previousIntString <- c(similarityTable$dist_table_w_code[, intString], similarityTable$dist_table_without_code[, intString], strings.not.found)
    new.unique.string <- setdiff(dataCopy$ans, previousIntString)

    if (length(new.unique.string) < 2) {
      cat("Using existing similarityTable. new.unique.string is empty or only one element long: ", new.unique.string, "\n")
    } else {
      cat("Updating similarityTable... Number of new unique strings: ", length(new.unique.string), "\n")
      similarityTableUpdate<- switch(dist.type,
                                     wordwise = createSimilarityTableWordwiseStringdist(unique.string = new.unique.string,
                                                                                   coding_index_w_codes = coding_index_w_codes,
                                                                                   coding_index_without_codes = coding_index_without_codes,
                                                                                   preprocessing = preprocessing,
                                                                                   dist.control = dist.control,
                                                                                   threshold = threshold[1]),
                                     substring =  createSimilarityTableSubstring(unique.string = new.unique.string,
                                                                            coding_index_w_codes = coding_index_w_codes,
                                                                            coding_index_without_codes = coding_index_without_codes),
                                     fulltext =  createSimilarityTableStringdist(unique.string = new.unique.string,
                                                                            coding_index_w_codes = coding_index_w_codes,
                                                                            coding_index_without_codes = coding_index_without_codes,
                                                                            dist.control = dist.control,
                                                                            threshold = threshold[1])
      )
      similarityTable$dist_table_w_code <- rbind(similarityTable$dist_table_w_code, similarityTableUpdate$dist_table_w_code)
      similarityTable$dist_table_without_code <- rbind(similarityTable$dist_table_without_code, similarityTableUpdate$dist_table_without_code)

      cat("Number of strings not found in previous data:", length(strings.not.found), "\n")
      strings.not.found <- c(strings.not.found, setdiff(new.unique.string, c(similarityTable$dist_table_w_code[, intString], similarityTable$dist_table_without_code[, intString])))
      cat("Number of strings not found after updating with current data (adding more elements):", length(strings.not.found), "\n")
      save(similarityTable, strings.not.found,  file = paste(tmp_folder, tmp.file.name, sep = "/"))
    }

  } else { # no previous matching file was created
    unique.string <- unique(dataCopy$ans) # first strategy
    similarityTable <- switch(dist.type,
                              wordwise = createSimilarityTableWordwiseStringdist(unique.string = unique.string,
                                                              coding_index_w_codes = coding_index_w_codes,
                                                              coding_index_without_codes = coding_index_without_codes,
                                                              preprocessing = preprocessing,
                                                              dist.control = dist.control,
                                                              threshold = threshold[1]),
                              substring =  createSimilarityTableSubstring(unique.string = unique.string,
                                                       coding_index_w_codes = coding_index_w_codes,
                                                       coding_index_without_codes = coding_index_without_codes),
                              fulltext =  createSimilarityTableStringdist(unique.string = unique.string,
                                                       coding_index_w_codes = coding_index_w_codes,
                                                       coding_index_without_codes = coding_index_without_codes,
                                                       dist.control = dist.control,
                                                       threshold = threshold[1])
                              )

    strings.not.found <- setdiff(unique.string, c(similarityTable$dist_table_w_code[, intString], similarityTable$dist_table_without_code[, intString]))
    cat("Number of strings not found: ", length(strings.not.found), "\n")

    save(similarityTable, strings.not.found, file = paste(tmp_folder, tmp.file.name, sep = "/"))
  }

  #######################################################
  # functions that will be called below
  #######################################################

  create.prediction.dataset.given.distance <- function(complete.data, sim.dict, sim.self.dict, dict, K, n, check.normality) {
    # make predictions using the official dictionary

    setkey(sim.dict, intString)
    setkey(sim.self.dict, intString)
    setkey(complete.data, ans)

    # merge with interview (i.e. what appeared previously a single time in u.string appears now as often as the u.string was mentioned in survey answers)
    # make predictions using monte carlo (this takes a while but the computations need only to be done a single time)
    predi <- make.predictions.using.dictionary.information(complete.data[sim.dict, list(dictString = dictString.title, dict.code = dictString.Code, surv.string = ans, surv.code = surv.code), allow.cartesian=TRUE],
                                                           dict = dict, K = K, n = n, check.normality = check.normality)
    predi$model.prob[, dist := "official"]
    predi$category.prob[, dist := "official"]

    # merge with interview (i.e. what appeared previously a single time in u.string appears now as often as the u.string was mentioned in survey answers)
    # make predictions using monte carlo (this takes a while but the computations need only to be done a single time)
    predi.sd <- make.predictions.not.using.dictionary.information(complete.data[sim.self.dict, list(dictString = dictString, surv.string = ans, surv.code = surv.code), allow.cartesian=TRUE],
                                                                  K = K, n = n, check.normality = check.normality)
    predi.sd$model.prob[, dist := "selfcreated"]
    predi.sd$category.prob[, dist := "selfcreated"]

    return(list(modelProb = rbind(predi$model.prob, predi.sd$model.prob),
                categoryProb =   rbind(predi$category.prob, predi.sd$category.prob)))
  }

  ############
  # 2 functions to make predictions usig the derived formulas
  # first if dictionary codes are available,
  # afterwards without
  ############

  # This function accepts training data and the dictionary to make probabilistic predictions for all entries in the dictionary
  make.predictions.using.dictionary.information <- function(table, dict = dict, K = K, n = n, check.normality = check.normality) {
    # accepts a data.table "table" that should have the following variables:
    # - dictString : the official rule from the dictionary
    # - dictCode : the official code from the dictionary
    # - surv.string (not used) : the observed survey answer
    # - surv.code : the assigned survey code
    # - dist (not used) : some distance measure between dictString and surv.string. Predictions are based on the assumption of exchangeability, meaning that all surv.strings are equally informative about the associated dictString independent of dist. This means dist should not vary too much
    # - fold (not used): the fold of the survey data
    # "dict" is a data.table with 2 columns
    # - title : list of job titles (dictString should be a subset of this)
    # - Code : the official code from the dictionary
    # "K" is the number of codes that may appear in the classification
    # "n" is the number of random draws for monte carlo integration -> larger n decreases the monte carlo error
    # "check.normality" : should a graphical display be created to see if  p(phi | y, x) is well approximated by Normal(post.modus.phi.given.y, posterior.covariance)

    # log density for p(phi | y, x)
    # phi needs to be a vector of length 2, the first element is the parameter \phi_D (relevant if dict.code == surv.code), the second element is the parameter \phi_U (relevant if dict.code != surv.code)
    p.log.phi.y <- function(phi, data, K) {
      data[, lgamma((K-1)*phi[2] + phi[1]) - lgamma((K-1)*phi[2] + phi[1] + sum(N)) + sum(lgamma(dict.predicted.category * phi[1] + (!dict.predicted.category) * phi[2] + N) - lgamma(dict.predicted.category * phi[1] + (!dict.predicted.category) * phi[2])), by = string][, sum(V1)]
    }

    # gradient for log p(phi | y, x)
    gradient.p.log.phi.y <- function(phi, data, K) {
      c(data[, digamma(phi[1] + (K-1)*phi[2]) + (-digamma(phi[1] + (K-1)*phi[2] + sum(N))) + sum(dict.predicted.category * (digamma(phi[1] + N) - digamma(phi[1]))) , by = string][, sum(V1)],
        data[, (K-1) * (digamma(phi[1] + (K-1)*phi[2]) - digamma(phi[1] + (K-1)*phi[2] + sum(N))) + sum((!dict.predicted.category) * (digamma(phi[2] + N) - digamma(phi[2]))), by = string][, sum(V1)])
    }
    # p.log.phi.y(c(0.9,  0.0003791069), data = freq_table, K = 1286)
    # gradient.p.log.phi.y(c(0.8892206409,  0.0003791069), data = freq_table, K = 1286)

    # inverse observed fisher information for p(phi | y, x) -> allows calculating the 2*2 covariance matrix
    observed.fisher.information <- function(phi, data, K) {
      i11 <- data[, trigamma(phi[1] + (K-1)*phi[2]) + (-trigamma(phi[1] + (K-1)*phi[2] + sum(N))) + sum(dict.predicted.category * (trigamma(phi[1] + N) - trigamma(phi[1]))) , by = string][, sum(V1)]
      i12 <- data[, (K-1) * (trigamma(phi[1] + (K-1)*phi[2]) - trigamma(phi[1] + (K-1)*phi[2] + sum(N))), by = string][, sum(V1)]
      i22 <- data[, (K-1)^2 * (trigamma(phi[1] + (K-1)*phi[2]) - trigamma(phi[1] + (K-1)*phi[2] + sum(N))) + sum((!dict.predicted.category) * (trigamma(phi[2] + N) - trigamma(phi[2]))), by = string][, sum(V1)]
      solve(- matrix(c(i11, i12, i12, i22), byrow = TRUE, nrow = 2, ncol = 2)) # posterior covariance
    }

    check.normality.approximation <- function(modus, variance, data, K) {
      # idea:
      # if the first derivative of the log posterior density is approximately equal to the first derivative of the log normal density, the normal distribution approximates the posterior well.
      # in a formula: pd / dphi log p(phi) \approx d / dphi log normal(phi)

      min <- modus - 2 * sqrt(diag(variance))
      max <- modus + 2 * sqrt(diag(variance))
      grid <- cbind(seq(min[1], max[1], length = 60), seq(min[2], max[2], length = 60))

      # calculate gradients close to the highest density region
      grad.log.p <- mapply(function(phi1, phi2) gradient.p.log.phi.y(c(phi1, phi2), data, K), grid[,1], grid[,2])
      grad.normal.approximation <- -solve(variance) %*% t(grid - c(rep(modus[1], 60), rep(modus[2], 60)))

      par(mfrow = c(1,2))
      plot(grid[,1], grad.log.p[1,], type = 'l', ylab = "dp \ d phi_D", xlab = "phi_D")
      points(grid[,1], grad.normal.approximation[1,], type = 'l', col = 2)
      legend("topright", legend = c("deriv p(phi_D | y)", "deriv Normal approx"), fill = c(1, 2))

      plot(grid[,2], grad.log.p[2,], type = 'l', ylab = "dp \ d phi_U", xlab = "phi_U")
      points(grid[,2], grad.normal.approximation[2,], type = 'l', col = 2)
      legend("topright", legend = c("deriv p(phi_U | y)", "deriv Normal approx"), fill = c(1, 2))
      par(mfrow = c(1,1))
    }

    # string (model) probability (and posterior expectation for those categories that were not observed in the training data)
    mean.d.y.given.phi <- function(phi, data, K) {
      data[, list(string.prob = (lgamma(phi[1] + (K - 1) * phi[2]) - lgamma(phi[1] + (K - 1) * phi[2] + sum(N)) + sum(lgamma(dict.predicted.category * phi[1] + (!dict.predicted.category) * phi[2] + N) - lgamma(dict.predicted.category * phi[1] + (!dict.predicted.category) * phi[2]))),
                  unobserved.mean.theta = phi[2] / (phi[1] + (K-1) * phi[2] + sum(N)),
                  N = sum(N)), by = string] # probability that for string a category is correct if neither the dictionary nor the training data suggest it
    }

    # posterior expectation /posterior predictive probability
    mean.theta.given.phi <- function(phi, data, K) {
      data[, list(code, mean.theta = (dict.predicted.category * phi[1] + (!dict.predicted.category) * phi[2] + N) / (phi[1] + (K-1) * phi[2] + sum(N))), by = string] # posterior predictive probability given phi
    }

    monte.carlo.approx.nv <- function(n, post.modus, post.cov, data, K) {
      # larger n decreases the monte carlo error

      random.phi <- mvtnorm::rmvnorm(n, mean = post.modus, sigma = post.cov)

      model.prob <- NULL
      category.prob <- NULL
      for (aa in 1:nrow(random.phi)) {
        # first alternative to calculate model prob: monte carlo integration -> insert "exp" in function mean.d.y.given.phi
        #   model.prob <- rbind(model.prob, mean.d.y.given.phi(random.phi[aa,], data, K), fill = FALSE)
        category.prob <- rbind(category.prob, mean.theta.given.phi(random.phi[aa,], data, K), fill = FALSE)
      }

      # if a string does not appear in the training data, its probability cannot be calculated. We just set a very small positive probability, implying that the strings effect is limited to future observations where no similar training data are available
      # model.prob[is.nan(string.prob), string.prob := 0.0000001]
      #
      # COMPUTATIONAL PROBLEM string.prob == 0 arises in function mean.d.y.given.phi if we try to calculate exp({small number}) -> insert a somehow larger number instead, eg. (exp(-900) != 0 but R sets it to 0)
      # model.prob[string.prob == 0, string.prob := exp(-700)]

      # model.prob <- model.prob[, list(string.prob = mean(string.prob)^(1/unique(N)), unobserved.mean.theta = mean(unobserved.mean.theta)), by = string] # n-th root of p(y_1, ..., y_n)
      category.prob <- category.prob[, list(mean.theta = mean(mean.theta)), by = list(string, code)]

      # second alternative to calculate model_prob: plugin estimate
      model.prob <- mean.d.y.given.phi(post.modus, data, K)[, list(string, string.prob = exp(1/N * string.prob), unobserved.mean.theta)] # exp(string.prob)^(1/N)
      model.prob[is.nan(string.prob), string.prob := 0.0000001]

      return(list(model.prob = model.prob, category.prob = category.prob))
    }

    # aggregate data to calculate posterior modus and posterior variance
    # this dataset contains for each center point (=entries from the dictionary) how many survey answers are similar and how they were coded
    freq_table <- table[, .N, by = list(string = dictString, code = surv.code, dict.predicted.category = dict.code == surv.code)]

    # merge with dictionary so that we can also make predictions about new strings that appear in the dictionary but not in the survey data
    # freq_table2 contains all combinations of coding_index strings and its associated codes (from trainging data and coding index) (and one could actually use it instead of freq_table because N = 0 cancels out in our formulas)
    freq_table2 <- merge(freq_table, dict, by.x = c("string", "code", "dict.predicted.category"), by.y = c("title", "Code", "dict.predicted.category"), all = TRUE)
    freq_table2[is.na(N), N := 0L]

    # calculate posterior modus and posterior variance
    post.modus.phi.given.y <- optim(c(1, 0.005), fn = p.log.phi.y, gr = gradient.p.log.phi.y, data = freq_table, K = K, method = "L-BFGS-B", lower = c(0.0001, 0.00001), upper = c(10,10), control = list(fnscale = -5))$par
    posterior.covariance <- observed.fisher.information(post.modus.phi.given.y, data = freq_table, K = K)
    # graphical check of the normality approximation
    if (check.normality) check.normality.approximation(post.modus.phi.given.y, posterior.covariance, data = freq_table, K = K)

    # run monte carlo integration
    # to calculate
    # root of p(y_r | M_r, \phi) = \int p(y_r | M_r, \phi) p(phi | y) dphi (function mean.d.y.given.phi, probability that a given rule/model should be used), should have the same dimension as coding_index
    # predictive probability p(y_fk | M_r) = \int p(y_fk | M_r, phi) p(phi) dphi (function mean.theta.given.phi, predictive probability of codes given the rule/model is correct), should have same dimensionality as freq_table2
    return(monte.carlo.approx.nv(n = n, post.modus.phi.given.y, posterior.covariance, freq_table2, K = K))
  }

  # This function accepts training data to make probabilistic predictions for all entries in the dictionary
  make.predictions.not.using.dictionary.information <- function(table, K = K, n = n, check.normality = FALSE) {
    # accepts a data.table "table" that should have the following variables:
    # - dictString : the official rule from the dictionary
    # - dictCode : the official code from the dictionary
    # - surv.string (not used) : the observed survey answer
    # - surv.code : the assigned survey code
    # - dist (not used) : some distance measure between dictString and surv.string. Predictions are based on the assumption of exchangeability, meaning that all surv.strings are equally informative about the associated dictString independent of dist. This means dist should not vary too much
    # - fold (not used): the fold of the survey data
    # "K" is the number of codes that may appear in the classification
    # "n" is the number of random draws for monte carlo integration -> larger n decreases the monte carlo error
    # "check.normality" : should a graphical display be created to see if  p(phi | y, x) is well approximated by Normal(post.modus.phi.given.y, posterior.covariance)

    # log density function for phi | y
    # for self-created dictionaries (.sd) with all phi equal
    p.log.phi.y.sd <- function(phi, data, K) {
      data[, lgamma(K * phi) - lgamma(K * phi + sum(N)) + sum(lgamma(phi + N) - lgamma(phi)), by = string][, sum(V1)]
    }

    # first derivative for log p(phi | y) without information from the dictionary
    deriv.log.p.phi.y <- function(phi, data, K) {
      data[, K * (digamma(phi * K) - digamma(phi * K + sum(N))) + sum(digamma(phi + N) - digamma(phi)), by = string][, sum(V1)]
    }

    # observed information for phi | y (allows calculating the variance)
    observed.fisher.information.sd <- function(phi, data, K) {
      - data[, (K^2 * (trigamma(K * phi) - trigamma((K * phi) + sum(N)))) + sum(trigamma(phi + N) - trigamma(phi)), by = string][, sum(V1)]
    }

    # # check normality approximation
    plot.distribution.phi.given.y <- function(modus, sd, data, K) {

      phi <- seq(modus - 5 * sd, modus + 5 * sd, by = 0.00001) # look at modus +- 5 standard deviations
      par(mfrow = c(1, 2))

      ###### plot distribution
      log.f.phi <- sapply(phi, p.log.phi.y.sd, data, K) # this is the form of the log density for p(phi | y)
      f.phi <- exp(log.f.phi - max(log.f.phi)) # bring the (unnormalized) density back on the original scale (not logarithmized, c.f. Gelman et al. 2014, p. 77), this must have maximum 1. Subtracting the minimum is necessary because otherwise exp() cannot be computed.
      # create the cumulative distribution function for normalization
      F.phi <- cumsum(f.phi) / max(cumsum(f.phi)) # normalized cumulative distribution function. Not quiet: comparison with cumsum(dnorm(phi - 0.001, mean = 0.570, sd = 0.0152439)) shows that both are different by a factor 1000 (because by = 0.001)
      # and this is the normalized density function for p(lampda | y)
      plot(phi[-1], 100000 * diff(F.phi), ylab = "Density", xlab = "phi", type = "l") # multiply with 1/step.width phi
      # compare with normal distribution -> about identical
      points(phi[-1], dnorm(phi[-1], mean = modus, sd = sd), type = 'l', col = 2)
      legend("topright", legend = c("p(phi | y)", "Normal approx"), fill = c(1, 2))
      #   points(phi[-1] - 0.001, diff(pnorm(phi, mean = post.modus.phi.given.y, sd = sd.phi)), type = 'l', col =2)

      #### plot first derivative
      plot(phi, sapply(phi, deriv.log.p.phi.y, data, K), type = 'l') # draws first derivative for p(phi | y)
      points(phi, - (phi - modus) / sd^2, col = 2, type = 'l') # draws first derivative for normal approximation N(mode, sd^2)
      legend("topright", legend = c("deriv p(phi | y)", "deriv Normal approx"), fill = c(1, 2))
    }


    # string (model) probability (and posterior expectation for those categories that were not observed in the training data)
    mean.d.y.given.phi.sd <- function(phi, data, K) {
      data[, list(string.prob = (lgamma(K * phi) - lgamma(K * phi + sum(N)) + sum(lgamma(phi + N) - lgamma(phi))),
                  unobserved.mean.theta = phi/(K * phi + sum(N)),
                  N = sum(N)), by = string] # categories with N = 0 cancel out
    }

    # posterior expectation
    mean.theta.given.phi.sd <- function(phi, data, K) {
      data[, list(code, mean.theta = (phi + N)/(K * phi + sum(N))), by = string]
    }

    ##############################
    # Problem: phi is random.
    # the plugin estimator
    # mean.d.y.and.theta.given.lambda(lambda = post.modus.lambda.given.y.dist0, freq_train_dist0)
    # thus provides wrong estimates.
    # In the following I approximate posterior expectations for string.prob and mean.theta with monte.carlo sampling:
    # draw lambda 100 times from its approximate posterior (i.e. normal), make 100 estimations, and average over those
    #
    monte.carlo.approx.nv.sd <- function(n, post.modus, post.sd, data, K) {
      # larger n decreases the monte carlo error
      #
      random.phi <- rnorm(n, mean = post.modus, sd = post.sd)

      model.prob <- NULL
      category.prob <- NULL
      for (phi in random.phi) {
        #      model.prob <- rbind(model.prob, mean.d.y.given.phi.sd(phi, data, K), fill = FALSE)
        category.prob <- rbind(category.prob, mean.theta.given.phi.sd(phi, data, K), fill = FALSE)
      }

      #    model.prob <- model.prob[, list(string.prob = mean(string.prob)^(1/unique(N)), unobserved.mean.theta = mean(unobserved.mean.theta)), by = string] # n-th root of p(y_1, ..., y_n)
      category.prob <- category.prob[, list(mean.theta = mean(mean.theta)), by = list(string, code)]

      # second alternative to calculate model_prob: plugin estimate (empirical bayes)
      model.prob <- mean.d.y.given.phi.sd(post.modus, data, K)[, list(string, string.prob = exp(1/N * string.prob), unobserved.mean.theta)] # exp(string.prob)^(1/N)
      model.prob[is.nan(string.prob), string.prob := 0.0000001]

      return(list(model.prob = model.prob, category.prob = category.prob))
    }

    # aggregate data
    freq_table <- table[, .N, by = list(string = dictString, code = surv.code)]

    # calculate posterior modus and posterior variance
    post.modus.phi.given.y <- optimize(f = p.log.phi.y.sd, interval = c(0, 10), freq_table, K = K, maximum = TRUE, tol = .Machine$double.eps^0.5)$maximum
    posterior.covariance <- sqrt(1/observed.fisher.information.sd(post.modus.phi.given.y, freq_table, K = K))
    # graphical check of the normality approximation
    if (check.normality) plot.distribution.phi.given.y(post.modus.phi.given.y, posterior.covariance, freq_table, K = K)

    # run monte carlo integration
    return(monte.carlo.approx.nv.sd(n = n, post.modus.phi.given.y, posterior.covariance, freq_table, K = K))
  }


  ############################################


  cat("Lenghty similarity calculations finished. Starting with prediction dataset..")

  return(list(prediction.datasets = create.prediction.dataset.given.distance(dataCopy[,list(ans, surv.code = code, id)],
                                                                             similarityTable$dist_table_w_code[intString %in% dataCopy[, ans] & dist <= threshold[2]],
                                                                             similarityTable$dist_table_without_code[intString %in% dataCopy[, ans] & dist <= threshold[2]],
                                                                             dict = coding_index_w_codes,
                                                                             K = num.allowed.codes, # no KldB categories + no special codes in training data
                                                                             n = simulation.control$n.draws,
                                                                             check.normality = simulation.control$check.normality),
              num.allowed.codes = num.allowed.codes,
              preprocessing = preprocessing,
              dist.type = dist.type,
              dist.control = dist.control,
              threshold = threshold,
              simulation.control = simulation.control))
}
