#' Plot agreement rate vs. production rate
#'
#' Plot the proportion of answers coded correctly for any given production rate (proportion of \code{n} that are coded automatically.)
#'
#' @param occupationalPredictionsAmongTopK a data table created with \code{\link{calcAccurateAmongTopK}}.
#' @param n Number of unique persons in test data (may be larger than the number of persons in occupationalPredictionsAmongTopK if for some persons no codes were suggested)
#' @param yintercept Where to draw a horizontal line?
#' @param filename If a \code{filename} is specified the diagram will be saved at with this name.
#'
#' @seealso \code{\link{plotTruePositivesVsFalsePositives}}, \code{\link{calcAccurateAmongTopK}}
#'
#' @return a ggplot
#' @import data.table
#' @import ggplot2
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
#' model <- trainLogisticRegressionWithPenalization(splitted.data$train, preprocessing = list(stopwords = tm::stopwords("de"), stemming = "de", countWords = FALSE), tuning = list(alpha = 0.05, maxit = 50^5, nlambda = 100, thresh = 1e-5))
#' res <- predictLogisticRegressionWithPenalization(model, splitted.data$test)
#'
#' # expand to contain more categories than the initial ones
#' res.proc1 <- expandPredictionResults(res, allowed.codes = c("12345", allowed.codes), method.name = "glmnet1")
#'
#' # we can use different methods to create a combined dataset. This is how to run the subsequent analysis functions only once.
#' res.proc2 <- expandPredictionResults(res, allowed.codes = c("12345", allowed.codes), method.name = "glmnet2")
#' res.proc <- rbind(res.proc1, res.proc2); class(res.proc) <- c(class(res.proc), "occupationalPredictionsComplete")
#'
#' calcAccurateAmongTopK(res.proc, k = 5)[,mean(acc), by = method.name]
#' plotAgreementRateVsProductionRate(calcAccurateAmongTopK(res.proc, k = 5), n = n.test, yintercept = 0.85)
#' plotAgreementRateVsProductionRate(calcAccurateAmongTopK(res.proc, k = 1), n = n.test, yintercept = 0.85, filename = "test.pdf")
plotAgreementRateVsProductionRate <- function(occupationalPredictionsAmongTopK, n, yintercept, filename = NULL) {

  print(paste0("If these numbers are different than the size of the test data ", n, ", make sure to understand what this means. (x-axis will end before 1)"))
  print(occupationalPredictionsAmongTopK[, .N, by = method.name])

  # calculate coordinates to plot
  agreementProductionGraph <- occupationalPredictionsAmongTopK[, .SD[order(pred.prob, acc, decreasing = TRUE), list(y = cumsum(acc)/ 1:.N, x = 1:.N/n)], by = method.name]

  p <- ggplot(agreementProductionGraph, aes(x = x, y = y, colour = method.name, group = paste(method.name))) +
    geom_line() +
    geom_hline(yintercept = yintercept) +
    #   geom_line(data = agreementProductionGraph[method == "dw averaged"], aes(x = x, y = y, colour = method, size = method))
    scale_colour_manual(values = c("chocolate", "blue4", "red", c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")), #
                        name = "Prediction Method",
                        guide = guide_legend(direction = "horizontal",
                                             nrow = 2, ncol=4,
                                             title.position = "top",
                                             title.hjust = 0.5,
                                             label.position="right",
                                             label.hjust = 0,
                                             label.vjust = 0.5,
                                             reverse = TRUE
                                             #                                  label.theme = element_text(angle = 90)
                        )) +
    labs(x = "Production Rate", y = "Agreement Rate") + # , title = "Agreement Rates for most probable category at various production rates") +
    theme(legend.position="bottom")

  if (!is.null(filename)) ggsave(filename, plot = p, width = 7, height = 7)

  return(p)
}

