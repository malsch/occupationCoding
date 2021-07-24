## Overview

This is an R package for occupation coding. It accompanies the following publication:

Malte Schierholz, Matthias Schonlau (forthcoming): Machine Learning for Occupation Coding — a Comparison Study, Journal of Survey Statistics and Methodology, https://doi.org/10.1093/jssam/smaa023

Two groups of users will profit from this package:
- Applied users, who have German survey answers and want to run machine learning for automated occupation coding
- Programmers, who want to compare various machine learning algorithms for occupation coding and set up a coding pipeline that works best for them

## Installation

The easiest way to install this package.
``` r
install.packages("devtools")
devtools::install_github("malsch/occupationCoding")
```

Applied German users will also want to download the file ``Gesamtberufsliste_der_BA.xlsx`` from https://download-portal.arbeitsagentur.de/ (requires free registration)

## Usage

### For Applied Users (German only, coding according to the 2010 German classification of occupation)

Load the coding index ``Gesamtberufsliste_der_BA.xlsx`` first.
``` r
path_to_file <- "./Gesamtberufsliste_der_BA.xlsx" # change path
try({coding_index_w_codes <- prepare_German_coding_index_Gesamtberufsliste_der_BA(path_to_file, 
                                  count.categories = FALSE)}, silent = TRUE)
```

Consult the coding index. For typical survey data, there is an entry in the coding index for about ~45% of all answers. In rare cases, when the same job title is used in different industries, there is more than one entry in the processed coding index.
``` r
text_input <- c("Bürokauffrau", "Stadtjugendpfleger", "Erzieherhelfer im Lehrlingswohnheim.", 
                "Mitarbeit bei einer Filmproduktion", "Abschleifer")
(res <- predictWithCodingIndex(text_input, coding_index = coding_index_w_codes))
##    id                                  ans pred.code
## 1:  1                         Bürokauffrau     71402
## 2:  2                   Stadtjugendpfleger     83124
## 3:  3 Erzieherhelfer im Lehrlingswohnheim.      <NA>
## 4:  4   Mitarbeit bei einer Filmproduktion      <NA>
## 5:  5                          Abschleifer 24222,21212
```

This was easy. Schierholz (2019, pp. 206-208) suggests an alternative method, which yields far better results. His technique relies on anonymized training data from various surveys (Antoni et al., 2010; Rohrbach-Schmidt and Hall, 2013; Lange et al., 2017; Hoffmann et al., 2018; Trappmann et al. 2010), which are provided as part of this package. 

Training the model will take several minutes ...
``` r
simBasedModelWordwise <- trainSimilarityBasedReasoning2(anonymized_data = surveyCountsWordwiseSimilarity,
                                                         num.allowed.codes = 1291,
                                                         coding_index_w_codes = coding_index_w_codes,
                                                         preprocessing = list(stopwords = NULL, stemming = NULL, strPreprocessing = TRUE, removePunct = FALSE),
                                                         dist.type = "wordwise",
                                                         dist.control = list(method = "osa", weight = c(d = 1, i = 1, s = 1, t = 1)),
                                                         threshold = c(max = NA, use = 1),
                                                         simulation.control = list(n.draws = 250, check.normality = FALSE)
)
simBasedModelSubstring <- trainSimilarityBasedReasoning2(anonymized_data = surveyCountsSubstringSimilarity,
                                                         num.allowed.codes = 1291,
                                                         coding_index_w_codes = coding_index_w_codes,
                                                         preprocessing = list(stopwords = NULL, stemming = NULL, strPreprocessing = TRUE, removePunct = FALSE),
                                                         dist.type = "substring",
                                                         dist.control = NA,
                                                         threshold = NA,
                                                         simulation.control = list(n.draws = 250, check.normality = FALSE)
                                                         )
```
Now predict possible codes
``` r
resWordwise <- predictSimilarityBasedReasoning(simBasedModelWordwise, text_input)
resSubstring <- predictSimilarityBasedReasoning(simBasedModelSubstring, text_input)
```

We actually trained two different models. We could now use either one, but the results become better if we combine them. If the output from one model makes poor predictions (that is, if the five most likeliest categories have a chance less than 50% to be correct), we simply use the other.

``` r
wordwiseModelUsefulIds <- resWordwise[, .(prob = sum(head(.SD[order(pred.prob, decreasing = TRUE), pred.prob], 5))), by = id][prob > 0.5, id]
resCombined <- rbind(resWordwise[id %in% wordwiseModelUsefulIds], resSubstring[!(id %in% wordwiseModelUsefulIds)])[order(id)]
```

Let's look at the results again. The two most probable codes for each answer are shown here. The algorithm is rather convinced of its predictions for answers 1 and 3, it has some confidence in its predictions for answers 2 and 5, but for answer 4 even the most likeliest code has just a 7% chance to be correct.

``` r
resCombined[, head(.SD[order(pred.prob, decreasing = TRUE)], 2), by = id]
##    id pred.code    pred.prob                                  ans
## 1:  1     71402 0.8928066326                         Bürokauffrau
## 2:  1     72213 0.0200043173                         Bürokauffrau
## 3:  2     83124 0.6698570028                   Stadtjugendpfleger
## 4:  2     -9999 0.0002560075                   Stadtjugendpfleger
## 5:  3     83112 0.8308403476 Erzieherhelfer im Lehrlingswohnheim.
## 6:  3     83132 0.0237893319 Erzieherhelfer im Lehrlingswohnheim.
## 7:  4     43423 0.0744033856   Mitarbeit bei einer Filmproduktion
## 8:  4     71393 0.0488194384   Mitarbeit bei einer Filmproduktion
## 9:  5     24222 0.5933915448                          Abschleifer
##10:  5     -9999 0.0003152365                          Abschleifer

## Note that the code `-9999` has a special meaning: It is always predicted with very low pred.prob and stands for all codes that have no evidence of being appropriate. Consider the second answer: There are 1291 allowed codes in the target classification used here. Code 83124 has probability 0.67 and every other code has probability 0.000256, for a total probability of 0.67 + 1290 * 0.000256 = 1.
```

I expect there are two main use cases for this algorithm: automated coding and computer-assisted coding. With (semi-)automated coding, the computer automatically selects the code that it finds to be most likely (at least if ``pred.prob`` is above a user-defined threshold). The code for semi-automated coding would look something like this:

``` r
threshold <- 0.8 # only for demonstration purposes
resCombined[, .SD[which.max(pred.prob), list(ans, predicted = ifelse(pred.prob > threshold, pred.code, "no prediction"), pred.prob)], by = id]
```

With computer-assisted coding, the computer suggests a number of codes (say, 5) and a human coder would select the most appropriate one from the list of suggestions. Building a user interface to support this mode of coding is beyond the scope of this package.

### For Programmers

Don't expect that this package will solve your problem without effort. You will probably need to do some programming to make this package suit your application.

To get started, run the example code in ``?predictLogisticRegressionWithPenalization``. This function implements penalized multinomial logistic regression (algorithm 5 in the paper) and produces the kind of output you can get with this package. 

If you know the algorithm you want to use, look at the examples of the ``predict``-function for this algorithm (e.g., ``?predictXgboost``).

Additional algorithms not mentioned in the paper are described in my dissertation at https://madoc.bib.uni-mannheim.de/50617/ The example code in ``?selectMaxProbMethod`` is a good start to see these algorithms at work. This function implements the ``Maximum Probability Algorithm`` (algorithm 10 in the dissertation).

## References

- Antoni, M., Drasch, K., Kleinert, C., Matthes, B., Ruland, M. and Trahms, A. (2010): Arbeiten und Lernen im Wandel * Teil 1: Überblick über die Studie, FDZ-Methodenreport 05/2010, Forschungsdatenzentrum der Bundesagentur für Arbeit im Institut für Arbeitsmarkt- und Berufsforschung, Nuremberg.
- Lange, C., Finger, J., Allen, J., Born, S., Hoebel, J., Kuhnert, R., Müters, S., Thelen, J., Schmich, P., Varga, M., von der Lippe, E., Wetzstein, M., Ziese, T. (2017): Implementation of the European Health Interview Survey (EHIS) into the German Health Update (GEDA), Archives of Public Health, 75, 1–14.
- Hoffmann, R., Lange, M., Butschalowsky, H., Houben, R., Schmich, P., Allen, J., Kuhnert, R., Schaffrath Rosario, A., Gößwald, A. (2018): KiGGS Wave 2 Cross-Sectional Study—Participant Acquisition, Response Rates and Representativeness, Journal of Health Monitoring, 3, 78–91. (only wave 2)
- Rohrbach-Schmidt, D., Hall, A. (2013): BIBB/BAuA Employment Survey 2012, BIBB-FDZ Data and Methodological Reports Nr. 1/2013. Version 4.1, Federal Institute for Vocational Education and Training (Research Data Centre), Bonn.
- Schierholz, Malte (2019): New methods for job and occupation classification. Dissertation, Mannheim. https://madoc.bib.uni-mannheim.de/50617/, pp. 206-208 and p. 268, pp. 308-320 
- Trappmann, M., Beste, J., Bethmann, A., Müller, G. (2013): The PASS Panel Survey after Six Waves, Journal for Labour Market Research, 46, 275–281. (only wave 10) 
