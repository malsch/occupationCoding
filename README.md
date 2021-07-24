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

Training the model (results are actually even better when we use two models) will take several minutes ...
``` r
simBasedModelSubstring <- trainSimilarityBasedReasoning2(anonymized_data = surveyCountsSubstringSimilarity,
                                                         num.allowed.codes = 1291,
                                                         coding_index_w_codes = coding_index_w_codes,
                                                         preprocessing = list(stopwords = NULL, stemming = NULL, strPreprocessing = TRUE, removePunct = FALSE),
                                                         dist.type = "substring",
                                                         dist.control = NA,
                                                         threshold = NA,
                                                         simulation.control = list(n.draws = 250, check.normality = FALSE)
                                                         )
                                                         
simBasedModelWordwise <- trainSimilarityBasedReasoning2(anonymized_data = surveyCountsWordwiseSimilarity,
                                                         num.allowed.codes = 1291,
                                                         coding_index_w_codes = coding_index_w_codes,
                                                         preprocessing = list(stopwords = NULL, stemming = NULL, strPreprocessing = TRUE, removePunct = FALSE),
                                                         dist.type = "wordwise",
                                                         dist.control = list(method = "osa", weight = c(d = 1, i = 1, s = 1, t = 1)),
                                                         threshold = c(max = NA, use = 1),
                                                         simulation.control = list(n.draws = 250, check.normality = FALSE)
)

```

``` r

```

### For Programmers

Don't expect that this package will solve your problem without effort. You will probably need to do some programming to make this package suit your application.

To get started, run the example code in ``?predictLogisticRegressionWithPenalization``. This function implements penalized multinomial logistic regression (algorithm 5 in the paper) and produces the kind of output you can get with this package. 

If you know the algorithm you want to use, look at the examples of the ``predict``-function for this algorithm (e.g., ``?predictXgboost``).

Additional algorithms not mentioned in the paper are described in my dissertation at https://madoc.bib.uni-mannheim.de/50617/ The example code in ``?selectMaxProbMethod`` is a good start to see these algorithms at work. This function implements the ``Maximum Probability Algorithm`` (algorithm 10 in the dissertation).

German users who wish to do coding into the German Classification of Occupation 2010 (KldB 2010) with a coding index should look at ``?prepare_German_coding_index_Gesamtberufsliste_der_BA`` and ``?predictWithCodingIndex``.

## References

- Antoni, M., Drasch, K., Kleinert, C., Matthes, B., Ruland, M. and Trahms, A. (2010): Arbeiten und Lernen im Wandel * Teil 1: Überblick über die Studie, FDZ-Methodenreport 05/2010, Forschungsdatenzentrum der Bundesagentur für Arbeit im Institut für Arbeitsmarkt- und Berufsforschung, Nuremberg.
- Lange, C., Finger, J., Allen, J., Born, S., Hoebel, J., Kuhnert, R., Müters, S., Thelen, J., Schmich, P., Varga, M., von der Lippe, E., Wetzstein, M., Ziese, T. (2017): Implementation of the European Health Interview Survey (EHIS) into the German Health Update (GEDA), Archives of Public Health, 75, 1–14.
- Hoffmann, R., Lange, M., Butschalowsky, H., Houben, R., Schmich, P., Allen, J., Kuhnert, R., Schaffrath Rosario, A., Gößwald, A. (2018): KiGGS Wave 2 Cross-Sectional Study—Participant Acquisition, Response Rates and Representativeness, Journal of Health Monitoring, 3, 78–91. (only wave 2)
- Rohrbach-Schmidt, D., Hall, A. (2013): BIBB/BAuA Employment Survey 2012, BIBB-FDZ Data and Methodological Reports Nr. 1/2013. Version 4.1, Federal Institute for Vocational Education and Training (Research Data Centre), Bonn.
- Schierholz, Malte (2019): New methods for job and occupation classification. Dissertation, Mannheim. https://madoc.bib.uni-mannheim.de/50617/, pp. 206-208 and p. 268, pp. 308-320 
- Trappmann, M., Beste, J., Bethmann, A., Müller, G. (2013): The PASS Panel Survey after Six Waves, Journal for Labour Market Research, 46, 275–281. (only wave 10) 
