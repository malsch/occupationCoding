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

### For Applied Users (German only)

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

### For Programmers

Don't expect that this package will solve your problem without effort. You will probably need to do some programming to make this package suit your application.

To get started, run the example code in ``?predictLogisticRegressionWithPenalization``. This function implements penalized multinomial logistic regression (algorithm 5 in the paper) and produces the kind of output you can get with this package. 

If you know the algorithm you want to use, look at the examples of the ``predict``-function for this algorithm (e.g., ``?predictXgboost``).

Additional algorithms not mentioned in the paper are described in my dissertation at https://madoc.bib.uni-mannheim.de/50617/ The example code in ``?selectMaxProbMethod`` is a good start to see these algorithms at work. This function implements the ``Maximum Probability Algorithm`` (algorithm 10 in the dissertation).

German users who wish to do coding into the German Classification of Occupation 2010 (KldB 2010) with a coding index should look at ``?prepare_German_coding_index_Gesamtberufsliste_der_BA`` and ``?predictWithCodingIndex``.

