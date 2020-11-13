## Overview

This is an R package for occupation coding. It accompanies the following publication:

Malte Schierholz, Matthias Schonlau (forthcoming): Machine Learning for Occupation Coding — a Comparison Study, Journal of Survey Statistics and Methodology, https://doi.org/10.1093/jssam/smaa023

Don't expect that this package will solve your problem without effort. You will probably need to do some programming to make this package suit your application.

## Installation

The easiest way to install this package.
``` r
install.packages("devtools")
devtools::install_github("malsch/occupationCoding")
```

## Usage

To get started, run the example code in ``?predictLogisticRegressionWithPenalization``. This function implements penalized multinomial logistic regression (algorithm 5 in the paper) and produces the kind of output you can get with this package. 

If you know the algorithm you want to use, look at the examples of the ``predict``-function for this algorithm (e.g., ``?predictXgboost``).

Additional algorithms not mentioned in the paper are described in my dissertation at https://madoc.bib.uni-mannheim.de/50617/ The example code in ``?selectMaxProbMethod`` is a good start to see these algorithms at work. This function implements the ``Maximum Probability Algorithm`` (algorithm 10 in the dissertation).

German users who wish to do coding into the German Classification of Occupation 2010 (KldB 2010) with a coding index should look at ``?prepare_German_coding_index_Gesamtberufsliste_der_BA`` and ``?predictWithCodingIndex``.

