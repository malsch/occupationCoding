## Overview

This is an R package for occupation coding. It accompanies the following publication:

Don't expect that this package will solve your problem. You will probably need to do some programming to make this package suit your application.

## Installation

The easiest way to install this package.
``` r
install.packages("devtools")
devtools::install_github("malsch/occupationCoding")
```

## Usage

To get started, run the example code in ``?selectMaxProbMethod``. This function implements the ``Maximum Probability Algorithm`` (algorithm 10 in the paper) and produces the kind of output you can get with this package. 

If you know the algorithm you want to use, look at the examples of the ``predict``-function for this algorithm (e.g., ``?predictXgboost'').

German users who wish to do coding into the German Classification of Occupation 2010 (KldB 2010) with a coding index should look at ``?prepare_German_coding_index_Gesamtberufsliste_der_BA`` and ``?predictWithCodingIndex``.

