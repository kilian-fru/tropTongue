
# tropTongue

<!-- badges: start -->
<!-- badges: end -->

The goal of tropTongue is to make allometric equations for tropical bees
accessible. Using the tropicalTongue-function the proboscis length of
tropical bees can be estimated. For Meliponini (Apidae), Euglossini
(Apidae) and Augochlorini (Halictidae) the function uses genus- or
tribe-specific equations. For all other bees the function uses the
function ITtongue from the BeeIT package to estimate the proboscis
length.

## Installation

You can install the development version of tropTongue from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("kilian-fru/tropTongue")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tropTongue)
## tropicalTongue("Apidae", "Meliponini", "Trigona", 0.7)
```
