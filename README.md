
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MSMplus

<!-- badges: start -->

<!-- badges: end -->

MSMplus is a useful tool for presentation of results from a multi-state
analysis in an easy, comprehensible and meaningful way. It aids the user
to present research findings to targeted audiences.

However, the results need to be provided to the app either as a
csv/excel or a json file of specific structure.

The advisable approach is the manual creation of an excel/csv file with
the analysis results by the researcher according to certain formatting
and naming rules described in this
[tutorial](https://nskbiostatistics.shinyapps.io/supplementary/).

Here is an example data file structure:

``` r
#load("excel_input_file_ex.rda", envir = parent.frame(), verbose = FALSE)

#excel_input_file_ex[104:106,1:15]
```

We have created an alternative way for deriving the MSMplus input files
to avoid the labour. The json files can be easily derived while running
the multi-state models. In Stata, this is done via the commands msboxes
and predictms.

In R the input files can be generated via the current package and the
use of its funtions: flexjson, msmjson, mstatejson. That being said we
still advise for the manual approach as the researcher does not need to
have any knowledge of R or Stata, as the analysis can be conducted via
any statistical software.

The user can locally launch the MSMplus application by writting
MSMplus\_prepare::runMSMplus() or access it online at
<https://nskbiostatistics.shinyapps.io/MSMplus/>

## Installation

You can install the development version of MSMplus from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nskourlis/MSMplus")
```

## Example

This is a basic example which shows you how to launch MSMplus:

``` r
library(MSMplus)
#runMSMplus()
```

Read the vignette to see how to automatically produce the input files
for MSMplus
