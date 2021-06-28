install.packages("pacman")
# this command checks if you have the packages already installed, 
# then installs the missing packages, then loads the libraries
pacman::p_load(knitr, rmarkdown, devtools, roxygen2, usethis) 

# identify yourself to Git with the usethis package
# use the exact same username and email associated
# with your GitHub account
usethis::use_git_config(user.name = "nskourlis", user.email = "nikolaos.skourlis@ki.se")

# creates description and namespace files
usethis::use_description()
usethis::use_namespace()

# Create R directory
base::dir.create("R")

# creates Package-level documentation so you can run ?nameofpackage
usethis::use_package_doc()

# created README.Rmd for Github landing page
# an .Rbuildignore file gets created
usethis::use_readme_rmd()

# creates license file
usethis::use_mit_license("Nikolaos Skourlis")

# creates news file
usethis::use_news_md()

# setup continuous integration via travis-ci
usethis::use_travis()

# sets up testing infrastructure
usethis::use_testthat()


?MSMplus
MSMplus::
  pacman::p_functions(MSMplus)

usethis::use_data_raw()


if (!require("pacman")) install.packages("pacman") 
library("pacman")
pacman::p_load(magrittr, dplyr, usethis, data.table, here)
pacman::p_load(here)


### An MSM exapmle ####
library("msm")
library("mstate")
library("tidyverse")
head(cav)
# clean data ----

#ebmt data
ebmt <-  read.csv("C:/Users/niksko/Desktop/mstate/jsonread/ebmt.csv",header=TRUE, sep=",")

# write data in correct format to data folder ----
usethis::use_data(ebmt, overwrite = TRUE)

# excel_input_file_ex
excel_input_file_ex <-  read.csv("C:/Users/niksko/Desktop/mstate4/datasets/csv/csv_present/excel_input_file_example.csv",header=TRUE, sep=",")

# write data in correct format to data folder ----
usethis::use_data(excel_input_file_ex, overwrite = TRUE)




#cav data
library(msm)
# write data in correct format to data folder ----
usethis::use_data(cav, overwrite = TRUE)


usethis::use_package("survival", type = "Imports")
usethis::use_package("mstate", type = "Imports")
usethis::use_package("msm", type = "Imports")
usethis::use_package("stringi", type = "Imports")
usethis::use_package("RJSONIO", type = "Imports")
usethis::use_package("visNetwork", type = "Imports")
usethis::use_package("shinyjs", type = "Imports")
usethis::use_package("shiny", type = "Imports")
usethis::use_package("mstate", type = "Imports")
usethis::use_package("tidyverse", type = "Imports")
usethis::use_package("tidyr", type = "Imports")
usethis::use_package("ggplot2", type = "Imports")
usethis::use_package("DiagrammeR", type = "Imports")
usethis::use_package("stringr", type = "Imports")
usethis::use_package("dplyr", type = "Imports")
usethis::use_package("gapminder", type = "Imports")
usethis::use_package("plyr", type = "Imports")
usethis::use_package("viridis", type = "Imports")
usethis::use_package("cowplot", type = "Imports")
usethis::use_package("magick", type = "Imports")
usethis::use_package("StatMeasures", type = "Imports")
usethis::use_package("processx", type = "Imports")
usethis::use_package("htmlwidgets", type = "Imports")
usethis::use_package("raster", type = "Imports")
usethis::use_package("jsonlite", type = "Imports")
usethis::use_package("devtools", type = "Imports")
usethis::use_package("usethis", type = "Imports")
usethis::use_package("githubinstall", type = "Imports")
usethis::use_package("shinyMatrix", type = "Imports")
usethis::use_package("dlm", type = "Imports")
usethis::use_package("rsvg", type = "Imports")
usethis::use_package("miniUI", type = "Imports")
usethis::use_package("htmltools", type = "Imports")
usethis::use_package("webshot", type = "Imports")
usethis::use_package("knitr", type = "Imports")
usethis::use_package("rmarkdown", type = "Imports")
usethis::use_package("reshape2", type = "Imports")

usethis::use_package("imager", type = "Imports")
usethis::use_package("stringr", type = "Imports")
usethis::use_package("usethis", type = "Imports")
usethis::use_package("plotly", type = "Imports")
usethis::use_package("gridExtra", type = "Imports")
usethis::use_package("DT", type = "Imports")



#if (!require("tidyverse")) install.packages("tidyverse")


usethis::use_package("tidyverse", type = "Imports")


usethis::use_vignette(name = "MSMplus_application_input1")



MSMplus::runMSMplus()



devtools::check()


usethis::use_appveyor()


install.packages("Rtools")

library("Rtools")

install.packages("visNetwork")
install.packages("shinyjs")
install.packages("DiagrammeR")
install.packages("plyr")
install.packages("gapminder")
install.packages("cowplot")
install.packages("magick")
install.packages("StatMeasures")
install.packages("webshot")
install.packages("raster")
install.packages("shinyMatrix")
install.packages("dlm")
install.packages("rsvg")
install.packages("RJSONIO")


devtools::check()

library("devtools")

###This may take a while
install_github("nskourlis/MSMplus")

library(MSMplus)
browseVignettes("MSMplus")


?ebmt
?cav
?MSMplus::msboxes_R
?MSMplus::msmjson
?MSMplus::mstatejson
?MSMplus::flexjson
MSMplus::runMSMplus()


