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

