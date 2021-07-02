install.packages('rsconnect')
library(rsconnect)

rsconnect::setAccountInfo(name='nskbiostatistics',
                          token='D7B8C94C605766DA39177FC1737F13FA',
                          secret='f8vO1dpNFHHN17QQKmPZwwrfvob9ji8MwzfhU80n')

rsconnect::terminateApp
rsconnect::deployApp('C:/Users/niksko/Desktop/mstate/msm_shiny')


setwd("C:/Users/niksko/Documents/shiny_online/")
