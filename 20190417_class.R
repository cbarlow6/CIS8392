install.packages("googleCloudStorageR") 
install.packages("googleLanguageR") 
install.packages("devtools")
install.packages("bigQueryR", repos = c(getOption("repos"),
                                        "http://cloudyr.github.io/drat")) 
devtools::install_github("cloudyr/RoogleVision")
devtools::install_github("cloudyr/googleComputeEngineR")

library(tidyverse) 
library(bigQueryR) 
library(googleCloudStorageR) 
library(googleLanguageR) 
library(RoogleVision) 
library(googleComputeEngineR)

#install.packages("googleCloudStorageR")
library(googleCloudStorageR)
Sys.setenv("GCS_AUTH_FILE" = "gcp-service-account-key.json") 
options("googleAuthR.scopes.selected" =
          "https://www.googleapis.com/auth/cloud-platform") 

gcs_auth()

proj <- "catresa-project"
buckets <- gcs_list_buckets(proj) 
buckets

