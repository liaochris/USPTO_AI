# Import libraries
library(data.table)
library(readr)
library(dplyr)
library(haven)
library(ggfittext)
library(doMC)
library(future)
library(vroom)
library(RCurl)
library(haven)
library(ggplot2)
library(zoo)
library(collections)
library(glue)
library(stringr)
library(tidyr)
library(openxlsx)
library(parallel)
library(doParallel)

`%ni%` <- Negate(`%in%`)

setwd("~/Google Drive/Non-Academic Work/Research/Traina/Post-2022/USPTO_AI/")

ai_data <- fread("data/ai_model_predictions.tsv")
ai_data_filt <- ai_data[predict50_any_ai == 1]
fwrite(ai_data_filt, "ai_model_predictions_filt.csv")

ai_patent_app_ids <- ai_data_filt[flag_patent == 1, appl_id]
length(ai_patent_app_ids)

app_data <- fread("data/application.tsv")
app_data_filt <- app_data[number %in% ai_patent_app_ids]

dim(app_data_filt)
