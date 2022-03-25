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

# home directory
#setwd("~/Google Drive/Non-Academic Work/Research/Traina/Post-2022/USPTO_AI/")
# RCC directory
#setwd("~/scratch-midway2/USPTO_AI")

ai_data <- fread("data/ai_model_predictions.tsv")
ai_data_filt <- ai_data[predict50_any_ai == 1]

app_data <- fread("data/application.tsv")
ai_app_ids <- ai_data_filt[flag_patent == 1, appl_id]
app_data_filt <- app_data[number %in% ai_app_ids]

patent_data <- fread("data/patent.tsv")
ai_patent_ids <- ai_data_filt[flag_patent == 1, doc_id]
patent_data_filt <- patent_data[id %in% ai_patent_ids]

rawinventor_data <- fread("data/rawinventor.tsv")
rawinventor_data_filt <- rawinventor_data[patent_id %in% ai_patent_ids]
rawinventor_data_filt

rawassignee_data <- fread("data/rawassignee.tsv")
rawassignee_data_filt_p <- rawassignee_data[patent_id %in% ai_patent_ids]
ai_assignee_ids <- rawassignee_data_filt_p$assignee_id
rawassignee_data_filt <- rawassignee_data[assignee_id %in% ai_assignee_ids]

rawlocation_data <- fread("data/rawlocation.tsv")
ai_rawlocation_ids <- unique(c(rawinventor_data_filt$rawlocation_id, 
                               rawassignee_data_filt$rawlocation_id))
rawlocation_data_filt <- rawlocation_data[id %in% ai_rawlocation_ids]

location_data <- fread("data/location.tsv")
ai_location_ids <- unique(rawlocation_data_filt$location_id)
location_data_filt <- location_data[id %in% ai_location_ids]

citation_data <- fread("data/uspatentcitation.tsv")
citation_data_filt <- citation_data[patent_id %in% ai_patent_ids]

fwrite(ai_data_filt, "cleaned/ai_predictions.csv")
fwrite(app_data_filt, "cleaned/ai_app.csv")
fwrite(citation_data_filt, "cleaned/ai_citation.csv")
fwrite(location_data_filt, "cleaned/ai_location.csv")
fwrite(patent_data_filt, "cleaned/ai_patent.csv")
fwrite(rawassignee_data_filt, "cleaned/ai_rawassignee.csv")
fwrite(rawassignee_data_filt_p, "cleaned/ai_rawassignee_p.csv")
fwrite(rawlocation_data_filt, "cleaned/ai_rawlocation.csv")
fwrite(rawinventor_data_filt, "cleaned/ai_rawinventor.csv")
