# Import libraries
library(data.table)
library(readr)
library(dplyr)
library(haven)
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

ai_data_filt <- fread("cleaned/ai_predictions.csv")
app_data_filt <- fread("cleaned/ai_app.csv")
citation_data_filt <- fread("cleaned/ai_citation.csv")
location_data_filt <- fread("cleaned/ai_location.csv")
patent_data_filt <- fread("cleaned/ai_patent.csv")
rawassignee_data_filt <- fread("cleaned/ai_rawassignee.csv")
rawassignee_data_filt_p <- fread("cleaned/ai_rawassignee_p.csv")
rawlocation_data_filt <- fread("cleaned/ai_rawlocation.csv")
rawinventor_data_filt <- fread("cleaned/ai_rawinventor.csv")
ai_cpc <- fread("cleaned/ai_cpc.csv")
# fexcluding all non-patents
ai_data_patent <- ai_data_filt[flag_patent == 1]
# filtering columns
ai_patent_cols <- c("doc_id", "pub_dt", "appl_id",
                    colnames(ai_data_patent)[grepl("predict", colnames(ai_data_patent))])
ai_data_patent <- ai_data_patent[,.SD,.SDcols = ai_patent_cols]
setnames(ai_data_patent, "doc_id", "patent_id")
ai_data_patent[, pub_y := year(pub_dt)]


# merge with applications
app_patents <- merge.data.table(ai_data_patent, app_data_filt[,-c("series_code")], 
                                by.x = c("patent_id", "appl_id"), 
                                by.y = c("patent_id", "number"))

setnames(app_patents, "date", "app_date")
app_patents[ , app_y := year(app_date)]
app_patents <- app_patents[, -c("id")]

# calculate number of citations made by each patent
# merge with main table
patent_cites <- citation_data_filt[, .(citations = max(sequence) + 1), by = patent_id]
app_patent_cites <- merge.data.table(app_patents, patent_cites, by = c("patent_id"),
                                     all.x = TRUE)

# selec columns and merge detailed patent data with main table
remcols <- c("abstract", "number", "date", "filename", "withdrawn", "country")
selcols <- colnames(patent_data_filt)[colnames(patent_data_filt) %ni% remcols]
app_patent_cites_full <- merge.data.table(app_patent_cites,
                                          patent_data_filt[,.SD,.SDcols = selcols],
                                          by.x = c("patent_id"), by.y = c("id"))
setnames(app_patent_cites_full, c("kind", "type"), c("patent kind", "patent type"))

# merge with data on assignees
app_patent_cites_assgn <- merge.data.table(app_patent_cites_full, 
                                           rawassignee_data_filt_p[,-c("uuid")],
                                           by = c("patent_id"), all.x = TRUE)
# rename columns
setnames(app_patent_cites_assgn, c("type", "sequence", "name_first", "name_last"), 
         paste("assignee", c("type", "sequence", "name_first", "name_last")))

# calculate number of inventors, merge with main table
inventor_cnt <- rawinventor_data_filt[, .(num_inventor = .N), by = "patent_id"]
app_patent_cites_assgn_inv <- merge.data.table(app_patent_cites_assgn, inventor_cnt,
                                               by = c("patent_id"),
                                               all.x = TRUE)
# merge raw location data with location data
location_tbl <- merge.data.table(rawlocation_data_filt[,-c("city", "state", "country")], 
                                 location_data_filt,
                                 by.x = c("location_id"), 
                                 by.y = c("id"))
setnames(location_tbl, c("id", "country"), c("rawlocation_id", "assignee country"))
setnames(app_patent_cites_assgn_inv, "country", "patent country")

# merge patent and inventor data with location data
ai_patent_full <- merge.data.table(app_patent_cites_assgn_inv, location_tbl,
                                   by = c("rawlocation_id"))
ai_inventor_full <- merge.data.table(rawinventor_data_filt, location_tbl,
                                     by = c("rawlocation_id"))

# export data
fwrite(ai_patent_full, "merged/ai_patents.csv")
fwrite(ai_inventor_full, "merged/ai_inventors.csv")
                                   

