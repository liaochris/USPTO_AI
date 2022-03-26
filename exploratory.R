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

ai_inventors <- fread("merged/ai_inventors.csv")
ai_patents <- fread("merged/ai_patents.csv")
ai_categories <- fread("cleaned/ai_cpc.csv")

##########################################################################################
##################### Histogram of Patent Publication and Application ####################
##########################################################################################
# graph of the patent count by publication year
ai_patents[,.N, by = pub_y] %>% 
  ggplot(aes(x = pub_y, y = N)) +
  geom_bar(stat = "identity") + 
  geom_line()
# kinks 1988/1989, 1996/1997, 2005/2006, 2011/2012/2013, 2018/2019

# rename columns, get application filing year
# produce graph of the patent count by publication year
pat_y <- ai_patents[, .N, by = pub_y]
pat_y[, type := "patents"]
setnames(pat_y, "pub_y", "year")
app_y <- ai_patents[, .N, by = app_y]
app_y[, type := "applications"]
setnames(app_y, "app_y", "year")
rbind(app_y, pat_y) %>% 
  ggplot(aes(x = year, y = N, color = type, group = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_line()

# looking at the graph it seems like there isn't any kink 
# with the applications, only the patents 
# maybe a hiring thing, able to process patents faster


#remove unneeded columns
ai_patents_clean <- ai_patents[,-c("rawlocation_id", "location_id")]

pred_cols <- colnames(ai_patents_clean)[grepl("predict50", colnames(ai_patents_clean))]
pred_cols <- pred_cols[pred_cols != "predict50_any_ai"]

ai_cat_sum <- ai_patents_clean[, lapply(.SD,sum), by=pub_y, .SDcols = pred_cols]
colnames(ai_cat_sum) <- gsub("predict50", "sum", colnames(ai_cat_sum))

#growth in different AI categories over time
melt(ai_cat_sum, id.vars = c("pub_y"), 
     measure.vars = colnames(ai_cat_sum)[grepl("sum", colnames(ai_cat_sum))]) %>%
  ggplot(aes(x = pub_y, y = value, color = variable, group = variable)) +
  geom_line()

ai_patents_clean[,.(mean_cit = mean(citations,na.rm = TRUE)), by = pub_y][order(pub_y)] %>%
  ggplot(aes(x = pub_y, y = mean_cit)) +
  geom_line()

# patent data summary statistics
# mean, stdev, min, max, median
# application year, patent year
# observations

# academic, private and government institutions
# us vs intl firms
# different subfields of inventions - application sectors



