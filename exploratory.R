# Import libraries
library(data.table)
library(readr)
library(dplyr)
library(haven)
library(doMC)
library(future)
library(vroom)
library(RCurl)
library(stargazer)
library(haven)
library(RColorBrewer)
library(ggplot2)
library(viridis) 
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

# import data
ai_inventors <- fread("merged/ai_inventors.csv")
ai_patents <- fread("merged/ai_patents.csv")
ai_categories <- fread("cleaned/ai_cpc.csv")
# clean data, define important variables
ai_patents_clean <- ai_patents[,-c("rawlocation_id", "location_id")]
ai_cols <- colnames(ai_patents_clean)[grepl("predict50",colnames(ai_patents_clean))]
ai_patents_clean[,`organization_lower` := tolower(organization)]
# generate custom color palettes for graphs
custom_cols_8 <- brewer.pal(n = 8, name = "Dark2")
custom_cols_8[length(custom_cols_8)] <- "#000000"
custom_cols_9 <- c("#FF0000", custom_cols_8)

########################################################################################## 
################################## Competition Analysis ##################################
########################################################################################## 
# prop published by new entrants (<5 years)
ai_patents_clean[, `first_year`:= min(pub_y), by = `organization_lower`]
ai_patents_clean[, `early_stage_pat`:= pub_y <= `first_year`+5]
entrance_p <- ai_patents_clean[, .(mean_pat = mean(early_stage_pat)), by = pub_y] %>%
  ggplot(aes(x = pub_y, y = mean_pat)) +
  geom_line() + 
  ggtitle("Prop Published by Institutions Entering <= 5 years ago") +
  labs(y= "Prop of AI Patents Published by New Institutions", 
       x = "Publication Year") +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  guides(size = "none")
ggsave("figures/competition/pat_entr.jpeg", entrance_p, width = 800, height = 800, units = "px")

# By AI Type
ai_new_g <- foreach(i = ai_cols, .combine = 'rbind') %do% {
  ai_new <- ai_patents_clean[get(i) == 1, .(mean_pat = mean(early_stage_pat)), by = pub_y] 
  ai_new$type <- gsub("predict50_", "", i)
  ai_new[, linesize := type == "any_ai"]
  ai_new
}
entrance_p_g <- ai_new_g %>%
  ggplot(aes(x = pub_y, y = mean_pat, color = type, group = type)) +
  geom_line(aes(size = linesize)) +
  ggtitle("Prop Published by Institutions Entering <= 5 years ago by AI Type") +
  labs(y= "Prop of AI Patents Published by New Institutions", 
       x = "Publication Year") +
  scale_colour_manual(values = rev(custom_cols_9)) +
  scale_size_manual(values = c("TRUE" = 1, "FALSE" = .4)) +
  theme(legend.title = element_text(colour="black", size=7, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  guides(size = "none")
ggsave("figures/competition/pat_entr_cat.jpeg", entrance_p_g, width = 800, height = 800, units = "px")


# number of new publishers each year
incoming_cnt <- unique(ai_patents_clean[,c("organization_lower", "first_year")])[, .N, by = first_year] %>%
  ggplot(aes(x = first_year, y = N)) +
  geom_line() + 
  ggtitle("New AI Patent-Publishing Institutions") +
  labs(y= "Number of Instituions First Publishing an AI Patent", 
       x = "Publication Year") +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  guides(size = "none")
ggsave("figures/competition/pat_entr_cnt.jpeg", incoming_cnt, width = 800, height = 800, units = "px")

# By AI Type
incoming_cnt_g <- foreach(i = ai_cols, .combine = 'rbind') %do% {
  incoming_cnts <- unique(ai_patents_clean[get(i) == 1,
                                           c("organization_lower", "first_year")])[, .N, by = first_year]
  incoming_cnts$type <- gsub("predict50_", "", i)
  incoming_cnts[, linesize := type == "any_ai"]
  incoming_cnts
}

incoming_cnt_p <- incoming_cnt_g %>%
  ggplot(aes(x = first_year, y = N, color = type, group = type)) +
  geom_line(aes(size = linesize)) +
  ggtitle("New AI Patent-Publishing Institutions by AI Type") +
  labs(y= "Number of Instituions First Publishing an AI Patent", 
       x = "Publication Year") +
  scale_colour_manual(values = rev(custom_cols_9)) +
  scale_size_manual(values = c("TRUE" = 1, "FALSE" = .4)) +
  theme(legend.title = element_text(colour="black", size=7, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  guides(size = "none")
ggsave("figures/competition/pat_entr_cnt_cat.jpeg", incoming_cnt_p, width = 800, height = 800, units = "px")

########################################################################################## 
################################## Complements Analysis ##################################
##########################################################################################
# complements - which subfields of AI are complements with each other - citations
# which fields is AI a complement with/substitute - citations + cat

# patent data summary statistics - mean, stdev, min, max, median
# application year, patent year
# observations + proportion in each category
# academic, private and government institutions
# us vs intl firms
# different subfields of inventions - categories
