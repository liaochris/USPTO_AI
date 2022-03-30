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
ai_patents_clean[, `first_year`:= min(pub_y), by = `assignee_id`]
ai_patents_clean[,`Company Decade` := floor((pub_y - first_year)/10)]
ai_patents_clean[, `early_stage_pat`:= pub_y <= `first_year`+5]

jpeg("figures/competition/pat_entr.jpeg", width = 800, height = 800)
entrance_p <- ai_patents_clean[, .(mean_pat = mean(early_stage_pat)), by = pub_y] %>%
  ggplot(aes(x = pub_y, y = mean_pat)) +
  geom_line() + 
  ggtitle("Prop Published by Institutions Entering <= 5 years ago") +
  labs(y= "Prop of AI Patents Published by New Institutions", 
       x = "Publication Year") +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  guides(size = "none")
entrance_p
dev.off()

# proportion published by company age
jpeg("figures/competition/pat_prop_compage.jpeg", width = 800, height = 800)
ai_pat_stages <- ai_patents_clean[, .N, by = c("pub_y", "`Company Decade`")]
ai_pat_stages[,mean_pat := N/sum(N), by = "pub_y"]
ai_pat_stages[, `Company Decade` := as.factor(`Company Decade`)]
many_stage <- ai_pat_stages %>%
  ggplot(aes(x = pub_y, y = mean_pat, color = `Company Decade`, group = `Company Decade`)) +
  geom_line() + 
  ggtitle("Prop Published by Institutions In their N-th Decade") +
  labs(y= "Prop of AI Patents Published", 
       x = "Publication Year") +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  guides(size = "none")
many_stage
dev.off()

# patent-assignee ratio by age of company
jpeg("figures/competition/pat_assgn_ratio_compage.jpeg", width = 800, height = 800)
ai_stage_means <- ai_patents_clean[, .N, by = c("pub_y", "`Company Decade`", "assignee_id")]
ai_stage_means_uq <- unique(ai_stage_means[,mean_N := mean(N), by = c("pub_y", "`Company Decade`")][,c("pub_y","`Company Decade`","mean_N")])
ai_stage_means_uq[, `Company Decade` := as.factor(`Company Decade`)]
mean_stage <- ai_stage_means_uq %>%
  ggplot(aes(x = pub_y, y = mean_N, color = `Company Decade`, group = `Company Decade`)) +
  geom_line() + 
  ggtitle("Patent-Assignee Ratio Dependent on Decade") +
  labs(y= "Patent-Assignee Ratio", 
       x = "Publication Year") +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  guides(size = "none")
mean_stage
dev.off()


# By AI Type
ai_new_g <- foreach(i = ai_cols, .combine = 'rbind') %do% {
  ai_new <- ai_patents_clean[get(i) == 1, .(mean_pat = mean(early_stage_pat)), by = pub_y] 
  ai_new$type <- gsub("predict50_", "", i)
  ai_new[, linesize := type == "any_ai"]
  ai_new
}
jpeg("figures/competition/pat_entr_cat.jpeg", width = 800, height = 800)
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
entrance_p_g
dev.off()

# number of new publishers each year
jpeg("figures/competition/pat_entr_cnt.jpeg", width = 800, height = 800)
incoming_cnt <- unique(ai_patents_clean[,c("assignee_id", "first_year")])[, .N, by = first_year] %>%
  ggplot(aes(x = first_year, y = N)) +
  geom_line() + 
  ggtitle("New AI Patent-Publishing Institutions") +
  labs(y= "Number of Instituions First Publishing an AI Patent", 
       x = "Publication Year") +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  guides(size = "none")
incoming_cnt
dev.off()

# By AI Type
incoming_cnt_g <- foreach(i = ai_cols, .combine = 'rbind') %do% {
  incoming_cnts <- unique(ai_patents_clean[get(i) == 1,
                                           c("assignee_id", "first_year")])[, .N, by = first_year]
  incoming_cnts$type <- gsub("predict50_", "", i)
  incoming_cnts[, linesize := type == "any_ai"]
  incoming_cnts
}

jpeg("figures/competition/pat_entr_cnt_cat.jpeg", width = 800, height = 800)
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
incoming_cnt_p
dev.off()

# analyze competition
# Which fields are new companies entering (ai and patent categories) 
#      Do we see established ai companies expanding into new categories, or companies from other
#      Categories expanding into AI?
# What do the claims about a patent mean? Do new companies have more/less claims?
# Where are new companies vs. old companies located?
# What can we learn about citations relating to patents from new/old companies?

########################################################################################## 
################################## Complements Analysis ##################################
##########################################################################################
# analyze complements
# Which subfields of AI are complements? As in which fields to inventions tend to overlap in?
# Which subfields of AI do AI patents cite (another way to measure complements - reliance)
# What can we learn about the patenting scene based off complements? 
#      Do we see companies expanding into complement areas or new companeis filling in these gaps
#      Relates to competition...

# Which patent categories are complements with AI - similar questions as bove

########################################################################################## 
##################################### Other Analysis #####################################
##########################################################################################
# patent data summary statistics - mean, stdev, min, max, median
# application year, patent year
# observations + proportion in each category
# academic, private and government institutions
# us vs intl firms
# different subfields of inventions - categories
