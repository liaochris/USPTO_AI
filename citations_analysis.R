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
ai_citations <- fread("cleaned/ai_citation.csv")
ai_citations <- ai_citations[str_length(date)!=0]
#ai_citations <- ai_citations[sample(nrow(ai_citations), 1000000)]

ai_patents <- fread("merged/ai_patents.csv")
ai_predictions <- fread("cleaned/ai_predictions.csv")[flag_patent == 1]
#ai_categories <- fread("data/cpc_current.tsv")
# clean data, define important variables
ai_patents_clean <- ai_patents[,-c("rawlocation_id", "location_id")]
ai_cols <- colnames(ai_patents_clean)[grepl("predict50",colnames(ai_patents_clean))]
pred_cols <- ai_cols[ai_cols != "predict50_any_ai"]
ai_patents_clean[,`organization_lower` := tolower(organization)]
ai_predictions$pub_y <- year(ai_predictions$pub_dt)
# generate custom color palettes for graphs
custom_cols_8 <- brewer.pal(n = 8, name = "Dark2")
custom_cols_8[length(custom_cols_8)] <- "#000000"
custom_cols_9 <- c("#FF0000", custom_cols_8)

########################################################################################## 
################################### Citations Analysis ###################################
########################################################################################## 
ai_cols <- colnames(ai_patents_clean)[grepl("predict50",colnames(ai_patents_clean))]
# function to calculate mean of a statistic by year and patent type 
mean_patent_type_stats <- function(col, graphtype) {
  stat_name <- paste("mean", col, sep = "_")
  # mean number of statistic made by ai patents over time
  # mean statistic for all ai patents, by category
  foreach(i = ai_cols, .combine = 'rbind') %do% {
    mean_statistics <- ai_patents_clean[get(i) == 1, .(stat_name = mean(get(col), na.rm = TRUE)), 
                                        by = pub_y]
    mean_statistics[, type := gsub("predict50_", "", i)]
    mean_statistics[, linesize := type == "any_ai"]
    mean_statistics
  }[pub_y >= 1990]  %>%
    ggplot(aes(x = pub_y, y = stat_name, color = type, group = type)) +
    geom_line(aes(size = linesize)) +
    ggtitle(paste("Mean Number of", graphtype ,"per AI Patent by Category and Year")) +
    labs(y= paste("Mean Number of", graphtype ,"per AI Patent"), x = "Publication Year", color = "AI Type") +
    scale_colour_manual(values = rev(custom_cols_9)) +
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = .4)) +
    theme(legend.title = element_text(colour="black", size=10, face="bold"),
          plot.title = element_text(hjust = 0.5, size = 15)) +
    guides(size = "none")
}
jpeg("figures/citations/mean_cit_cat.jpeg", width = 300, height = 300)
mean_patent_type_stats("citations", "Citations")
dev.off()

# creating custom name columns
setnames(ai_predictions, "doc_id", "patent_id")
patent_attr <- c("patent_id", ai_cols)
pat_type <- ai_predictions[,.SD, .SDcols = patent_attr]
colnames(pat_type)[-1] <- unlist(lapply(paste("patent", colnames(pat_type)[-1], sep = "_"),
                                        function(x) gsub("predict50_", "", x)))
cit_type <- ai_predictions[,.SD, .SDcols = patent_attr]
colnames(cit_type) <- c("citation_id", unlist(lapply(paste("citation", colnames(pat_type)[-1], sep = "_"),
                                                     function(x) gsub("patent_", "", x))))
# merging with patent and citation data ai types
ai_citations_full <- merge.data.table(merge.data.table(ai_citations, pat_type, 
                                                       by = "patent_id"), 
                                      cit_type, by = c("citation_id"), all.x = TRUE)
setnames(ai_citations_full, "date", "citation_date")

ai_citations_full <- merge.data.table(ai_citations_full, ai_predictions[,c("patent_id", "pub_y")],
                                      by = "patent_id")
ai_citations_full[grepl("-00", citation_date),citation_date:=gsub("-00","-01",citation_date)]
ai_citations_full[,cit_y:=str_sub(citation_date,1,4)]

# Proportion of citations that are AI
mean_ai_g <- foreach(i = ai_cols, .combine = 'rbind') %do% {
  patent_col <- paste("patent", gsub("predict50_", "", i), sep = "_")
  citation_col <- paste("citation", gsub("predict50_", "", i), sep = "_")
  mean_ai <- ai_citations_full[get(patent_col) == 1, 
                               .(mean_nonai_cit = mean(is.na(get(citation_col)))), by = "pub_y"]
  mean_ai$type <- gsub("predict50_", "", i)
  mean_ai[, linesize := type == "any_ai"]
  mean_ai
}
# Graph of the proportion of citations that are AI
jpeg("figures/citations/mean_cit_ai.jpeg", width = 300, height = 300)
mean_ai_g %>%
  ggplot(aes(x = pub_y, y = mean_nonai_cit, color = type, group = type)) +
  geom_line(aes(size = linesize)) +
  ggtitle(paste("Proportion of Citations that are AI Patents Over Time")) +
  labs(y= paste("Proportion of AI Citations"), x = "Publication Year", color = "AI Type") +
  scale_colour_manual(values = rev(custom_cols_9)) +
  scale_size_manual(values = c("TRUE" = 1, "FALSE" = .4)) +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  guides(size = "none")
dev.off()

#citation column names
citation_collist <- colnames(ai_citations_full)[unlist(lapply(colnames(ai_citations_full), 
                                                              function (x) grepl("citation_",x) & 
                                                                x != "citation_id" & x != "citation_date"))]
# table of distribution of ai patent type citations
mean_cit_g <- foreach(i = ai_cols, .combine = 'rbind') %do% {
  patent_col <- paste("patent", gsub("predict50_", "", i), sep = "_")
  mean_cit <- ai_citations_full[get(patent_col) == 1 & !is.na(citation_any_ai), 
                    lapply(.SD, mean), .SDcols = citation_collist]
  mean_cit$type <- gsub("predict50_", "", i)
  mean_cit
}
stargazer(mean_cit_g, summary = FALSE, out = "figures/citations/ai_citation_type.txt", type = "text")

ai_citations_full[,pre2001 := pub_y <= 2001]
mean_cit_g_yrs <- foreach(i = ai_cols, .combine = 'rbind') %do% {
  patent_col <- paste("patent", gsub("predict50_", "", i), sep = "_")
  mean_cit <- ai_citations_full[get(patent_col) == 1 & !is.na(citation_any_ai), 
                                lapply(.SD, mean), .SDcols = citation_collist, by = "pre2001"]
  mean_cit$type <- gsub("predict50_", "", i)
  mean_cit
}
stargazer(mean_cit_g_yrs, summary = FALSE, out = "figures/citations/ai_citation_type_split.txt", type = "text")

ai_citations_full[,cit_y:=as.numeric(cit_y)]

# mean time difference from citations
mean_cit_dif_g <- foreach(i = ai_cols, .combine = 'rbind') %do% {
  patent_col <- paste("patent", gsub("predict50_", "", i), sep = "_")
  mean_cit_dif <- ai_citations_full[get(patent_col) == 1,.(pub_y - mean(cit_y)), 
                                    by = c("patent_id", "pub_y")][,.(mean_time_dif = mean(V1)), by = "pub_y"]
  mean_cit_dif$type <- gsub("predict50_", "", i)
  mean_cit_dif[, linesize := type == "any_ai"]
  mean_cit_dif
}
jpeg("figures/citations/mean_cit_age.jpeg", width = 300, height = 300)
mean_cit_dif_g[pub_y >= 1990] %>%
  ggplot(aes(x = pub_y, y = mean_time_dif, color = type, group = type)) +
  geom_line(aes(size = linesize)) +
  ggtitle(paste("Mean Age of Citations by Year")) +
  labs(y= paste("Mean Age"), x = "Publication Year", color = "AI Type") +
  scale_colour_manual(values = rev(custom_cols_9)) +
  scale_size_manual(values = c("TRUE" = 1, "FALSE" = .4)) +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  guides(size = "none")
dev.off()

# mean year of citations for patents
mean_cit_age_g <- foreach(i = ai_cols, .combine = 'rbind') %do% {
  patent_col <- paste("patent", gsub("predict50_", "", i), sep = "_")
  mean_cit_dif <- ai_citations_full[get(patent_col) == 1, mean(cit_y), 
                                    by = c("pub_y", "patent_id")][,.(mean_cit_y = mean(V1)), by = "pub_y"]
  mean_cit_dif$type <- gsub("predict50_", "", i)
  mean_cit_dif[, linesize := type == "any_ai"]
  mean_cit_dif
}

jpeg("figures/citations/mean_cit_yr.jpeg", width = 300, height = 300)
mean_cit_age_g[pub_y >= 1990] %>%
  ggplot(aes(x = pub_y, y = mean_cit_y, color = type, group = type)) +
  geom_line(aes(size = linesize)) + 
  geom_abline(slope = 1, intercept = -10) +
  ggtitle(paste("Mean Year of Citations by Year")) +
  labs(y= paste("Mean Year"), x = "Publication Year", color = "AI Type") +
  scale_colour_manual(values = rev(custom_cols_9)) +
  scale_size_manual(values = c("TRUE" = 1, "FALSE" = .4)) +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  guides(size = "none")
dev.off()
