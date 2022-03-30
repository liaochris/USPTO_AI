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

jpeg("figures/exploratory/sum_app_pat.jpeg", width = 800, height = 800)
p_app_pat <- rbind(app_y, pat_y) %>% 
  ggplot(aes(x = year, y = N, color = type, group = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_line() + 
  ggtitle("AI Patent (and Associated Application) Count by Year") +
  labs(y= "Annual Count", x = "Publication Year", color = "Document Type") +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15)) 
p_app_pat
dev.off()
# looking at the graph it seems like there isn't any kink 
# with the applications, only the patents 
# maybe a hiring thing, able to process patents faster

##########################################################################################
############## AI Patent Type Category-Level Analysis - Claims and Citations ############# 
##########################################################################################
#remove unneeded columns
ai_patents_clean <- ai_patents[,-c("rawlocation_id", "location_id")]
pred_cols <- colnames(ai_patents_clean)[grepl("predict50", colnames(ai_patents_clean))]
pred_cols <- pred_cols[pred_cols != "predict50_any_ai"]
ai_cat_sum <- ai_patents_clean[, lapply(.SD,sum), by=pub_y, .SDcols = pred_cols]
colnames(ai_cat_sum) <- gsub("predict50", "sum", colnames(ai_cat_sum))

# number of patents in different AI categories over time
sum_cat <- melt(ai_cat_sum, id.vars = c("pub_y"), 
                measure.vars = colnames(ai_cat_sum)[grepl("sum", colnames(ai_cat_sum))])
sum_cat[, variable:= unlist(lapply(variable, function (x) gsub("sum_", "", x)))]

# custom color palette
custom_cols_8 <- brewer.pal(n = 8, name = "Dark2")
custom_cols_8[length(custom_cols_8)] <- "#000000"
custom_cols_9 <- c("#FF0000", custom_cols_8)

jpeg("figures/exploratory/sum_cat.jpeg", width = 800, height = 800)
p_sum_cat <- sum_cat %>%
  ggplot(aes(x = pub_y, y = value, color = variable, group = variable)) +
  geom_line() + 
  #xlim(1990, 2020) +
  ggtitle("Number of AI Patents Published") +
  labs(y= "AI Patents Published Each Year", x = "Publication Year", color = "AI Type") +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  scale_colour_manual(values = custom_cols_8)
p_sum_cat
dev.off()

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
jpeg("figures/exploratory/mean_cit_cat.jpeg", width = 800, height = 800)
mean_patent_type_stats("citations", "Citations")
dev.off()

# mean number of citations made by ai patents over time - overall
ai_patents_clean[,.(mean_citation = mean(citations,na.rm = TRUE)), by = pub_y][order(pub_y)] %>%
  ggplot(aes(x = pub_y, y = mean_citation)) +
  geom_line() + geom_bar(stat = "identity")

jpeg("figures/exploratory/mean_claims_cat.jpeg", width = 800, height = 800)
mean_patent_type_stats("num_claims", "Claims")
dev.off()

# mean number of claims made by ai patents over time - overall
ai_patents_clean[,.(mean_claim = mean(num_claims,na.rm = TRUE)), by = pub_y][order(pub_y)] %>%
  ggplot(aes(x = pub_y, y = mean_claim)) +
  geom_line() + geom_bar(stat = "identity")

##########################################################################################
########## AI Patent Type Category-Level Analysis - Assignees and Inventors ############## 
##########################################################################################

# distribution of assignee types 2 and 3 - 2 is domestic, 3 is intl
# prop US decreases around 1990 and then increases and remains stable 
ai_patents_clean[, `assignee type` := as.factor(`assignee type`)]
ai_assignee_type <- ai_patents_clean[,.N, by = c("pub_y", "assignee type")]
ai_assignee_type[`assignee type` %in% c(2, 3), 
                 `assignee name`:= ifelse(`assignee type` == 2, 'US Company or Corporation', 
                                          'Foreign Company or Corporation')]

jpeg("figures/exploratory/sum_assgn.jpeg", width = 800, height = 800)
ai_assignee_type[`assignee type` %in% c(2, 3)] %>% 
  ggplot(aes(x = pub_y, y = N, color = `assignee name`, group = `assignee name`)) +
  geom_line() +
  ggtitle("Number of AI Patents Published by Assignee Type and Year") +
  labs(y= "Number of AI Patents Published", x = "Publication Year", color = "Assignee Type") +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15))
dev.off()

ai_assignee_type[, prop := prop.table(`N`), by = "pub_y"]
jpeg("figures/exploratory/prop_assgn.jpeg", width = 800, height = 800)
ai_assignee_type[`assignee type` %in% c(2, 3)]  %>%
  ggplot(aes(x = pub_y, y = prop, color = `assignee name`, group = `assignee name`)) +
  geom_line() +
  ggtitle("Proportion of AI Patents Published Grouped by Assignee Type and Year") +
  labs(y= "Proportion of AI Patents Published", x = "Publication Year", color = "Assignee Type") +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15))
dev.off()

# distribution of assignee types 2/3 by ai patent type 
# computer vision has relatively lower American
ai_assignee_type_g <- foreach(i = ai_cols, .combine = 'rbind') %do% {
  assgn_type <- ai_patents_clean[get(i) == 1,.N, by = c("pub_y", "assignee type")]
  assgn_type[, prop := prop.table(`N`), by = "pub_y"]
  assgn_type$type <- gsub("predict50_", "", i)
  assgn_type[, linesize := type == "any_ai"]
  assgn_type
}
jpeg("figures/exploratory/prop_assgn_cat.jpeg", width = 800, height = 800)
ai_assignee_type_g[`assignee type` == 2 & pub_y >= 1990] %>%
  ggplot(aes(x = pub_y, y = prop, color = `type`, group = `type`)) +
  geom_line(aes(size = linesize)) +
  ggtitle("Proportion of Patents Published by US Institutions by AI Type ") +
  labs(y= "Proportion of Patents Published by US Institutions", x = "Publication Year", color = "AI Type") +
  scale_colour_manual(values = rev(custom_cols_9)) +
  scale_size_manual(values = c("TRUE" = 1, "FALSE" = .4)) +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  guides(size = "none")
dev.off()

#reformatting names
#ai_patents_clean[, organization_lower := tolower(organization)]
top20_counts <- ai_patents_clean[, .N, by = "assignee_id"][order(-N)][1:20]
top20_counts
ai_patents_clean[,top_20:=assignee_id %in% top20_counts$assignee_id]

# top n companies by parent count
shareoftop <- function(n) {
  top_counts <- ai_patents_clean[, .N, by = "assignee_id"][order(-N)][1:n]
  ai_patents_clean_cop <- ai_patents_clean
  ai_patents_clean_cop[,top_n:=assignee_id %in% top_counts$assignee_id]
  ai_patents_clean_cop[, .(mean_top_n = mean(top_n)), by = "pub_y"] %>%
    ggplot(aes(x = pub_y, y = mean_top_n)) +
    geom_line() +
    ggtitle(paste("Proportion of AI Patents Published by Top",n, "AI Patent Publishers")) +
    labs(y= paste("Proportion Published by Top", n), x = "Publication Year", color = "AI Type") +
    theme(legend.title = element_text(colour="black", size=10, face="bold"),
          plot.title = element_text(hjust = 0.5, size = 15))
}
# share of top 10,20,30 has decreased in recent years
#shareoftop(10)
jpeg("figures/exploratory/prop_assgn_t20.jpeg", width = 800, height = 800)
shareoftop(20)
dev.off()
#shareoftop(30)

# descriptive statistics of top 20 institutions
entry_year <- ai_patents_clean[`assignee_id` %in% top20_counts$assignee_id,
                               .("Entry Year" = min(pub_y)), by = "assignee_id"]
top20_orgs <- merge.data.table(entry_year, top20_counts)[order(-N)]
colnames(top20_orgs)[c(1,3)] <- c("Assignee ID", "Number of Patents")
top20_orgs[,`Entry Year` := as.character(`Entry Year`)]
id_name <- unique(ai_patents_clean[,c("assignee_id", "organization_lower")])
id_name <- id_name[!duplicated(id_name, by = c("assignee_id"))]
top20_orgs_complete <- merge.data.table(top20_orgs, id_name, by.x = c("Assignee ID"), by.y = c("assignee_id"))
top20_orgs_complete <- top20_orgs_complete[order(-`Number of Patents`)]
stargazer(top20_orgs_complete, summary = FALSE, out = "tables/top20_stats.txt", type = "text")

# distribution of top n companies by ai patent type
ai_share20_g <- foreach(i = ai_cols, .combine = 'rbind') %do% {
  top_counts <- ai_patents_clean[get(i) == 1, .N, by = "assignee_id"][order(-N)][1:20]
  ai_share <- ai_patents_clean[,.(mean_top_20 = mean(assignee_id
                                                     %in% top_counts$assignee_id)),
                               by = "pub_y"]
  ai_share$type <- gsub("predict50_", "", i)
  ai_share[, linesize := type == "any_ai"]
  ai_share
}
jpeg("figures/exploratory/prop_assgn_t20_cat.jpeg", width = 800, height = 800)
ai_share20_g %>%
  ggplot(aes(x = pub_y, y = mean_top_20, color = type, group = type)) +
  geom_line(aes(size = linesize)) +
  ggtitle("Proportion of AI Patents Published by Top 20 AI Patent Publishers by AI Type") +
  labs(y= "Proportion of Patents Published by Top 20", x = "Publication Year", color = "AI Type") +
  scale_colour_manual(values = rev(custom_cols_9)) +
  scale_size_manual(values = c("TRUE" = 1, "FALSE" = .4)) +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  guides(size = "none")
dev.off()

# mean number of inventors over time
m_inventor <- ai_patents_clean[, .(mean_inventor = mean(num_inventor, na.rm = TRUE)), 
                               by = c("pub_y")]
m_inventor$top_20 <- "ALL"
# large companies have more inventors
# as time has passed the number of inventors has increased
jpeg("figures/exploratory/mean_inv_t20.jpeg", width = 800, height = 800)
rbind(m_inventor,
      ai_patents_clean[, .(mean_inventor = mean(num_inventor, na.rm = TRUE)), 
                       by = c("top_20", "pub_y")]) %>%
  ggplot(aes(x = pub_y, y = mean_inventor, color = top_20, group = top_20)) +
  geom_line() +
  ggtitle("Mean Number of Inventors per AI Patent") +
  labs(y= "Mean Number of Inventors", x = "Publication Year", color = "Assignee Type") +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15))
dev.off()

# mean number of claims by company type
jpeg("figures/exploratory/mean_claim_t20.jpeg", width = 800, height = 800)
ai_patents_clean[,.(mean_claim = mean(num_claims,na.rm = TRUE)), by = c('pub_y', 'top_20')][order(pub_y)] %>%
  ggplot(aes(x = pub_y, y = mean_claim, color = top_20, group = top_20)) +
  geom_line() + 
  ggtitle("Mean Number of Claims per AI Patent by Top 20 Status") +
  labs(y= "Mean Number of Claims", x = "Publication Year", color = "Top 20 Company") +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15))
dev.off()

# mean number of inventors over time for patent types
m_inventor_g <- foreach(i = ai_cols, .combine = 'rbind') %do% {
  m_inventors <- 
    ai_patents_clean[get(i) == 1, .(mean_inventor = mean(num_inventor, na.rm = TRUE)), 
                     by = c("top_20", "pub_y")][order(top_20)][,.(top20_dif = 
                                                                    diff(mean_inventor)), 
                                                               by = pub_y]
  m_inventors$type <- gsub("predict50_", "", i)
  m_inventors[, linesize := type == "any_ai"]
  m_inventors
}
jpeg("figures/exploratory/meandif_inv_t20.jpeg", width = 800, height = 800)
m_inventor_g %>%
  ggplot(aes(x = pub_y, y = top20_dif, color = type, group = type)) +
  geom_line(aes(size = linesize)) +
  ggtitle("Difference between Mean Inventor Count for Top 20 Publishers by AI Type") +
  labs(y= "Difference between Mean Inventor Count for Top 20", x = "Publication Year", color = "AI Type") +
  scale_colour_manual(values = rev(custom_cols_9)) +
  scale_size_manual(values = c("TRUE" = 1, "FALSE" = .4)) +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  guides(size = "none") +
  geom_hline(yintercept = 0)
dev.off()

