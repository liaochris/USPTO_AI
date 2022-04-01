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

#table for the top 20 companies - measured by total patents over all time
top_counts <- ai_patents_clean[, .N, by = "assignee_id"][order(-N)][1:20]
entry_year <- ai_patents_clean[`assignee_id` %in% top_counts$assignee_id,
                               .("Entry Year" = min(pub_y)), by = "assignee_id"]
top20_orgs <- merge.data.table(entry_year, top_counts)[order(-N)]
colnames(top20_orgs)[c(1,3)] <- c("Assignee ID", "Number of Patents")
top20_orgs[,`Entry Year` := as.character(`Entry Year`)]
id_name <- unique(ai_patents_clean[,c("assignee_id", "organization_lower")])
id_name <- id_name[!duplicated(id_name, by = c("assignee_id"))]
top20_orgs_complete <- merge.data.table(top20_orgs, id_name, by.x = c("Assignee ID"), by.y = c("assignee_id"))
top20_orgs_complete <- top20_orgs_complete[order(-`Number of Patents`)]
stargazer(top20_orgs_complete, summary = FALSE, out = "figures/companies/top20_stats.txt", type = "text")

########################################################################################## 
################################## Competition Analysis ##################################
########################################################################################## 
pred_cols <- colnames(ai_patents_clean)[grepl("predict50", colnames(ai_patents_clean))]
pred_cols <- pred_cols[pred_cols != "predict50_any_ai"]
ai_cols <- colnames(ai_patents_clean)[grepl("predict50", colnames(ai_patents_clean))]
# distribution of top 20 companies by ai patent type
ai_share20_g <- foreach(i = ai_cols, .combine = 'rbind') %do% {
  ai_share <- ai_patents_clean[get(i)==1,.(mean_top_20 = mean(assignee_id
                                                              %in% top_counts$assignee_id)),
                               by = "pub_y"]
  ai_share$type <- gsub("predict50_", "", i)
  ai_share[, linesize := type == "any_ai"]
  ai_share
}
jpeg("figures/companies/prop_assgn_t20_cat.jpeg", width = 500, height = 500)
ai_share20_g %>%
  ggplot(aes(x = pub_y, y = mean_top_20, color = type, group = type)) +
  geom_line(aes(size = linesize)) +
  ggtitle("Proportion of AI Patents Published by Top 20 Publishers") +
  labs(y= "Proportion of AI Patents Published", x = "Publication Year", color = "AI Type") +
  scale_colour_manual(values = rev(custom_cols_9)) +
  scale_size_manual(values = c("TRUE" = 1, "FALSE" = .4)) +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  guides(size = "none")
dev.off()
ai_share20_g[type == "any_ai" & pub_y >= 2008][order(pub_y)]
ai_patents_clean[pub_y %in% c(2009, 2020),.N, by = "pub_y"]
#generate annual stats
assgn_growth <- ai_patents_clean[assignee_id %in% top_counts$assignee_id & pub_y >= 2009, 
                                 .N, by = c("pub_y", "assignee_id")]
wided <- dcast(assgn_growth, assignee_id ~ pub_y, value.var = "N")
wided[, indiv_growth := 100*(1 + (`2020`-`2009`)/`2009`)]
count_09 <- ai_patents_clean[pub_y == 2009, .N]
count_20 <- ai_patents_clean[pub_y == 2020, .N]
wided[, all_growth := 100*(1 + (`count_20`-`count_09`)/`count_09`)]
wided_named <- merge.data.table(wided, top20_orgs_complete[,c("Assignee ID", "organization_lower")], 
                                by.x = "assignee_id", by.y = "Assignee ID")
wided_named[, outpaced_ind := ifelse(indiv_growth >= all_growth, "Outpaced Industry-wide Growth",
                                     "Lagged Behind Industry-wide Growth")]
stargazer(wided_named[top20_orgs_complete$`Assignee ID`], 
          summary = FALSE, out = "figures/companies/top20_annualstats.txt", type = "text")

#turn to long
long_table <- melt(wided_named, id.vars = c("assignee_id", "organization_lower", "indiv_growth", "all_growth", "outpaced_ind"),
                   mmeasure.vars = 2009:2020,
                   variable.name = "pub_y", value.name = "N")
cum_growth <- long_table[,sum(N), by = c("outpaced_ind", "pub_y")]
cum_growth[,prop:=V1/sum(V1), by = "pub_y"] 

jpeg("figures/companies/prop_t20_growth_type.jpeg", width = 500, height = 500)
cum_growth  %>%
  ggplot(aes(x = pub_y, y = prop, group = outpaced_ind, fill = outpaced_ind)) +
  geom_area() + 
  ggtitle("Top 20 Company Patenting Share by Growth Type") +
  labs(y= "Proportion of Top 20 Patents Published", x = "Publication Year", fill = "Industry Growth Status") +
  scale_colour_manual(values = rev(custom_cols_9)) +
  scale_size_manual(values = c("TRUE" = 1, "FALSE" = .4)) +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  guides(size = "none")
dev.off()

# create graph relative to overall patent market
outpaced_ind_comps <- unique(long_table[outpaced_ind == "Outpaced Industry-wide Growth", 
                                        assignee_id])
unoutpaced_ind_comps <- unique(long_table[outpaced_ind == "Lagged Behind Industry-wide Growth", 
                                          assignee_id])
ai_patents_clean[,outpaced_ind:="Not Included"]
ai_patents_clean[assignee_id %in% unoutpaced_ind_comps ,outpaced_ind:="Lagged Behind"]
ai_patents_clean[assignee_id %in% outpaced_ind_comps, outpaced_ind:="Outpaced" ]
ai_patents_clean[, outpaced_ind := factor(outpaced_ind, levels = c("Not Included", "Outpaced", "Lagged Behind"))]

ai_share_g <- foreach(i = unique(ai_patents_clean$outpaced_ind), .combine = 'rbind') %do% {
  ai_share <- ai_patents_clean[,.(prop_market = mean(outpaced_ind == i)),
                               by = "pub_y"]
  ai_share$outpaced_ind <- i
  ai_share
}


jpeg("figures/companies/prop_t20_growth_type_market.jpeg", width = 500, height = 500)
ai_share_g %>%
  ggplot(aes(x = pub_y, y = prop_market, group = outpaced_ind, fill = outpaced_ind)) +
  geom_area() + 
  ggtitle("Proportion of the Overall Patenting Market by Growth Type") +
  labs(y= "Proportion of Patent Market", x = "Publication Year", fill = "Industry Growth Status") +
  scale_colour_manual(values = rev(custom_cols_9)) +
  scale_size_manual(values = c("TRUE" = 1, "FALSE" = .4)) +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  guides(size = "none")
dev.off()

#google amazon microsoft ibm
top4tech <- c("8aaa669d-3724-42c3-9ca7-afde78fea37d","ca3017d1-264d-47e0-b76f-f91e7cebd260",
              "cd43e2af-5c58-4750-94ea-7f92d9c54e27", "db09fa49-2d9d-4d49-bf93-5ae6cce77bd1")
top20_counts <- ai_patents_clean[, .N, by = "assignee_id"][order(-N)][1:20]$assignee_id
top50_counts <- ai_patents_clean[, .N, by = "assignee_id"][order(-N)][1:50]$assignee_id
top100_counts <- ai_patents_clean[, .N, by = "assignee_id"][order(-N)][1:100]$assignee_id

ai_patents_clean[,cat:="Everyone Else"]
ai_patents_clean[assignee_id %in% top100_counts,cat:="Top 100 Patenters"]
ai_patents_clean[assignee_id %in% top50_counts,cat:="Top 50 Patenters"]
ai_patents_clean[assignee_id %in% top20_counts,cat:="Top 20 Patenters"]
ai_patents_clean[assignee_id %in% top4tech,cat:="GOOGL/MSF/IBM/AMZN"]
ai_patents_clean[, cat := factor(cat, levels = c("Everyone Else", "Top 100 Patenters", 
                                                 "Top 50 Patenters", "Top 20 Patenters", "GOOGL/MSF/IBM/AMZN"))]

ai_share_cat <- foreach(i = unique(ai_patents_clean$cat), .combine = 'rbind') %do% {
  ai_share <- ai_patents_clean[,.(prop_market = mean(cat == i)),
                               by = "pub_y"]
  ai_share$cat <- i
  ai_share
}

jpeg("figures/companies/prop_market_comptypes.jpeg", width = 500, height = 500)
ai_share_cat %>%
  ggplot(aes(x = pub_y, y = prop_market, group = cat, fill = cat)) +
  geom_area() + 
  ggtitle("Proportion of the Market Occupied by Top Companies") +
  labs(y= "Proportion of Patent Market", x = "Publication Year", fill = "Company Category") +
  scale_colour_manual(values = rev(custom_cols_9)) +
  scale_size_manual(values = c("TRUE" = 1, "FALSE" = .4)) +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  guides(size = "none")
dev.off()

# mean patent fields in top 20 companies
jpeg("figures/companies/mean_fields_t20.jpeg", width = 500, height = 500)
ai_cats_t20 <- ai_patents_clean[assignee_id %in% top20_counts, lapply(.SD, function(x) any(x == 1)), 
                                .SDcols =  pred_cols, by = c("assignee_id", "pub_y")]
ai_cats_t20[,num_techs := rowSums(.SD),.SDcols = pred_cols]
ai_cats_t20[,.(mean_tech = mean(num_techs)), by = pub_y][order(pub_y)] %>%
  ggplot(aes(x = pub_y, y = mean_tech)) +
  geom_line() + 
  ggtitle("Mean Number of AI Fields Top 20 Companies Publish In") +
  labs(y= "PMean Number of AI Fields", x = "Publication Year") +
  scale_colour_manual(values = rev(custom_cols_9)) +
  scale_size_manual(values = c("TRUE" = 1, "FALSE" = .4)) +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  guides(size = "none")
dev.off()

# mean patent fields in non-top 20 companies
jpeg("figures/companies/mean_fields_not_t20.jpeg", width = 500, height = 500)
ai_cats_nt20 <- ai_patents_clean[assignee_id %ni% top20_counts, lapply(.SD, function(x) any(x == 1)), 
                                 .SDcols =  pred_cols, by = c("assignee_id", "pub_y")]
ai_cats_nt20[,num_techs := rowSums(.SD),.SDcols = pred_cols]
ai_cats_nt20[,.(mean_tech = mean(num_techs)), by = pub_y][order(pub_y)] %>%
  ggplot(aes(x = pub_y, y = mean_tech)) +
  geom_line() + 
  ggtitle("Mean Number of AI Fields None-Top 20 Companies Publish In") +
  labs(y= "Proportion of Patent Market", x = "Publication Year", fill = "Company Category") +
  scale_colour_manual(values = rev(custom_cols_9)) +
  scale_size_manual(values = c("TRUE" = 1, "FALSE" = .4)) +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  guides(size = "none")
dev.off()

top20_pattypes <- melt(ai_patents_clean[assignee_id %in% top20_counts, lapply(.SD, function(x) mean(x)),
                                        .SDcols =  pred_cols, by = "pub_y"],
                       id.vars = "pub_y", variable.name = "type", value.name = "prop_patents")
top20_pattypes[, type := gsub("predict50_", "", type)]

jpeg("figures/companies/t20_ai_type_dist.jpeg", width = 500, height = 500)
top20_pattypes %>% 
  ggplot(aes(x = pub_y, y = prop_patents, group = type, color = type)) +
  geom_line() + 
  ggtitle("Proportion of Top 20 Company Patents by AI Type") +
  labs(y= "Proportion by AI Type", x = "Publication Year", color = "AI Type") +
  scale_colour_manual(values = rev(custom_cols_9)) +
  scale_size_manual(values = c("TRUE" = 1, "FALSE" = .4)) +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  guides(size = "none")
dev.off()

ntop20_pattypes <- melt(ai_patents_clean[assignee_id %ni% top20_counts, lapply(.SD, function(x) mean(x)),
                                        .SDcols =  pred_cols, by = "pub_y"],
                       id.vars = "pub_y", variable.name = "type", value.name = "prop_patents")
ntop20_pattypes[, type := gsub("predict50_", "", type)]
jpeg("figures/companies/nt20_ai_type_dist.jpeg", width = 500, height = 500)
ntop20_pattypes %>% 
  ggplot(aes(x = pub_y, y = prop_patents, group = type, color = type)) +
  geom_line() + 
  ggtitle("Proportion of None-Top 20 Company Patents by AI Type") +
  labs(y= "Proportion of Patent Market", x = "Publication Year", color = "AI Type") +
  scale_colour_manual(values = rev(custom_cols_9)) +
  scale_size_manual(values = c("TRUE" = 1, "FALSE" = .4)) +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  guides(size = "none")
dev.off()

# most companies are small 
small_ai_comps <- ai_patents_clean[,.N, by = c("assignee_id", "pub_y")][N==1]
all_comp <- ai_patents_clean[,.(.N), by = c("pub_y", "assignee_id")][,.(.N, type = "all companies"), 
                                                                     by = "pub_y"]
jpeg("figures/companies/1patent_companies.jpeg", width = 500, height = 500)
rbind(all_comp, small_ai_comps[,.(.N, type = "1 patent company"), by = "pub_y"]) %>% 
  ggplot(aes(x = pub_y, y = N, color = type, group = type)) +
  geom_line() + 
  ggtitle("Number of Companies Publishing AI Patents") +
  labs(y= "Number of Companies", x = "Publication Year", color = "Company Type") +
  scale_colour_manual(values = rev(custom_cols_9)) +
  scale_size_manual(values = c("TRUE" = 1, "FALSE" = .4)) +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  guides(size = "none")
dev.off()

small_ai_comps$size <- "1 patent/year company"
ai_patents_clean_size <- merge.data.table(ai_patents_clean, small_ai_comps[,-"N"], by = c("assignee_id", "pub_y"),
                                          all.x = TRUE)
ai_patents_clean_size[is.na(size),size:="not 1 patent/year company"]
jpeg("figures/companies/onepatent_company_market.jpeg", width = 500, height = 500)
ai_patents_clean_size[,mean(size == "1 patent/year company"), by = "pub_y"] %>% 
  ggplot(aes(x = pub_y, y = V1)) +
  geom_line() + 
  ggtitle("Proportion of the Patent Market Occupied by 1-Patent Companies") +
  labs(y= "Proportion of Patent Market", x = "Publication Year") +
  scale_colour_manual(values = rev(custom_cols_9)) +
  scale_size_manual(values = c("TRUE" = 1, "FALSE" = .4)) +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  guides(size = "none")
dev.off()

#company patenting activity 
assgn_size <- ai_patents_clean[,.N, by = c("assignee_id", "pub_y")]
assgn_size[N == 1, size := "1"]
assgn_size[is.na(size) & N <= 10, size := "2-10"]
assgn_size[is.na(size) & N <= 50, size := "11-20"]
assgn_size[is.na(size) & N <= 100, size := "21-50"]
assgn_size[is.na(size), size := "50+"]
assgn_size[assignee_id %in% top20_counts, size := "top 20"]
assgn_size[, size := factor(size, levels = rev(c("top 20", "50+", "21-50", "11-20", "2-10", "1")))]
ai_patents_clean_size2 <- merge.data.table(ai_patents_clean, assgn_size, by = c("assignee_id", "pub_y"),
                                          all.x = TRUE)
assgn_size_all <- ai_patents_clean_size2[,.N, by = c("pub_y", "size")]
assgn_size_all[,prop:=N/sum(N), by = "pub_y"]
jpeg("figures/companies/company_market_patsize.jpeg", width = 500, height = 500)
assgn_size_all[pub_y >= 2000] %>% 
  ggplot(aes(x = pub_y, y = prop, fill = size, group = size)) +
  geom_area() + 
  ggtitle("Proportion of the Patent Market Occupied by Patenting Activity") +
  labs(y= "Proportion of Patent Market", x = "Publication Year", fill = "Annual Patent Activity") +
  scale_colour_manual(values = rev(custom_cols_9)) +
  scale_size_manual(values = c("TRUE" = 1, "FALSE" = .4)) +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  guides(size = "none")
dev.off()

long_small1 <- melt(ai_patents_clean_size[size == "1 patent/year company"][,lapply(.SD,mean),by = "pub_y", 
                                                                           .SDcols = pred_cols],
                    id.vars = "pub_y", variable.name = "type", value.name = "prop_patents")
long_small1[, type := gsub("predict50_", "", type)]

jpeg("figures/companies/onepatent_company_aitype.jpeg", width = 500, height = 500)
long_small1 %>% 
  ggplot(aes(x = pub_y, y = prop_patents, group = type, color = type)) +
  geom_line() + 
  ggtitle("Proportion of 1-Patent Company Patents by AI Type") +
  labs(y= "Proportion of Patent Market", x = "Publication Year", fill = "AI Type") +
  scale_colour_manual(values = rev(custom_cols_9)) +
  scale_size_manual(values = c("TRUE" = 1, "FALSE" = .4)) +
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  guides(size = "none")
dev.off()

