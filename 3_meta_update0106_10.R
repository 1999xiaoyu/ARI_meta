library(readxl)
library(plyr)
library(dplyr)
library(tidyr)
library(metafor)
library(stringr)
rm(list = ls())
source("3_Rscript/functions.R")

# direction definition----

INPUT_PATH = "1_Input/"
OUTPUT_PATH = "2.3_Output/"

# data loading----

allData <- read_xlsx(paste0(INPUT_PATH,"ari_data_extraction_v4.xlsx"),sheet = 5)
indexData <- read_xlsx(paste0(INPUT_PATH,"ari_data_extraction_v4.xlsx"),sheet = 2)


# data cleaning----

cleanData <- get_clean_data(allData)
cleanData$author <- apply(cleanData, 1, get_author)

indexData$year_group <- cut(indexData$study_year,
                            breaks = c(-Inf, 2010, 2015, Inf),
                            labels = c("-2009", "2010-2014", "2015-"))

## delete COVID-19 and SARS researches
cleanData <- cleanData[-which(cleanData$ID %in% indexData$ID[which(indexData$condition %in% c("COVID-19", "SARS"))]),]

### delete data from Hong Kong
cleanData <- cleanData[-which(cleanData$ID == "27"),]

####delete 10-
cleanData <- cleanData[which(cleanData$ID %in% indexData$ID[which(indexData$potential_bias10 %in% c("High"))]),]

# summarize costs & LoSs table----

USD_summaryTables <- get_summary_table(cleanData,data_type = "USD")
LoS_summaryTables <- get_summary_table(cleanData,data_type = "LoS")

## output summary table
write.csv(USD_summaryTables$data_summary,file = paste0(OUTPUT_PATH,"USD_summary10.csv"),row.names = F)
write.csv(USD_summaryTables$nstudies_summary,file = paste0(OUTPUT_PATH,"USD_summary_nstudies10.csv"),row.names = F)
write.csv(LoS_summaryTables$data_summary,file = paste0(OUTPUT_PATH,"LoS_summary10.csv"),row.names = F)
write.csv(LoS_summaryTables$nstudies_summary,file = paste0(OUTPUT_PATH,"LoS_summary_nstudies10.csv"),row.names = F)


# subgroup summary table ----

## hospital grade
sub_hospital1 <- get_subgroup(cleanData, indexData, "hospital_grade", 1)
hosp1_summaryTables <- get_summary_table(sub_hospital1,data_type = "USD")
write.csv(hosp1_summaryTables$data_summary,file = paste0(OUTPUT_PATH,"hosp1_summary10.csv"),row.names = F)
write.csv(hosp1_summaryTables$nstudies_summary,file = paste0(OUTPUT_PATH,"hosp1_summary_nstudies10.csv"),row.names = F)

sub_hospital2 <- get_subgroup(cleanData, indexData, "hospital_grade", 2)
hosp2_summaryTables <- get_summary_table(sub_hospital2,data_type = "USD")
write.csv(hosp2_summaryTables$data_summary,file = paste0(OUTPUT_PATH,"hosp2_summary10.csv"),row.names = F)
write.csv(hosp2_summaryTables$nstudies_summary,file = paste0(OUTPUT_PATH,"hosp2_summary_nstudies10.csv"),row.names = F)

sub_hospital3 <- get_subgroup(cleanData, indexData, "hospital_grade", 3)
hosp3_summaryTables <- get_summary_table(sub_hospital3,data_type = "USD")
write.csv(hosp3_summaryTables$data_summary,file = paste0(OUTPUT_PATH,"hosp3_summary10.csv"),row.names = F)
write.csv(hosp3_summaryTables$nstudies_summary,file = paste0(OUTPUT_PATH,"hosp3_summary_nstudies10.csv"),row.names = F)


## condition
sub_flu <- get_subgroup(cleanData, indexData, "condition", "流感")
flu_summaryTables <- get_summary_table(sub_flu,data_type = "USD")
write.csv(flu_summaryTables$data_summary,file = paste0(OUTPUT_PATH,"flu_summary10.csv"),row.names = F)
write.csv(flu_summaryTables$nstudies_summary,file = paste0(OUTPUT_PATH,"flu_summary_nstudies10.csv"),row.names = F)

sub_pneumonia <- get_subgroup(cleanData, indexData, "condition", "肺炎")
pneumonia_summaryTables <- get_summary_table(sub_pneumonia,data_type = "USD")
write.csv(pneumonia_summaryTables$data_summary,file = paste0(OUTPUT_PATH,"pneumonia_summary10.csv"),row.names = F)
write.csv(pneumonia_summaryTables$nstudies_summary,file = paste0(OUTPUT_PATH,"pneumonia_summary_nstudies10.csv"),row.names = F)

#sub_sars <- get_subgroup(cleanData, indexData, "condition", "SARS")
#sars_summaryTables <- get_summary_table(sub_sars,data_type = "USD")
#write.csv(sars_summaryTables$data_summary,file = paste0(OUTPUT_PATH,"sars_summary10.csv"),row.names = F)
##write.csv(sars_summaryTables$nstudies_summary,file = paste0(OUTPUT_PATH,"sars_summary_nstudies10.csv"),row.names = F)

#sub_covid19 <- get_subgroup(cleanData, indexData, "condition", "COVID-19")
#covid19_summaryTables <- get_summary_table(sub_covid19,data_type = "USD")
#write.csv(covid19_summaryTables$data_summary,file = paste0(OUTPUT_PATH,"covid19_summary10.csv"),row.names = F)
#write.csv(covid19_summaryTables$nstudies_summary,file = paste0(OUTPUT_PATH,"covid19_summary_nstudies10.csv"),row.names = F)

sub_ari <- get_subgroup(cleanData, indexData, "condition", "ARI")
ari_summaryTables <- get_summary_table(sub_ari,data_type = "USD")
write.csv(ari_summaryTables$data_summary,file = paste0(OUTPUT_PATH,"ari_summary10.csv"),row.names = F)
write.csv(ari_summaryTables$nstudies_summary,file = paste0(OUTPUT_PATH,"ari_summary_nstudies10.csv"),row.names = F)

## region
sub_eastern <- get_subgroup(cleanData, indexData, "region", "Eastern China")
eastern_summaryTables <- get_summary_table(sub_eastern,data_type = "USD")
write.csv(eastern_summaryTables$data_summary,file = paste0(OUTPUT_PATH,"eastern_summary10.csv"),row.names = F)
write.csv(eastern_summaryTables$nstudies_summary,file = paste0(OUTPUT_PATH,"eastern_summary_nstudies10.csv"),row.names = F)

sub_northeastern <- get_subgroup(cleanData, indexData, "region", "Northeastern China")
northeastern_summaryTables <- get_summary_table(sub_northeastern,data_type = "USD")
write.csv(northeastern_summaryTables$data_summary,file = paste0(OUTPUT_PATH,"northeastern_summary10.csv"),row.names = F)
write.csv(northeastern_summaryTables$nstudies_summary,file = paste0(OUTPUT_PATH,"northeastern_summary10_nstudies.csv"),row.names = F)

# sub_central <- get_subgroup(cleanData, indexData, "region", "Central China")
# central_summaryTables <- get_summary_table(sub_central,data_type = "USD")
# write.csv(central_summaryTables$data_summary,file = paste0(OUTPUT_PATH,"central_summary10.csv"),row.names = F)
# write.csv(central_summaryTables$nstudies_summary,file = paste0(OUTPUT_PATH,"central_summary_nstudies10.csv"),row.names = F)

sub_western <- get_subgroup(cleanData, indexData, "region", "Western China")
western_summaryTables <- get_summary_table(sub_western,data_type = "USD")
write.csv(western_summaryTables$data_summary,file = paste0(OUTPUT_PATH,"western_summary10.csv"),row.names = F)
write.csv(western_summaryTables$nstudies_summary,file = paste0(OUTPUT_PATH,"western_summary_nstudies10.csv"),row.names = F)


## research year

sub_2009 <- get_subgroup(cleanData, indexData, "year_group", "-2009")
y2009_summaryTables <- get_summary_table(sub_2009,data_type = "USD")
write.csv(y2009_summaryTables$data_summary,file = paste0(OUTPUT_PATH,"y2009_summary10.csv"),row.names = F)
write.csv(y2009_summaryTables$nstudies_summary,file = paste0(OUTPUT_PATH,"y2009_summary_nstudies10.csv"),row.names = F)

sub_2014 <- get_subgroup(cleanData, indexData, "year_group", "2010-2014")
y2014_summaryTables <- get_summary_table(sub_2014,data_type = "USD")
write.csv(y2014_summaryTables$data_summary,file = paste0(OUTPUT_PATH,"y2014_summary10.csv"),row.names = F)
write.csv(y2014_summaryTables$nstudies_summary,file = paste0(OUTPUT_PATH,"y2014_summary_nstudies10.csv"),row.names = F)

sub_2015u <- get_subgroup(cleanData, indexData, "year_group", "2015-")
y2015u_summaryTables <- get_summary_table(sub_2015u,data_type = "USD")
write.csv(y2015u_summaryTables$data_summary,file = paste0(OUTPUT_PATH,"y2015u_summary10.csv"),row.names = F)
write.csv(y2015u_summaryTables$nstudies_summary,file = paste0(OUTPUT_PATH,"y2015u_summary_nstudies10.csv"),row.names = F)


##critical
sub_criticalOnly <- get_subgroup(cleanData, indexData, "critical", "YES")
criticalOnly_summaryTables <- get_summary_table(sub_criticalOnly,data_type = "USD")
write.csv(criticalOnly_summaryTables$data_summary,file = paste0(OUTPUT_PATH,"criticalOnly_summary10.csv"),row.names = F)
write.csv(criticalOnly_summaryTables$nstudies_summary,file = paste0(OUTPUT_PATH,"criticalOnly_summary_nstudies10.csv"),row.names = F)

sub_noncritical <- get_subgroup(cleanData, indexData, "critical", "NO")
noncritical_summaryTables <- get_summary_table(sub_noncritical,data_type = "USD")
write.csv(noncritical_summaryTables$data_summary,file = paste0(OUTPUT_PATH,"noncritical_summary10.csv"),row.names = F)
write.csv(noncritical_summaryTables$nstudies_summary,file = paste0(OUTPUT_PATH,"noncritical_summary_nstudies10.csv"),row.names = F)



# Meta analysis ----

metaResult60125InDMC <- get_meta("60-125", "住院", "直接医疗费用")
summary(metaResult60125InDMC[[2]])
get_forest_plot(filename = paste0(OUTPUT_PATH,"forest60125InDMC10.jpeg"), dat = metaResult60125InDMC, 
                ilab.xpos = c(-4000,-2000), text_loci = c(-7500, -4000, -2000, 11000, 13000))
get_funnel_plot(filename = paste0(OUTPUT_PATH,"funnel60125InDMC10.jpeg"), dat = metaResult60125InDMC)


metaResult65125InDMC <- get_meta("65-125", "住院", "直接医疗费用")
summary(metaResult65125InDMC[[2]])
get_forest_plot(filename = paste0(OUTPUT_PATH,"forest65125InDMC10.jpeg"), dat = metaResult65125InDMC, 
                ilab.xpos = c(-3000,-1000), text_loci = c(-7700, -3000, -1000,15000, 18000))
get_funnel_plot(filename = paste0(OUTPUT_PATH,"funnel65125InDMC10.jpeg"), dat = metaResult65125InDMC)





# Meta analysis subgroup ----

## hospital grade
metaResult60125InDMC_hosp2 <- get_meta("60-125", "住院", "直接医疗费用", sub_group = sub_hospital2$ID)
summary(metaResult60125InDMC_hosp2[[2]])
get_forest_plot(filename = paste0(OUTPUT_PATH,"forest60125InDMC_hosp210.jpeg"), dat =metaResult60125InDMC_hosp2, 
                ilab.xpos = c(-400,-100), text_loci = c(-1000, -400, -100, 3500, 4050))
# dev.off()

metaResult60125InDMC_hosp3 <- get_meta("60-125", "住院", "直接医疗费用", sub_group = sub_hospital3$ID)
summary(metaResult60125InDMC_hosp3[[2]])
get_forest_plot(filename = paste0(OUTPUT_PATH,"forest60125InDMC_hosp310.jpeg"), dat =metaResult60125InDMC_hosp3, 
                ilab.xpos = c(-3000,-1000), text_loci = c(-7500, -3000, -1000, 11000,13000))

## conditions
metaResult60125InDMC_pneumonia <- get_meta("60-125", "住院", "直接医疗费用", sub_group = sub_pneumonia$ID)
summary(metaResult60125InDMC_pneumonia[[2]])
get_forest_plot(filename = paste0(OUTPUT_PATH,"forest60125InDMC_pneumonia10.jpeg"), dat =metaResult60125InDMC_pneumonia, 
                ilab.xpos = c(-3000,-1000), text_loci = c(-7500, -3000, -1000, 11000, 13000))

## region
metaResult60125InDMC_eastern <- get_meta("60-125", "住院", "直接医疗费用", sub_group = sub_eastern$ID)
summary(metaResult60125InDMC_eastern[[2]])
get_forest_plot(filename = paste0(OUTPUT_PATH,"forest60125InDMC_eastern10.jpeg"), dat =metaResult60125InDMC_eastern, 
                ilab.xpos = c(-2000,-1000), text_loci = c(-3700, -2000, -1000,5800, 7000))



metaResult60125InDMC_western <- get_meta("60-125", "住院", "直接医疗费用", sub_group = sub_western$ID)
summary(metaResult60125InDMC_western[[2]])
get_forest_plot(filename = paste0(OUTPUT_PATH,"forest60125InDMC_western10.jpeg"), dat =metaResult60125InDMC_western, 
                ilab.xpos = c(-3000,-1500), text_loci = c(-6000, -3000, -1500, 10000, 12000))


## research year
metaResult60125InDMC_2009 <- get_meta("60-125", "住院", "直接医疗费用", sub_group = sub_2009$ID)
summary(metaResult60125InDMC_2009[[2]])
get_forest_plot(filename = paste0(OUTPUT_PATH,"forest60125InDMC_200910.jpeg"), dat =metaResult60125InDMC_2009, 
                ilab.xpos = c(-3000,-1500), text_loci = c(-6000, -3000, -1500, 10000, 12000))


metaResult60125InDMC_2014 <- get_meta("60-125", "住院", "直接医疗费用", sub_group = sub_2014$ID)
summary(metaResult60125InDMC_2014[[2]])
get_forest_plot(filename = paste0(OUTPUT_PATH,"forest60125InDMC_201410.jpeg"), dat =metaResult60125InDMC_2014, 
                ilab.xpos = c(-3000,-1500), text_loci = c(-6800, -3000, -1500, 10000, 12000))


metaResult60125InDMC_2015u <- get_meta("60-125", "住院", "直接医疗费用", sub_group = sub_2015u$ID)
summary(metaResult60125InDMC_2015u[[2]])
get_forest_plot(filename = paste0(OUTPUT_PATH,"forest60125InDMC_2015u10.jpeg"), dat =metaResult60125InDMC_2015u, 
                ilab.xpos = c(-800,-300), text_loci = c(-2200, -800, -300, 5000, 6000))

##critical
metaResult60125InDMC_criticalOnly <- get_meta("60-125", "住院", "直接医疗费用", sub_group = sub_criticalOnly$ID)
summary(metaResult60125InDMC_criticalOnly[[2]])
get_forest_plot(filename = paste0(OUTPUT_PATH,"forest60125InDMC_criticalOnly10.jpeg"), dat =metaResult60125InDMC_criticalOnly, 
                ilab.xpos = c(-2000,-800), text_loci = c(-5300, -2000, -800, 9000, 11000))

metaResult60125InDMC_noncritical <- get_meta("60-125", "住院", "直接医疗费用", sub_group = sub_noncritical$ID)
summary(metaResult60125InDMC_noncritical[[2]])
get_forest_plot(filename = paste0(OUTPUT_PATH,"forest60125InDMC_noncritical10.jpeg"), dat =metaResult60125InDMC_noncritical, 
                ilab.xpos = c(-2000,-800), text_loci = c(-7300, -2000, -800, 11000, 13000))



# a <- c(0.449612403,0.411267606)
# median(a)
# quantile(a,0.25)
# quantile(a,0.75)


## calculate weighted.mean
cleanData <- cleanData[-which(cleanData$ID == 42),]
data_summary_weighted_mean <- do.call(rbind,by(cleanData, cleanData[c("setting","cost_type")], get_weighted_mean))

## calculate weighted.mean_meta
weightsMeta60 <- data.frame(ID = metaResult60125InDMC[[1]]$ID, 
                          weights_meta60 = weights(metaResult60125InDMC[[2]]))
weightsMeta65 <- data.frame(ID = metaResult65125InDMC[[1]]$ID, 
                            weights_meta65 = weights(metaResult65125InDMC[[2]]))
cleanData <- cleanData %>% left_join(weightsMeta60) %>% left_join(weightsMeta65)

cleanData_weighted_meta_60125 <- cleanData[which(cleanData$age_group == "60-125" & cleanData$cost_type == "直接医疗费用" & cleanData$setting == "住院" & !is.na(cleanData$weights_meta60)),]
weighted_mean_meta_60125 = weighted.mean(cleanData_weighted_meta_60125$cost_average, cleanData_weighted_meta_60125$weights_meta60)

cleanData_weighted_meta_65125 <- cleanData[which(cleanData$age_group == "65-125" & cleanData$cost_type == "直接医疗费用" & cleanData$setting == "住院" & !is.na(cleanData$weights_meta65)),]
weighted_mean_meta_65125 = weighted.mean(cleanData_weighted_meta_65125$cost_average, cleanData_weighted_meta_65125$weights_meta65)

## calculate median
data_summary_median <- do.call(rbind,by(cleanData, cleanData[c("setting","cost_type")], get_median))

## results combination
median_mean <- left_join(data_summary_median[,1:4], data_summary_weighted_mean, by = c("setting", "cost_type", "age_group"))

## calculate Pearson-r
cor.test(median_mean$P50, median_mean$wmean)


# Correlation of Median.IQR and Weighted.mean ----

## There are 17 (age,setting,cost_type) groups 
vec_weighted.mean <- median_mean$wmean
vec_median.IQR <- median_mean$P50
cor.test(vec_median.IQR, vec_weighted.mean)
x <- vec_median.IQR
y <- vec_weighted.mean
plot(x, y, xlim = c(0,3500), ylim = c(0,3500), main = "Correlation of Median.IQR and Weighted.mean",
     xlab = "median.IQR", ylab = "weighted.mean",
     pch = 20, frame = T)
abline(lm(y ~ x), col = "#0072B5")

# Correlation of Median.IQR and weighted.mean_meta ----

## There are 2 (age,setting,cost_type) groups 
vec_weighted.mean_meta <- c(1865,3287)
vec_median.IQR <- c(2077,2580)
x <- vec_median.IQR
y <- vec_weighted.mean_meta
plot(x, y, xlim = c(0,3500), ylim = c(0,3500), main = "Correlation of Median.IQR and weighted.mean_meta",
     xlab = "median.IQR", ylab = "weighted.mean_meta",
     pch = 20, frame = T)

# Correlation of Median.IQR and Meta ----

## There are 2 (age,setting,cost_type) groups 
vec_meta <- c(1865,3287)
vec_median.IQR <- c(2077,2580)
x <- vec_median.IQR
y <- vec_meta
plot(x, y, xlim = c(0,3500), ylim = c(0,3500), main = "Correlation of Median.IQR and Meta",
     xlab = "median.IQR", ylab = "meta",
     pch = 20, frame = T)
