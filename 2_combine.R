library(readxl)
library(dplyr)

source("3_Rscript/functions.R")

# direction definition----

INPUT_PATH = "1_Input/"
OUTPUT_PATH = "2_Output/"



data_USD <- read_xlsx("C:/Users/徐筱萸/Desktop/with tian/RESCEU/数据整理分析/ari数据提取表v2.1.xlsx",5)

data_USD[33,8] <- get_combined_mean(as.numeric(data_USD[31,8]),as.numeric(data_USD[31,7]),
                                    as.numeric(data_USD[32,8]),as.numeric(data_USD[32,7]))

data_USD[33,15] <- get_combined_mean(as.numeric(data_USD[31,15]),as.numeric(data_USD[31,7]),
                                     as.numeric(data_USD[32,15]),as.numeric(data_USD[32,7]))

data_USD[51,8] <- get_combined_mean(as.numeric(data_USD[49,8]),as.numeric(data_USD[49,7]),
                                    as.numeric(data_USD[50,8]),as.numeric(data_USD[50,7]))

data_USD[51,15] <- get_combined_mean(as.numeric(data_USD[49,15]),as.numeric(data_USD[49,7]),
                                     as.numeric(data_USD[50,15]),as.numeric(data_USD[50,7]))

data_USD[51,16] <- get_combined_sd(as.numeric(data_USD[49,15]),as.numeric(data_USD[49,16]),as.numeric(data_USD[49,7]),
                                   as.numeric(data_USD[50,15]),as.numeric(data_USD[50,16]),as.numeric(data_USD[50,7]))

step1_LOS_mean <- get_combined_mean(as.numeric(data_USD[58,8]),as.numeric(data_USD[58,7]),
                                    as.numeric(data_USD[59,8]),as.numeric(data_USD[59,7]))
step2_LOS_mean <- get_combined_mean(step1_LOS_mean, as.numeric(data_USD[58,7])+as.numeric(data_USD[59,7]),
                                    as.numeric(data_USD[60,8]),as.numeric(data_USD[60,7]))
step3_LOS_mean <- get_combined_mean(step2_LOS_mean, as.numeric(data_USD[58,7])+as.numeric(data_USD[59,7])+as.numeric(data_USD[60,7]),
                                    as.numeric(data_USD[57,8]),as.numeric(data_USD[57,7]))

step1_cost_mean <- get_combined_mean(as.numeric(data_USD[58,15]),as.numeric(data_USD[58,7]),
                                     as.numeric(data_USD[59,15]),as.numeric(data_USD[59,7]))
step2_cost_mean <- get_combined_mean(step1_cost_mean, as.numeric(data_USD[58,7])+as.numeric(data_USD[59,7]),
                                     as.numeric(data_USD[60,15]),as.numeric(data_USD[60,7]))
step3_cost_mean <- get_combined_mean(step2_cost_mean, as.numeric(data_USD[58,7])+as.numeric(data_USD[59,7])+as.numeric(data_USD[60,7]),
                                     as.numeric(data_USD[57,15]),as.numeric(data_USD[57,7]))

step1_cost_sd <- get_combined_sd(as.numeric(data_USD[58,15]),as.numeric(data_USD[58,16]),as.numeric(data_USD[58,7]),
                                 as.numeric(data_USD[59,15]),as.numeric(data_USD[59,16]),as.numeric(data_USD[59,7]))
step2_cost_sd <- get_combined_sd(step1_cost_mean,step1_cost_sd,as.numeric(data_USD[58,7])+as.numeric(data_USD[59,7]),
                                 as.numeric(data_USD[60,15]),as.numeric(data_USD[60,16]),as.numeric(data_USD[60,7]))
step3_cost_sd <- get_combined_sd(step2_cost_mean,step2_cost_sd,as.numeric(data_USD[58,7])+as.numeric(data_USD[59,7])+as.numeric(data_USD[60,7]),
                                 as.numeric(data_USD[57,15]),as.numeric(data_USD[57,16]),as.numeric(data_USD[57,7]))



