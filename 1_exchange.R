library(readxl)
library(dylyr)

source("3_Rscript/functions.R")

# direction definition----

INPUT_PATH = "1_Input/"
OUTPUT_PATH = "2_Output/"


# data loading----

allDataOrigin <- read_xlsx(paste0(INPUT_PATH,"ari_data_extraction_v3.xlsx"),sheet = 3)

USD_CNY <- data.frame(year = c(2009, 2010, 2013, 2015, 2020),
                      cny_per_usd = c(6.83, 6.77, 6.20, 6.23, 6.90))

exchangeRate <- read.csv(paste0(INPUT_PATH,"exchange_rate.csv"), header = T)


# exchange currency----

CNY_2021 <- get_exchanged_currency(dat = allDataOrigin,
                                   currencyCols = 13:21,
                                   targetCurrencyType = "CNY")

USD_2021 <- get_exchanged_currency(dat = allDataOrigin,
                                   currencyCols = 13:21,
                                   targetCurrencyType = "USD")


# output results----

CNY_2021[is.na(CNY_2021)] <- ""
USD_2021[is.na(USD_2021)] <- ""
write.csv(CNY_2021,paste0(OUTPUT_PATH,"CNY_2021.csv"))
write.csv(USD_2021,paste0(OUTPUT_PATH,"USD_2021.csv"))
