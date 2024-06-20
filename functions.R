get_median <- function(each.df) {
  res <- each.df %>% group_by(setting, cost_type, age_group) %>%
    dplyr::summarise(
      P50 = median(cost_average),
      P25 = quantile(cost_average, 0.25),
      P75 = quantile(cost_average, 0.75)
    )
  return(res)
}

get_weighted_mean <- function(each.df) {
  res <- each.df %>% group_by(setting, cost_type, age_group) %>%
    dplyr::summarise(
      wmean = weighted.mean(cost_average, population)
    )
  return(res)
}

get_weighted_mean_meta <- function(each.df) {
  res <- each.df %>% group_by(setting, cost_type, age_group) %>%
    dplyr::summarise(
      wmean_meta = weighted.mean(cost_average, weights_meta)
    )
  return(res)
}

get_median_los <- function(each.df) {
  res <- each.df %>% group_by(setting, cost_type, age_group) %>%
    dplyr::summarise(
      P50 = median(los_average),
      P25 = quantile(los_average, 0.25),
      P75 = quantile(los_average, 0.75)
    )
  return(res)
}

get_n_studies <- function(sum.vec, origin_data) {
  n_studies <- nrow(unique(origin_data[which(origin_data$setting == as.character(unlist(sum.vec[1])) & 
                                               origin_data$cost_type == as.character(unlist(sum.vec[2])) &
                                               origin_data$age_group == as.character(unlist(sum.vec[3]))),"ID"]))
  return(n_studies)
}


get_n_studies_los <- function(sum.vec, origin_data) {
  n_studies <- nrow(unique(origin_data[which(origin_data$age_group == sum.vec[3]),"ID"]))
  return(n_studies)
}


get_sub_sd <- function(mean_0, sd_0, n_0, n_1) {
  sd_1 <- sqrt(n_0 / n_1 * sd_0^2 - (n_0 - n_1) * n_0 / n_1^2 * mean_0^2)
  return(sd_1)
}


get_combined_mean <- function(mean_1, n_1, mean_2, n_2) {
  mean_combined <- (mean_1 * n_1 + mean_2 * n_2) / (n_1 + n_2)
  return(mean_combined)
}


get_combined_sd <- function(mean_1, sd_1, n_1,
                            mean_2, sd_2, n_2) {
  sd_combined <- sqrt(
    ((n_1 - 1)*sd_1^2 + (n_2 - 1)*sd_2^2 + n_1*n_2/(n_1+n_2) * (mean_1^2 + mean_2^2 - 2*mean_1*mean_2)) /
      (n_1 + n_2 - 1)
  )
  return(sd_combined)
}


get_author <- function(vec) {
  author_year <- indexData$source[which(indexData$ID == unlist(vec)["ID"])]
  author <- str_sub(author_year, 1, str_locate(author_year,"\\，")[1]-1)
  
  return(author)
}

get_year <- function(vec) {
  author_year <- indexData$source[which(indexData$ID == unlist(vec)["ID"])]
  year <- str_sub(author_year, str_locate(author_year,"\\，")[2]+1, str_length(author_year))
  
  return(year)
}

get_exchanged_currency <- function(dat, currencyCols, targetCurrencyType, targetYear = 2021,
                                   currencyYearCol = "currency_year",
                                   currencyTypeCol = "currency_type",
                                   ex_USD_CNY = USD_CNY, exchangeRateTable = exchangeRate) {
  if(!targetCurrencyType %in% c("USD", "CNY"))
    stop("Currency type should be one of USD/CNY")
  
  if(targetYear != 2021 & targetCurrencyType == "USD")
    stop("Not capable to calculate USD exchange rate other than 2021 (temporarily)")
  
  dat_new <- dat
  for (j in currencyCols) {
    for (i in 1:nrow(dat_new)) {
      
      if(dat[i,currencyTypeCol] == "US$") {
        dat_new[i,j] = dat_new[i,j] * 
                        ex_USD_CNY[which(ex_USD_CNY$year == as.numeric(dat_new[i,currencyYearCol])),"cny_per_usd"]
      }
      
      exr = exchangeRateTable[which(exchangeRateTable$year == targetYear),"index_cny"] / 
             exchangeRateTable[which(exchangeRateTable$year == as.numeric(dat_new[i,currencyYearCol])),"index_cny"]
      
      if(!is.na(dat[i,j])) {
        dat_new[i,j] = as.numeric(dat_new[i,j]) * exr
      }
      
      if(targetCurrencyType == "USD") {
        dat_new[i,j] <- as.numeric(dat_new[i,j]) / 6.45
      }
    }
  }
  
  return(as.data.frame(dat_new))
}

get_clean_data <- function(origin_data) {
  
  clean_data <- origin_data
  clean_data <- clean_data[!is.na(clean_data$setting),]
  clean_data <- clean_data[!is.na(clean_data$age_group),]
  
  clean_data$setting <- factor(clean_data$setting)
  clean_data$cost_type <- factor(clean_data$cost_type)
  clean_data$age_group <- factor(clean_data$age_group)
  clean_data$cost_average <- clean_data$cost_mean
  clean_data$cost_average[is.na(clean_data$cost_average)] <- clean_data$cost_median[is.na(clean_data$cost_average)]
  clean_data$los_average <- clean_data$los_mean
  clean_data$los_average[is.na(clean_data$los_average)] <- clean_data$los_median[is.na(clean_data$los_average)]
  
  clean_data$author <- apply(clean_data, 1, get_author)
  clean_data$year <- apply(clean_data, 1, get_year)
  return(clean_data)
}

get_summary_table <- function(clean_data, data_type) {
  
  if(data_type == "USD") {
    data_summary <- do.call(rbind,by(clean_data, clean_data[c("setting","cost_type")], get_median))
  }
  
  if(data_type == "LoS") {
    # 首先整理存在los_average的数据
    los_clean_data <- clean_data[!is.na(clean_data$los_average),]
    data_summary <- do.call(rbind,by(los_clean_data, los_clean_data[c("setting","cost_type")], get_median_los))
  }
  
  data_summary[,4:6] <- round(data_summary[,4:6],digits = 0)
  data_summary$Median_P25_P75 <- paste0(data_summary$P50,"[",data_summary$P25,",",data_summary$P75,"]")
  data_summary <- data_summary[,-c(4:6)]
  data_summary_wider <- pivot_wider(data_summary, names_from = c(cost_type, setting), values_from = Median_P25_P75)
  
  nstudies_summary <- data_summary[,1:3]
  
  if(data_type == "USD") {
    nstudies_summary$n.studies <- apply(nstudies_summary, 1, get_n_studies, clean_data)
  }
  
  if(data_type == "LoS") {
    nstudies_summary$n.studies <- apply(nstudies_summary, 1, get_n_studies_los, clean_data)
  }
  
  nstudies_summary_wider <- pivot_wider(nstudies_summary, names_from = c(cost_type, setting), values_from = n.studies)
    
  return(list(data_summary = data_summary_wider,nstudies_summary = nstudies_summary_wider))
}

get_subgroup <- function(all_data, index_data, group_name, subgroup) {
  
  num_study <- unlist(index_data[which(index_data[,group_name] == subgroup), "ID"])
  subgroup_data <- all_data[which(all_data$ID %in% num_study),]
  
  return(subgroup_data)
}


get_meta <- function(meta_ageGroup, meta_setting, meta_costType, sub_group = NA, meta_method = "DL") {
  
  temp_cleanData <- cleanData
  temp_cleanData$cost_se <- temp_cleanData$cost_sd / sqrt(temp_cleanData$population)
  
  if(!is.na(sub_group[1])) {
    temp_cleanData <- temp_cleanData[which(temp_cleanData$ID %in% sub_group),]
  }
  
  temp_cleanData <- temp_cleanData[which(temp_cleanData$age_group == meta_ageGroup &
                                           temp_cleanData$setting == meta_setting &
                                           temp_cleanData$cost_type == meta_costType),]
  
  temp_cleanData <- temp_cleanData[which(!is.na(temp_cleanData$cost_mean) & !is.na(temp_cleanData$cost_se)),]
  
  if(sum(duplicated(temp_cleanData$ID)) > 0) {
    stop(paste0("There are rows from a same study in that subgroup, please double check!", "\n",
                "Duplicated study ID: ", temp_cleanData$ID[which(duplicated(temp_cleanData$ID))]))
  }
  
  if(nrow(temp_cleanData) < 3) {
    stop("There are less than 3 rows in that subgroup, please double check!")
  }
  
  temp_cleanData_ordered <- temp_cleanData[order(temp_cleanData$cost_mean),]
  
  metamod <- rma(yi = cost_mean, sei = cost_se, data = temp_cleanData_ordered, method = meta_method)
  
  return(list(temp_cleanData_ordered,metamod))
}


get_forest_plot <- function(filename, width = 8000, res = 720,
                            dat,
                            ilab.xpos, text_loci) {
  jpeg(
    filename = filename,
    width = width,
    height = (nrow(dat[[1]]) + 4) * 400,
    units = "px",
    bg = "white",
    res = res)
  forest(dat[[2]], refline = 0,
         mlab = "Random-effect Model",
         slab = paste(dat[[1]]$author,dat[[1]]$year,sep = ","),
         xlab = "Cost",
         ilab = cbind(round(cost_mean,0),round(cost_se,0)),
         ilab.xpos = ilab.xpos,
         showweights = T,
         digits = 0)
  text(text_loci, nrow(dat[[1]]) + 2, c("Author and Year", "Mean", "SE", "Weight", "Mean[95%CI]"))
  dev.off()
}


get_funnel_plot <- function(filename, width = 5000, height = 5000, res = 720, dat) {
  jpeg(
    filename = filename,
    width = width,
    height = height,
    units = "px",
    bg = "white",
    res = res)
  funnel(dat[[2]])
  dev.off()
}

