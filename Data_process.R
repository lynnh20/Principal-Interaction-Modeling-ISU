
data_process <- function(data_numeric,data_categorical){
  
  ##################################################################################
  ######## 1. reshape data and merge data ##########################################
  ##################################################################################
  data_numeric_wide <- pivot_wider(data = data_numeric, id_cols = c("STOCK_ID","DATE"), names_from = FACTOR_NAME, values_from = FACTOR_VALUE)
  # Merge the data_numeric_wide and data_categorical
  data_merged <- inner_join(data_categorical, data_numeric_wide, by = c("STOCK_ID","DATE"))
  data_merged <- data_merged %>% 
    mutate(DATE = ymd(DATE)) %>% 
    mutate_if(is.character,as.factor) # transfer all character variables to factor
  
  ##################################################################################
  ######## 2. re-range LOI_17200 ###################################################
  ##################################################################################
  
  range01 <- function(x){
    (x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))
  }
  data_merged$LOI_17200 <- range01(data_merged$LOI_17200) # Transform LOI_17200 to [0,1]
  
  ##################################################################################
  ######## 3. Move Real Estate from Financials Section to Real Estate###############
  ##################################################################################
  
  data_merged$SECTOR = as.character(data_merged$SECTOR)
  # Changes sector to "Real Estate" is INDUSTRY_GROUP is "Real Estate"
  data_merged <- data_merged %>% 
    mutate(SECTOR = ifelse(INDUSTRY_GROUP == "Real Estate", "Real Estate", SECTOR)) %>%
    mutate(SECTOR = as.factor(SECTOR))       # Cast SECTOR back to factor
  
  ##################################################################################
  ########   4. Renama the same levels in INDUSTRY_GROUP and SECTOR  ###############
  ##################################################################################
  
  # Energy, Utilities, Materials, Real Estate are same names used in INDUSTRY_GROUP and SECTOR
  common_levels <- intersect(levels(data_merged$SECTOR),levels(data_merged$INDUSTRY_GROUP))
  data_merged <- data_merged %>% 
    mutate(SECTOR = as.character(SECTOR))
  data_merged <- data_merged %>% 
    mutate(SECTOR = ifelse(SECTOR == common_levels[1],paste0(common_levels[1],"_S"),
                           ifelse(SECTOR == common_levels[2],paste0(common_levels[2],"_S"),
                                  ifelse(SECTOR == common_levels[3],paste0(common_levels[3],"_S"),
                                         ifelse(SECTOR == common_levels[4],paste0(common_levels[4],"_S"),SECTOR))))) %>%
    mutate(SECTOR = as.factor(SECTOR))       # Cast SECTOR back to factor
  
  
  ##################################################################################
  ######## 5. Missing data imputation ##############################################
  ##################################################################################
  cols <- colnames(data_merged)
  LOI_names <- cols[grepl('^LOI_', cols)]
  trad_names <-  c("MCAP","PREV_12M_RET","PREV_12M_VOL","ROE","BETA_PREV_1Y","BK_P") 
  return_names <- cols[grepl('^FUT_', cols)]
  
  d = data_merged[,c("DATE",LOI_names,trad_names,return_names)]
  
  # Impute column means by date for LOI and traditional equity factors
  imputed = impute_median(dat = d, formula = .-DATE ~DATE )
  # Replace LOI and traditional equity factors in merged dataset with data containing imputed means
  data_merged[,c(LOI_names,trad_names,return_names)] = imputed[,-1] 
  
  ##################################################################################
  ######## 6. Calculate the Relative Return ########################################
  ##################################################################################
  
  # Calculate the average return for each date
  Avg_RET <- data_merged %>%
    group_by(DATE) %>%
    summarise(FUT_52W_RET_Mean = mean(FUT_52W_RET,na.rm = T),
              FUT_24W_RET_Mean = mean(FUT_24W_RET,na.rm = T),
              FUT_1W_RET_Mean = mean(FUT_1W_RET,na.rm = T),
              FUT_4W_RET_Mean = mean(FUT_4W_RET,na.rm = T),
              FUT_12W_RET_Mean = mean(FUT_12W_RET,na.rm = T))
  
  # Subtract the average return from the return of each stock on that day
  data_merged <- inner_join(data_merged, Avg_RET, by = c("DATE"))
  ##Subtract the average return to get relative return
  data_merged <- data_merged %>% mutate(
    FUT_52W_RLT_RET = FUT_52W_RET - FUT_52W_RET_Mean,
    FUT_24W_RLT_RET = FUT_24W_RET - FUT_24W_RET_Mean,
    FUT_1W_RLT_RET = FUT_1W_RET - FUT_1W_RET_Mean,
    FUT_4W_RLT_RET = FUT_4W_RET - FUT_4W_RET_Mean,
    FUT_12W_RLT_RET = FUT_12W_RET - FUT_12W_RET_Mean
  )
  ## remove mean cols
  data_merged <- data_merged %>% mutate(
    FUT_52W_RET_Mean = NULL,
    FUT_24W_RET_Mean = NULL,
    FUT_1W_RET_Mean = NULL,
    FUT_4W_RET_Mean = NULL,
    FUT_12W_RET_Mean = NULL
  )
  ##################################################################################
  ################################ Return ##########################################
  ##################################################################################
  
  return(data_merged)
}


# used for calculate the terms needed to be removed
# due to the purpose, there is no need to go through all cleaning steps
data_process_no_impute <- function(data_numeric,data_categorical){
  
  ##################################################################################
  ######## 1. reshape data and merge data ##########################################
  ##################################################################################
  data_numeric_wide <- pivot_wider(data = data_numeric, id_cols = c("STOCK_ID","DATE"), names_from = FACTOR_NAME, values_from = FACTOR_VALUE)
  # Merge the data_numeric_wide and data_categorical
  data_merged <- inner_join(data_categorical, data_numeric_wide, by = c("STOCK_ID","DATE"))
  data_merged <- data_merged %>% 
    mutate(DATE = ymd(DATE)) %>% 
    mutate_if(is.character,as.factor) # transfer all character variables to factor
  
  ##################################################################################
  ######## 2. Move Real Estate from Financials Section to Real Estate###############
  ##################################################################################
  
  data_merged$SECTOR = as.character(data_merged$SECTOR)
  # Changes sector to "Real Estate" is INDUSTRY_GROUP is "Real Estate"
  data_merged <- data_merged %>% 
    mutate(SECTOR = ifelse(INDUSTRY_GROUP == "Real Estate", "Real Estate", SECTOR)) %>%
    mutate(SECTOR = as.factor(SECTOR))       # Cast SECTOR back to factor
  
  ##################################################################################
  ########   3. Renama the same levels in INDUSTRY_GROUP and SECTOR  ###############
  ##################################################################################
  
  # Energy, Utilities, Materials, Real Estate are same names used in INDUSTRY_GROUP and SECTOR
  common_levels <- intersect(levels(data_merged$SECTOR),levels(data_merged$INDUSTRY_GROUP))
  data_merged <- data_merged %>% 
    mutate(SECTOR = as.character(SECTOR))
  data_merged <- data_merged %>% 
    mutate(SECTOR = ifelse(SECTOR == common_levels[1],paste0(common_levels[1],"_S"),
                           ifelse(SECTOR == common_levels[2],paste0(common_levels[2],"_S"),
                                  ifelse(SECTOR == common_levels[3],paste0(common_levels[3],"_S"),
                                         ifelse(SECTOR == common_levels[4],paste0(common_levels[4],"_S"),SECTOR))))) %>%
    mutate(SECTOR = as.factor(SECTOR))       # Cast SECTOR back to factor
  
  ##################################################################################
  ################################ Return ##########################################
  ##################################################################################
  
  return(data_merged)
}