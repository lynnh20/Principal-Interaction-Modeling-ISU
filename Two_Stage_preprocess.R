
Two_stage_preprocess <- function(data, response){
  # Prepare the dataset to input in Two-stage Lasso process
  # Arg:
  # data: a dataframe object. The data you would like to convert the format
  # response: a character. response variables you choose. eg: "FUT_12W_RLT_RET"
  # Return:
  # a dataframe ready for Two-stage Lasso process
  
  
  ##################################################################################
  ########################### 1. center and scale Y ################################
  ##################################################################################
  Y <- data[,response] # get the response variable
  Y <- scale(Y,center = TRUE, scale = TRUE) # scale Y
  
  ##################################################################################
  ########################### 2. one-hot encoding asset group ######################
  ##################################################################################
  
  # one-hot encoding
  group_region <- one_hot(as.data.table(data$REGION))
  group_sector <- one_hot(as.data.table(data$SECTOR))
  group_industry <- one_hot(as.data.table(data$INDUSTRY_GROUP))
  
  colnames(group_region) <- str_remove(colnames(group_region), "V1_")
  colnames(group_sector) <- str_remove(colnames(group_sector), "V1_")
  colnames(group_industry) <- str_remove(colnames(group_industry), "V1_")
  
  ##################################################################################
  ########################### 3. get X  ############################################
  ##################################################################################
  
  factor_LOI <- data %>% select(starts_with(c("LOI_")))
  X <- cbind(factor_LOI,group_region,group_sector,group_industry)
  
  #################################################################################
  ########################### 5. combine Y,X ######################################
  #################################################################################
  ana.data <- cbind(Y,X)
  return(ana.data)
  
}