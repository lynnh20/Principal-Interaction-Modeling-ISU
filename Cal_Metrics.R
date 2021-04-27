
Metrics_testdat <- function(testdat , baseline = "False"){
  # Calculate the 6 metrics to evalute the performance 
  # of our model based on a testdat contains y and y hat
  #
  # Arg:
  #  testdat should be a data.frame contain at least the following cols:
  #    y: response variable (eg: FUT_12W_RLT_RET)
  #    y_hat: the estimated response variable from our model (when baseline = F)
  #    DATE: in order to calculate the matrix across DATE
  #    INDUSTRY_GROUP: in order to calculate the matrix across INDUSTRY_GROUP
  #    LOI_18600, LOI_18400: to calcaute the result of baseline model
  #  baseline: default is "False", indicating the testdat is a test dataset. 
  #    When it's 'True', indicating it's for evaluate the performance of baseline model only
  # 
  # Return:
  #  a list. list[[1]] is a 6*2 matrix, each row represents for each metric in the order of spearman corrlation avg,
  #  spearman corrlation across INDUSTRY_GROUP, spearman corrlation across DATE, TPR, TNR, FPR
  #  col 1 represents if it's accepted under absolute acceptance threshold (0: no, 1: Yes))
  #  col 2 represents if it's accepted under baseline relative acceptance threshold (0: no, 1: Yes))
  #  list[[2]] is a 6*2 matrix. Rows are defined same as list[[1]]
  #  col 1 represents the value of each metric
  #  col 2 represents the difference in each metric betwwen our model result and the baseline result
  
  output_dummy <- output_actual <- matrix(NA,nrow = 6,ncol = 2) # initial the output matrix
  
  # Step 1: add the baseline predicted rank col in testdat
  # percentile rank the LOI_18600 and LOI_18400
  testdat <- testdat %>% group_by(DATE) %>%
    mutate(LOI_18600_pct = percent_rank(LOI_18600),
           LOI_18400_pct = percent_rank(LOI_18400))
  
  # get y hat by weighted score from baseline model
  testdat <- testdat %>% mutate(y_hat_bsl = 0.7*LOI_18600_pct + 0.3*LOI_18400_pct) 
  
  # if it's only for baseline model, we need to assign a col y_hat
  # to adapt to the code below
  if(baseline){ 
    testdat$y_hat <- testdat$y_hat_bsl 
  }
  
  # percentile rank the y, y_hat, y_hat_bsl
  testdat <- testdat %>% drop_na(y,y_hat,y_hat_bsl)
  testdat <- testdat %>% group_by(DATE) %>%
    mutate(y_pct = percent_rank(y),
           y_hat_pct = percent_rank(y_hat),
           y_hat_bsl_pct = percent_rank(y_hat_bsl))
  
  # Step 2: Calculate the six metrics and check if satisfied
  
  ####################################################
  ######## Spearman correlation average #############
  ####################################################
  corr_date <- testdat %>% 
    group_by(DATE) %>% 
    summarize(cor = cor(y_pct, y_hat_pct, method = "spearman"),
              cor_diff = cor(y_pct, y_hat_pct, method = "spearman") - cor(y_pct, y_hat_bsl_pct, method = "spearman")) 
  
  
  test_corr <- t.test(corr_date$cor,mu=0,alternative = "greater")
  test_corr_diff <- t.test(corr_date$cor_diff,mu=0,alternative = "greater")
  
  # check if the absolute threshold is satisfied
  output_dummy[1,1] <- ifelse(test_corr$p.value < 0.1,1,0) 
  output_actual[1,1] <- mean(corr_date$cor, na.rm = T)
  # check if the model's spearman is greater than the baseline's spearman at a 10% significance level
  output_dummy[1,2] <- ifelse(test_corr_diff$p.value < 0.1,1,0) 
  output_actual[1,2] <- mean(corr_date$cor_diff, na.rm = T)
  
  
  ####################################################
  # Spearman correlation consistency - across groups #
  ####################################################
  corr_group <- testdat %>% 
    group_by(INDUSTRY_GROUP) %>% 
    summarize(cor = cor(y_pct, y_hat_pct, method = "spearman"),
              cor_diff = cor(y_pct, y_hat_pct, method = "spearman") - cor(y_pct, y_hat_bsl_pct, method = "spearman")) 
  
  ## get the % of postive value in each col (cor, cor_bsl) in corr_group
  corr_group <- corr_group %>%
    mutate(cor = ifelse(cor > 0, 1, 0),
           cor_diff = ifelse(cor_diff > 0, 1, 0))
  corr_group <- apply(corr_group[,c("cor","cor_diff")],2,mean,na.rm=TRUE)
  
  output_dummy[2,1] <- ifelse(corr_group[1] >= 0.6,1,0) 
  output_dummy[2,2] <- ifelse(corr_group[2] >= 0.6,1,0) 
  
  output_actual[2,1] <- corr_group[1]
  output_actual[2,2] <- corr_group[2]
  
  ####################################################
  ## Spearman correlation consistency - across time ##
  ####################################################

  ## get the % of postive value in each col (cor, cor_bsl) in corr_group
  corr_date <- corr_date %>%
    mutate(cor = ifelse(cor > 0, 1, 0),
           cor_diff = ifelse(cor_diff > 0, 1, 0))
  corr_date <- apply(corr_date[,c("cor","cor_diff")],2,mean)
  
  output_dummy[3,1] <- ifelse(corr_date[1] >= 0.6,1,0) 
  output_dummy[3,2] <- ifelse(corr_date[2] >= 0.6,1,0) 
  
  output_actual[3,1] <- corr_date[1]
  output_actual[3,2] <- corr_date[2]
  
  ####################################################
  ### Ture Positive, Ture Negative, False Postive ####
  ####################################################
  
  # convert data into three groups
  # extract the cols we need to calculate the metrics
  y <- testdat$y_pct
  y_hat <- testdat$y_hat_pct
  y_hat_bsl <- testdat$y_hat_bsl_pct
  ## get 0.2 quantile
  y_bottom <- as.numeric(quantile(y,0.2,na.rm=T))
  y_hat_bottom <- as.numeric(quantile(y_hat,0.2,na.rm=T))
  y_hat_bsl_bottom <- as.numeric(quantile(y_hat_bsl,0.2,na.rm=T))
  ## get 0.8 quantile
  y_top <- as.numeric(quantile(y,0.8,na.rm=T))
  y_hat_top <- as.numeric(quantile(y_hat,0.8,na.rm=T))
  y_hat_bsl_top <- as.numeric(quantile(y_hat_bsl,0.8,na.rm=T))
  
  # bucket them into 3 groups
  y <- ifelse(y>y_top,"top",
              ifelse(y<y_bottom,"bottom","middle"))
  y_hat <- ifelse(y_hat>y_hat_top,"top",
                  ifelse(y_hat<y_hat_bottom,"bottom","middle"))
  y_hat_bsl <- ifelse(y_hat_bsl>y_hat_bsl_top,"top",
                      ifelse(y_hat_bsl<y_hat_bsl_bottom,"bottom","middle"))
  # 4. TP Rate
  m <- table(y_hat,y) # two-way table of y_hat and y
  m_bsl <- table(y_hat_bsl,y) # two-way table of y_hat_bsl and y
  
  # use prop.table() to get the value of each cell divided by the sum of the column cells
  TP <- prop.table(m,2)[1,1] 
  TP_bsl <- prop.table(m_bsl,2)[1,1]
  
  output_dummy[4,1] <- ifelse(TP > 0.2,1,0) 
  output_dummy[4,2] <- ifelse(TP > TP_bsl,1,0) 
  
  output_actual[4,1] <- TP
  output_actual[4,2] <- TP-TP_bsl
  
  # 5. TN Rate
  TN <- prop.table(m,2)[3,3]
  TN_bsl <- prop.table(m_bsl,2)[3,3]
  output_dummy[5,1] <- ifelse(TN > 0.2,1,0) 
  output_dummy[5,2] <- ifelse(TN > TN_bsl,1,0)
  
  output_actual[5,1] <- TN
  output_actual[5,2] <- TN - TN_bsl
  
  # 6. FP Rate
  FP <- prop.table(m,2)[1,3]
  FP_bsl <- prop.table(m_bsl,2)[1,3]
  output_dummy[6,1] <- ifelse(FP < 0.2,1,0) 
  output_dummy[6,2] <- ifelse(FP < FP_bsl,1,0)
  
  output_actual[6,1] <- FP
  output_actual[6,2] <- FP - FP_bsl
  
  ####################################################
  ####################### Ruturn #####################
  ####################################################
  metric_names <- c("spearman correlation average",
                    "spearman correlation consistency - across groups",
                    "spearman correlation consistency - across time",
                    "True Positive Rate",
                    "True Negative Rate",
                    "False Positive Rate")
  col_names <- c("absolute criteria", "baseline relative criteria")
  rownames(output_dummy) <- rownames(output_actual) <- metric_names
  colnames(output_dummy) <- colnames(output_actual) <- col_names
  
  return(list(matrix_dummy = output_dummy,
              matrix_actual = output_actual))

}