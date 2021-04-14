GetTR <- function(valid_dat){
  # Cal the sum of TPR and TNR 
  # Arg:
  # valid_dat: a dataframe object contains cols: y, y_hat and DATE
  # Return:
  # the sum of True Postive Rate and Ture Negative Rate
  
  
  # percentile rank the y, y_hat, y_hat_bsl
  valid_dat <- valid_dat %>% group_by(DATE) %>%
    mutate(y = percent_rank(y),
           y_hat = percent_rank(y_hat))
  # extract the cols we need to calculate the metrics
  y <- valid_dat$y
  y_hat <- valid_dat$y_hat
  
  # convert data into three groups
  ## bottom
  y_bottom <- as.numeric(quantile(y,0.2,na.rm=T))
  y_hat_bottom <- as.numeric(quantile(y_hat,0.2,na.rm=T))
  ## top
  y_top <- as.numeric(quantile(y,0.8,na.rm=T))
  y_hat_top <- as.numeric(quantile(y_hat,0.8,na.rm=T))
  
  y <- ifelse(y>y_top,"top",
              ifelse(y<y_bottom,"bottom","middle"))
  y_hat <- ifelse(y_hat>y_hat_top,"top",
                  ifelse(y_hat<y_hat_bottom,"bottom","middle"))
  
  # TP Rate
  m <- table(y_hat,y)
  TP <- prop.table(m,2)[1,1]
  # TN Rate
  TN <- prop.table(m,2)[3,3]
  
  return(TP+TN)
  
}