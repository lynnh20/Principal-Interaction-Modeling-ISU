get_na = function(X){
  mean(is.na(X))
}


NA_coverage = function(X, group)
{
  Temp = X %>% select(group, starts_with("LOI_"))
  
  rownames = colnames(Temp)
  names = levels(Temp[,1]) # level in the asset group
  nrow = ncol(Temp)
  ncol = nlevels(Temp[,1]) # length of names
  coverage.matrix = matrix(0, nrow, ncol)
  
  for(i in 1:ncol){
    Y = Temp %>% filter(Temp[,1] == names[i])
    coverage.matrix[,i] = apply(Y,2,get_na)
  }
  
  colnames(coverage.matrix) = names
  rownames(coverage.matrix) = rownames
  return(coverage.matrix)
}


Get_NA_groups = function(mat)
{
  reshape_mat <- setNames(reshape2::melt(mat), c('Factor', 'Group', 'Perc_NA'))
  reshape_mat <-reshape_mat %>% filter(Perc_NA > 0.6)
  reshape_mat %>% select(Factor,Group)
}

remove_interact = function(data_merged_w_na,start_date,end_date){
  # Function to obtain which factors have insufficient coverage for a given time window
  # Args:  
  # data_merged_w_na: a data.frame
  # start_date: the first date in the desired window
  # end_date: the last date in the desired window
  # Returns:
  # A csv containing all LOI with more than 60% missing for a specific asset group given the data_merged
  
  start_date <- ymd(start_date)
  end_date <- ymd(end_date)
  window <- data_merged_w_na %>% filter(DATE > start_date & DATE < end_date)
  #Get coverage of each LOI for each asset group
  coverage.region = NA_coverage(window,"REGION")
  coverage.sector = NA_coverage(window,"SECTOR")
  coverage.industry_group = NA_coverage(window,"INDUSTRY_GROUP")
  
  #Get which LOI have >60% missing for each asset group
  
  #Combine matrices for region, sector, industry group
  missing = rbind(Get_NA_groups(coverage.region),
                  Get_NA_groups(coverage.sector),
                  Get_NA_groups(coverage.industry_group))
  
  return(missing)
}


