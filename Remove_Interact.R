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
  colnames = colnames(mat) # levels in asset group
  indexes = which(mat >= .6, arr.ind = TRUE)
  n = names(indexes[,1])
  col = rep(0,nrow(indexes))
  for(i in 1:nrow(indexes))
  {
    x = indexes[i,2]
    col[i] =  colnames[x]
  }
  cbind.data.frame(Factor = n, Group = col, Perc_NA = mat[indexes])
}

remove_interact = function(data_merged_w_na,start_date,end_date)
{
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
  NA.region = Get_NA_groups(coverage.region)
  NA.sector = Get_NA_groups(coverage.sector)
  NA.industry_group = Get_NA_groups(coverage.industry_group)
  
  #Combine matrices for region, sector, industry group
  missing = rbind(NA.region,NA.sector,NA.industry_group)
  
  return(missing)
}


