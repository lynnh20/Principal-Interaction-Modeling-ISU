Get_train_test_rolling_CV <- function(data,start_date, buffer_length, test_length)
{
  ## data: A matrix containing preprocessed data for an interaction model.
  ## Note that the data matrix must contain a date column
  ## start_date: a characater variable of the date in the form "yyyy-mm-dd"
  ## buffer_length: desired buffer window; matches number of weeks in type of future returns being predicted
  
  train_start_date = ymd(start_date)
  train_end_date = train_start_date + years(10)
  
  
  test_start_date = train_end_date + weeks(buffer_length)
  test_end_date = test_start_date + months(test_length)
  
  # Get initial train set
  train_set = data %>% filter(DATE >= train_start_date, DATE<=train_end_date)
  
  # if the latest date in the train set is before the desired end date; include one more week
  if(max(train_set$DATE) < train_end_date){
    train_set = data %>% filter(DATE >= train_start_date, DATE<=train_end_date+weeks(1))
  }
  # Get initial test set
  test_set = data %>% filter(DATE >= test_start_date, DATE <= test_end_date)
  
  # if the latest date in the test set is before the desired end date; include one more week
  if(max(test_set$DATE) < test_end_date){
    test_set = data %>% filter(DATE >= test_start_date, DATE<=test_end_date+weeks(1))
  }
  
  # Return a list containing: 
  # 1. The training set 
  # 2. The testing set
  return(list(train_set, test_set))
}