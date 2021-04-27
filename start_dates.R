start_dates <- function(t0,t1,train_length,buffer_length,test_length){
  # Get a vector of start_dates 
  #
  # Arg:
  #  t0: start date of the whole dataset
  #  t1: end date of the whole dataset
  #  train_length: length of train dataset
  #  buffer_length: ilength of buffer in(in weeks), consistent with out response variable
  #  test_length: length of test dat(in months), currently we use 1 year/ 12 months
  # 
  # Return:
  #  a vector. start dates for each (tain/buffer/test) window

  start_date = ymd(t0)
  # record the corrent start date in the loop
  current_start_date = ymd(t0)
  test_end_date = start_date + years(train_length) + months(test_length) + weeks(buffer_length)
  # loop until we reach the end date
  while(test_end_date < t1){
    current_start_date <- current_start_date + months(test_length)
    test_end_date <- current_start_date + years(train_length) + months(test_length) + weeks(buffer_length)
    start_date <- c(start_date,current_start_date)
  }
  return(start_date)
}
