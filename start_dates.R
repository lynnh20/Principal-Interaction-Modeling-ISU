# a vector of start date of train data
start_dates <- function(t0,t1,buffer_length,test_length){
  # t0: start date of the dataset
  # t1: end date of the dataset
  # buffer_length: in weeks
  # test_length: in months
  start_date = ymd(t0)
  current_start_date = ymd(t0)
  test_end_date = start_date + years(10) + weeks(buffer_length) + months(test_length)
  while(test_end_date<t1){
    current_start_date <- current_start_date + months(test_length)
    test_end_date <- current_start_date + years(10) + weeks(buffer_length) + months(test_length)
    start_date <- c(start_date,current_start_date)
  }
  return(start_date)
}