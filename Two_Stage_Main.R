option_list <- list(
  optparse::make_option("--i", default = 1,
                        help = "different window [default %default].")
)
opt_parser <- optparse::OptionParser(option_list = option_list)
opt <- optparse::parse_args(opt_parser)

# load packages
suppressMessages(library(stringr))
suppressMessages(library(simputation))
suppressMessages(library(mltools))
suppressMessages(library(glmnet))
suppressMessages(library(reshape2))
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(lubridate))
suppressMessages(library(data.table))

# import other functions
source(file = "./Data_process.R")
source(file = "./Get_train_test.R")
source(file = "./Remove_Interact.R")
source(file = "./GetTR.R")
source(file = "./Cal_Metrics.R")
source(file = "./Two_Stage_preprocess.R")
source(file = "./Model_Two_Stage.R")
source(file = "./start_dates.R")
source(file = "./Remove_Interact.R")

# read data
zz = gzfile("./Data/capstone_data_numeric_US")
data_numeric <- read.csv(zz,sep='\t')
zz = gzfile("./Data/capstone_data_categorical_US")
data_categorical <- read.csv(zz,sep="\t")

# get the merged data
data_merged <- data_process(data_numeric,data_categorical)
data_merged_w_na <- data_process_no_impute(data_numeric,data_categorical)

# define the reponse variable
response <- "FUT_12W_RET_pct"
# define the window size
train_length <- 10 # in years
# get the buffer length: extract the number in response
buffer_length <- as.numeric(gsub(".*?([0-9]+).*", "\\1", response)) # in weeks
test_length <- 12 # in months
# define the asset group
asset_group <- "INDUSTRY_GROUP"
# define possible parameters
# paras <- expand.grid(para1 = c(0.001,0.005,0.01,0.015), 
#                      para2 = c(0.005,0.01,0.02))

# paras <- expand.grid(para1 = c(0.0005,0.001,0.005,0.01), 
#                      para2 = c(0.001,0.003,0.005,0.01))

# paras <- expand.grid(para1 = c(0.001,0.005,0.01),
#                      para2 = c(0.005,0.01,0.02))

# paras <- expand.grid(para1 = c(0.001,0.005,0.01),
#                      para2 = c(0.004,0.005,0.01))

paras <- expand.grid(para1 = c(0.001,0.002,0.003,0.004,0.005),
                     para2 = c(0.001,0.002,0.003))

# get all start dates
t0 <- min(data_merged$DATE)
t1 <- max(data_merged$DATE)
startdates <- start_dates(t0,t1,train_length,buffer_length,test_length)


# one window
i=opt$i
start_date_t0 <- startdates[i]
# get train dat and test dat
datset_list <- Get_train_test_rolling_CV(data_merged,start_date_t0,
                                         train_length,buffer_length,test_length)
train_set <- datset_list[[1]]
test_set <- datset_list[[2]]


# missing summary
start_date <- min(train_set$DATE)
end_date <- max(test_set$DATE)
remove_terms <- remove_interact(data_merged_w_na,start_date,end_date)


TR_para <- c()  # collect the (True Positive Rate + True Negative Rate) under each parameter 
nrFolds <- 10
folds <- rep_len(1:nrFolds, nrow(train_set)) # 10-fold index
# t0 <- min(train_set$DATE)
# t1 <- max(train_set$DATE)
# tsCV_date <- start_dates(t0,t1,train_length/2,buffer_length,test_length/2)
# nrFolds <- length(tsCV_date)
for(r in 1:nrow(paras)){
  TR_fold <- c() # collect the TPR in 10 folds when they treated as validation set
  for(k in 1:nrFolds) {
    # datset_list <- Get_train_test_rolling_CV(train_set,tsCV_date[k],train_length/2,buffer_length,test_length/2)
    # data.train <- datset_list[[1]]
    # data.valid <- datset_list[[2]]
    fold <- which(folds == k)
    data.train <- train_set[-fold,]
    data.valid <- train_set[fold,]
    data.valid <- Interaction_Model(data.train,data.valid,paras[r,1:2],remove_terms,response,asset_group)[[1]] 
    if(is.character(data.valid)){
      TR_fold <- c(TR_fold,0)
    }else{
      TR_fold <- c(TR_fold,GetTR(data.valid))
    }
    
  }
  TR_para <- c(TR_para,mean(TR_fold)) # use mean from 10-fold to represent the performance under the 'para' in this loop
}
# get the best parameter 
paras$res <- TR_para
max_TR <- max(TR_para)
paras <- paras %>% filter(res > max_TR-0.01)
paras <-  paras %>% mutate(sum = para1 + para2)
paras <- paras %>% arrange(desc(sum))
best_para <- as.numeric(paras[1,c("para1","para2")])  

# fit the model on the whole train_set and get a new test_set with y_hat col
res <- Interaction_Model(train_set,test_set,best_para,remove_terms,response,asset_group)
test_set <- res[[1]]  
metrics_eval <- Metrics_testdat(test_set)
# output for each window
# 1: test dat with predicted y
test_set_name <- paste0(min(test_set$DATE),"_",max(test_set$DATE))
write.csv(test_set,paste0('./Output/Predicted/y_hat_',test_set_name,".csv"))
# 2: coef of the model from stage 2
write.csv(res[[2]],paste0('./Output/Interactions/coef_',test_set_name,".csv"))
# 3: output the metrics
write.csv(cbind(metrics_eval[[1]],metrics_eval[[2]]),paste0('./Output/Evaluation/evaluation_',test_set_name,".csv"))
# 4: a txt file contained the other information
str1 <- paste0("Train Data:",start_date_t0," ~ ",max(train_set$DATE))
str2 <- paste0("Test Data:",min(test_set$DATE)," ~ ",max(test_set$DATE))
str3 <- paste0("Best para in stage 1:",best_para[1])
str4 <- paste0("Best para in stage 2:",best_para[2])
sink(paste0('./Output/txtfiles/info_',test_set_name,".txt"))
cat(str1)
cat("\n")
cat(str2)
cat("\n")
cat(str3)
cat("\n")
cat(str4)
sink()

