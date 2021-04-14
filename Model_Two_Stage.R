
Interaction_Model <- function(train_set,test_set,para){
  ##### Fit Two Stage Lasso on train_set ##############
  ana.data <- Two_stage_preprocess(train_set,"FUT_12W_RLT_RET")
  # No missing data is required
  ana.data <- ana.data[complete.cases(ana.data),]
  
  ###################  Stage 1 ######################
  # only LOI_ would be put as covariates
  X1 <- ana.data %>% select(starts_with(c("LOI_"))) # all LOI_ 
  Y <- ana.data[,1] # according to the return of Two_stage_preprocess(), y is in the first col
  fit1 <- coef(glmnet(as.matrix(X1),Y,lambda=para[1]))
  fit1 <- as.data.frame(as.matrix(fit1))
  fit1[,"variable"]<- rownames(fit1)
  Stage2_vari <- fit1[which(abs(fit1$s0)>0),"variable"] # choose variables that has coef != 0
  Stage2_LOI <- Stage2_vari[-1] # remove the intercept 
  
  ################## Stage 2 ##########################
  if(identical(Stage2_LOI,character(0))){
    print("No significant LOI_ main effect, please select a smaller lambda")
  }else{
    cols <- colnames(ana.data)
    factor_names <- cols[grepl('^LOI_', cols)] # all LOI_name
    Stage2_group <- setdiff(cols,c("Y",factor_names)) # all asset groups
    Interaction_term <- expand.grid(Factor = Stage2_LOI, Group = Stage2_group)
    Interaction_term <- setdiff(Interaction_term,remove_interact) # remove those has low coverage 
    inter_names <- paste0(Interaction_term$Factor,"*",Interaction_term$Group)
    
    V1 <- Stage2_LOI[1]
    inter_dat <- ana.data[,V1]* ana.data[,Stage2_group]
    colnames(inter_dat) <- paste0(V1,"*",Stage2_group)
    for(i in 2:length(Stage2_LOI)){
      V1 <- Stage2_LOI[i]
      temp <- ana.data[,V1]* ana.data[,Stage2_group]
      colnames(temp) <- paste0(V1,"*",Stage2_group)
      inter_dat <- cbind(inter_dat,temp)
    }
    # get the subset of inter_dat 
    inter_dat <- inter_dat[,inter_names]
    X2 <- cbind(ana.data[,Stage2_LOI],inter_dat)
    m_lasso <- glmnet(as.matrix(X2),Y,lambda=para[2])
    fit2 <- coef(m_lasso)
    fit2 <- as.data.frame(as.matrix(fit2))
    fit2[,"variable"]<- rownames(fit2)
    Inter_vari <- fit2[which(abs(fit2$s0)>0),"variable"]
    Inter_vari <- Inter_vari[-1]
    # significant interaction terms in step 2
    Inter_vari <- Inter_vari[Inter_vari %in% inter_names] 
    
    if(identical(Inter_vari,character(0))){
      print("No significant interaction terms in stage 2, please select a smaller lambda")
    }else{
      ################ Prepare test dat ####################
      # test dat should have same format as X2 in stage 2
      ana.test <- Two_stage_preprocess(test_set,"FUT_12W_RLT_RET")
      # ana.test <- ana.data[complete.cases(ana.data),]
      # create inter_dat for test_data give Interaction_term
      V1 <- Stage2_LOI[1]
      inter_dat_test <- ana.test[,V1]* ana.test[,Stage2_group]
      colnames(inter_dat_test) <- paste0(V1,"*",Stage2_group)
      for(i in 2:length(Stage2_LOI)){
        V1 <- Stage2_LOI[i]
        temp <- ana.test[,V1]* ana.test[,Stage2_group]
        colnames(temp) <- paste0(V1,"*",Stage2_group)
        inter_dat_test <- cbind(inter_dat_test,temp)
      }
      # get the subset of inter_dat 
      inter_dat_test <- inter_dat_test[,inter_names]
      X_test <- cbind(ana.test[,Stage2_LOI],inter_dat_test)
      test_set$y_hat <- predict(m_lasso, newx = as.matrix(X_test))
      test_set <- test_set %>%
        rename(y = FUT_12W_RLT_RET)
      return(list(test_set,Inter_vari))  
    }
  }
}