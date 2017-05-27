macro = fread('macro.csv', sep=',', na.strings = 'NA')
test = fread('test.csv', sep=',',na.strings = 'NA')
train = fread('train.csv', sep=',', na.strings = 'NA')

macro$timestamp = as.Date(macro$timestamp, format='%Y-%m-%d')
train$timestamp = as.Date(train$timestamp, format='%Y-%m-%d')

macro_numeric = sapply(macro, is.numeric)

macro_with_prc = right_join(macro, train[,c('price_doc', 'timestamp')])



  id_test = test$id
  
  y_train <- macro_with_prc$price_doc
  
  x_train <- subset(macro_with_prc, select = -c(timestamp, price_doc))
  x_test <- subset(test, select = -c(id, timestamp))
  
  len_train <- nrow(x_train)
  len_test <- nrow(x_test)
  
  train_test <- rbind(x_train, x_test, fill=TRUE)
  
  features <- colnames(train_test)
  
  for (f in features) {
    if ((class(train_test[[f]])=="factor") || (class(train_test[[f]])=="character")) {
      #cat("VARIABLE : ",f,"\n")
      levels <- unique(train_test[[f]])
      train_test[[f]] <- as.numeric(factor(train_test[[f]], levels=levels))
    }
  }
  
  x_train = train_test[1:len_train,]
  x_test = train_test[(len_train+1):(len_train+len_test),]
  
  dtrain = xgb.DMatrix(as.matrix(x_train), label=log(y_train+1))
  dtest = xgb.DMatrix(as.matrix(x_test))
  
  xgb_params = list(
    seed = 0,
    colsample_bytree = 0.7,
    subsample = 0.7,
    eta = 0.075,
    objective = 'reg:linear',
    max_depth = 6,
    num_parallel_tree = 1,
    min_child_weight = 1,
    base_score = 15.8123
  )
  
  #res = xgb.cv(xgb_params,
  #             dtrain,
  #             nrounds=2000,
  #             nfold=10,
  #             early_stopping_rounds=20,
  #             print_every_n = 10,
  #             verbose= 1,
  #             maximize=F)
  
  #best_nrounds = res$best_iteration
  best_nrounds = 145
  
  gbdt = xgb.train(xgb_params, dtrain, best_nrounds)
  
  prediction <- predict(gbdt,dtest)
  #sample_submission$price_doc <- as.integer(exp(prediction)-1) / 10000) * 10000 # ?? round to thousands
  sample_submission$price_doc <- exp(prediction)-1 # ?? round to thousands
  
  write.csv(sample_submission, "xgb_out.csv", row.names = F, quote = FALSE)
  