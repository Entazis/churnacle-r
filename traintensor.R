
  # Load libraries
  library(keras)
  library(lime)
  library(tidyquant)
  library(rsample)
  library(recipes)
  library(yardstick)
  library(corrr)
  library(parsedate)
  
  source('readfiles.R')
  readFiles()
  source('preparedata3.R')
  source('processcentral3.R')
  source('processorange3.R')
  prepareData3()
  
  # Investigate correlations
  user_next_all3_p2 <- user_next_all3[-(1:79),]
  row.names(user_next_all3_p2) <- 1:nrow(user_next_all3_p2)
  user_next_all3_p2 <- user_next_all3_p2[complete.cases(user_next_all3_p2),]
  user_next_all3_p2 <- user_next_all3_p2 %>%
    select(-email) %>%
    select(-endDate) %>%
    drop_na() %>%
    select(paidnext, everything())
  user_next_all3_p2 <- cbind(as.data.frame(as.numeric(as.factor(user_next_all3_p2[,1]))-1),as.data.frame(sapply(user_next_all3_p2[,-1], as.numeric)))
  colnames(user_next_all3_p2)[1] <- "paidnext"
  correlations <- user_next_all3_p2 %>%
    correlate() %>%
    focus(paidnext) %>%
    fashion()
  correlations <- correlations[rev(order(correlations$paidnext)),]
  row.names(correlations) <- 1:nrow(correlations)
  
  # Remove unnecessary data
  user_next_all3_p <- user_next_all3[-(1:79),]
  row.names(user_next_all3_p) <- 1:nrow(user_next_all3_p)
  user_next_all3_p <- user_next_all3_p[complete.cases(user_next_all3_p),]
  user_next_all3_p <- user_next_all3_p %>%
    select(-email) %>%
    select(-endDate) %>%
    drop_na() %>%
    select(paidnext, correlations$rowname) #everything()
  churn_data_tbl <- user_next_all3_p[,1:50]
  
  # Split test/training sets
  set.seed(100)
  train_test_split <- initial_split(churn_data_tbl, prop = 0.9)
  
  # Retrieve train and test sets
  train_tbl <- training(train_test_split)
  test_tbl  <- testing(train_test_split) 

  # Create recipe
  rec_obj <- recipe(paidnext ~ ., data = train_tbl) %>%  
    #step_log(period) %>% 
    #step_log(spent) %>%
    #step_log(cost) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_center(all_predictors(), -all_outcomes()) %>%
    step_scale(all_predictors(), -all_outcomes()) %>%
    prep(data = train_tbl)

  # Predictors
  x_train_tbl <- bake(rec_obj, newdata = train_tbl) %>% select(-paidnext)
  x_test_tbl  <- bake(rec_obj, newdata = test_tbl) %>% select(-paidnext)
  
  # Response variables for training and testing sets
  y_train_vec <- ifelse(pull(train_tbl, paidnext) == "yes", 1, 0)
  y_test_vec  <- ifelse(pull(test_tbl, paidnext) == "yes", 1, 0)
  
  # Building our Artificial Neural Network
  model_keras <- keras_model_sequential()
  
  model_keras %>% 
    
    # First hidden layer
    layer_dense(
      units              = FLAGS$dense1, 
      kernel_initializer = "uniform", 
      activation         = "relu", 
      input_shape        = ncol(x_train_tbl)) %>% 
    
    # Dropout to prevent overfitting
    layer_dropout(rate = FLAGS$dropout1) %>%
    
    # Second hidden layer
    layer_dense(
      units              = FLAGS$dense2, 
      kernel_initializer = "uniform", 
      activation         = "relu") %>% 
    
    # Dropout to prevent overfitting
    layer_dropout(rate = FLAGS$dropout2) %>%
    
    # Output layer
    layer_dense(
      units              = 1, 
      kernel_initializer = "uniform", 
      activation         = "sigmoid") %>% 
    
    # Compile ANN
    compile(
      optimizer = 'adam',
      loss      = 'binary_crossentropy',
      metrics   = c('accuracy')
    )

  # Fit the keras model to the training data
  history <- fit(
    object           = model_keras, 
    x                = as.matrix(x_train_tbl), 
    y                = y_train_vec,
    batch_size       = FLAGS$batchsize, 
    epochs           = FLAGS$epochs,
    #callbacks        = callback_early_stopping(patience = FLAGS$patience, monitor = 'loss'),
    validation_split = FLAGS$validationsplit
  )
  
  # Print a summary of the training history
  print(history)
  
  # Plot the training/validation history of our Keras model
  plot(history) 
  
  # Predicted Class
  yhat_keras_class_vec <- predict_classes(object = model_keras, x = as.matrix(x_test_tbl)) %>%
    as.vector()
  
  # Predicted Class Probability
  yhat_keras_prob_vec  <- predict_proba(object = model_keras, x = as.matrix(x_test_tbl)) %>%
    as.vector()
  
  # Format test data and predictions for yardstick metrics
  estimates_keras_tbl <- tibble(
    truth      = as.factor(y_test_vec) %>% fct_recode(yes = "1", no = "0"),
    estimate   = as.factor(yhat_keras_class_vec) %>% fct_recode(yes = "1", no = "0"),
    class_prob = yhat_keras_prob_vec
  )
  
  #print(estimates_keras_tbl)
  estimates_keras_tbl$truth <- factor(estimates_keras_tbl$truth, levels=c("no","yes"))
  estimates_keras_tbl$estimate <- factor(estimates_keras_tbl$estimate, levels=c("no","yes"))
  
  options(yardstick.event_first = FALSE)
  
  estimates_keras_tbl %>% conf_mat(truth, estimate) %>% print()
  
  print(paste("Accuracy: ", estimates_keras_tbl %>% metrics(truth, estimate)))
  print(paste("AUC: ", estimates_keras_tbl %>% roc_auc(truth, class_prob)))
  print(paste("Precision: ", estimates_keras_tbl %>% precision(truth, estimate)))
  print(paste("Recall: ", estimates_keras_tbl %>% recall(truth, estimate)))
  print(paste("F1-Statistic", estimates_keras_tbl %>% f_meas(truth, estimate, beta = 1)))
  

  
