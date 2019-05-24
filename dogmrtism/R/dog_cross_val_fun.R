
dog_cross <- function(data = da, folds_col, model_formula, perc = 0.5, above_perc, below_perc, positive){ 
  
  
  folds <- createFolds(folds_col, 15)
  
  #running loop
  cross_val <- sapply(seq_along(folds), function(x) {
    
    train_folds = filter(data, !(as.numeric(post_id) %in% folds[[x]]))
    predict_fold = filter(data, as.numeric(post_id) %in% folds[[x]])
    
    train_model <-  glm(model_formula, train_folds ,family="binomial")
    
    
    predict_fold <- predict_fold %>% 
      mutate(predictions_perc = inv.logit(predict(train_model, predict_fold, allow.new.levels = T)),
             predictions = ifelse(predictions_perc > perc, above_perc,below_perc),
             predictions = as.factor(predictions))
    
    conf_mat <- caret::confusionMatrix(data = predict_fold$predictions, reference = predict_fold$class, positive = positive) 
    
    accuracy <- conf_mat$overall[1]
    sensitivity <- conf_mat$byClass[1]
    specificity <- conf_mat$byClass[2]
    
    predict_fold$class <- as.factor(predict_fold$class)
    rocCurve <- roc(response = predict_fold$class,   predictor = predict_fold$predictions_perc)
    
    auc = pROC::auc(rocCurve) 
    
    
    fixed_ef <- coef(train_model) 
    
    output <- c(accuracy, sensitivity, specificity, auc, fixed_ef)
    
    
  })
  
  cross_df <- t(cross_val) %>% 
    as.data.frame() %>% 
    rename("auc" ="V4")
  
  
  return(cross_df)
}
