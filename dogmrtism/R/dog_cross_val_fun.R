#' Cross validation function for dogmatism analysis
#'
#' This function is designed to test discriminability between two groups based on lingustic features related to dogmatic thinking.
#' The function is desgined to do its work in the context of the dogmatism analytics shiny app. It will need tweaking to work in different
#' contexts.
#' The analyis is a 10 fold cross-validation using a binomial logistic regression. Model performence is measured through mean
#' area-under-the-curve (AUC) of each fold.
#'
#' @param data A Twitter data frame
#' @param model_formula the model to test
#' @param perc classification percentage, i.e. threshold for when a prediction will be assigned to a specific group
#' @param above_perc name assigned when p(x) > perc
#' @param below_perc name assigned when p(x) < perc
#' @param positive Which factor level is the target.
#'
#' @return returns a 10 by 5 data frame. The five columsn are accuracy, sensitivity,  specificity, auc and fixed effects of each fold.
#' @export
#'
#' @examples
#'

dog_cross_validation <- function(data = da, model_formula, perc = 0.5, above_perc, below_perc, positive, n_fold = 10){


  data$fold_id <- 1:nrow(data)
  folds <- createFolds(data$fold_id, n_fold)

  #running loop
  cross_val <- sapply(seq_along(folds), function(x) {


    train_folds = dplyr::filter(data, !(as.numeric(fold_id) %in% folds[[x]]))
    predict_fold = dplyr::filter(data, as.numeric(fold_id) %in% folds[[x]])

    train_model <-  glm(model_formula, train_folds ,family="binomial",na.action = na.exclude)


    predict_fold <- predict_fold %>%
      dplyr::mutate(predictions_perc = inv.logit(predict(train_model, predict_fold, allow.new.levels = T)),
                    predictions = ifelse(predictions_perc > perc, above_perc,below_perc),
                    predictions = as.factor(predictions))

    conf_mat <- caret::confusionMatrix(data = predict_fold$predictions, reference = predict_fold$hashtag, positive = positive)

    accuracy <- conf_mat$overall[1]
    sensitivity <- conf_mat$byClass[1]
    specificity <- conf_mat$byClass[2]

    predict_fold$class <- as.factor(predict_fold$hashtag)
    rocCurve <- roc(response = predict_fold$class,   predictor = predict_fold$predictions_perc)

    auc = pROC::auc(rocCurve)


    fixed_ef <- coef(train_model)

    output <- c(accuracy, sensitivity, specificity, auc, fixed_ef)


  })

  cross_df <- t(cross_val) %>%
    as.data.frame() %>%
    dplyr::rename("auc" ="V4")


  return(cross_df)
}

#' Dogmatism cross validation return
#'
#' dog_list_return returns the results of the dogmatism cross validation in a ggplot2 friendly format.
#'
#' @param da takes a data frame as it's main argument. This function is designed to worked with Twitter data frame used
#' in the dogmatism analytics Shiny app.
#' @param vars a list of variables that will form the predictors in the cross-validation. For dogmatism analysis this
#' will be close-minded and open-minded words
#' @param outcome_var the name of column storing the outcome variable. Mostly likely "hashtag
#'
#' @return
#' @export
#'
#' @examples
dog_list_return = function(da, vars, outcome_var = "hashtag", above, below, pos, n_fold = 10){
  ldply(seq_along(vars), function(num){

    var1 = vars[num]

    mdl_formula <- as.formula(paste(outcome_var, "~", `var1`))


    Result <- dog_cross_validation(data = da,
                                   model_formula = mdl_formula,
                                   perc = 0.5,
                                   above_perc = above,
                                   below_perc = below,
                                   positive = pos,
                                   n_fold = n_fold)
    Result %>%
      summarise(mean_auc = mean(auc),
                sd_auc = sd(auc)) %>%
      mutate(var = var1)


  }
  )
}
