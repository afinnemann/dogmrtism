#' Cross validation function for dogmatism analysis
#'
#' This function is designed to test discriminability between two groups based on lingustic features related to dogmatic thinking.
#' It main works through "dog-list-return()" in the the context of the dogmatism analytics shiny app. It will need tweaking to work in different
#' contexts.
#' The analyis is by default a 10 fold cross-validation using a binomial logistic regression. Model performence is measured through mean
#' area-under-the-curve (AUC) of each fold.
#'
#' @param data A data frame
#' @param model_formula A model formula of outcome and predictor to be used as a logistic regression
#' @param outcome_var colum containing the true classes of each observation.
#' @param perc classification percentage, i.e. threshold for predicting classes from the model. This forms the basis of a confusion matrix
#' @param above_perc name assigned when p(x) > perc
#' @param below_perc name assigned when p(x) < perc
#' @param positive Which factor level is the target.
#' @param n_fold number of folds
#'
#' @return returns a 10 by 5 data frame. The five columsn are accuracy, sensitivity,  specificity, auc and fixed effects of each fold.
#' @export
#'
#' @examples
#'

dog_cross_validation <- function(data = da, model_formula, outcome_var ="hashtag",perc = 0.5, above_perc, below_perc, positive, n_fold = 10){

  #creates folds
  data$fold_id <- 1:nrow(data)
  folds <- caret::createFolds(data$fold_id, n_fold)

  #looping over each fold
  cross_val <- sapply(seq_along(folds), function(x) {

    #filter into training and prediction sets
    train_folds = dplyr::filter(data, !(as.numeric(fold_id) %in% folds[[x]]))
    predict_fold = dplyr::filter(data, as.numeric(fold_id) %in% folds[[x]])

    #train model
    train_model <-  glm(model_formula, train_folds ,family="binomial",na.action = na.exclude)

    #create predictions from trained model
    predict_fold <- predict_fold %>%
      dplyr::mutate(predictions_perc = boot::inv.logit(predict(train_model, predict_fold, allow.new.levels = T)), #turn odds into probabilities
                    predictions = ifelse(predictions_perc > perc, above_perc,below_perc), #classify classes based on probabilities
                    predictions = as.factor(predictions))
    #create confusion matrix
    conf_mat <- caret::confusionMatrix(data = predict_fold$predictions, reference = predict_fold[,outcome_var], positive = positive)

    #extract performance measures
    accuracy <- conf_mat$overall[1]
    sensitivity <- conf_mat$byClass[1]
    specificity <- conf_mat$byClass[2]

    #create ROC curve
    predict_fold$class <- as.factor(predict_fold[,outcome_var])
    rocCurve <- pROC::roc(response = predict_fold$class,   predictor = predict_fold$predictions_perc)
    #extract AUC score
    auc = pROC::auc(rocCurve)

    #extract parameter estimates
    fixed_ef <- coef(train_model)

    output <- c(accuracy, sensitivity, specificity, auc, fixed_ef)


  })
  #table of output
  cross_df <- t(cross_val) %>%
    as.data.frame() %>%
    dplyr::rename("auc" ="V4")


  return(cross_df)
}

#' Dogmatism cross validation return
#'
#' dog_list_return returns the results of the dogmatism cross validation in a ggplot2 friendly format. This function is designed to work with the Twitter data frames used
#' in the dogmatism analytics Shiny app.
#'
#' @param da A data frame
#' @param vars a list of variables that will form the predictors in the cross-validation. The will create 1 model for each variable with one predictor. The default of "open_mind" and "close_mind"
#' is compatible with the DF returned by "dogmrtism()".
#' @param outcome_var the name of column storing the outcome variable. In case of tweets, "hashtag".
#' @param above For classification, which class will be assigned for high scores on predictor variables, this is a factor level from "outcome-var".
#' @param below The other factor level in the "outcome var"
#' @param pos What is counted as a positive in the confusion matrix
#' @param n_fold number of folds
#'
#' @return
#' @export
#'
#' @examples
#'  dog_df <- dogmrtism(df, "txt")
#'  cros_val_table <- dog_list_return(df2,
#'                                    vars = c("open_mind", "close_mind")
#'                                    outcome_var = "class",
#'                                    above = "dogmatic",
#'                                    below = "non_dogmatic",
#'                                    pos = "dogmatic")
#'
dog_list_return = function(da, vars = c("open_mind","close_mind"), outcome_var = "hashtag", above, below, pos, n_fold = 10){
  plyr::ldply(seq_along(vars), function(num){

    #extract a predictor from "vars"
    var1 = vars[num]

    #create model formula with outcome and predictor
    mdl_formula <- as.formula(paste(outcome_var, "~", `var1`))

    #run cross validation function
    Result <- dog_cross_validation(data = da,
                                   model_formula = mdl_formula,
                                   perc = 0.5, #classification threshold
                                   above_perc = above,
                                   below_perc = below,
                                   positive = pos,
                                   n_fold = n_fold,
                                   outcome_var = outcome_var)

    #create return table
    Result %>%
      dplyr::summarise(mean_auc = mean(auc),
                       sd_auc = sd(auc)) %>%
      dplyr::mutate(var = var1)


  }
  )
}

