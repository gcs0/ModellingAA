# 07_test_evaluate.R

evaluate_model <- function(model, test_df, glm_formula, lambda, return_preds = FALSE) {
  x_test <- model.matrix(glm_formula, data = test_df)[, -1]
  y_test <- as.numeric(test_df$label) - 1
  
  pred_probs <- predict(model, type = "response", newx = x_test, s = lambda)
  pred_class <- ifelse(pred_probs > 0.5, 1, 0)
  
  roc_curve <- pROC::roc(test_df$label, pred_probs)
  auc_value <- pROC::auc(roc_curve)
  
  confusion <- caret::confusionMatrix(factor(pred_class), factor(test_df$label))
  
  if (return_preds) {
    test_df$pred_prob <- pred_probs
    return(list(auc = auc_value, confusion = confusion, roc = roc_curve, preds = test_df))
  } else {
    return(list(auc = auc_value, confusion = confusion, roc = roc_curve))
  }
}

evaluate_top1_accuracy <- function(test_df_with_probs) {
  top1 <- test_df_with_probs %>%
    group_by(doc_id) %>%
    top_n(1, pred_prob) %>%
    ungroup()
  
  pred_class <- ifelse(top1$pred_prob > 0.5, 1, 0)
  confusion <- caret::confusionMatrix(factor(pred_class), factor(top1$label))
  return(confusion)
}
