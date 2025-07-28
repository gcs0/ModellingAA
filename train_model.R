# 06_train_model.R

prepare_diff_sets <- function(train_df, valid_df, test_df, features, ngramScaler = NULL) {
  for (df_name in c("train_df", "valid_df", "test_df")) {
    df <- get(df_name)
    df[, features] <- abs(df[, features])
    if (!is.null(ngramScaler)) {
      df <- cbind(predict(ngramScaler, df[, "ngram_sim", drop = FALSE]),
                  df[, setdiff(names(df), "ngram_sim")])
    }
    df$label <- as.factor(ifelse(df$doc_From == df$author_From, 1, 0))
    assign(paste0("P2DIFF", toupper(gsub("_df", "", df_name))), df, envir = .GlobalEnv)
  }
}

train_logistic_model <- function(train_df, valid_df, features, alpha = 0.8) {
  glm_formula <- as.formula(
    paste("label ~", paste(c(features, "ngram_sim"), collapse = " + "))
  )
  x <- model.matrix(glm_formula, data = train_df)[, -1]
  y <- as.numeric(train_df$label) - 1
  
  x_valid <- model.matrix(glm_formula, data = valid_df)[, -1]
  y_valid <- as.numeric(valid_df$label) - 1
  
  cv_model <- cv.glmnet(x, y, family = "binomial", alpha = alpha, standardize = FALSE, type.measure = "auc")
  
  # Evaluate multiple lambdas
  lambda_values <- cv_model$lambda
  scores <- data.frame(lambda = lambda_values, logloss = NA, auc = NA)
  for (i in seq_along(lambda_values)) {
    pred_prob <- predict(cv_model, newx = x_valid, s = lambda_values[i], type = "response")
    scores$logloss[i] <- logLoss(y_valid, pred_prob)
    scores$auc[i]     <- auc(y_valid, pred_prob)
  }
  
  best_auc_lambda <- scores$lambda[which.max(scores$auc)]
  
  return(list(
    model = cv_model,
    formula = glm_formula,
    best_lambda = best_auc_lambda,
    scores = scores
  ))
}
