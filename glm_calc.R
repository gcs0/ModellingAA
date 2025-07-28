# Ensure label is binary (as.factor is optional for glm, but good for clarity)
ZValid_diff2$label <- as.factor(ifelse(ZValid_diff2$doc_From == ZValid_diff2$author_From, 1, 0))
filteredFeatures<- features[!features %in% c("RelDisc", "RelDQuote", "RelComa", "RelQMark", "RelSemiC", "RelAnd", "RelBracket")]
# Build formula: label ~ all features + ngram_sim
glm_formula <- as.formula(
  paste("label ~", paste(c(filteredFeatures, "ngram_sim"), collapse = " + "))
)
glm_formula2 <- as.formula(
  paste("label~", paste("(",paste(c(features, "ngram_sim"), collapse = " + "), ")^2"))
)
glm_formula_ngramless <- as.formula(
  paste("label ~", paste(c(features), collapse = " + "))
)
# Train logistic regression model (binomial family = logistic)
glm_model<- glm(glm_formula2, data = Zdiff_df, family = binomial(), )

# View model summary
summary(glm_model)
pred_probs <- predict(glm_model, type = "response", newdata = Tmini_diff)
pred_probs <- predict(glm_model, type = "response", newdata = Zdiff_df)

# Convert predictions to binary (thresholding at 0.5)
pred_class <- ifelse(Zdiff_df_max$pred_prob> 0.25, 1, 0)

# Plot ROC curve
roc_curve_2 <- roc(Zdiff_df$label, pred_probs)  # Assuming your labels are in the 'label' column
plot(roc_curve_2, main = "ROC Curve")

# AUC value
auc(roc_curve_2)

confusion_matrix <- confusionMatrix(as.factor(pred_class), as.factor(Zdiff_df_max$label))

# Print Confusion Matrix and performance metrics
print(confusion_matrix)


Zdiff_df$pred_prob <- pred_probs

# Find the author with the maximum probability for each document
# We do this by first reshaping the data so we can apply the max function for each document
Zdiff_df_max <- Zdiff_df%>%
  group_by(doc_id) %>%
  top_n(1, pred_prob) %>%
  ungroup()

# Now Zdiff_df_max will contain the author with the highest probability for each document
# You can assign the predicted author label based on the highest score
Zmini_diff_max$predicted_author <- Zmini_diff_max$author_From

# Check the resulting predictions
head(Zmini_diff_max)
Zmini_diff_max$actual_author <- Zmini_diff_max$doc_From

# Calculate accuracy
accuracy <- mean(Zmini_diff_max$predicted_author == Zmini_diff_max$actual_author)
cat("Accuracy:", accuracy, "\n")

# Generate confusion matrix
all_authors <- union(levels(as.factor(Zdiff_df_max$predicted_author)), levels(as.factor(Zdiff_df_max$actual_author)))

# Generate confusion matrix using the 'labels' argument
conf_matrix <- confusionMatrix(
  as.factor(Zdiff_df_max$predicted_author), 
  as.factor(Zdiff_df_max$actual_author),
  levels = all_authors  # Ensure the levels are consistent
)

# Print the confusion matrix
print(conf_matrix)
