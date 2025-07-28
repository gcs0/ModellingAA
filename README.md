# ModellingAA: Authorship Attribution Modeling Framework

[![R](https://img.shields.io/badge/R-4.0%2B-blue.svg)](https://www.r-project.org/)
[![Python](https://img.shields.io/badge/Python-3.7%2B-green.svg)](https://www.python.org/)
[![spaCy](https://img.shields.io/badge/spaCy-3.0%2B-orange.svg)](https://spacy.io/)
[![License](https://img.shields.io/badge/License-Academic-lightgrey.svg)]()

A comprehensive framework for authorship attribution using multi-feature analysis and machine learning. This research-grade implementation combines lexical, syntactic, and stylometric features with elastic net regularization for robust author identification in text documents.

## Overview

ModellingAA is designed for computational linguistics and forensic text analysis researchers who need a reliable framework for authorship attribution. The system employs a document-vs-author comparison approach, extracting diverse linguistic features and training logistic regression models with elastic net regularization to identify text authorship.

### Key Features

- **Multi-feature Analysis**: Combines lexical, syntactic (POS tagging), and stylometric measures
- **Punctuation Analysis**: Comprehensive analysis of 13 punctuation types and their relative frequencies  
- **spaCy Integration**: Advanced syntactic analysis using spaCy's en_core_web_sm model
- **Author Profiling**: Aggregates document-level features into author-level signatures
- **Elastic Net Regularization**: Robust model training with cross-validation using glmnet
- **Comprehensive Evaluation**: AUC-ROC, accuracy, confusion matrices, and top-1 accuracy metrics

## Repository Structure

```
ModellingAA/
├── setup.R                           # Dependencies and environment configuration
├── features.R                        # Core feature extraction functions
├── puncmarkFunc.R                     # Punctuation analysis utilities
├── prepare_corpus.R                   # Corpus preparation and splitting
├── split_data.R                       # Author-based data splitting
├── prepare_scaled_sets.R              # Feature scaling and normalization  
├── create_author_level_features.R     # Author profile aggregation
├── ngram_matrix_calc.R                # N-gram similarity calculations
├── greatMerger.R                      # Document-to-author comparison framework
├── train_model.R                      # Model training with elastic net
├── test_evaluate.R                    # Model evaluation and metrics
└── glm_calc.R                         # Additional GLM analysis examples
```

### File Functions

| File | Purpose |
|------|---------|
| `setup.R` | Loads required R packages and initializes spaCy environment |
| `features.R` | Implements `compute_base_features()` and `add_pos_scaled_features()` |
| `puncmarkFunc.R` | Provides `add_punct_counts()` and `add_punct_rel_freq()` functions |
| `prepare_corpus.R` | Handles corpus splitting with `prepare_split_corpora()` |
| `split_data.R` | Author-based data splitting using `split_authors()` |
| `prepare_scaled_sets.R` | Feature standardization via `prepare_scaled_sets()` |
| `create_author_level_features.R` | Author profiling with `create_author_level_features()` |
| `ngram_matrix_calc.R` | Document-author comparison using `prepare_doc_author_comparison()` |
| `greatMerger.R` | Core comparison logic in `compare_doc_to_authors()` |
| `train_model.R` | Model training via `train_logistic_model()` and `prepare_diff_sets()` |
| `test_evaluate.R` | Evaluation functions including `evaluate_model()` and `evaluate_top1_accuracy()` |

## Dependencies

### R Packages
```r
# Core text analysis
library(quanteda)
library(quanteda.textstats) 
library(tidyverse)

# Machine learning
library(glmnet)
library(caret)
library(pROC)

# Performance and parallel processing
library(doParallel)
library(data.table)
library(future)
library(doFuture)
library(progressr)
library(foreach)

# Python integration and utilities
library(spacyr)
library(Metrics)
library(broom)
```

### Python Dependencies
```bash
# spaCy and English model
pip install spacy
python -m spacy download en_core_web_sm
```

## Installation

### 1. R Environment Setup
```r
# Install required R packages
install.packages(c(
  "quanteda", "quanteda.textstats", "tidyverse", "glmnet", 
  "caret", "pROC", "doParallel", "data.table", "future", 
  "doFuture", "progressr", "foreach", "spacyr", "Metrics", "broom"
))
```

### 2. Python and spaCy Setup
```bash
# Install spaCy
pip install spacy

# Download English language model
python -m spacy download en_core_web_sm
```

### 3. Initialize Environment
```r
# Clone and setup
git clone https://github.com/gcs0/ModellingAA.git
cd ModellingAA

# Load setup
source("setup.R")
```

## Usage Guide

### Basic Workflow

#### 1. Environment Setup
```r
# Load all dependencies and configure parallel processing
source("setup.R")
```

#### 2. Data Preparation
```r
# Split authors into train/validation/test sets
source("split_data.R")
splits <- split_authors(your_dataframe, author_col = "From", 
                       min_docs = 20, max_docs = 30)

# Prepare corresponding corpora
source("prepare_corpus.R")
corpus_splits <- prepare_split_corpora(your_corpus, splits)
```

#### 3. Feature Extraction
```r
# Load feature extraction functions
source("features.R")
source("puncmarkFunc.R")

# Extract base features (lexical + punctuation)
train_df <- compute_base_features(corpus_splits$train_corpus, 
                                 train_dfm, splits$train_df)

# Add POS-based syntactic features
train_df <- add_pos_scaled_features(corpus_splits$train_corpus, train_df)
```

#### 4. Feature Scaling and Author Profiling
```r
# Scale features and create author profiles
source("prepare_scaled_sets.R")
source("create_author_level_features.R")

# Define feature set
features <- c("Token", "Alphabetic", "Uppercase", "RelCase", "CTTR",
              "Dot", "Coma", "QMark", "EMark", "Colon", "SemiC",
              "ADJ_scaled", "NOUN_scaled", "VERB_scaled", "ADV_scaled")

scaled_data <- prepare_scaled_sets(splits$train_df, splits$valid_df, 
                                  splits$test_df, features, 
                                  corpus_splits, add_punct_rel_freq)
```

#### 5. Document-Author Comparison
```r
# Create document-author comparison matrices
source("ngram_matrix_calc.R")
source("greatMerger.R")

comparison_df <- compare_doc_to_authors(
  doc_df = scaled_data$P2train_df,
  author_df = scaled_data$P2author_train_df,
  feature_cols = features,
  ngram_sim_matrix = ngram_similarity_matrix
)
```

#### 6. Model Training
```r
# Train elastic net logistic regression
source("train_model.R")

model_result <- train_logistic_model(
  train_df = comparison_df,
  valid_df = validation_comparison_df,
  features = features,
  alpha = 0.8  # Elastic net mixing parameter
)
```

#### 7. Model Evaluation
```r
# Evaluate model performance
source("test_evaluate.R")

evaluation <- evaluate_model(
  model = model_result$model,
  test_df = test_comparison_df,
  glm_formula = model_result$formula,
  lambda = model_result$best_lambda,
  return_preds = TRUE
)

# Print results
print(paste("AUC-ROC:", round(evaluation$auc, 3)))
print(evaluation$confusion)
```

### Advanced Usage

#### Custom Feature Sets
```r
# Define custom feature combinations
lexical_features <- c("Token", "Alphabetic", "RelCase", "CTTR")
punctuation_features <- c("RelDot", "RelComa", "RelQMark", "RelEMark")
syntactic_features <- grep("_scaled$", names(your_df), value = TRUE)

# Combine for comprehensive analysis
all_features <- c(lexical_features, punctuation_features, syntactic_features)
```

#### Cross-Validation Analysis
```r
# Analyze lambda selection
lambda_performance <- model_result$scores
best_lambda_idx <- which.max(lambda_performance$auc)
optimal_lambda <- lambda_performance$lambda[best_lambda_idx]

print(paste("Optimal lambda:", optimal_lambda))
print(paste("Best AUC:", lambda_performance$auc[best_lambda_idx]))
```

## Methodology

### Feature Engineering

#### Lexical Features
- **Token Count**: Total tokens with and without punctuation
- **Alphabetic Characters**: Letter count and uppercase ratio
- **Type-Token Ratio**: Corrected TTR (CTTR) capped at 12 for stability
- **Case Analysis**: Relative frequency of uppercase characters

#### Punctuation Analysis
The framework analyzes 13 punctuation types:
- Periods, commas, question marks, exclamation marks
- Quotation marks (single and double), colons, semicolons
- Dashes, slashes, ampersands, brackets, angle brackets

Each punctuation type is measured in absolute counts and relative frequencies (normalized by total tokens).

#### Syntactic Features (POS Tagging)
Using spaCy's `en_core_web_sm` model:
- Part-of-speech tag frequencies (ADJ, NOUN, VERB, ADV, etc.)
- Scaled by document length for cross-document comparison
- Multithread processing for efficiency with large corpora

### Classification Approach

#### Document-vs-Author Framework
1. **Author Profiling**: Aggregate document features by author
2. **Similarity Computation**: Calculate absolute differences between document and author features
3. **N-gram Integration**: Incorporate n-gram similarity scores
4. **Binary Classification**: Predict whether document matches candidate author

#### Model Architecture
- **Algorithm**: Logistic regression with elastic net regularization
- **Regularization**: L1 + L2 penalty with mixing parameter α = 0.8
- **Cross-Validation**: Automated lambda selection using AUC optimization
- **Feature Scaling**: Standardization (center and scale) applied to all numeric features

### Performance Metrics

#### Primary Metrics
- **AUC-ROC**: Area under ROC curve for binary classification performance
- **Accuracy**: Overall classification accuracy
- **Confusion Matrix**: Detailed classification breakdown

#### Specialized Metrics
- **Top-1 Accuracy**: Accuracy when selecting highest-probability author per document
- **Log-Loss**: Probabilistic loss function for model calibration
- **Cross-Validation AUC**: Performance across multiple data folds

#### Evaluation Protocol
1. Author-based data splitting (70% train, 28.9% validation, 1.1% test)
2. Feature standardization using training set statistics
3. Lambda optimization on validation set
4. Final evaluation on held-out test set

## Performance Considerations

- **Parallel Processing**: Configured for 8-core parallel execution
- **Memory Management**: Efficient handling of large text corpora
- **spaCy Configuration**: Increased max_length to 3,000,000 characters
- **Reproducibility**: Fixed random seed (1923) for consistent results

## Citation

### Software Citation
```bibtex
@software{modellingaa2024,
  title = {ModellingAA: Authorship Attribution Modeling Framework},
  author = {[Author Names]},
  year = {2024},
  url = {https://github.com/gcs0/ModellingAA},
  note = {R package for computational authorship attribution}
}
```

### Research Paper Citation
```bibtex
@article{[citation_key],
  title = {[Paper Title]},
  author = {[Author Names]},
  journal = {[Journal Name]},
  year = {[Year]},
  note = {Associated research paper - citation to be updated upon publication}
}
```

## Contributing

This is a research project designed for computational linguistics and forensic text analysis. For questions about methodology or implementation details, please refer to the associated research paper or contact the authors.

## License

This project is intended for academic and research purposes. Please cite appropriately when using this framework in your research.

---

**Note**: This framework is designed for research reproducibility in computational linguistics and forensic text analysis. The implementation follows academic standards for transparent and replicable authorship attribution research.