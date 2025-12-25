# scRNA-seq
The repository contains source code and a report on biological data analysis (scRNA-Seq), as well as an interactive web application written in R Shiny. The script and report focus on the analysis of the explanatory variable **CD36** (gene expression) based on a dataset of 6,800 cells and the expression levels of 9,000 genes.

- distribution_samples.R is a Shiny application that allows you to draw samples from various distributions.
- Report.pdf contains all analyses performed (Identification of the most correlated gene - BLVRB; Statistical tests; Comparison of the effectiveness of different regression models; Random Forest training).
- Script.R To run the script, ensure that the data files (```X_train.csv```, ```y_train.csv```, ```X_test.csv```) are located in the working directory, then you need the R environment and the following libraries installed:

```
install.packages(c(
  "shiny", "DT",                                           # Shiny applications
  "ggplot2", "readr", "caret", "dplyr", "tidyr", "tibble",
  "RColorBrewer", "qqplotr", "nortest", "ggdist",
  "parallel", "parallelMap",                               # Parallel computing
  "glmnet",                                                # ElasticNet
  "randomForest", "ranger"                                 # Random Forest
))
```
