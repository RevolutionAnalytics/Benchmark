#-------------------------------------------------------------------------------
#
# Project: Revoluton R Enterprise Benchmarking
# Task: Revoluton R Enterprise Benchmark Script
# Description: This script is to carry out the analyses detailed in the whitepaper.
#              The analysis data are assumed to be in ANALYSIS (analysis_table.xdf),
#              and the prediction data are in PREDICTION (prediction_table.xdf).
#
# version : date : author : change
# 0 : 2014-03-31 : Revolution Analytics : Create Final Script
#
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#- Program Setup
#-------------------------------------------------------------------------------
#- Load Packages
library(RevoScaleR)
library(methods)

#- Define Directories and Files for Work
data_dir <- getwd()

ANALYSIS <- file.path(data_dir, "analysis_table.xdf")
PREDICTION <- file.path(data_dir, "prediction_table.xdf")
PREDICTOUT <- file.path(data_dir, "predict_out_table.xdf")

#-------------------------------------------------------------------------------
#- Main Program
#-------------------------------------------------------------------------------

questions <- c("Run descriptive statistics", 
                "Compute median and deciles", 
                "Run frequency distribution", 
                "Linear regression 20 numeric variables", 
                "Linear regression 20 mixed variables", 
                "Stepwise linear 100 numeric variables",
                "Logistic regression", 
                "Generalized linear model",
                "k-means clustering v=20 k=3",
                "k-means clustering v=100 k=5", 
                "Score Prediction table and retain results")
timings <- rep(NA_real_, length(questions))


# For all of the following tasks, use the Analysis table output from the Data Preparation step.
numeric1 <- paste("N", 2:21, sep = "", collapse = "+")
numeric2 <- paste("N", 2:101, sep = "", collapse = "+")
numeric3 <- paste("N", 1:20, sep = "", collapse = "+")
numeric4 <- paste("N", 1:100, sep = "", collapse = "+")
text1 <- paste("T", 1:10, sep = "", collapse = "+")

#- Run descriptive statistics
timings[1] <- system.time(
  numeric_summary <- rxSummary(~ F1, data = ANALYSIS)
  )[[3]]

#- Compute median and deciles
timings[2] <- system.time(
  f1_deciles <- rxQuantile(varName = "F1", data = ANALYSIS, probs = seq(0,1, 0.1))
  )[[3]]

#- Run frequency distribution
timings[3] <- system.time(
  t1_freq <- rxCube(formula = ~ T1, data = ANALYSIS, means = FALSE)
  )[[3]]

#- Linear regression 20 numeric variables
timings[4] <- system.time(
  lm1 <- rxLinMod(as.formula(paste("N1 ~", numeric1)), data = ANALYSIS)
  )[[3]]

#- Linear regression 20 mixed variables
timings[5] <- system.time(
  lm2 <- rxLinMod(as.formula(paste("N1 ~", paste(numeric1, text1, sep = "+"))), data = ANALYSIS)
  )[[3]]

#- Stepwise linear 100 numeric variables
scope <- list(
    lower = ~ 1,
    upper = as.formula(paste("~", numeric2))
    )
varsel <- rxStepControl(method = "stepwise", scope = scope)

timings[6] <- system.time(
  lm3 <- rxLinMod(N1 ~ 1, data = ANALYSIS, variableSelection = varsel)
  )[[3]]

#- Logistic regression
# Compute median for numeric field F1
f1_median <- rxQuantile(varName = "F1", data = ANALYSIS, probs = 0.5)
# Create one new field B1 by “binning” F1 in two equal bins; assign values of 0 and 1
rxDataStep(inData = ANALYSIS, outFile = ANALYSIS, 
  transforms = list(B1 = as.integer(F1 > median_cut)), 
  overwrite = TRUE, transformObjects = list(median_cut = f1_median))

timings[7] <- system.time(
  glm1 <- rxLogit(as.formula(paste("B1 ~", numeric1)), data = ANALYSIS)
  )[[3]]

#- Generalized linear model
timings[8] <- system.time(
  glm2 <- rxGlm(as.formula(paste("N1 ~", numeric1)), data = ANALYSIS, family = Gamma())
  )[[3]]

#- k-means clustering v=20 k=3
timings[9] <- system.time(
  kmeans1 <- rxKmeans(as.formula(paste("~", numeric3)), data = ANALYSIS, 
    numClusters = 3, numStarts = 0, maxIterations = 2)
  )[[3]]

#- k-means clustering v=100 k=5
timings[10] <- system.time(
  kmeans2 <- rxKmeans(as.formula(paste("~", numeric4)), data = ANALYSIS, 
    numClusters = 5, numStarts = 0, maxIterations = 2)
  )[[3]]

#- Score Prediction table and retain results
timings[11] <- system.time(
  rxPredict(modelObject = lm1, data = PREDICTION,
    outData = PREDICTOUT, overwrite = TRUE)
  )[[3]]