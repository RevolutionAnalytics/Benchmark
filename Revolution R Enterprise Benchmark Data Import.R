#-------------------------------------------------------------------------------
#
# Project: Revoluton R Enterprise Benchmarking
# Task: Revoluton R Enterprise Benchmark Data Import to XDF
# Description: This script is to import the data detailed in the whitepaper.
#              The analysis data are imported into analysis_table.xdf,
#              and the prediction data are imported into prediction_table.xdf.
#
# version : date : author : change
# 0 : 2014-03-31 : Revolution Analytics : Create Final Script
#
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#- Program Setup
#-------------------------------------------------------------------------------

#- Define Data Generation Parameters
xdf_chunk_size <- 125000

#- Define Table Names
analysis_csv <- "analysis_table.csv"
prediction_csv <- "prediction_table.csv"

ANALYSIS <- "analysis_table.xdf"
PREDICTION <- "prediction_table.xdf"

#- Load Packages
library(RevoScaleR)
library(methods)

#- Create Column Info for importing
analysis_index_field <- "INDEX"
analysis_text_fields <- paste("T", 1:20, sep = "")
analysis_integer_fields <- paste("G", 1:250, sep = "")
analysis_numeric_fields <- paste("N", 1:250, sep = "")
fact_integer_fields <- paste("F", 1:20, sep = "")

prediction_numeric_fields <- paste("N", 2:21, sep = "")

#- Define Column Info
analysis_fields <- c(analysis_index_field, analysis_text_fields, 
  analysis_integer_fields, analysis_numeric_fields, fact_integer_fields)
analysis_field_types <- c("integer", 
  rep("factor", length(analysis_text_fields)), 
  rep("integer", length(analysis_integer_fields)), 
  rep("float32", length(analysis_numeric_fields)),
  rep("integer", length(fact_integer_fields))
)

analysisColInfo <- vector(mode = "list", length = length(analysis_fields))

for (i in seq_along(analysisColInfo)) {
  analysisColInfo[[i]] <- if (analysis_field_types[i] == "factor") {
    list(type = analysis_field_types[i], width = 8)
  } else {
    list(type = analysis_field_types[i])
  }
}
names(analysisColInfo) <- analysis_fields

predictionColInfo <- lapply(prediction_numeric_fields, function(x) list(type = "float32"))
names(predictionColInfo) <- prediction_numeric_fields

#-------------------------------------------------------------------------------
#- Main Program
#-------------------------------------------------------------------------------

# Import ANALYSIS table from CSV
analysis_ds <- RxTextData(analysis_csv,
  colInfo = analysisColInfo)
    
rxImport(inData = analysis_ds, outFile = ANALYSIS, 
  overwrite = TRUE, colInfo = analysisColInfo, 
  rowsPerRead = xdf_chunk_size)

# Import PREDICTION table from CSV
prediction_ds <- RxTextData(prediction_csv,
  colInfo = predictionColInfo)
    
rxImport(inData = prediction_ds, outFile = PREDICTION, 
    stringsAsFactors = TRUE, overwrite = TRUE,
    rowsPerRead = xdf_chunk_size)