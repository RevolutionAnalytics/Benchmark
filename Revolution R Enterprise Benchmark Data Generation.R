#-------------------------------------------------------------------------------
#
# Project: Revoluton R Enterprise Benchmarking
# Task: Revoluton R Enterprise Benchmark Data Generation
# Description: This script is to generate the data detailed in the whitepaper.
#
# version : date : author : change
# 0 : 2014-03-31 : Revolution Analytics : Create Final Script
#
#-------------------------------------------------------------------------------

#- Data Description
# Analysis table:  
#   - "Wide" table with measures that describe entities (such as customers);
# 	- Split in advance into roughly equal replicates;
#   - Fields:
#     - Index key (INDEX) (Numeric integer)
#     - Text fields (T1-T20) (Ten distinct 8-character values @ random)
#     - Integer (G1-G250)  (Uniform distribution from 1 to 10)
#     - Numeric (N1-N250) (Uniform distribution from 0 to 10000)
#     - Integer (F1-F20)  (Uniform distribution from 1 to 10) 10 values summed to 1 value per row

# Prediction table: 
#   - Table to be used for scoring;
#   - No need to match index key to analysis;
#     - Twenty numeric fields (TBD)

#-------------------------------------------------------------------------------
#- Program Setup
#-------------------------------------------------------------------------------
show_progress <- TRUE
#- Define Data Generation Parameters
random_seed <- 12345
data_size <- 1e4
chunk_size <- 2e3 # data_size/chunk_size should be an integer and small enough to fit in RAM
num_text_fields <- 20
num_integer_fields <- 250
num_numeric_fields <- 250
num_integer_fields_fact <- 20

#- Load Packages
library(RevoScaleR)
library(iterators)
library(methods)

#-------------------------------------------------------------------------------
#- Main Program
#-------------------------------------------------------------------------------
#- Set Random Seed
set.seed(random_seed)

#- Define Table Names
analysis_csv <- "analysis_table.csv"
prediction_csv <- "prediction_table.csv"

#- Open File Connections
analysis_con <- file(analysis_csv, "w")
prediction_con <- file(prediction_csv, "w")

#- Define Fields
analysis_index_field <- "INDEX"
analysis_text_fields <- paste("T", seq_len(num_text_fields), sep = "")
analysis_integer_fields <- paste("G", seq_len(num_integer_fields), sep = "")
analysis_numeric_fields <- paste("N", seq_len(num_numeric_fields), sep = "")
fact_integer_fields <- paste("F", 1:num_integer_fields_fact, sep = "")

# Define Field Values
analysis_text_levels <- sapply(LETTERS[1:10], FUN = function(x) paste(rep(x, 8), collapse = ""))

# Number of Chunks
n_chunks <- data_size / chunk_size

#- Create Data
index_iterator <- icount(data_size)
cat(" Generating csv files for size:", data_size, 
    "\n ----------------------------------------\n")
if (show_progress) {
  n_overall <- 40
  cat(paste0("\r|", paste(rep(" ", n_overall), collapse = ""), "| 0% Complete"))
  flush.console()
  }

for (chunk in seq_len(n_chunks)) {
  chunk_index <- sapply(seq_len(chunk_size), FUN = function(x) nextElem(index_iterator))

  analysis_df <- data.frame(INDEX = chunk_index)
  analysis_df[, analysis_text_fields] <- analysis_text_levels[(chunk_index %% 10) + 1]
  analysis_df[, analysis_integer_fields] <- sapply(seq_along(analysis_integer_fields), 
    function(x) sample.int(10, size = chunk_size, replace = TRUE))
  analysis_df[, analysis_numeric_fields] <- sapply(seq_along(analysis_numeric_fields), 
    function(x) runif(chunk_size, max = 10000))
  analysis_df[, fact_integer_fields] <- sapply(seq_along(fact_integer_fields),
    function(x) aggregate(sample.int(10, size = chunk_size * 10, replace = TRUE), list(rep(seq(length=chunk_size), 10)), sum)[,2])
  
  prediction_df <- as.data.frame(sapply(seq_along(analysis_numeric_fields[2:21]), 
    function(x) runif(chunk_size * 10, max = 10000)))
  names(prediction_df) <- analysis_numeric_fields[2:21]
  
  if (chunk == 1) {
    write.table(analysis_df, file = analysis_con, sep = ",", col.names = TRUE,
            qmethod = "double", row.names = FALSE)
    write.table(prediction_df, prediction_con, sep = ",", col.names = TRUE,
            qmethod = "double", row.names = FALSE)
  } else {
    write.table(analysis_df, file = analysis_con, sep = ",", col.names = FALSE,
            qmethod = "double", row.names = FALSE)
    write.table(prediction_df, prediction_con, sep = ",", col.names = FALSE,
            qmethod = "double", row.names = FALSE)
  }
  if (show_progress) {
    value <- chunk/n_chunks
    n_prog <- trunc(n_overall * value)
    n_blank <- n_overall - n_prog
    cat(paste0("\r|", paste(c(rep("=", n_prog), rep(" ", n_blank)), collapse = ""), "| ", round(100*value), "% Complete"))
    flush.console()
  }
  rm(analysis_df, prediction_df)
}
cat("\n ... DONE!\n\n")
close(analysis_con)
close(prediction_con)