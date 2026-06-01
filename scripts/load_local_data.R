#!/usr/bin/env Rscript
options(stringsAsFactors = FALSE)
library(purrr)
library(readr)

data_dir <- "wolff_TUR_data"
if (!dir.exists(data_dir)) stop(paste0("Data directory not found: ", data_dir))

fpaths <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE, recursive = FALSE)

data_list <- purrr::map(set_names(fpaths, basename(fpaths)), ~readr::read_csv(.x, show_col_types = FALSE))

cat("Loaded", length(data_list), "files:\n")
cat(paste(names(data_list), collapse = "\n"), "\n")
