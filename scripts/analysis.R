#!/usr/bin/env Rscript
# Simple analysis: read all CSVs in wolff_TUR_data, combine, and compute numeric summaries
options(stringsAsFactors = FALSE)

repo_root <- "./"
data_dir <- file.path(repo_root, "wolff_TUR_data")
out_dir <- file.path(repo_root, "results")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
if (length(files) == 0) {
  message("No CSV files found in ", data_dir)
  quit(status = 1)
}

read_safe <- function(f) {
  tryCatch({
    df <- read.csv(f, stringsAsFactors = FALSE)
    df$.__source_file__ <- basename(f)
    df
  }, error = function(e) {
    message("Failed to read ", f, ": ", e$message)
    NULL
  })
}

dfs <- lapply(files, read_safe)
dfs <- Filter(Negate(is.null), dfs)
if (length(dfs) == 0) {
  message("No readable CSVs")
  quit(status = 1)
}

# unify columns
all_cols <- unique(unlist(lapply(dfs, names)))
dfs2 <- lapply(dfs, function(df) {
  missing <- setdiff(all_cols, names(df))
  for (m in missing) df[[m]] <- NA
  df[all_cols]
})

big <- do.call(rbind, dfs2)

# numeric summaries
is_num <- sapply(big, is.numeric)
num_cols <- names(big)[is_num]

if (length(num_cols) == 0) {
  message("No numeric columns found; writing empty summary")
  write.csv(data.frame(), file = file.path(out_dir, "statistics.csv"), row.names = FALSE)
  quit(status = 0)
}

res <- data.frame(variable = character(), N = integer(), mean = double(), sd = double(), min = double(), max = double(), na_count = integer(), stringsAsFactors = FALSE)
for (col in num_cols) {
  x <- big[[col]]
  res <- rbind(res, data.frame(
    variable = col,
    N = sum(!is.na(x)),
    mean = ifelse(all(is.na(x)), NA, mean(x, na.rm = TRUE)),
    sd = ifelse(all(is.na(x)), NA, sd(x, na.rm = TRUE)),
    min = ifelse(all(is.na(x)), NA, min(x, na.rm = TRUE)),
    max = ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE)),
    na_count = sum(is.na(x)),
    stringsAsFactors = FALSE
  ))
}

write.csv(res, file = file.path(out_dir, "statistics.csv"), row.names = FALSE)
writeLines(c("# Analysis summary", "", "Summary statistics saved to results/statistics.csv."), con = file.path(out_dir, "summary.md"))
message("Wrote ", file.path(out_dir, "statistics.csv"))
