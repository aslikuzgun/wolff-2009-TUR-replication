#!/usr/bin/env Rscript
options(stringsAsFactors = FALSE)

repo_root <- "./"
data_dir <- file.path(repo_root, "wolff_TUR_data")
out_dir <- file.path(repo_root, "results")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

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

files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
if (length(files) == 0) stop("No CSV files in wolff_TUR_data")

dfs <- lapply(files, read_safe)
dfs <- Filter(Negate(is.null), dfs)
if (length(dfs) == 0) stop("No readable CSVs")

all_cols <- unique(unlist(lapply(dfs, names)))
dfs2 <- lapply(dfs, function(df) {
  missing <- setdiff(all_cols, names(df))
  for (m in missing) df[[m]] <- NA
  df[all_cols]
})

library(dplyr)
library(ordinal)
library(emmeans)
library(ggplot2)

# use base rbind to avoid column type coercion issues across files
big <- do.call(rbind, dfs2)

# Prepare as in report
big <- big %>% mutate(across(any_of(c("rt", "response", "rating")), as.character))

if (!"task" %in% names(big)) big$task <- NA

experiment_only <- big %>% filter(!is.na(task))

modeling_data <- experiment_only %>%
  filter(item_type == "critical") %>%
  filter(participant_id != "byt4qop83n") %>%
  group_by(participant_id) %>%
  filter(n() > 10) %>%
  ungroup() %>%
  mutate(
    rating = factor(as.character(rating), levels = c("0","1","2","3","4"), ordered = TRUE),
    condition = factor(as.character(condition))
  )

# Ensure contrasts
if (nlevels(modeling_data$condition) > 1) contrasts(modeling_data$condition) <- contr.sum(nlevels(modeling_data$condition))

if (nrow(modeling_data) == 0) stop("No rows in modeling_data after filtering")

model <- tryCatch({
  clmm(rating ~ condition + (1|participant_id) + (1|item_id), data = modeling_data)
}, error = function(e) {
  writeLines(paste("Model failed:", e$message), con = file.path(out_dir, "model_error.txt"))
  stop(e)
})

cat("Model fitted. Writing outputs to results/\n")
capture.output(summary(model), file = file.path(out_dir, "model_summary.txt"))

# EMMs
em <- tryCatch({
  emmeans(model, ~ condition)
}, error = function(e) {
  writeLines(paste("EMMeans failed:", e$message), con = file.path(out_dir, "emmeans_error.txt"))
  NULL
})
if (!is.null(em)) {
  capture.output(em, file = file.path(out_dir, "emmeans.txt"))
  # write probabilities/data.frame
  em_df <- as.data.frame(em)
  write.csv(em_df, file = file.path(out_dir, "emmeans.csv"), row.names = FALSE)
}

# Simple plot: mean numeric rating by condition
plot_data <- modeling_data %>% mutate(rating_num = as.integer(as.character(rating))) %>% group_by(condition) %>% summarize(mean = mean(rating_num, na.rm = TRUE), sd = sd(rating_num, na.rm = TRUE), n = n())
png(filename = file.path(out_dir, "rating_by_condition.png"), width = 800, height = 600)
ggplot(plot_data, aes(x = condition, y = mean)) +
  geom_col(fill = "#56B4E9") +
  geom_errorbar(aes(ymin = mean - sd / sqrt(n), ymax = mean + sd / sqrt(n)), width = 0.2) +
  labs(y = "Mean rating (0-4)", x = "Condition") + theme_minimal()
dev.off()

message("Wrote model_summary.txt, emmeans.*, and rating_by_condition.png in results/")
