library(futurize)
library(progressify)

handlers("cli", global = TRUE)
options(progressr.enable = TRUE)

source("code/internal_blocking.R")
source("code/functions_eval.R")

methods <- c(
  "mec_b",
  "mec_b_rho",
  "mec_b_jw",
  "mec_b_rho_jw",
  "mec_c",
  "mec_c_rho",
  "fs_b",
  "fs_c"
)

threshold <- 0.95

c_comparators <- list(
  "fname" = jarowinkler_complement(),
  "surname" = jarowinkler_complement()
)
c_methods <- list(
  "fname" = "continuous_parametric",
  "surname" = "continuous_parametric"
)
threshold_comparators <- list(
  "fname" = jw_threshold(threshold),
  "surname" = jw_threshold(threshold)
)

iter <- 100
workers <- 10

set.seed(123)

plan(multisession, workers = workers)

results <- rbindlist(lapply(1:iter, function(i) {
  options(text2vec.mc.cores = 1L)

  df <- read_data(i)
  data <- preprocess_data(df)
  df1 <- data$df1
  df2 <- data$df2
  true_matches <- extract_true_matches(df1, df2)

  res_mec_b <- perform_mec_blocking(
    df1 = df1,
    df2 = df2,
    true_matches = true_matches
  )
  res_mec_b_rho <- perform_mec_blocking(
    df1 = df1,
    df2 = df2,
    rho = 0.5,
    true_matches = true_matches
  )

  res_mec_c <- perform_mec_blocking(
    df1 = df1,
    df2 = df2,
    comparators = c_comparators,
    methods = c_methods,
    true_matches = true_matches
  )

  res_mec_c_rho <- perform_mec_blocking(
    df1 = df1,
    df2 = df2,
    comparators = c_comparators,
    methods = c_methods,
    rho = 0.5,
    true_matches = true_matches
  )

  res_fs_b <- perform_fs(df1 = df1, df2 = df2)

  res_fs_c <- perform_fs(df1 = df1, df2 = df2, comparator = cmp_jarowinkler(threshold))

  res_mec_b_jw <- perform_mec_blocking(
    df1 = df1,
    df2 = df2,
    comparators = threshold_comparators,
    true_matches = true_matches
  )

  res_mec_b_rho_jw <- perform_mec_blocking(
    df1 = df1,
    df2 = df2,
    comparators = threshold_comparators,
    rho = 0.5,
    true_matches = true_matches
  )

  n_M_est <- c(
    res_mec_b$n_M_est,
    res_mec_b_rho$n_M_est,
    res_mec_b_jw$n_M_est,
    res_mec_b_rho_jw$n_M_est,
    res_mec_c$n_M_est,
    res_mec_c_rho$n_M_est,
    res_fs_b$n_pred_matches,
    res_fs_c$n_pred_matches
  )

  flr <- c(
    res_mec_b$eval_metrics["FLR"],
    res_mec_b_rho$eval_metrics["FLR"],
    res_mec_b_jw$eval_metrics["FLR"],
    res_mec_b_rho_jw$eval_metrics["FLR"],
    res_mec_c$eval_metrics["FLR"],
    res_mec_c_rho$eval_metrics["FLR"],
    res_fs_b$FLR,
    res_fs_c$FLR
  )

  mmr <- c(
    res_mec_b$eval_metrics["MMR"],
    res_mec_b_rho$eval_metrics["MMR"],
    res_mec_b_jw$eval_metrics["MMR"],
    res_mec_b_rho_jw$eval_metrics["MMR"],
    res_mec_c$eval_metrics["MMR"],
    res_mec_c_rho$eval_metrics["MMR"],
    res_fs_b$MMR,
    res_fs_c$MMR
  )

  data.table(
    method = methods,
    n_M_est = n_M_est,
    flr = flr,
    mmr = mmr,
    iter = rep(i, 8),
    candidate_pair_count = rep(res_mec_c$candidate_pair_count, 8)
  )

}) |> progressify() |> futurize(seed = TRUE))

plan(sequential)

results_blocking <- results[, .(n_M_est = mean(n_M_est),
                                flr = mean(flr),
                                mmr = mean(mmr),
                                candidate_pair_count = mean(candidate_pair_count)),
                            by = .(method)]

eval_table_blocking <- generate_latex_table_blocking(
  results_blocking = results_blocking,
  iterations = iter
)

thr_name <- as.character(threshold * 100)

save(
  results,
  file = paste0("results-raw/results_raw_blocking_", thr_name, ".RData")
)
save(
  results_blocking,
  file = paste0("results/results_blocking_", thr_name, ".RData")
)
writeLines(eval_table_blocking, con = paste0("results/table_blocking_", thr_name, ".txt"))
