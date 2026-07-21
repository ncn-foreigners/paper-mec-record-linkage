library(futurize)
library(progressify)

handlers("cli", global = TRUE)
options(progressr.enable = TRUE)

source("code/internal_blocking.R")
source("code/functions_eval.R")

methods <- c(
  "mec_b",
  "mec_b_rho",
  "mec_c",
  "mec_c_rho",
  "fs_b",
  "fs_c"
)

c_comparators <- list(
  "fname" = jarowinkler_complement(),
  "surname" = jarowinkler_complement()
)
c_methods <- list(
  "fname" = "continuous_parametric",
  "surname" = "continuous_parametric"
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

  res_fs_c <- perform_fs(df1 = df1, df2 = df2, comparator = cmp_jarowinkler(0.9))

  n_M_est <- c(
    res_mec_b$n_M_est,
    res_mec_b_rho$n_M_est,
    res_mec_c$n_M_est,
    res_mec_c_rho$n_M_est,
    res_fs_b$n_pred_matches,
    res_fs_c$n_pred_matches
  )

  flr <- c(
    res_mec_b$eval_metrics["FLR"],
    res_mec_b_rho$eval_metrics["FLR"],
    res_mec_c$eval_metrics["FLR"],
    res_mec_c_rho$eval_metrics["FLR"],
    res_fs_b$FLR,
    res_fs_c$FLR
  )

  mmr <- c(
    res_mec_b$eval_metrics["MMR"],
    res_mec_b_rho$eval_metrics["MMR"],
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
    iter = rep(i, 6)
  )

}) |> progressify() |> futurize(seed = TRUE))

plan(sequential)

results_blocking <- results[, .(n_M_est = mean(n_M_est),
                                flr = mean(flr),
                                mmr = mean(mmr)),
                            by = .(method)]

eval_table_blocking <- generate_latex_table_blocking(
  results_blocking = results_blocking,
  iterations = iter
)

save(results_blocking, file = "results/results_blocking.RData")
writeLines(eval_table_blocking, con = "results/table_blocking.txt")
