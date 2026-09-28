library(data.table)
library(kableExtra)
library(ggplot2)

calculate_metrics <- function(res_list, true_n_M) {

  metrics <- lapply(res_list, function(x) {
    flr <- x$eval_metrics["FLR"]
    mmr <- x$eval_metrics["MMR"]
    names(flr) <- NULL
    names(mmr) <- NULL
    c(
      n_M_est = x$n_M_est,
      flr = flr,
      mmr = mmr,
      flr_est = x$flr_est,
      mmr_est = x$mmr_est
    )
  })

  metrics <- do.call(rbind, metrics)
  mean_metrics <- colMeans(metrics)
  n_M_est_vec <- metrics[, "n_M_est"]
  se_n_M <- sd(n_M_est_vec)
  rmse_n_M <- sqrt(mean((n_M_est_vec - true_n_M)^2))

  c(
    mean_metrics,
    se_n_M = se_n_M,
    rmse_n_M = rmse_n_M
  )

}

eval_lee_2022 <- function(res, true_n_M) {

  res_b <- lapply(res, function(x) x$b)
  res_cpar <- lapply(res, function(x) x$cpar)
  res_cnonpar <- lapply(res, function(x) x$cnonpar)

  metrics_b <- calculate_metrics(res_b, true_n_M)
  metrics_cpar <- calculate_metrics(res_cpar, true_n_M)
  metrics_cnonpar <- calculate_metrics(res_cnonpar, true_n_M)

  res_table <- data.table(do.call(rbind, list(
    "binary" = metrics_b,
    "cpar" = metrics_cpar,
    "cnonpar" = metrics_cnonpar
  )))
  method_vec <- data.table(
    method = c("Binary", "Continuous parametric", "Continuous nonparametric")
  )
  cbind(method_vec, res_table)

}

n_M_plot <- function(results_8, results_5, results_3) {

  results_b_8_list <- lapply(results_8, function(x) x[[1]])
  results_cpar_8_list <- lapply(results_8, function(x) x[[2]])
  results_cnonpar_8_list <- lapply(results_8, function(x) x[[3]])
  results_b_5_list <- lapply(results_5, function(x) x[[1]])
  results_cpar_5_list <- lapply(results_5, function(x) x[[2]])
  results_cnonpar_5_list <- lapply(results_5, function(x) x[[3]])
  results_b_3_list <- lapply(results_3, function(x) x[[1]])
  results_cpar_3_list <- lapply(results_3, function(x) x[[2]])
  results_cnonpar_3_list <- lapply(results_3, function(x) x[[3]])

  n_M_s_b_8 <- unlist(lapply(results_b_8_list, function(x) {
    x$n_M_est
  }))
  n_M_s_cpar_8 <- unlist(lapply(results_cpar_8_list, function(x) {
    x$n_M_est
  }))
  n_M_s_cnonpar_8 <- unlist(lapply(results_cnonpar_8_list, function(x) {
    x$n_M_est
  }))

  n_M_s_b_5 <- unlist(lapply(results_b_5_list, function(x) {
    x$n_M_est
  }))
  n_M_s_cpar_5 <- unlist(lapply(results_cpar_5_list, function(x) {
    x$n_M_est
  }))
  n_M_s_cnonpar_5 <- unlist(lapply(results_cnonpar_5_list, function(x) {
    x$n_M_est
  }))

  n_M_s_b_3 <- unlist(lapply(results_b_3_list, function(x) {
    x$n_M_est
  }))
  n_M_s_cpar_3 <- unlist(lapply(results_cpar_3_list, function(x) {
    x$n_M_est
  }))
  n_M_s_cnonpar_3 <- unlist(lapply(results_cnonpar_3_list, function(x) {
    x$n_M_est
  }))

  data <- data.frame(
    n_M_est = c(
      n_M_s_b_8,
      n_M_s_b_5,
      n_M_s_b_3,
      n_M_s_cpar_8,
      n_M_s_cpar_5,
      n_M_s_cpar_3,
      n_M_s_cnonpar_8,
      n_M_s_cnonpar_5,
      n_M_s_cnonpar_3
    ),
    method = rep(c("Binary", "C. param.", "C. nonparam."), each = 3 * length(n_M_s_b_8)),
    n_M = rep(rep(c(400, 250, 150), each = length(n_M_s_b_8)), 3)
  )
  data$method <- factor(
    data$method,
    levels = c("Binary", "C. param.", "C. nonparam.")
  )

  plot <- ggplot(data, aes(x = method, y = n_M_est)) +
    geom_violin(trim = FALSE) +
    geom_jitter(size = 0.5, alpha = 0.6, width = 0.15) +
    facet_wrap(
      ~ factor(n_M, levels = sort(unique(n_M), decreasing = TRUE)),
      labeller = as_labeller(
        function(labels) {
          paste0("n[M] == ", labels)
        },
        default = label_parsed
      )
    ) +
    theme_minimal(base_size = 18) +
    labs(
      x = "Estimation method",
      y = "Estimated number of matches"
    ) +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    )

  plot

}

generate_latex_table <- function(e_8, e_5, e_3, iterations) {
  e_8 <- copy(e_8)
  e_5 <- copy(e_5)
  e_3 <- copy(e_3)
  e_8[, n_M := 400]
  e_5[, n_M := 250]
  e_3[, n_M := 150]

  eval_table <- rbindlist(list(e_8, e_5, e_3))
  setcolorder(
    eval_table,
    c(
      "n_M",
      "method",
      "n_M_est",
      "se_n_M",
      "rmse_n_M",
      "flr",
      "mmr",
      "flr_est",
      "mmr_est"
    )
  )

  eval_table[method == "Continuous parametric", method := "C. param."]
  eval_table[method == "Continuous nonparametric", method := "C. nonparam."]

  eval_table[, `:=`(
    n_M_est = sprintf("%.1f (%.1f)", n_M_est, se_n_M),
    rmse_n_M = sprintf("%.1f", rmse_n_M),
    flr = sprintf("%.4f", flr),
    mmr = sprintf("%.4f", mmr),
    flr_est = sprintf("%.4f", flr_est),
    mmr_est = sprintf("%.4f", mmr_est)
  )]
  eval_table[, se_n_M := NULL]
  eval_table[, n_M := as.character(n_M)]
  eval_table[,
    n_M := fifelse(
      seq_len(.N) == 1,
      sprintf("\\multirow{3}{*}{$%s$}", n_M),
      ""
    ),
    by = n_M
  ]
  rownames(eval_table) <- NULL

  latex_table <- kbl(
    x = eval_table,
    format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    linesep = "",
    align = c("c|", "l", rep("c", 6)),
    col.names = c(
      "$n_M$",
      "Method",
      "$\\hat{n}_M$ (SE)",
      "$\\text{RMSE}(\\hat{n}_M)$",
      "$\\FLR$",
      "$\\MMR$",
      "$\\widehat{\\FLR}$",
      "$\\widehat{\\MMR}$"
    ),
    caption = paste0("True number of matches, average estimates with standard errors (SE), root mean square errors (RMSE), and average linkage error rates across $", iterations, "$ simulations."),
    label = "sim-lee-2022"
  )

  latex_table <- as.character(latex_table)
  latex_table <- sub(
    "\\\\begin\\{tabular\\}(\\[[^]]+\\])?\\{([^}]*)\\}",
    "\\\\begin{tabular*}{\\\\textwidth}{@{\\\\extracolsep{\\\\fill}}\\2}",
    latex_table
  )
  latex_table <- sub(
    "\\\\end\\{tabular\\}",
    "\\\\end{tabular*}",
    latex_table
  )

  lines <- strsplit(latex_table, "\n", fixed = TRUE)[[1]]
  nonpar_rows <- grep("C. nonparam.", lines)

  lines <- append(lines, "\\midrule", after = nonpar_rows[1])
  lines <- append(lines, "\\midrule", after = nonpar_rows[2] + 1)

  latex_table <- paste(lines, collapse = "\n")

  latex_table

}

generate_latex_table_blocking <- function(results_blocking, iterations) {

  eval_table <- (copy(results_blocking))
  set(eval_table, j = "candidate_pair_count", value = NULL)

  method_labels <- c(
    "MEC (binary, $\\rho = 0$)",
    "MEC (binary, $\\rho = 0.5$)",
    "MEC (binary, with JW threshold, $\\rho = 0$)",
    "MEC (binary, with JW threshold, $\\rho = 0.5$)",
    "MEC (continuous parametric, $\\rho = 0$)",
    "MEC (continuous parametric, $\\rho = 0.5$)",
    "FS (binary)",
    "FS (with JW threshold)"
  )
  eval_table[, method := method_labels]
  eval_table[, `:=`(
    n_M_est = formatC(n_M_est, format = "f", digits = 1, big.mark = ","),
    flr = formatC(flr, format = "f", digits = 4, big.mark = ","),
    mmr = formatC(mmr, format = "f", digits = 4, big.mark = ",")
  )]
  rownames(eval_table) <- NULL

  latex_table <- kbl(
    x = eval_table,
    format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    linesep = "",
    align = c("l", rep("c", 3)),
    col.names = c(
      "Method",
      "$\\hat{n}_M$",
      "FLR",
      "MMR"
    ),
    caption = paste0(
      "Average estimates of the number of matches and average error rates across $",
      iterations,
      "$ simulations."
    ),
    label = "sim-blocking"
  )

  latex_table <- as.character(latex_table)
  latex_table <- sub(
    "\\\\begin\\{tabular\\}(\\[[^]]+\\])?\\{([^}]*)\\}",
    "\\\\begin{tabular*}{\\\\textwidth}{@{\\\\extracolsep{\\\\fill}}\\2}",
    latex_table
  )
  latex_table <- sub(
    "\\\\end\\{tabular\\}",
    "\\\\end{tabular*}",
    latex_table
  )

  lines <- strsplit(latex_table, "\n", fixed = TRUE)[[1]]

  latex_table <- paste(lines, collapse = "\n")

  latex_table
}
