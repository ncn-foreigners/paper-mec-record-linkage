library(data.table)
library(kableExtra)

calculate_metrics <- function(res_list) {

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
  colMeans(metrics)

}

eval_lee_2022 <- function(res) {

  res_b <- lapply(res, function(x) x$b)
  res_cpar <- lapply(res, function(x) x$cpar)
  res_cnonpar <- lapply(res, function(x) x$cnonpar)

  metrics_b <- calculate_metrics(res_b)
  metrics_cpar <- calculate_metrics(res_cpar)
  metrics_cnonpar <- calculate_metrics(res_cnonpar)

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
      "flr",
      "mmr",
      "flr_est",
      "mmr_est"
    )
  )

  eval_table[, `:=`(
    n_M_est = sprintf("%.1f", n_M_est),
    flr = sprintf("%.4f", flr),
    mmr = sprintf("%.4f", mmr),
    flr_est = sprintf("%.4f", flr_est),
    mmr_est = sprintf("%.4f", mmr_est)
  )]
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
    align = c("c|", "l", rep("c", 5)),
    col.names = c(
      "$n_M$",
      "Method",
      "$\\hat{n}_M$",
      "FLR",
      "MMR",
      "$\\widehat{\\text{FLR}}$",
      "$\\widehat{\\text{MMR}}$"
    ),
    caption = paste0("True number of matches, average estimates, and average error rates across $", iterations, "$ simulations."),
    label = "tab:sim-lee-2022"
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
  nonpar_rows <- grep("Continuous nonparametric", lines)

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
    "MEC (continuous parametric, $\\rho = 0$)",
    "MEC (continuous parametric, $\\rho = 0.5$)",
    "FS (binary)",
    "FS (with JW similarity)"
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
