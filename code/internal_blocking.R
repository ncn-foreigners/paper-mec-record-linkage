library(data.table)
library(blocking)
library(automatedRecLin)
library(reclin2)

read_data <- function(i) {

  fread(
    paste0("data-iter/data-pesel-", sprintf("%03d", i), ".csv.gz"),
    na.strings = "<NA>"
  )

}

preprocess_data <- function(df) {

  df1 <- df[!duplicated(true_id)]
  df2 <- df[duplicated(true_id)]
  df1_un <- df1[!(true_id %in% df2[["true_id"]])]
  df1 <- df1[true_id %in% df2[["true_id"]]]
  to_move <- sample(1:nrow(df1_un), size = 1000)
  df2 <- rbind(df2, df1_un[to_move])
  df1 <- rbind(df1, df1_un[!to_move])

  df1[is.na(df1)] <- ""
  df2[is.na(df2)] <- ""

  df1[, dob_year := substr(date, 1, 4)]
  df2[, dob_year := substr(date, 1, 4)]
  df1[, dob_mon := substr(date, 6, 7)]
  df2[, dob_mon := substr(date, 6, 7)]
  df1[, dob_day := substr(date, 9, 10)]
  df2[, dob_day := substr(date, 9, 10)]
  df1[, txt := paste(fname, surname, gsub("/", " ", date), country)]
  df2[, txt := paste(fname, surname, gsub("/", " ", date), country)]

  list(df1 = df1, df2 = df2)

}

extract_true_matches <- function(df1, df2) {

  merge(
    x = df1[, .(a = .I, true_id)],
    y = df2[, .(b = .I, true_id)],
    by = "true_id"
  )[, .(a, b)]

}

perform_mec_blocking <- function(df1,
                                 df2,
                                 variables = c(
                                   "fname",
                                   "surname",
                                   "dob_day",
                                   "dob_mon",
                                   "dob_year"
                                 ),
                                 comparators = NULL,
                                 methods = NULL,
                                 rho = 0,
                                 ann = "nnd",
                                 epsilon_blocking = 0.5,
                                 n_shingles = 3,
                                 true_matches = NULL
                                 ) {
  
  ann_control_pars <- controls_ann()
  ann_control_pars$nnd$epsilon <- epsilon_blocking
  
  mec_blocking(
    A = df1,
    B = df2,
    variables = variables,
    comparators = comparators,
    methods = methods,
    rho = rho,
    blocking_x = df1[["txt"]],
    blocking_y = df2[["txt"]],
    controls_blocking = list(
      ann = ann,
      control_ann = ann_control_pars,
      control_txt = controls_txt(n_shingles = n_shingles),
      n_threads = 1
    ),
    true_matches = true_matches
  )
  
}

perform_fs <- function(
  df1,
  df2,
  comparator = cmp_identical(),
  p0 = 0.05,
  threshold = 0,
  ann = "nnd",
  n_shingles = 3
) {
  variables <- c("fname", "surname", "dob_day",
                 "dob_mon", "dob_year")

  ann_control_pars <- controls_ann()
  ann_control_pars$nnd$epsilon <- 0.5

  fs_comparators <- list(
    fname = comparator,
    surname = comparator
  )

  fs_pairs <- pair_ann(
    x = df1,
    y = df2,
    on = "txt",
    deduplication = FALSE,
    ann = ann,
    n_threads = 1,
    control_ann = ann_control_pars,
    control_txt = controls_txt(n_shingles = n_shingles)
  )
  fs_pairs <- compare_pairs(
    fs_pairs,
    on = variables,
    comparators = fs_comparators
  )

  fs_model <- problink_em(
    ~ fname + surname + dob_day + dob_mon + dob_year,
    data = fs_pairs,
    p0 = p0
  )
  fs_pairs <- predict(fs_model, pairs = fs_pairs, add = TRUE)
  fs_pairs <- select_greedy(
    fs_pairs,
    variable = "fs_greedy",
    score = "weights",
    threshold = threshold
  )

  selected <- fs_pairs[fs_greedy == TRUE, .(a = .x, b = .y)]
  true_matches <- extract_true_matches(df1, df2)
  setkey(selected, a, b)
  setkey(true_matches, a, b)

  tp <- nrow(fintersect(selected, true_matches))
  fp <- nrow(fsetdiff(selected, true_matches))
  fn <- nrow(fsetdiff(true_matches, selected))

  list(
    n_pred_matches = nrow(selected),
    FLR = if ((tp + fp) == 0L) NA_real_ else fp / (tp + fp),
    MMR = if ((tp + fn) == 0L) NA_real_ else fn / (tp + fn)
  )
}
