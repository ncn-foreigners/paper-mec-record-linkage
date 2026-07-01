library(data.table)
library(automatedRecLin)

single_run <- function(p_A) {
  ids_prd <- sample(ids, n_A)
  matched_individ <- sample(ids_prd, n_A * p_A)
  ids_cis <- sample(setdiff(cis_filtered$person_id, ids_prd), n_B - n_A * p_A)
  ids_cis <- c(matched_individ, ids_cis)

  df1 <- prd[person_id %in% ids_prd, ]
  df2 <- cis[person_id %in% ids_cis, ]

  matches <- merge(
    x = df1[, .(x = 1:.N, person_id)],
    y = df2[, .(y = 1:.N, person_id)],
    by = "person_id"
  )
  setnames(matches, c("x", "y"), c("a", "b"))
  set(matches, j = "person_id", value = NULL)

  result_b <- mec(
    A = df1,
    B = df2,
    variables = variables,
    true_matches = matches
  )

  result_cpar <- mec(
    A = df1,
    B = df2,
    variables = variables,
    comparators = comparators,
    methods = methods_cpar,
    true_matches = matches
  )

  result_cnonpar <- mec(
    A = df1,
    B = df2,
    variables = variables,
    comparators = comparators,
    methods = methods_cnonpar,
    true_matches = matches
  )

  list(
    b = result_b,
    cpar = result_cpar,
    cnonpar = result_cnonpar
  )
}