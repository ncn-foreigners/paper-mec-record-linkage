library(data.table)
library(automatedRecLin)

source("code/functions_eval.R")

# Read data
data(census)
data(cis)
prd <- fread("data-raw/prd.csv")

setDT(census)
setDT(cis)

# Preprocess data
cis_filtered <- cis[
  sex != "" & dob_year != "" & dob_mon != "" &
  sex == "F" & as.numeric(dob_year) <= 1970 & (as.numeric(dob_mon) %% 2) == 1
]

prd <- as.data.table(lapply(prd, as.character))
prd[is.na(prd)] <- ""
colnames(prd) <- tolower(colnames(prd))

census[, pername1 := gsub("-", "", pername1)]
cis[, pername1 := gsub("-", "", pername1)]
prd[, pername1 := gsub("-", "", pername1)]

# Set simulation parameters
n_A <- 500
n_B <- 1000
ids <- Reduce(intersect, list(census$person_id, prd$person_id, cis$person_id))
variables <- c(
  "pername1",
  "pername2",
  "sex",
  "dob_day",
  "dob_mon",
  "dob_year"
)
comparators <- list(
  "pername1" = jarowinkler_complement(),
  "pername2" = jarowinkler_complement()
)
methods_cpar <- list(
  "pername1" = "continuous_parametric",
  "pername2" = "continuous_parametric"
)
methods_cnonpar <- list(
  "pername1" = "continuous_nonparametric",
  "pername2" = "continuous_nonparametric"
)

# Define simulation
iter <- 2
main <- function(iterations = iter, workers = 2, seed = 123) {
  
  source("code/functions_lee_2022.R", local = TRUE)

  old_plan <- plan()
  on.exit(plan(old_plan), add = TRUE)

  registerDoFuture()
  plan(multisession, workers = workers)
  options(progressr.enable = TRUE)
  handlers("txtprogressbar")

  # p_A = 0.8
  results_8 <- perform_simulation(
    p_A = 0.8,
    iterations = iterations,
    seed = seed + 8
  )

  # p_A = 0.5
  results_5 <- perform_simulation(
    p_A = 0.5,
    iterations = iterations,
    seed = seed + 5
  )

  # p_A = 0.3
  results_3 <- perform_simulation(
    p_A = 0.3,
    iterations = iterations,
    seed = seed + 3
  )

  list(
    results_8 = results_8,
    results_5 = results_5,
    results_3 = results_3
  )

}

# Perform simulation
results <- main()

results_8 <- results$results_8
results_5 <- results$results_5
results_3 <- results$results_3

# Evaluate results
eval_8 <- eval_lee_2022(results_8)
eval_5 <- eval_lee_2022(results_5)
eval_3 <- eval_lee_2022(results_3)

eval_table <- generate_latex_table(eval_8, eval_5, eval_3, iterations = iter)

# Save results
save(results, file = "results/results_lee_2022.RData")
writeLines(eval_table, con = "results/table_lee_2022.txt")
