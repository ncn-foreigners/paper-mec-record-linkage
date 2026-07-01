library(data.table)
library(automatedRecLin)
library(doFuture)
library(progressr)
library(doRNG)

perform_simulation <- function(p_A,
                               iterations,
                               seed) {
  with_progress({
    p <- progressor(steps = iterations)
    set.seed(seed)
    foreach(i = 1:iterations,
            .packages = c(
              "data.table",
              "automatedRecLin"
            )) %dorng% {
              p(sprintf("Iteration: %g", i))
              single_run(p_A)
            }
  })
}