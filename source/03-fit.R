# situating --------------------------------------------------------------------
library(tidymodels)
library(readr)
library(baguette)
library(bundle)
library(doMC)
library(finetune)
registerDoMC(cores = max(1, parallelly::availableCores() - 1))



head(Mutagen_Dragon)

# in the paper, their model results on the test set read:
truth <- as.factor(c(rep(F, 163), rep(T, 290), rep(F, 30), rep(T, 52)))
estimate <- as.factor(c(rep(F, 163), rep(T, 290), rep(T, 30), rep(F, 52)))

accuracy_vec(truth, estimate)
# [1] 0.846729