library(ggplot2)
library(rstan)
library(tidyverse)
library(coda)
library(datasets)

df <- data.frame(trees)
train_ind <- sample(seq_len(nrow(df)), size = floor(0.75 * nrow(df)))
train <- df[train_ind, ]
test <- df[-train_ind, ]

# model
treesdata <- list(
  N = nrow(train),
  N_test = nrow(test),
  K = 2,
  y = train[['Girth']],
  l = train %>% select(Height, Volume)
)
model <- stan_model('BasicFiniteMixture.stan')

# Fit
fit1 <- sampling(model, treesdata, iter = 6000, warmup = 2000, chains=4)

# Summary Stats
print(fit1, pars=c("mu", "sigma", "lp__"))

# Using pairs plot to see distributions of parameters
pairs(fit1, pars=c("mu", "sigma", "lp__"))

# Extracting distribution parameters from the fit object
la <- rstan::extract(fit1, permuted = TRUE)
mu <- colMeans(la$mu)
sigma <- colMeans(la$sigma)

# Predict the consumption of the test data from the ratios
test$y <- test$Height * rlnorm(nrow(test), mu[1], sigma[1]) +
  test$Volume * rlnorm(nrow(test), mu[2], sigma[2])

# Plot to compare measured and predicted consumption on the test data
ggplot(data=test) +
  geom_histogram(mapping=aes(x=Girth), bins=50, color='blue', alpha=0.3) +
  geom_histogram(mapping=aes(x=y), bins=50, color='red', alpha=0.3)
