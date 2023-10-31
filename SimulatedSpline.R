library(rstan)
library(splines)

set.seed(2500)
num_knots <- 7
# Degree of polynomials (cubic spline in this case)
spline_degree <- 3
num_basis <- num_knots + spline_degree - 1
X <- seq(from=-8, to=8, by=.1)
num_data <- length(X)
knots <- unname(quantile(X,probs=seq(from=0, to=1, length.out = num_knots)))
a0 <- 0.5
a <- rnorm(num_basis, 0, 1)
B_true <- t(bs(X, df=num_basis, degree=spline_degree, intercept = TRUE))
Y_true <- as.vector(a0*X + a%*%B_true)
Y <- Y_true + rnorm(length(X), 0, 0.5)

# model
dat1 = list(num_data=num_data, num_basis=num_basis, num_knots=num_knots, X=X, 
           Y=Y, spline_degree=spline_degree, knots=knots)
spline_model <- stan_model("simplespline.stan")
fit_spline <- sampling(spline_model, dat1, iter=2000)

ff1<-extract(fit_spline)
Y_hat_med <- array(NA, length(Y))
Y_hat_ub <- array(NA, length(Y))
Y_hat_lb <- array(NA, length(Y))
for (i in 1:length(Y)) {
  Y_hat_med[i] <- median(ff1$Y_hat[,i]);
  Y_hat_lb[i] <- quantile(ff1$Y_hat[,i],probs = 0.25)
  Y_hat_ub[i] <- quantile(ff1$Y_hat[,i],probs = 0.75)
}
# Plot 
plot(X,Y, col="black")
polygon(c(rev(X), X), c(rev(Y_hat_lb), Y_hat_ub), col = 'grey')
lines(X, Y_hat_med, col="Red", lw=2)
lines(X, Y_true, col="blue",lw=2)

