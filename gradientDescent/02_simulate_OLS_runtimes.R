rm(list = ls())
library("MASS")
library("tidyverse")

simulateRuntime <- function(X, y){
    start <- Sys.time()
    lm(y ~ X)
    end <- Sys.time()
    end - start
}

n_sample_sizes <- 6
n_replications <- 100
runtimes <- matrix(0, nrow = n_replications, ncol = n_sample_sizes)

N <- 3 * 10 ^ seq(n_sample_sizes)
colnames(runtimes) <- as.character(N)
K <- 2
mu <- rep(0, K)
Sigma <- diag(K)
theta <- rnorm(K)
alpha <- .05

for (n in N) {
    cat("Processing sample size: ", n, "\n")
    X <- mvrnorm(n, mu, Sigma)
    y <- X %*% theta + rnorm(n)
    runtimes[, as.character(n)] <- replicate(n_replications, simulateRuntime(X, y))
}
runtimes <- as.matrix(runtimes)
runtimes <- apply(runtimes, 2, function(x) {
    mu <- mean(x); sigma <- sd(x); c(mu = mu, sigma = sigma) 
    }
)
runtimes <- t(runtimes)
runtimes <- cbind(
    runtimes, "lower" = runtimes[, "mu"] + qnorm(alpha/2) * runtimes[, "sigma"]
)
runtimes <- cbind(
    runtimes, 
    "upper" = runtimes[, "mu"] + qnorm(1 - alpha/2) * runtimes[, "sigma"]
)