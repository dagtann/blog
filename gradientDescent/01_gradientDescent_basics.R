rm(list = ls())
set.seed(49018321)

computeCost <- function(X, theta, y){
    m <- nrow(X)
    J <- 1 / (2 * m) * sum((X %*% theta - y) ^ 2)
    return(J)
}


updateTheta <- function(X, theta, y, alpha = .5){
    m <- length(y)
    theta.prime <- theta - alpha * (1 / m) * t(X) %*% (X %*% theta - y)
    return(theta.prime)
}

executeGradientDescent <- function(x, y, iter = 100) {
    if(is.matrix(x)){ k <- ncol(x) } else { k <- 1}
    X <- cbind(1, x)

    initialTheta <- runif(n = k + 1)
    J <- vector("numeric", length = iter)
    Theta <- matrix(FALSE, nrow = iter, ncol = k + 1)
    Theta[1, ] <- initialTheta

    for(i in seq(iter)[-iter]){
        J[i] <- computeCost(X, Theta[i, ], y)
        Theta[i + 1, ] <- updateTheta(X, Theta[i, ], y)
        if(i == iter - 1){
            J[i + 1] <- t(computeCost(X, Theta[i, ], y))
        }
    }
    return(list(J = J, Theta = Theta))
}


generateContourData <- function(k, grid.range = 2, grid.size = 100){
    betas <- paste0("beta", seq(k))
    betaData <- vapply(
        betas,
        FUN = function(x){seq(-grid.range, grid.range, length.out = grid.size)},
        FUN.VALUE = numeric(grid.size)
    )
    out <- data.frame(betaData[, 1])
    for(i in seq(ncol(betaData))[-1]){
        out <- expandGridDf(out, betaData[, i])
    }
    names(out) <- betas
    return(out)
}

expandGridDf <- function(...){
    Reduce(function(...) merge(..., by=NULL), list(...))
}
# Source: https://stackoverflow.com/questions/11693599/alternative-to-expand-grid-for-data-frames


k <- 2
theta <- runif(k, min = -2, max = 2)
sigma <- 1
N <- 1000
x <- MASS::mvrnorm(
    N, mu = rep(0, length(theta) - 1), Sigma = diag(length(theta) - 1)
)
y <- as.vector(cbind(1, x) %*% theta) + rnorm(N, sd = sigma)
results <- executeGradientDescent(x, y)


beta_contour <- generateContourData(k = length(theta), grid.size = 100)
beta_contour[, "J"] <- vapply(seq(nrow(beta_contour)),
    FUN = function(i){computeCost(cbind(1, x), t(beta_contour[i, ]), y)},
    FUN.VALUE = numeric(1)
)

library("ggplot2")
pdta <- data.frame(results[["Theta"]], results[["J"]])
names(pdta) <- names(beta_contour)
ggplot(data = beta_contour, aes(x = beta1, y = beta2, z = J)) +
    geom_contour(aes(colour = ..level..), bins = 20) +
    scale_colour_gradient2( ) +
    geom_path(data = pdta) + geom_point(data = pdta) +
    geom_vline(xintercept = theta[1]) +
    geom_hline(yintercept = theta[2])