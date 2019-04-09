rm(list = ls())
library("tidyverse")

create_Sigma <- function(rho){
    matrix(c(1, rho, rho, 1), ncol = 2, byrow = TRUE)
}
K <- 5
N_k <- 1e3
rho_k <- runif(K, .2, .7)
dta <- data.frame(x = NULL, y = NULL)
for(i in seq(K)){
    dta <- rbind.data.frame(
        dta, MASS::mvrnorm(N_k, c(i, K - i), create_Sigma(rho_k[i]))
    )
}
names(dta) <- c("x", "y")
dta <- mutate(dta, group = factor(rep(seq(K), each = N_k)))

ggplot(data = dta, aes(x = x, y = y)) +
    geom_point(aes(colour = group), alpha = .6) +
    geom_smooth(aes(linetype = "In group", group = group), method = "lm", se = FALSE) +
    geom_smooth(aes(linetype = "Across groups"), method = "lm", se = FALSE) +
    labs(title = "Simpson's Paradox Demonstrated", linetype = "Correlation", colour = "Group") +
    ggthemes::theme_fivethirtyeight() +
    theme(legend.box = "horizontal")

lm(y ~ x, data = dta)
lm(y ~ x + group, data = dta)
