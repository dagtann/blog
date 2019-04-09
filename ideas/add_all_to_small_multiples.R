rm(list = ls())
library("tidyverse")
data(diamonds)

all_level <- "All diamonds"
levels(diamonds$cut) <- c(levels(diamonds$cut), all_level)
table(diamonds$cut)

pdta <- diamonds %>%
    mutate(cut = factor(all_level, levels = levels(diamonds$cut))) %>%
    bind_rows(diamonds)
table(pdta$cut)

ggplot(data = pdta, aes(x = carat, y = price)) +
    geom_point() +
    scale_y_log10() +
    geom_smooth() +
    facet_wrap(vars(cut))