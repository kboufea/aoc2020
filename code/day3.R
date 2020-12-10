library(dplyr)
library(tidyr)

count_trees <- function(df, i_step, j_step) {
    i <- 1
    j <- 1
    ntrees <- 0
    while (i <= nrow(df)) {
        if (df[i, j] == "#") ntrees <- ntrees + 1
        i <- i + i_step
        j <- j + j_step
    }
    ntrees
}

grid <- read.delim("data/day3_input.txt", stringsAsFactors = F, header = F)
df <- t(apply(grid, 1, function(x) unlist(strsplit(x, ""))))
df <- df[, rep(1:ncol(df), nrow(df))]

# Part 1
count_trees(df, i_step = 1, j_step = 3)

# Part 2
steps <- list(
    c(1, 1), 
    c(1, 3), 
    c(1, 5), 
    c(1, 7), 
    c(2, 1)
)
res <- unlist(lapply(steps, function(x) count_trees(df, i_step = x[1], j_step = x[2])))
prod(res)
