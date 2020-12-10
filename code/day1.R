
input <- read.delim("../data/day1_input.txt", stringsAsFactors = FALSE, head = F)$V1
l <- length(input)

f <- function(values) {
    if (sum(values) == 2020) prod(values)
}

# Part 1
combinations <- t(combn(1:l, 2))
unlist(apply(
    combinations, 
    1, 
    function(x) f(values = c(input[x[1]], input[x[2]]))
))
            
# Part 2
combinations <- t(combn(1:l, 3))
unlist(apply(
    combinations, 
    1, 
    function(x) f(values = c(input[x[1]], input[x[2]], input[x[3]]))
))


