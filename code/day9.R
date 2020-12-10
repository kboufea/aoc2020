library(dplyr)

numbers <- read.delim("adventOfCode2020/data/day9_input.txt", stringsAsFactors = F, header = F)$V1

preamble_length <- 25

isValid <- function(n, values) {
    
    combinations <- expand.grid(seq_along(values), seq_along(values)) %>%
        filter(Var1 != Var2)
    allSums <- apply(combinations, 1, function(x) values[x[1]] + values[x[2]])
    
    n %in% allSums
}

# Part 1
# Apply in sliding window of preamble_length
res <- unlist(lapply(
    (preamble_length + 1) : length(numbers), 
    function(x) isValid(
        n = numbers[x], 
        values = numbers[(x - preamble_length) : (x - 1)]
    )
))
part1_res <- numbers[which(!res) + preamble_length]

# Part 2

part2_res <- NA
for (i in 1:(length(numbers) - 1)) {
    j <- i + 1
    while((j < length(numbers)) && is.na(part2_res)) {
        if (sum(numbers[i:j]) == part1_res) part2_res <- min(numbers[i:j]) + max(numbers[i:j])
        j <- j + 1
    }
    if (!is.na(part2_res)) break
}
part2_res
