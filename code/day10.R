# input <- sort(c(16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4))

# input <- sort(c(
#     28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49,
#     45, 19, 38, 39, 11, 1, 32, 25, 35,8, 17, 7, 9, 4,
#     2, 34, 10, 3
# ))

input <- sort(read.delim(
    "aoc2020/data/day10_input.txt", 
    stringsAsFactors = FALSE,
    header = FALSE
)$V1)

# Part 1
input <- c(0, input)

v2 <- c(input[-1],  max(input) + 3)

res <- table(v2 - input)
res[1] * res[2]

# Part 2
res <- v2 - input

s <- strsplit(paste(res, collapse = ""), "13")[[1]]
s <- unlist(lapply(s, function(x) strsplit(x, "3")))
s <- stringi::stri_remove_empty(s)


count_combinations <- function(v) {
    combinations <- apply(
        expand.grid(replicate(nchar(v), c(0,1), simplify=FALSE)), 
        1, 
        function(x) paste(x, collapse = "")
    )
    # We can't skip more that 2 consecutive 1s
    if (nchar(v) >= 3) {
        remove <- c()
        for (i in 3:nchar(v)) {
            remove<- c(
                remove, 
                which(grepl(paste(rep(0, i), collapse = ""), combinations))
            )
        }
        combinations <- combinations[-unique(remove)]
    }
    length(combinations)
}
prod(n)
