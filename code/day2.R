
data <- read.delim("../data/day2_input.txt", stringsAsFactors = FALSE, head = FALSE)$V1

# Part 1
n <- 0
for (i in seq_along(data)) {
    info <- strsplit(data[i], " |:|-")[[1]]
    s <- stringr::str_count(info[5], info[3])
    if (s >= as.numeric(info[1]) && s <= as.numeric(info[2])) {
        n <- n + 1
    }
}

print(n)

# Part 2
n <- 0
for (i in seq_along(data)) {
    info <- strsplit(data[i], " |:|-")[[1]]
    chars <- stringr::str_split(info[5], "")[[1]]
    if (xor(chars[as.numeric(info[1])] == info[3], chars[as.numeric(info[2])] == info[3])) {
        n <- n + 1
    }
}

print(n)


