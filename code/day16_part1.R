#input <- read.delim("aoc2020/data/day16_example.txt", stringsAsFactors = F, header = F)$V1
input <- read.delim("aoc2020/data/day16_input.txt", stringsAsFactors = F, header = F)$V1

s1 <- grep("your ticket:", input)
s2 <- grep("nearby tickets:", input)

rules <- input[1:(s1-1)]
tickets <- input[(s2+1):length(input)]

valid_numbers <- c()
for (rule in rules){
    info <- strsplit(rule, ": | or ")[[1]][-1]
    for (i in info) {
        l <- as.numeric(strsplit(i, "-")[[1]][1])
        h <- as.numeric(strsplit(i, "-")[[1]][2])
        valid_numbers <- c(valid_numbers, l:h)
    }
}

invalid_values <- c()
for (ticket in tickets) {
    values <- as.numeric(strsplit(ticket, ",")[[1]])
    invalid_values <- c(invalid_values, values[!values %in% valid_numbers])
}
sum(invalid_values)
