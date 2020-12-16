#input <- read.delim("aoc2020/data/day16_example2.txt", stringsAsFactors = F, header = F)$V1
input <- read.delim("aoc2020/data/day16_input.txt", stringsAsFactors = F, header = F)$V1

s1 <- grep("your ticket:", input)
s2 <- grep("nearby tickets:", input)

rules <- input[1:(s1-1)]
tickets <- input[(s2+1):length(input)]

fields <- list()
for (rule in rules){
    info <- strsplit(rule, ": | or ")[[1]]
    valid_numbers <- c()
    for (i in info[-1]) {
        l <- as.numeric(strsplit(i, "-")[[1]][1])
        h <- as.numeric(strsplit(i, "-")[[1]][2])
        valid_numbers <- c(valid_numbers, l:h)
    }
    fields[[info[1]]] <- valid_numbers
}

# Parse tickets removing invalid ones
tickets_df <- data.frame(matrix(NA, nrow = 1, ncol = length(rules)))
for (i in seq_along(tickets)) {
    values <- as.numeric(strsplit(tickets[i], ",")[[1]])
    if (all(values %in% unique(unlist(fields)))) tickets_df <- rbind(tickets_df, values)
}
tickets_df <- tickets_df[complete.cases(tickets_df), ]

# Find fields
matching_fields <- list()
for (i in 1:ncol(tickets_df)) {
    values <- unlist(tickets_df[, i])
    s <- c()
    for (j in 1:length(fields)) {
        if (all(values %in% fields[[j]])) s <- c(s, names(fields)[j])
    }
    matching_fields[[i]] <- s
}
matching_fields

# Iteratively assign fields to columns of df
while (length(unlist(matching_fields)) > 0) {
    f <- which(sapply(matching_fields, function(x) length(x)) == 1)
    colnames(tickets_df)[f] <- unlist(matching_fields[f])
    s <- unlist(matching_fields[f])
    matching_fields <- lapply(matching_fields, function(x) setdiff(x, s))
}

my_ticket <- as.numeric(strsplit(input[(s1+1):(s2-1)], ",")[[1]])
prod(my_ticket[grep("departure", colnames(tickets_df))])

