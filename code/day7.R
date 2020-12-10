library(stringr)

data <- read.delim("data/day7_input.txt", stringsAsFactors = FALSE, header = FALSE)$V1

rules <- list()
for (i in seq_along(data)) {
    info <- str_split(data[i], " bags contain |, ")[[1]]
    info <- gsub("[0-9]+|bags|bag|\\.", "", info)
    info <- str_trim(info)
    if ("no other" %in% info) next
    rules[[info[1]]] <- info[-1]
}

my_bag <- "shiny gold"
n <- 0
found <- c()
for (i in seq_along(rules)) {
    # Check direct fold
    if (my_bag %in% rules[[i]]) {
        n <- n + 1
        found <- c(found, rules[[i]])
    } else {
        # Check indirect
        for (bag in rules[[i]]) {
            if (my_bag %in% rules[[bag]]) {
                n <- n + 1
                found <- c(found, bag)
                break
            }
        }
        
    }
    
}
