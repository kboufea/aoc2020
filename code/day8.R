
parse_operation <- function(op, acc, id) {
    value <- as.numeric(gsub("[^0-9.]", "",  op))
    if (grepl("nop", op)) {
        return(list(acc = acc, id = id + 1))
    } else if (grepl("acc", op)) {
        ifelse(
            grepl("\\+", op), 
            return(list(acc = acc + value, id = id + 1)), 
            return(list(acc = acc - value, id = id + 1))
        )
    } else if (grepl("jmp", op)) {
        ifelse(
            grepl("\\+", op), 
            return(list(acc = acc, id = id + value)), 
            return(list(acc = acc, id = id - value))
        )
    }
}

# input <- c(
#     "nop +0",
#     "acc +1",
#     "jmp +4",
#     "acc +3",
#     "jmp -3",
#     "acc -99",
#     "acc +1",
#     "jmp -4",
#     "acc +6"
# )

input <- read.delim("adventOfCode2020/data/day8_input.txt", stringsAsFactors = FALSE, header = FALSE)$V1

# Part 1
acc <- 0
track <- data.frame(
    OperationID = seq_along(input), 
    Count = c(1, rep(0, length(input)-1))
)
ID = 1
while(!any(track$Count == 2) && ID <= length(input)) {
    res <- parse_operation(input[ID], acc, ID)
    ID <- res$id
    acc <- res$acc
    track[ID, 2] <- track[ID, 2] + 1
}
acc

# Part 2
nops <- grep("nop", input)
for (i in nops) {
    new_input <- input
    new_input[i] <- gsub("nop", "jmp", new_input[i])
    acc <- 0
    track <- data.frame(
        OperationID = seq_along(new_input), 
        Count = c(1, rep(0, length(new_input)-1))
    )
    ID = 1
    while(!any(track$Count == 2) && ID <= length(new_input)) {
        res <- parse_operation(new_input[ID], acc, ID)
        previousID <- ID
        ID <- res$id
        acc <- res$acc
        track[ID, 2] <- track[ID, 2] + 1
    }
    if (ID > length(new_input)) {
        print(acc)
        break
    }
    
}

jmps <- grep("jmp", input)
for (i in jmps) {
    new_input <- input
    new_input[i] <- gsub("jmp", "nop", new_input[i])
    acc <- 0
    track <- data.frame(
        OperationID = seq_along(new_input), 
        Count = c(1, rep(0, length(new_input)-1))
    )
    ID = 1
    while(!any(track$Count == 2) && ID <= length(new_input)) {
        res <- parse_operation(new_input[ID], acc, ID)
        previousID <- ID
        ID <- res$id
        acc <- res$acc
        track[ID, 2] <- track[ID, 2] + 1
    }
    if (ID > length(new_input)) {
        print(acc)
        break
    }
    
}

