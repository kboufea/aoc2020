
codes <- read.delim("adventOfCode2020/data/day5_input.txt", stringsAsFactors = F, header = F)$V1

find_seat <- function(code) {
  characters <- stringr::str_split(code, "")[[1]]
  
  row <- 0:127
  for (i in 1:7) {
    l <- length(row)
    if (characters[i] == "F") row <- row[1:round(l/2)]
    else if (characters[i] == "B") row <- row[(round(l/2) + 1):l]
    else stop("Invalid code")
  }
  
  column <- 0:7
  for (i in 8:10) {
    l <- length(column)
    if (characters[i] == "L") column <- column[1:round(l/2)]
    else if (characters[i] == "R") column <- column[(round(l/2) + 1):l]
    else stop("Invalid code")
  }
  
  ID <- row * 8 + column
  
  return(list("row" = row, "column" = column, "ID" = ID))
}

# Part 1
max(unlist(lapply(codes, function(x) find_seat(x)$ID)))

# Part 2

get_ID <- function(x) {
  info <- strsplit(x, "_")[[1]]
  as.numeric(info[1]) * 8 + as.numeric(info[2])
  
}

# Do not include rows 0 and 127 as it's not my seat
remaining_seats <- expand.grid(1:126, 0:7)
remaining_seats <- unlist(apply(remaining_seats, 1, function(x) paste(x[1], x[2], sep = "_")))

for (i in seq_along(codes)) {
  res <- find_seat(codes[i])
  remaining_seats <- setdiff(remaining_seats, paste(res$row, res$column, sep = "_"))
}

# Calculate IDs of remaining seats
remaining_IDs <- unlist(lapply(remaining_seats, function(x) get_ID(x))) 

# Find ID that also has +1 and -1 in list
for (i in seq_along(remaining_IDs)) {
  ID <- remaining_IDs[i]
  if (!(((ID + 1) %in% remaining_IDs) && ((ID - 1) %in% remaining_IDs))) {
    remaining_IDs <- setdiff(remaining_IDs, ID)
  }
}




