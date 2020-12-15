input <- read.table("aoc2020/data/day11_input.txt", stringsAsFactors = F, header = F)
df <- t(apply(input, 1, function(x) unlist(strsplit(x, ""))))

changes <- 1
combinations <- expand.grid(1:nrow(df), 1:ncol(df))
new_df <- df

# Part 1
while (changes > 0) {
  changes <- 0
  for (c in 1:nrow(combinations)) {
    i <- combinations[c, 1]
    j <- combinations[c, 2]
    if (df[i, j] != "\\.") {
      adjacent_seats <- list(
        c(i-1, j-1),
        c(i-1, j),
        c(i-1, j+1),
        c(i, j-1),
        c(i, j+1),
        c(i+1, j-1),
        c(i+1, j),
        c(i+1, j+1)
      )
      # remove seats outside frame
      adjacent_seats <- adjacent_seats[unlist(lapply(adjacent_seats, function(x) x[2] > 0 && x[2] <= ncol(df) && x[1] > 0 && x[1] <= nrow(df)))]
      if (df[i, j] == "L" && sum(unlist(lapply(adjacent_seats, function(x) df[x[1], x[2]])) == "#") == 0) {
          new_df[i, j] <- "#"
          changes <- changes + 1
      } else if (df[i, j] == "#" && sum(unlist(lapply(adjacent_seats, function(x) df[x[1], x[2]])) == "#") >= 4){
          new_df[i, j] <- "L"
          changes <- changes + 1
      }
    }
  }
  df <- new_df
  changes <- changes - 1
}

sum(new_df == "#")


# Part 2
get_seat <- function(direction, df, i, j) {
  if (direction == "N") {
    i <- i - 1
    seat <- "."
    while ((i > 0) && (seat == ".")) {
      if (df[i, j] != ".") seat <- df[i, j]
      i <- i - 1
    }
  } else if (direction == "S") {
    i <- i + 1
    seat <- "."
    while ((i <= nrow(df) ) && (seat == ".")) {
      if (df[i, j] != ".") seat <- df[i, j]
      i <- i + 1
    }
  } else if (direction == "E") {
    j <- j + 1
    seat <- "."
    while ((j <= ncol(df)) && (seat == ".")) {
      if (df[i, j] != ".") seat <- df[i, j]
      j <- j + 1
    }
  } else if (direction == "W") {
    j <- j - 1
    seat <- "."
    while ((j > 0) && (seat == ".")) {
      if (df[i, j] != ".") seat <- df[i, j]
      j <- j - 1
    }
  } else if (direction == "NE") {
    i <- i - 1
    j <- j + 1
    seat <- "."
    while (((i > 0) && (j <= ncol(df))) && (seat == ".")) {
      if (df[i, j] != ".") seat <- df[i, j]
      i <- i - 1
      j <- j + 1
    }
  } else if (direction == "NW") {
    i <- i - 1
    j <- j - 1
    seat <- "."
    while (((i > 0) && (j > 0)) && (seat == ".")) {
      if (df[i, j] != ".") seat <- df[i, j]
      i <- i - 1
      j <- j - 1
    }
  } else if (direction == "SE") {
    i <- i + 1
    j <- j + 1
    seat <- "."
    while (((i <= nrow(df)) && (j <= ncol(df))) && (seat == ".")) {
      if (df[i, j] != ".") seat <- df[i, j]
      i <- i + 1
      j <- j + 1
    }
  } else if (direction == "SW") {
    i <- i + 1
    j <- j - 1
    seat <- "."
    while (((i <= nrow(df)) && (j > 0)) && (seat == ".")) {
      if (df[i, j] != ".") seat <- df[i, j]
      i <- i + 1
      j <- j - 1
    }
  }
  seat
}

while (changes > 0) {
  print(changes)
  changes <- 0
  for (c in 1:nrow(combinations)) {
    i <- combinations[c, 1]
    j <- combinations[c, 2]
    if (df[i, j] != "\\.") {
      closest_seats <- sapply(
        c("N", "S", "E", "W", "NE", "NW", "SE", "SW"), 
        function(x) get_seat(direction = x, df = df, i = i, j = j)
      )
      if (df[i, j] == "L" && !("#" %in% closest_seats)) {
        new_df[i, j] <- "#"
        changes <- changes + 1
      } else if (df[i, j] == "#" && sum(closest_seats == "#") >= 5){
        new_df[i, j] <- "L"
        changes <- changes + 1
      }
    }
  }
  df <- new_df
  changes <- changes - 1
}

sum(new_df == "#")
