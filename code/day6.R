library(dplyr)
library(tidyr)

data <- readLines("data/day6_input.txt")
nvec <- length(data)
breaks <- which(! nzchar(data))
nbreaks <- length(breaks)
if (breaks[nbreaks] < nvec) {
  breaks <- c(breaks, nvec + 1L)
  nbreaks <- nbreaks + 1L
}
if (nbreaks > 0L) {
  data <- mapply(function(a,b) paste(data[a:b], collapse = " "),
                 c(1L, 1L + breaks[-nbreaks]),
                 breaks - 1L)
}

# Part 1
nYes <- c()
for (i in seq_along(data)) {
  chars <- setdiff(stringr::str_split(data[i], "")[[1]], " ")
  nYes[i] <- length(unique(chars))
}
sum(nYes)


# Part 2
nYes <- c()
for (i in seq_along(data)) {
  answers <- stringr::str_split(data[i], " ")[[1]]
  chars <- unlist(lapply(answers, function(x) stringr::str_split(x, "")[[1]]))
  nYes[i] <- sum(table(chars) == length(answers))
}
sum(nYes)
