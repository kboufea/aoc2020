change_direction <- function(action, value, current_dir) {
  new_dir <- ifelse(action == "R", current_dir + value, current_dir - value)
  
  if (new_dir > 270) new_dir <- new_dir - 360
  else if (new_dir < 0) new_dir <- new_dir + 360
  
  new_dir
}

move_ship <- function(action, value) {
  if (action == "N") return(c(0, value))
  else if (action == "S") return(c(0, -value))
  else if (action == "E") return(c(-value, 0))
  else if (action == "W") return(c(value, 0))
  
}


#instructions <- c("F10", "N3", "F7", "R90", "F11")
instructions <- read.delim("aoc2020/data/day12_input.txt", stringsAsFactors = F, header = F)$V1

# Assuming the following for direction
# North is 0 degrees
# East is 90 degrees
# South is 180 degrees
# West is 270 degrees
d <- 90

directions <- c("N", "E", "S", "W")
names(directions) <- c(0, 90, 180, 270)

pos <- c(0, 0)

for (instruction in instructions) {
  action <- substring(instruction, 1, 1)
  value <- as.numeric(gsub(action, "", instruction))
  if (action %in% c("R", "L")) {
    d <- change_direction(action, value, d)
  } else {
    if (action == "F") action <- directions[as.character(d)]
    pos <- pos + move_ship(action, value)
  }
}

sum(abs(pos))

