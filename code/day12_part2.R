rotate_waypoint <- function(action, value, waypoint) {
  if (action == "R") {
    if (value == 90) waypoint <- c(-waypoint[2], waypoint[1])
    else if (value == 180) waypoint <- c(-waypoint[1], -waypoint[2])
    else if (value == 270) waypoint <- c(waypoint[2], -waypoint[1])
  } else {
    if (value == 90) waypoint <- c(waypoint[2], -waypoint[1])
    else if (value == 180) waypoint <- c(-waypoint[1], -waypoint[2])
    else if (value == 270) waypoint <- c(-waypoint[2], waypoint[1])
  }
  return(waypoint)
}

move_waypoint <- function(action, value) {
  if (action == "N") return(c(0, value))
  else if (action == "S") return(c(0, -value))
  else if (action == "E") return(c(-value, 0))
  else if (action == "W") return(c(value, 0))
  
}

instructions <- c("F10", "N3", "F7", "R90", "F11")
instructions <- read.delim("aoc2020/data/day12_input.txt", stringsAsFactors = F, header = F)$V1

waypoint <- c(-10, 1)
waypoint_dir <- c("E", "N")
ship <- c(0, 0)
for (instruction in instructions) {
  action <- substring(instruction, 1, 1)
  value <- as.numeric(gsub(action, "", instruction))
  if (action %in% c("R", "L")) {
    #waypoint_dir <- change_direction(action, value, waypoint_dir)
    waypoint <- rotate_waypoint(action, value, waypoint)
    #waypoint <- rotate_waypoint(waypoint, d)
  } else if (action %in% c("N", "E", "S", "W")) {
    waypoint <- waypoint + move_waypoint(action, value)
  } else if (action == "F") {
    ship <- ship + value * waypoint
  }
  print(waypoint)
  print(ship)
  print("")
}


sum(abs(ship))


