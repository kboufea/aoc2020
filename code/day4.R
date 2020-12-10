library(dplyr)
library(tidyr)

data <- readLines("data/day4_input.txt")
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

mandatory_fields <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
n <- 0
for (i in seq_along(data)) {
    info <- stringr::str_split(data[i], " ")[[1]]
    fields <- unlist(lapply(info, function(x) stringr::str_split(x, ":")[[1]][1]))
    if (!all(mandatory_fields %in% fields)) next
    
    byr <- as.numeric(stringr::str_split(info[grep("byr", info)], ":")[[1]][2])
    if (byr < 1920 || byr > 2002) next
    
    iyr <- as.numeric(stringr::str_split(info[grep("iyr", info)], ":")[[1]][2])
    if (iyr < 2010 || iyr > 2020) next
    
    eyr <- as.numeric(stringr::str_split(info[grep("eyr", info)], ":")[[1]][2])
    if (eyr < 2020 || eyr > 2030) next
    
    hgt <- stringr::str_split(info[grep("hgt", info)], ":")[[1]][2]
    if (grepl("in", hgt)) {
        hgt <- as.numeric(gsub("in", "", hgt))
        if (hgt < 59 || hgt > 76) next
    } else if (grepl("cm", hgt)) {
        hgt <- as.numeric(gsub("cm", "", hgt))
        if (hgt < 150 || hgt > 193) next
    } else {
        next
    }
    
    hcl <- stringr::str_split(info[grep("hcl", info)], ":")[[1]][2]
    if (!(grepl("^#[0-9a-f]{6}", hcl) && nchar(hcl) == 7)) next
    
    ecl <- stringr::str_split(info[grep("ecl", info)], ":")[[1]][2]
    if (!ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")) next
    
    pid <- stringr::str_split(info[grep("pid", info)], ":")[[1]][2]
    if (!(!grepl("[A-Za-z]", pid) && nchar(pid) == 9)) next
    
    n <- n + 1
}
print(n)
