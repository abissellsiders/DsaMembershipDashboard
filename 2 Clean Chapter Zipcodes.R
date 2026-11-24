#####
# init
#####
# clear old data
rm(list=ls(all=TRUE))

# set working directory
(wd = "C:/gdrive/dsa/DsaMembershipDashboard")
setwd(wd)

# load globals
source("0 Setup.R")

#####
# read data
#####
setwd(wd_1raw)
read_zipcodes = function(path) {
  read.csv(path, fileEncoding = "UTF-16LE", colClasses = "character",
           check.names = FALSE)
}

zipcodes_2025 = read_zipcodes("zipcodes v2025-05.csv")
zipcodes_2026 = read_zipcodes("zipcodes v2026-02.csv")

#####
# munge data
#####
clean_zipcodes = function(data, chapter_col, zipcode_col) {
  required_cols = c(chapter_col, "State", zipcode_col)
  stopifnot(all(required_cols %in% names(data)))

  chapter = trimws(data[[chapter_col]])
  state = trimws(data[["State"]])
  zipcodes = strsplit(trimws(data[[zipcode_col]]), "[,;[:space:]]+")
  stopifnot(!anyNA(chapter), !anyNA(state), all(nzchar(chapter)),
            all(nzchar(state)), all(lengths(zipcodes) > 0L))

  zipcode = unlist(zipcodes, use.names = FALSE)
  stopifnot(!anyNA(zipcode), all(grepl("^[0-9]{1,5}$", zipcode)))

  long = data.table(
    chapter = rep(chapter, lengths(zipcodes)),
    state = rep(state, lengths(zipcodes)),
    zipcode = stringr::str_pad(zipcode, width = 5, side = "left", pad = "0")
  )
  long = unique(long)
  setorder(long, chapter, state, zipcode)
  long
}

zipcodes_2025 = clean_zipcodes(zipcodes_2025, "Chapter", "ZIP Code")
zipcodes_2026 = clean_zipcodes(zipcodes_2026, "Name", "Zipcodes")

#####
# save data
#####
setwd(wd_2clean)
qs2::qs_save(zipcodes_2025, "zipcodes v2025-05.qs2")
qs2::qs_save(zipcodes_2026, "zipcodes v2026-02.qs2")
