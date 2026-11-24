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
# read zipcode snapshots
setwd(wd_2clean)
zip_files = list.files(pattern = "^zipcodes v[0-9]{4}-(0[1-9]|1[0-2])[.]qs2$")
if (!length(zip_files)) stop("No versioned ZIP snapshots in 2 Clean.")

# read chapter membership data
setwd(wd_2clean)
membership = qs2::qs_read("chapter_membership.qs2")

month_key = c("cid", "month_year")
stopifnot(inherits(membership$month_year, "Date"),
          !anyDuplicated(membership, by = month_key))

read_snapshot = function(prefix, snapshot, key) {
  path = paste0(prefix, " v", snapshot, ".qs2")
  if (!file.exists(path)) stop("Missing input in 2 Clean: ", path)
  setwd(wd_2clean)
  input = qs2::qs_read(path)
  if (!all(c(key, "zip_snapshot") %in% names(input)) ||
      anyNA(input[, ..key]) || anyDuplicated(input, by = key) ||
      !all(input$zip_snapshot %in% snapshot)) {
    stop("Invalid join keys or ZIP snapshot in ", path)
  }
  input[, zip_snapshot := NULL]
  input
}

#####
# merge and save data
#####
chapter_data_list = list()
for (zip_file in zip_files) {
  snapshot = substr(zip_file, 11, 17)
  population = read_snapshot("chapter_population_monthly", snapshot, month_key)
  demographics = read_snapshot("chapter_acs_demographics", snapshot, "cid")
  aip = read_snapshot("chapter_aip", snapshot, "cid")
  election = qs2::qs_read(file.path(
    wd_2clean, paste0("chapter_election_fekrazad v", snapshot, ".qs2")
  ))
  stopifnot(!anyNA(election$cid), !anyDuplicated(election$cid))
  stopifnot(inherits(population$month_year, "Date"))

  # Population coverage varies by ACS year. Keep the demographics coverage separately.
  coverage_cols = c("chapter_zip_count", "matched_zcta_count", "unmatched_zip_count")
  setnames(demographics, coverage_cols, paste0("demographics_", coverage_cols))

  chapter_data = merge(membership, population, by = month_key, all.x = TRUE)
  chapter_data = merge(chapter_data, demographics, by = "cid", all.x = TRUE)
  chapter_data = merge(chapter_data, aip, by = "cid", all.x = TRUE)
  chapter_data = merge(chapter_data, election, by = "cid", all.x = TRUE)
  chapter_data[, zip_snapshot := snapshot]
  setorder(chapter_data, cid, month_year)
  stopifnot(nrow(chapter_data) == nrow(membership),
            !anyDuplicated(chapter_data, by = month_key))
  qs2::qs_save(chapter_data, file.path(wd_3merge, paste0("chapter_data v", snapshot, ".qs2")))

  # add to list for main merged file
  chapter_data[, zip_vintage := snapshot]
  chapter_data_list = append(chapter_data_list, list(chapter_data))
}

#####
# write master merged file
#####
chapter_data_main = rbindlist(chapter_data_list)
setwd(wd_3merge)
qs2::qs_save(chapter_data_main, "chapter_data.qs2")
