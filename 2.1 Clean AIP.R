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
# read AIP data
setwd(wd_1raw)
aip = read.dta("aip_zips_ideology_v2022a.dta")
aip = data.table(aip)

# read membership data
setwd(wd_1raw)
chapter_metadata = qs2::qs_read("membership_chapters.qs2")

#####
# munge data
#####
# subset years
aip = aip[survey_period == "2017-2021", ]

# Reverse both scales so higher values indicate more left-wing ideology.
aip[, `:=`(self_ideology = 6 - self_ideology, mrp_ideology = -mrp_ideology)]

# remove columns
cols_rm = c("zip_name", "demshare_pres")
aip[, (cols_rm) := NULL]

#####
# save overall data
#####
setwd(wd_2clean)
qs2::qs_save(aip, "aip.qs2")

#####
# construct AIP measures for each chapter-group of zipcodes
#####
metrics = c("self_ideology", "mrp_ideology")
aip_zips = aip[!is.na(zip), c("zip", "population_2020", metrics), with = FALSE]
aip_zips[, zipcode := sprintf("%05d", as.integer(zip))]
aip_zips[, zip := NULL]
aip_zips = unique(aip_zips)
stopifnot(!anyDuplicated(aip_zips$zipcode))

# read chapter ids from metadata
chapter_ids = unique(chapter_metadata[
  !is.na(cid) & nzchar(trimws(cid)),
  .(chapter = original_name, state = original_state, cid)
])

# list zipfiles
setwd(wd_2clean)
zip_files = list.files(pattern = "^zipcodes v[0-9]{4}-(0[1-9]|1[0-2])[.]qs2$")
if (!length(zip_files)) stop("No versioned ZIP snapshots in 2 Clean.")

population_weighted_mean = function(values, weights) {
  usable = is.finite(values) & is.finite(weights) & weights > 0
  if (!any(usable)) return(NA_real_)
  weighted.mean(values[usable], weights[usable])
}

for (zip_file in zip_files) {
  # read zip file
  snapshot = substr(zip_file, 11, 17)
  setwd(wd_2clean)
  chapter_zips = qs2::qs_read(zip_file)

  # obtain zip ids
  zip_ids = merge(chapter_zips, chapter_ids, by = c("chapter", "state"), all.x = TRUE)
  if (nrow(zip_ids) != nrow(chapter_zips) || anyNA(zip_ids$cid)) {
    stop("Missing or ambiguous chapter IDs in ", zip_file)
  }
  zip_ids = unique(zip_ids[, .(cid, zipcode)])
  data = merge(zip_ids, aip_zips, by = "zipcode", all.x = TRUE)
  estimates = data[, lapply(.SD, population_weighted_mean, weights = population_2020),
                   by = cid, .SDcols = metrics]
  estimates[, zip_snapshot := snapshot]
  setcolorder(estimates, c("cid", "zip_snapshot"))
  setorder(estimates, cid)

  # write data
  setwd(wd_2clean)
  qs2::qs_save(estimates, paste0("chapter_aip v", snapshot, ".qs2"))
}
