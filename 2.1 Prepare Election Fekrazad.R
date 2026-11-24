#####
# init
#####
rm(list = ls(all = TRUE))
setwd("C:/gdrive/dsa/DsaMembershipDashboard")
source("0 Setup.R")

#####
# read elections and chapter ZIP snapshots
#####
chapter_metadata = qs2::qs_read(file.path(wd_1raw, "membership_chapters.qs2"))
chapter_ids = unique(chapter_metadata[
  !is.na(cid) & nzchar(trimws(cid)),
  .(chapter = original_name, state = original_state, cid)
])
zip_files = list.files(wd_2clean, pattern = "^zipcodes v[0-9]{4}-(0[1-9]|1[0-2])[.]qs2$")
if (!length(zip_files)) stop("No versioned ZIP snapshots in 2 Clean.")

years = c(2024, 2020, 2016)
elections = lapply(years, function(year) {
  path = file.path(wd_2clean, paste0("election_fekrazad_", year, ".qs2"))
  columns = c("zcta_GEOID", "votes_dem", "votes_total_2p", "votes_total")
  if (year != 2024) columns = c(columns, "votes_green")
  election = qs2::qs_read(path)[, ..columns]
  setnames(election, "zcta_GEOID", "zipcode")
  election
})

for (zip_file in zip_files) {
  snapshot = substr(zip_file, 11, 17)
  chapter_zips = qs2::qs_read(file.path(wd_2clean, zip_file))
  zip_ids = merge(chapter_zips, chapter_ids, by = c("chapter", "state"), all.x = TRUE)
  if (nrow(zip_ids) != nrow(chapter_zips) || anyNA(zip_ids$cid)) {
    stop("Missing or ambiguous chapter IDs in ", zip_file)
  }
  zip_ids = unique(zip_ids[, .(cid, zipcode)])
  estimates = unique(zip_ids[, .(cid)])

  for (index in seq_along(years)) {
    # A chapter's share is the ratio of summed votes across its matched ZCTAs.
    matched = merge(zip_ids, elections[[index]], by = "zipcode")
    shares = matched[is.finite(votes_total_2p) & votes_total_2p > 0,
                     .(share = sum(votes_dem) / sum(votes_total_2p)), by = cid]
    setnames(shares, "share", paste0("share_dem_2p_", years[index]))
    estimates = merge(estimates, shares, by = "cid", all.x = TRUE)
  }
  for (index in which(years != 2024)) {
    matched = merge(zip_ids, elections[[index]], by = "zipcode")
    shares = matched[is.finite(votes_total) & votes_total > 0,
                     .(share = sum(votes_green) / sum(votes_total)), by = cid]
    setnames(shares, "share", paste0("share_green_", years[index]))
    estimates = merge(estimates, shares, by = "cid", all.x = TRUE)
  }
  qs2::qs_save(estimates, file.path(wd_2clean, paste0("chapter_election_fekrazad v", snapshot, ".qs2")))
}
