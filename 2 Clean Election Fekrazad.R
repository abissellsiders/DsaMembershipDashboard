#####
# init
#####
rm(list = ls(all = TRUE))
setwd("C:/gdrive/dsa/DsaMembershipDashboard")
source("0 Setup.R")

#####
# clean and save presidential votes by ZCTA
#####
dem_columns = list(
  `2016` = c("G16PREDCLI", "G16PREDCli"),
  `2020` = "G20PREDBID"
)
rep_columns = list(
  `2016` = c("G16PRERTRU", "G16PRERTru"),
  `2020` = "G20PRERTRU"
)
zcta_columns = c("zcta_GEOID", "zcta_population")
vote_columns = c("votes_dem", "votes_rep", "votes_total")
green_columns = list(
  `2016` = c("G16PREGSTE", "G16PREGSte", "G16PREISTE", "G16PREMSTE"),
  `2020` = c("G20PREGHAW", "G20PREOHAW", "G20PREMHAW")
)

for (year in c("2016", "2020", "2024")) {
  filename = paste0("election_fekrazad_", year)
  path = file.path(wd_1raw, paste0(filename, ".csv"))
  if (year == "2024") {
    election = fread(path, select = c(zcta_columns, vote_columns),
                     colClasses = c(zcta_GEOID = "character"))
  } else {
    header = names(fread(path, nrows = 0))
    presidential_columns = header[startsWith(header, paste0("G", substr(year, 3, 4), "PRE"))]
    stopifnot(all(c(dem_columns[[year]], rep_columns[[year]], green_columns[[year]]) %in%
                    presidential_columns))
    raw = fread(path, select = c(zcta_columns, presidential_columns),
                colClasses = c(zcta_GEOID = "character"))
    election = raw[, ..zcta_columns]
    # Older files leave candidate fields blank where the column does not apply.
    election[, `:=`(
      votes_dem = rowSums(raw[, dem_columns[[year]], with = FALSE], na.rm = TRUE),
      votes_rep = rowSums(raw[, rep_columns[[year]], with = FALSE], na.rm = TRUE),
      votes_total = rowSums(raw[, presidential_columns, with = FALSE], na.rm = TRUE)
    )]
  }

  stopifnot(all(c(zcta_columns, vote_columns) %in% names(election)),
            all(grepl("^[0-9]{5}$", election$zcta_GEOID)),
            !anyDuplicated(election$zcta_GEOID))
  election[, votes_total_2p := votes_dem + votes_rep]
  election[, `:=`(
    share_dem_2p = fifelse(votes_total_2p > 0, votes_dem / votes_total_2p, NA_real_),
    share_rep_2p = fifelse(votes_total_2p > 0, votes_rep / votes_total_2p, NA_real_)
  )]
  if (year %in% names(green_columns)) {
    # Include nominee votes reported under other ballot-line labels.
    election[, votes_green := rowSums(raw[, green_columns[[year]], with = FALSE], na.rm = TRUE)]
    election[, share_green := fifelse(votes_total > 0, votes_green / votes_total, NA_real_)]
  }
  qs2::qs_save(election, file.path(wd_2clean, paste0(filename, ".qs2")))
}
