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
zcta_zip = file.path(wd_1raw, "cb_2020_us_zcta520_500k.zip")
zcta_county_crosswalk = file.path(wd_1raw, "tab20_zcta520_county20_natl.txt")
output_dir = file.path(wd_2clean, "us_zcta_reference_shapefile")
output_path = file.path(output_dir, "us_zcta_reference.shp")

if (!all(file.exists(c(zcta_zip, zcta_county_crosswalk)))) {
  sidecars = paste0(tools::file_path_sans_ext(output_path), c(".shp", ".shx", ".dbf", ".prj"))
  if (!all(file.exists(sidecars))) {
    stop("Run 1 Download US ZCTA Reference.R to obtain the ZCTA boundaries and county crosswalk.")
  }
  message("Using the existing cleaned ZCTA reference shapefile. Raw Census inputs are unavailable.")
} else {
  zcta_path = paste0("/vsizip/", normalizePath(zcta_zip, winslash = "/"),
                     "/cb_2020_us_zcta520_500k.shp")
  zctas = sf::st_read(zcta_path, quiet = TRUE)
  crosswalk = fread(zcta_county_crosswalk, sep = "|", colClasses = "character")

  #####
  # munge data
  #####
  state_lookup = data.frame(
    state_fips = c(
      "01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13",
      "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25",
      "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36",
      "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", "48",
      "49", "50", "51", "53", "54", "55", "56", "72"
    ),
    map_state = c(
      "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA",
      "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA",
      "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY",
      "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX",
      "UT", "VT", "VA", "WA", "WV", "WI", "WY", "PR"
    ),
    stringsAsFactors = FALSE
  )

  crosswalk = crosswalk[!is.na(GEOID_ZCTA5_20) & GEOID_ZCTA5_20 != ""]
  crosswalk[, AREALAND_PART_NUM := suppressWarnings(as.numeric(AREALAND_PART))]
  data.table::setorder(crosswalk, GEOID_ZCTA5_20, -AREALAND_PART_NUM)
  crosswalk = crosswalk[, .SD[1], by = GEOID_ZCTA5_20]
  crosswalk[, state_fips := substr(GEOID_COUNTY_20, 1, 2)]
  crosswalk = crosswalk[, .(ZCTA5CE20 = GEOID_ZCTA5_20, state_fips)]

  zctas = merge(zctas, crosswalk, by = "ZCTA5CE20", all.x = FALSE)
  zctas = merge(zctas, state_lookup, by = "state_fips", all.x = FALSE)

  reference_shapes = withr::with_options(list(sf_use_s2 = FALSE), stats::aggregate(
    zctas["map_state"],
    by = list(map_state = zctas$map_state),
    FUN = function(x) x[1],
    do_union = TRUE
  ))
  reference_shapes = sf::st_make_valid(reference_shapes)
  reference_shapes = sf::st_transform(reference_shapes, 4326)

  #####
  # write data
  #####
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  sf::st_write(reference_shapes, output_path, delete_layer = TRUE, quiet = TRUE)

  message("Wrote ", nrow(reference_shapes), " ZCTA-derived reference geometries to ", output_path)
}
