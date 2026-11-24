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
# get data
#####
acs_year = 2024L
census_key = Sys.getenv("CENSUS_API_KEY")
if (!nzchar(census_key) && file.exists(".Renviron")) {
  readRenviron(".Renviron")
  census_key = Sys.getenv("CENSUS_API_KEY")
}
if (!nzchar(census_key)) stop("Set CENSUS_API_KEY in the project .Renviron.")

fetch_acs = function(variables, geography, profile = FALSE) {
  endpoint = paste0("https://api.census.gov/data/", acs_year, "/acs/acs5",
                    if (profile) "/profile" else "")
  geo_col = sub(":.*$", "", geography)
  batches = split(variables, ceiling(seq_along(variables) / 48))
  parts = lapply(batches, function(batch) {
    request = httr2::request(endpoint)
    request = httr2::req_url_query(request, get = paste(batch, collapse = ","),
                                  `for` = geography, key = census_key)
    request = httr2::req_retry(httr2::req_timeout(request, 120),
                               max_tries = 5, retry_on_failure = TRUE)
    tryCatch({
      rows = httr2::resp_body_json(httr2::req_perform(request), simplifyVector = FALSE)
      header = unlist(rows[[1]], use.names = FALSE)
      stopifnot(length(rows) > 1L, all(c(batch, geo_col) %in% header))
      part = as.data.table(setNames(lapply(seq_along(header), function(i) {
        vapply(rows[-1], function(row) {
          if (is.null(row[[i]])) NA_character_ else as.character(row[[i]])
        }, character(1))
      }), header))
      stopifnot(!anyNA(part[[geo_col]]), !anyDuplicated(part[[geo_col]]))
      part
    }, error = function(e) {
      stop("ACS ", acs_year, " request failed for ", geography, " (",
           batch[1], ").", call. = FALSE)
    })
  })
  Reduce(function(x, y) merge(x, y, by = geo_col, all = TRUE), parts)
}

zcta = "zip code tabulation area:*"
raw = list(
  acs_year = acs_year,
  profile = fetch_acs(c("NAME", "GEO_ID", names(acs_profile_columns)), zcta, TRUE),
  age = fetch_acs(sprintf("B01001_%03dE", c(1, 3:25, 27:49)), zcta),
  household = fetch_acs(sprintf("B11001_%03dE", c(1, 7, 8)), zcta),
  commute = fetch_acs(sprintf("B08301_%03dE", c(1, 10, 18, 19)), zcta),
  income = fetch_acs(sprintf("B19001_%03dE", 1:17), zcta),
  state = fetch_acs("DP03_0062E", "state:*", TRUE),
  cbsa = fetch_acs("DP03_0062E", "metropolitan statistical area/micropolitan statistical area:*", TRUE)
)

county_url = paste0("https://www2.census.gov/geo/docs/maps-data/data/rel2020/zcta520/",
                    "tab20_zcta520_county20_natl.txt")
cbsa_url = paste0("https://www2.census.gov/programs-surveys/metro-micro/geographies/",
                  "reference-files/2023/delineation-files/list1_2023.xlsx")
raw$zcta_county = fread(county_url, sep = "|", colClasses = "character")
cbsa_file = tempfile(fileext = ".xlsx")
download.file(cbsa_url, cbsa_file, mode = "wb", quiet = TRUE)
raw$county_cbsa = as.data.table(readxl::read_excel(cbsa_file, skip = 2, col_types = "text"))
unlink(cbsa_file)

#####
# write data
#####
setwd(wd_1raw)
qs2::qs_save(raw, "acs_demographics.qs2")
