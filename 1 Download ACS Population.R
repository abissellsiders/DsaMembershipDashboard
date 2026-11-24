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
census_key = Sys.getenv("CENSUS_API_KEY")
if (!nzchar(census_key) && file.exists(".Renviron")) {
  readRenviron(".Renviron")
  census_key = Sys.getenv("CENSUS_API_KEY")
}
if (!nzchar(census_key)) {
  stop("Set CENSUS_API_KEY in the project .Renviron.")
}

fetch_population = function(year, key) {
  request = httr2::request(sprintf("https://api.census.gov/data/%d/acs/acs5/profile", year))
  request = httr2::req_url_query(
    request, get = "NAME,GEO_ID,DP05_0001E,DP05_0001M",
    `for` = "zip code tabulation area:*", key = key
  )
  request = httr2::req_timeout(request, 120)
  request = httr2::req_retry(request, max_tries = 5, retry_on_failure = TRUE)
  tryCatch({
    rows = httr2::resp_body_json(httr2::req_perform(request), simplifyVector = TRUE)
    required = c("NAME", "GEO_ID", "DP05_0001E", "DP05_0001M", "zip code tabulation area")
    stopifnot(is.matrix(rows), nrow(rows) > 1L, all(required %in% rows[1, ]))
    colnames(rows) = rows[1, ]
    rows = rows[-1, , drop = FALSE]
    data.table(
      acs_year = year, acs_period_start = year - 4L, acs_period_end = year,
      zcta = rows[, "zip code tabulation area"], name = rows[, "NAME"],
      geo_id = rows[, "GEO_ID"],
      total_population = census_count(rows[, "DP05_0001E"]),
      total_population_moe = census_count(rows[, "DP05_0001M"])
    )
  }, error = function(e) {
    stop("Census ACS population request or response failed for ", year, ".", call. = FALSE)
  })
}

census_count = function(values) {
  counts = suppressWarnings(as.integer(values))
  counts[counts < 0L] = NA_integer_
  counts
}

population = rbindlist(lapply(2012:2024, fetch_population, key = census_key))

#####
# write data
#####
setwd(wd_1raw)
qs2::qs_save(population, "zcta_acs5_population_annual.qs2")
