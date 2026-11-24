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
raw_path = file.path(wd_1raw, "acs_demographics.qs2")
chapter_metadata = qs2::qs_read(file.path(wd_1raw, "membership_chapters.qs2"))
zip_files = list.files(wd_2clean, pattern = "^zipcodes v[0-9]{4}-(0[1-9]|1[0-2])[.]qs2$")
if (!length(zip_files)) stop("No versioned ZIP snapshots in 2 Clean.")

#####
# munge data
#####
sum_counts = function(x) {
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

acs_number = function(x) {
  value = suppressWarnings(as.numeric(x))
  value[value %in% c(-999999999, -888888888, -666666666, -555555555, -333333333, -222222222)] = NA_real_
  value
}

pct = function(numerator, denominator) {
  round(100 * numerator / fifelse(denominator > 0, denominator, NA_real_), 4)
}

median_from_bins = function(counts, edges) {
  counts[is.na(counts)] = 0
  midpoint = sum(counts) / 2
  if (midpoint <= 0) return(list(NA_real_, "no_households_or_population"))

  cumulative = cumsum(counts)
  bin = which(cumulative >= midpoint)[1]
  if (is.infinite(edges[bin + 1L])) return(list(edges[bin], "topcoded"))

  fraction = (midpoint - (cumulative[bin] - counts[bin])) / counts[bin]
  estimate = edges[bin] + fraction * (edges[bin + 1L] - edges[bin])
  list(round(estimate, 2), "interpolated")
}

age_vars = sprintf("b01001_%03de_count", c(1, 3:25, 27:49))
income_vars = sprintf("b19001_%03de_count", 1:17)
age_edges = c(0, 5, 10, 15, 18, 20, 21, 22, 25, 30, 35, 40, 45, 50,
              55, 60, 62, 65, 67, 70, 75, 80, 85, Inf)
income_edges = c(0, 10000, 15000, 20000, 25000, 30000, 35000, 40000, 45000,
                 50000, 60000, 75000, 100000, 125000, 150000, 200000, Inf)

if (file.exists(raw_path)) {
  raw = qs2::qs_read(raw_path)
  acs_year = raw$acs_year
  tables = lapply(raw[c("profile", "age", "household", "commute", "income")], function(x) {
    setnames(x, "zip code tabulation area", "zcta")
    x
  })
  setnames(tables$profile, names(acs_profile_columns), unname(acs_profile_columns))
  setnames(tables$age, sprintf("B01001_%03dE", c(1, 3:25, 27:49)), age_vars)
  setnames(tables$income, sprintf("B19001_%03dE", 1:17), income_vars)
  setnames(tables$household, c("B11001_001E", "B11001_007E", "B11001_008E"),
           c("household_total", "nonfamily_households", "living_alone_households"))
  setnames(tables$commute, c("B08301_001E", "B08301_010E", "B08301_018E", "B08301_019E"),
           c("commute_total_workers", "public_transit_commuters", "bicycle_commuters", "walk_commuters"))
  zcta = Reduce(function(x, y) merge(x, y, by = "zcta", all.x = TRUE), tables)
  zcta[, walk_bike_commuters := {
    bike = acs_number(bicycle_commuters)
    walk = acs_number(walk_commuters)
    fifelse(is.na(bike) & is.na(walk), NA_real_, fcoalesce(bike, 0) + fcoalesce(walk, 0))
  }]
  county = raw$zcta_county[order(-fcoalesce(as.numeric(AREALAND_PART), 0))]
  county = unique(county, by = "GEOID_ZCTA5_20")
  zcta = merge(zcta, county[, .(
    zcta = GEOID_ZCTA5_20, dominant_state_fips = substr(GEOID_COUNTY_20, 1, 2),
    dominant_county_fips = GEOID_COUNTY_20
  )], by = "zcta", all.x = TRUE)
  state_income = raw$state[, .(dominant_state_fips = state, state_median_household_income = acs_number(DP03_0062E))]
} else{
  stop("Demographics file missing.")
}
stopifnot(!anyDuplicated(zcta$zcta), !anyDuplicated(state_income$dominant_state_fips))

race_counts = grep("^(race|eth)_.*_count$", unname(acs_profile_columns), value = TRUE)
profile_counts = unname(acs_profile_columns[grepl("E$", names(acs_profile_columns))])
profile_counts = setdiff(profile_counts, c("median_age", "median_household_income"))
detail_counts = c("household_total", "nonfamily_households", "living_alone_households",
                  "commute_total_workers", "public_transit_commuters", "walk_bike_commuters")
count_cols = c(profile_counts, age_vars, income_vars, detail_counts)
zcta[, (count_cols) := lapply(.SD, acs_number), .SDcols = count_cols]

chapter_ids = unique(chapter_metadata[!is.na(cid) & nzchar(trimws(cid)),
  .(chapter = original_name, state = original_state, cid)])

#####
# compute and save one chapter table per ZIP snapshot
#####
for (zip_file in zip_files) {
  snapshot = substr(zip_file, 11, 17)
  chapter_zips = qs2::qs_read(file.path(wd_2clean, zip_file))
  zip_ids = merge(chapter_zips, chapter_ids, by = c("chapter", "state"), all.x = TRUE)
  if (nrow(zip_ids) != nrow(chapter_zips) || anyNA(zip_ids$cid)) {
    stop("Missing or ambiguous chapter IDs in ", zip_file)
  }
  zip_ids = unique(zip_ids[, .(cid, zcta = zipcode)])
  matched = merge(zip_ids, zcta, by = "zcta", all.x = TRUE)
  demographics = matched[, lapply(.SD, sum_counts), by = cid, .SDcols = count_cols]
  zcta_codes = zcta$zcta
  coverage = matched[, .(
    chapter_zip_count = .N, matched_zcta_count = sum(zcta %chin% zcta_codes)
  ), by = cid]
  coverage[, unmatched_zip_count := chapter_zip_count - matched_zcta_count]
  demographics = merge(demographics, coverage, by = "cid")
  demographics[, `:=`(acs_year = acs_year, zip_snapshot = snapshot)]

  state_totals = matched[!is.na(dominant_state_fips) & total_population > 0,
                          .(population = sum(total_population)), by = .(cid, dominant_state_fips)]
  setorder(state_totals, cid, -population, dominant_state_fips)
  dominant_state = unique(state_totals, by = "cid")[, .(cid, dominant_state_fips)]
  demographics = merge(demographics, dominant_state, by = "cid", all.x = TRUE)
  demographics = merge(demographics, state_income, by = "dominant_state_fips", all.x = TRUE)

  male_bins = age_vars[2:24]
  female_bins = age_vars[25:47]
  age_counts = as.matrix(demographics[, ..male_bins])
  female_counts = as.matrix(demographics[, ..female_bins])
  age_counts[is.na(age_counts)] = 0
  female_counts[is.na(female_counts)] = 0
  age_counts = age_counts + female_counts
  income_bins = income_vars[-1]
  age_medians = transpose(lapply(seq_len(nrow(demographics)), function(i) median_from_bins(age_counts[i, ], age_edges)), list.cols = TRUE)
  income_medians = transpose(lapply(seq_len(nrow(demographics)), function(i) {
    median_from_bins(unlist(demographics[i, ..income_bins], use.names = FALSE), income_edges)
  }), list.cols = TRUE)
  demographics[, c("estimated_median_age", "estimated_median_age_method") := lapply(age_medians, unlist)]
  demographics[, c("estimated_median_household_income", "estimated_median_household_income_method") := lapply(income_medians, unlist)]

  demographics[, `:=`(
    age_total_population = get(age_vars[1]), income_households = get(income_vars[1]),
    age_18_24_count = rowSums(age_counts[, 5:8, drop = FALSE]),
    age_18_29_count = rowSums(age_counts[, 5:9, drop = FALSE]),
    age_18_34_count = rowSums(age_counts[, 5:10, drop = FALSE])
  )]
  demographics[is.na(age_total_population),
               c("age_18_24_count", "age_18_29_count", "age_18_34_count") := NA_real_]
  combined_counts = list(
    race_asian_nhpi_alone_count = c("race_asian_alone_count", "race_nhpi_alone_count"),
    eth_nonhispanic_asian_nhpi_alone_count = c("eth_nonhispanic_asian_alone_count", "eth_nonhispanic_nhpi_alone_count"),
    households_with_children_under18 = grep("_households_with_children_under18$", profile_counts, value = TRUE),
    marital_status_population_15plus = c("male_population_15plus_marital_status", "female_population_15plus_marital_status"),
    never_married_15plus = c("male_never_married_15plus", "female_never_married_15plus"),
    public_transit_walk_bike_commuters = c("public_transit_commuters", "walk_bike_commuters")
  )
  for (column in names(combined_counts)) {
    components = as.matrix(demographics[, combined_counts[[column]], with = FALSE])
    totals = rowSums(components, na.rm = TRUE)
    totals[rowSums(!is.na(components)) == 0L] = NA_real_
    set(demographics, j = column, value = totals)
  }
  demographics[, households_without_children_under18 := households_dp02_total - households_with_children_under18]

  race_columns = c(race_counts, "race_asian_nhpi_alone_count", "eth_nonhispanic_asian_nhpi_alone_count")
  demographics[, (paste0("pct_", sub("_count$", "", race_columns))) :=
                 lapply(.SD, pct, denominator = total_population), .SDcols = race_columns]
  demographics[, `:=`(
    pct_graduate_or_professional_degree_25plus = pct(graduate_or_professional_degree_25plus_count, education_population_25plus),
    pct_bachelors_or_higher_25plus = pct(bachelors_or_higher_25plus_count, education_population_25plus),
    pct_households_without_children_under18 = pct(households_without_children_under18, households_dp02_total),
    pct_never_married_15plus = pct(never_married_15plus, marital_status_population_15plus),
    pct_renter_occupied = pct(renter_occupied_units, occupied_housing_units),
    pct_no_vehicle = pct(no_vehicle_units, occupied_housing_units),
    pct_government_workers = pct(government_workers, civilian_employed_population_16plus_class_worker),
    pct_age_18_24 = pct(age_18_24_count, age_total_population),
    pct_age_18_29 = pct(age_18_29_count, age_total_population),
    pct_age_18_34 = pct(age_18_34_count, age_total_population),
    pct_nonfamily_households = pct(nonfamily_households, household_total),
    pct_living_alone = pct(living_alone_households, household_total),
    pct_public_transit_commute = pct(public_transit_commuters, commute_total_workers),
    pct_walk_bike_commute = pct(walk_bike_commuters, commute_total_workers),
    pct_public_transit_walk_bike_commute = pct(public_transit_walk_bike_commuters, commute_total_workers),
    estimated_median_household_income_pct_of_state = pct(estimated_median_household_income, state_median_household_income)
  )]
  setcolorder(demographics, c("cid", "zip_snapshot", "acs_year"))
  setorder(demographics, cid)
  qs2::qs_save(demographics, file.path(wd_2clean, paste0("chapter_acs_demographics v", snapshot, ".qs2")))
}
