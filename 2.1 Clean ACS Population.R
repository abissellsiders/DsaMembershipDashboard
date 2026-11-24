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
population = qs2::qs_read(file.path(wd_1raw, "zcta_acs5_population_annual.qs2"))
chapter_metadata = qs2::qs_read(file.path(wd_1raw, "membership_chapters.qs2"))

#####
# munge data
#####
chapter_ids = unique(chapter_metadata[
  !is.na(cid) & nzchar(trimws(cid)),
  .(chapter = original_name, state = original_state, cid)
])
stopifnot(!anyDuplicated(population, by = c("acs_year", "zcta")))
setwd(wd_2clean)
zip_files = sort(list.files(pattern = "^zipcodes v[0-9]{4}-(0[1-9]|1[0-2])[.]qs2$"))
if (!length(zip_files)) {
  stop("No dated chapter ZIP snapshots found in 2 Clean.")
}

for (zip_file in zip_files) {
  snapshot = substr(zip_file, 11, 17)
  chapter_zips = qs2::qs_read(zip_file)
  zip_ids = merge(chapter_zips, chapter_ids, by = c("chapter", "state"), all.x = TRUE)
  stopifnot(nrow(zip_ids) == nrow(chapter_zips), !anyNA(zip_ids$cid))
  zip_ids = unique(zip_ids[, .(cid, zcta = zipcode)])
  assigned_zctas = unique(zip_ids[!cid %chin% c("us-total", "us-at-large"), zcta])
  zip_ids = zip_ids[!cid %chin% c("us-total", "us-at-large")]

  annual = rbindlist(lapply(sort(unique(population$acs_year)), function(year) {
    estimates = population[acs_year == year & !is.na(total_population)]
    chapter_year = merge(zip_ids, estimates[, .(zcta, total_population)], by = "zcta", all.x = TRUE)
    totals = chapter_year[, .(
      chapter_zip_count = .N,
      matched_zcta_count = sum(!is.na(total_population)),
      total_population = sum(total_population, na.rm = TRUE)
    ), by = cid]
    totals = rbind(totals, estimates[, .(
      cid = "us-total", chapter_zip_count = .N, matched_zcta_count = .N,
      total_population = sum(total_population)
    )])
    totals = rbind(totals, estimates[!zcta %chin% assigned_zctas, .(
      cid = "us-at-large", chapter_zip_count = .N, matched_zcta_count = .N,
      total_population = sum(total_population)
    )])
    totals[, `:=`(
      acs_year = year, acs_period_start = year - 4L, acs_period_end = year,
      sample_date = as.Date(sprintf("%d-07-01", year)),
      unmatched_zip_count = chapter_zip_count - matched_zcta_count
    )]
    totals
  }))
  setorder(annual, cid, acs_year)

  # July of each ACS release year anchors the interpolated monthly series.
  # ACS five-year estimates describe overlapping periods.
  monthly = annual[, {
    anchors = year(sample_date) * 12L + month(sample_date)
    sample_months = seq.int(anchors[1], tail(anchors, 1))
    last_anchor = tail(anchors, 1)
    from = findInterval(sample_months, anchors)
    to = pmin(from + 1L, .N)
    values = if (.N > 1L) {
      approx(anchors, total_population, xout = sample_months)$y
    } else {
      total_population
    }
    result = data.table(
      index = sample_months, population = values,
      source = ifelse(sample_months %in% anchors, "sample", "interpolated"),
      from_acs_year = acs_year[from], to_acs_year = acs_year[to],
      chapter_zip_count = chapter_zip_count[from], matched_zcta_count = matched_zcta_count[from],
      unmatched_zip_count = unmatched_zip_count[from]
    )
    forecast_end = 2026L * 12L + 12L
    if (.N >= 2L && last_anchor < forecast_end) {
      trend = tail(seq_len(.N), 5)
      forecast_months = seq.int(last_anchor + 1L, forecast_end)
      fit = lm(total_population[trend] ~ index, data = data.frame(index = anchors[trend]))
      forecast = predict(fit, newdata = data.frame(index = forecast_months))
      result = rbind(result, data.table(
        index = forecast_months, population = pmax(0, forecast),
        source = sprintf("projected_linear_trend_%dyr", length(trend)),
        from_acs_year = acs_year[trend[1]], to_acs_year = acs_year[.N],
        chapter_zip_count = chapter_zip_count[.N], matched_zcta_count = matched_zcta_count[.N],
        unmatched_zip_count = unmatched_zip_count[.N]
      ))
    }
    result
  }, by = cid]
  monthly[, `:=`(month_year = as.Date(sprintf("%04d-%02d-01", (index - 1L) %/% 12L, (index - 1L) %% 12L + 1L)),
                 population = round(population, 2))]
  monthly[, index := NULL]
  setcolorder(monthly, c("cid", "month_year", "population"))

  annual[, zip_snapshot := snapshot]
  monthly[, zip_snapshot := snapshot]
  qs2::qs_save(annual, paste0("chapter_population_annual v", snapshot, ".qs2"))
  qs2::qs_save(monthly, paste0("chapter_population_monthly v", snapshot, ".qs2"))
}
