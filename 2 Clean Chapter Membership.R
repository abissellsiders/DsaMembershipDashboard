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
years = 2020:2026
year_tables = lapply(sprintf("membership_%d.qs2", years), qs2::qs_read)

#####
# munge data
#####
join_cols = "cid"
clean_year = function(raw, year) {
  headers = trimws(unlist(raw[2, ], use.names = FALSE))
  if (!all(c(join_cols, "Chapter") %in% headers)) {
    stop("Missing membership headers in the ", year, " snapshot.")
  }

  # Keep row 1 (national totals) and remove only the embedded header row.
  members = as.data.table(raw[-2, ])
  setnames(members, headers)
  missing_cid = is.na(members$cid) | !nzchar(trimws(members$cid))
  if (any(missing_cid)) {
    chapter_names = members$Chapter[missing_cid]
    stop("Missing cid in the ", year, " snapshot for chapter(s): ",
         paste(unique(chapter_names), collapse = ", "), call. = FALSE)
  }

  # Exclude dated columns outside the sheet year and annual summaries.
  dated_year = stringr::str_match(headers, "^[A-Za-z]{3}[[:space:]]+([0-9]{4})[[:space:]]+")[, 2]
  outside_year = !is.na(dated_year) & dated_year != as.character(year)
  summaries = c(paste(year, "Change"), "Starting Point", paste(year, "Growth"))
  metadata = c("Chapter", "State", "States", "CH/OC", "Chapter Size")
  keep = headers[!(outside_year | headers %in% c(summaries, metadata))]
  members = members[, ..keep]

  if (anyNA(members[, ..join_cols]) || anyDuplicated(members, by = join_cols)) {
    stop("Missing or duplicate cid keys in the ", year, " snapshot.")
  }

  # Qualify measurements so names stay unique after merging.
  year_cols = setdiff(keep, join_cols)
  setnames(members, year_cols, paste(year, year_cols))
  members
}

year_tables = Map(clean_year, year_tables, years)
# merge() dispatches to data.table's merge method for these inputs.
membership = Reduce(function(left, right) {
  merge(left, right, by = join_cols, all = TRUE)
}, year_tables)

#####
# reshape data long to wide
#####
# first reshape data to long
membership = data.table::melt(
  membership, id.vars = join_cols, variable.factor = FALSE
)

# convert year-month-metric format into columns
labels = stringr::str_match(
  membership$variable, "^([0-9]{4} [A-Za-z]{3})[[:space:]]+(.+)$"
)
stopifnot(!anyNA(labels[, 2]), substr(labels[, 2], 6, 8) %in% month.abb)
membership[, `:=`(month_year = labels[, 2], metric = trimws(labels[, 3]))]
membership[metric == "new", metric := "New"]

if (anyDuplicated(membership, by = c(join_cols, "month_year", "metric"))) {
  stop("Duplicate membership metrics for a chapter and month.")
}

# cast data from long to wide on variable names
membership = data.table::dcast(
  membership, cid + month_year ~ metric, value.var = "value"
)
membership[, month_year := as.Date(sprintf(
  "%s-%02d-01", substr(month_year, 1, 4), match(substr(month_year, 6, 8), month.abb)
))]
setorder(membership, cid, month_year)

#####
# join chapter metadata
#####
chapter_cols = c("cid", "name", "state", "merger", "notes")
chapters = qs2::qs_read("membership_chapters.qs2")
# A2 may contain the header when the sheet has a title row.
chapters = chapters[!is.na(cid) & nzchar(trimws(cid)) &
                    !(cid == "cid" & original_name == "original_name"), ..chapter_cols]
# Fill blanks within each cid when the metadata has one distinct value.
fill_chapter_metadata = function(values) {
  blank = is.na(values) | !nzchar(trimws(values))
  values[blank] = NA_character_
  known = unique(values[!blank])
  if (length(known) == 1L) {
    values[blank] = known
  }
  values
}

fill_cols = c("merger", "notes")
chapters[, (fill_cols) := lapply(.SD, fill_chapter_metadata),
         by = cid, .SDcols = fill_cols]
chapters = unique(chapters)
if (anyDuplicated(chapters, by = "cid")) {
  duplicate_cids = unique(chapters$cid[duplicated(chapters$cid)])
  stop("Conflicting AllChapters rows for cid(s): ", paste(duplicate_cids, collapse = ", "))
}
membership = merge(membership, chapters, by = "cid", all.x = TRUE, sort = FALSE)

# custom information for special chapters
membership[cid == "us-total", name := "Total DSA Members"]
membership[cid %chin% c("us-total", "us-at-large"), state := "US"]

#####
# derive membership measures
#####
# to numeric
count_cols = c("Members", "Total", "Monthly", "SIBD")
membership[, (count_cols) := lapply(.SD, function(x) {
  as.numeric(gsub(",", "", trimws(x), fixed = TRUE))
}), .SDcols = count_cols]

# last date in month
membership[, date1 := ceiling_date(month_year, unit = "month") - days(1)]

# new columns: annual dues payers
membership[, Annual := Members - (Monthly + SIBD)]

# rename columns
cols_old = c("Expired", "Lapsed")
cols_new = c("Former Members Lapsed <2 years", "Former Members Lapsed >2 years")
setnames(membership, cols_old, cols_new)

# remove columns
membership[, c("% Combined", "Growth") := NULL]

# # new columns
# this does NOT work because of rejoins
# membership[, Lapsed := New - Change]

# remove columns
membership[, namestate := paste0(name, ", ", state)]
setorderv(membership, c("name", "state", "date1"))

#####
# save data
#####
setwd(wd_2clean)
qs2::qs_save(membership, "chapter_membership.qs2")
