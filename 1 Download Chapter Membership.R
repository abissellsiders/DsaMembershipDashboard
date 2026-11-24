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
# acquire data
#####
gs4_deauth()

#####
# membership data
#####
# download data
ss = "https://docs.google.com/spreadsheets/d/1f5G4-ak9OBlBHA2ydeKhs3P3-4kdhdYDcqoR5BJwOCg/edit"
for (year in 2020:2026) {
  sheet = as.character(year)
  mems_src = googlesheets4::read_sheet(
    ss = ss, sheet = sheet, col_names = FALSE, skip = 0, col_types = "c"
  )
  mems = data.table(mems_src)

  # write data
  output_file = file.path(wd_1raw, paste0("membership_", year, ".qs2"))
  qs2::qs_save(mems, output_file)
  message("Saved ", sheet, " to ", output_file)
}

#####
# chapter details
#####
# download data
chapter_cols = c("original_name", "original_state", "cid", "name",
                 "state", "merger", "notes")
chapters = googlesheets4::read_sheet(
  ss = ss, sheet = "AllChapters", range = "A4:G",
  col_names = chapter_cols, col_types = "c"
)
# write data
qs2::qs_save(as.data.table(chapters), file.path(wd_1raw, "membership_chapters.qs2"))
