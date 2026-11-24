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

# shiny directory
wd_shiny = fs::path(wd_5export, "shiny")

#####
# copy files qs2 to csv
#####
# read data file
setwd(wd_3merge)
chapter_data = qs2::qs_read("chapter_data.qs2")
# write data file
setwd(wd_shiny)
fwrite(chapter_data, "chapter_data.csv")
# saveRDS(chapter_data, "chapter_data.rds")

#####
# copy directories
#####
# copy shapefiles
fs::dir_copy(
  fs::path(wd_2clean, "chapter_zcta_shapefiles"),
  fs::path(wd_shiny, "chapter_zcta_shapefiles"),
  overwrite = TRUE
)
fs::dir_copy(
  fs::path(wd_2clean, "us_state_reference"),
  fs::path(wd_shiny, "us_state_reference"),
  overwrite = TRUE
)
