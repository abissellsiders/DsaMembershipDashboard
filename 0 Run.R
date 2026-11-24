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
# run numbered pipeline scripts
#####
# Give each script its own environment and restore the project directory after it runs.
run_script = function(script) {
  source(file.path(wd, script), local = new.env(parent = globalenv()), chdir = TRUE)
}

# (slow!!!)
# set this to true if you want to redownload ALL machine-downloadable files
redownload_raw_data = FALSE
# (slow!!!)
# set this to true if you want to recompute ALL shapefiles
redo_shapefile_references = FALSE
# Rebuild chapter boundaries without recomputing the national reference layers.
redo_chapter_shapefiles = FALSE

# 1 raw
if (redownload_raw_data) {
  run_script("1 Download ACS Demographics.R")
  run_script("1 Download ACS Population.R")
  run_script("1 Download Chapter Membership.R")
  run_script("1 Download Tigris State Reference.R")
  run_script("1 Download US ZCTA Reference.R")
}

# 2 clean
if (redo_shapefile_references) {
  run_script("2 Clean Tigris State Reference.R")
  run_script("2 Clean US ZCTA Reference.R")
}
run_script("2 Clean Chapter Membership.R")
run_script("2 Clean Chapter Zipcodes.R")
run_script("2 Clean Election Fekrazad.R")

# 2.1 clean (after chapter zipcode cleaning)
if (redo_shapefile_references || redo_chapter_shapefiles) {
  run_script("2.1 Clean Chapter ZCTA Shapefiles.R")
}
run_script("2.1 Clean ACS Population.R")
run_script("2.1 Clean AIP.R")
run_script("2.1 Prepare Election Fekrazad.R")
run_script("2.1 Compute ACS Demographics for Chapter Zipcodes.R")

# 3 merge
run_script("3 Merge Data.R")

# 5 export
run_script("5 Export Shiny.R")
