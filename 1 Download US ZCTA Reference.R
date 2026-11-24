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
setwd(wd_1raw)
zcta_url = paste0("https://www2.census.gov/geo/tiger/GENZ2020/shp/",
                  "cb_2020_us_zcta520_500k.zip")
county_url = paste0("https://www2.census.gov/geo/docs/maps-data/data/rel2020/zcta520/",
                    "tab20_zcta520_county20_natl.txt")

#####
# write data
#####
download.file(zcta_url, "cb_2020_us_zcta520_500k.zip", mode = "wb")
download.file(county_url, "tab20_zcta520_county20_natl.txt", mode = "wb")
