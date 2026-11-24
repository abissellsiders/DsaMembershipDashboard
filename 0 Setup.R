#####
# init
#####
# data sources:
# Ben's Version - Chapter Breakdown of Membership: https://docs.google.com/spreadsheets/d/1f5G4-ak9OBlBHA2ydeKhs3P3-4kdhdYDcqoR5BJwOCg/edit
# Maeve's DSA data: https://github.com/mvhove/dsa-data/tree/main
# Adithya's February 2026 chapter zipcode data: https://docs.google.com/spreadsheets/d/1Uxwzhu3lDjyriZswmJnf3XDu3KhyXyyfHPFpSx1rG4o/htmlview
# ^ Aidan's copy:
# Adithya's June 2025 chapter zipcode data: https://docs.google.com/spreadsheets/d/1JGWgEJbxWmIg4Zt_Q8mvhdKBAtZBE1U01TLS8G3V61M/edit?gid=1084270392#gid=1084270392

# Ideology data: American Ideology Project
# website https://americanideologyproject.com/
# data https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/BQKU4M

# Election data: Fekrazad 2025
# paper https://www.nature.com/articles/s41597-025-05140-3
# code https://github.com/fekrazad/precinct-to-census-mapping/tree/main
# data https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/Z8TSH3

# Social Capital Atlas
# map https://socialcapital.org/?dimension=EconomicConnectednessIndividual&geoLevel=zcta&selectedId=&dim1=EconomicConnectednessIndividual&dim2=CohesivenessClustering&dim3=CivicEngagementVolunteeringRates&bigModalSection=&bigModalChart=scatterplot&showOutliers=false&colorBy=
# data https://data.humdata.org/dataset/social-capital-atlas

# clear old data
# rm(list=ls(all=TRUE))

# working directories
wd = "C:/gdrive/dsa/DsaMembershipDashboard"
wd_1raw = paste0(wd, "/1 Raw")
wd_2clean = paste0(wd, "/2 Clean")
wd_3merge = paste0(wd, "/3 Merge")
wd_4analyze = paste0(wd, "/4 Analysis")
wd_5export = paste0(wd, "/5 Export")

# load packages: os
library(fs)
library(qs2)
# load packages: data management
library(data.table)
library(googlesheets4)
library(stringr)
library(lubridate)
library(foreign)
# load packages: graphing
library(scales)
library(ggplot2)

# set global options
options(datatable.prettyprint.char = 10L)
options(tigris_use_cache = TRUE)

# helper function
ggsave_good = function(..., width = 8, height = 6) {
  return(ggsave(..., width = width, height = height))
}

# colorblind colors
# https://stackoverflow.com/questions/57153428/r-plot-color-combinations-that-are-colorblind-accessible
# https://stackoverflow.com/questions/65013406/how-to-generate-30-distinct-colors-that-are-color-blind-friendly
# https://jacksonlab.agronomy.wisc.edu/2016/05/23/15-level-colorblind-friendly-palette/
rcarto_cbg8_safe = c(
  "#CC6677", "#332288", "#E69F00", "#009E73", "#56B4E9", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
  "#444",
  "#DDCC77", "#117733", "#88CCEE", "#AA4499", "#44AA99", "#999933", "#882255", "#661100", "#6699CC",
  "#004949","#009292","#ff6db6","#ffb6db",
  "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
  "#920000","#924900","#db6d00","#24ff24","#ffff6d")


# ACS profile variable names used by the demographics download and cleaning.
acs_profile_columns = c(
  DP05_0001E = "total_population",
  DP05_0001M = "total_population_moe",
  DP05_0018E = "median_age",
  DP05_0018M = "median_age_moe",
  DP05_0037E = "race_white_alone_count",
  DP05_0037PE = "pct_race_white_alone",
  DP05_0045E = "race_black_alone_count",
  DP05_0045PE = "pct_race_black_alone",
  DP05_0053E = "race_aian_alone_count",
  DP05_0053PE = "pct_race_aian_alone",
  DP05_0061E = "race_asian_alone_count",
  DP05_0061PE = "pct_race_asian_alone",
  DP05_0069E = "race_nhpi_alone_count",
  DP05_0069PE = "pct_race_nhpi_alone",
  DP05_0074E = "race_some_other_alone_count",
  DP05_0074PE = "pct_race_some_other_alone",
  DP05_0075E = "race_multiracial_count",
  DP05_0075PE = "pct_race_multiracial",
  DP05_0090E = "eth_hispanic_any_race_count",
  DP05_0090PE = "pct_eth_hispanic_any_race",
  DP05_0096E = "eth_nonhispanic_white_alone_count",
  DP05_0096PE = "pct_eth_nonhispanic_white_alone",
  DP05_0097E = "eth_nonhispanic_black_alone_count",
  DP05_0097PE = "pct_eth_nonhispanic_black_alone",
  DP05_0098E = "eth_nonhispanic_aian_alone_count",
  DP05_0098PE = "pct_eth_nonhispanic_aian_alone",
  DP05_0099E = "eth_nonhispanic_asian_alone_count",
  DP05_0099PE = "pct_eth_nonhispanic_asian_alone",
  DP05_0100E = "eth_nonhispanic_nhpi_alone_count",
  DP05_0100PE = "pct_eth_nonhispanic_nhpi_alone",
  DP05_0101E = "eth_nonhispanic_some_other_alone_count",
  DP05_0101PE = "pct_eth_nonhispanic_some_other_alone",
  DP05_0102E = "eth_nonhispanic_multiracial_count",
  DP05_0102PE = "pct_eth_nonhispanic_multiracial",
  DP02_0001E = "households_dp02_total",
  DP02_0003E = "married_couple_households_with_children_under18",
  DP02_0005E = "cohabiting_couple_households_with_children_under18",
  DP02_0007E = "male_householder_households_with_children_under18",
  DP02_0011E = "female_householder_households_with_children_under18",
  DP02_0025E = "male_population_15plus_marital_status",
  DP02_0026E = "male_never_married_15plus",
  DP02_0031E = "female_population_15plus_marital_status",
  DP02_0032E = "female_never_married_15plus",
  DP02_0059E = "education_population_25plus",
  DP02_0066E = "graduate_or_professional_degree_25plus_count",
  DP02_0066PE = "pct_graduate_or_professional_degree_25plus",
  DP02_0066PM = "pct_graduate_or_professional_degree_25plus_moe",
  DP02_0068E = "bachelors_or_higher_25plus_count",
  DP02_0068PE = "pct_bachelors_or_higher_25plus",
  DP02_0068PM = "pct_bachelors_or_higher_25plus_moe",
  DP04_0045E = "occupied_housing_units",
  DP04_0047E = "renter_occupied_units",
  DP04_0047PE = "pct_renter_occupied",
  DP04_0047PM = "pct_renter_occupied_moe",
  DP04_0058E = "no_vehicle_units",
  DP04_0058PE = "pct_no_vehicle",
  DP04_0058PM = "pct_no_vehicle_moe",
  DP03_0046E = "civilian_employed_population_16plus_class_worker",
  DP03_0048E = "government_workers",
  DP03_0048PE = "pct_government_workers",
  DP03_0062E = "median_household_income",
  DP03_0062M = "median_household_income_moe",
  DP03_0128PE = "pct_people_below_poverty"
)
