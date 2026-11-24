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
states = tigris::states(cb = TRUE, resolution = "500k", year = 2024,
                        progress_bar = FALSE)

#####
# write data
#####
setwd(wd_1raw)
qs2::qs_save(states, "tigris_states_2024_500k.qs2")
