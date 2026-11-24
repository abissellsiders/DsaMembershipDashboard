#####
# init
#####
# clear old data
rm(list=ls(all=TRUE))

# set working directory
(wd = "C:/gdrive/dsa/DsaMembershipDashboard/5 Export/shiny")
setwd(wd)

# load packages
library(shiny)
library(rsconnect)

# set global options
options(datatable.prettyprint.char=10L)
options(error = NULL)
options(shiny.error = NULL)
options(shiny.reactlog = FALSE)

#####
# run locally
#####
shiny::runApp(
  launch.browser = TRUE
)

#####
# deploy
#####
# initial sign-in
# rsconnect::connectCloudUser()

# set environment variables in Cloud Connect
# https://connect.posit.cloud/aidanb/content/01a20b52-b307-c069-20a4-50dfcfefbc94/settings/variables

# redeploy using Connect Cloud content ID
rsconnect::deployApp(
  appId = "01a20b52-b307-c069-20a4-50dfcfefbc94",
  account = "aidanb",
  server = "connect.posit.cloud"
)

# view logs on Connect Cloud
# https://connect.posit.cloud/aidanb/content/01a20b52-b307-c069-20a4-50dfcfefbc94/history

