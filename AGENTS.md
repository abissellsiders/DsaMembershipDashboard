# Folder architecture
Please use the following folder architecture, which helps structure this as an ETL pipeline with 5 stages:

```{r}
# 0 global variables and helper functions
0 Setup.R

# 1 obtain raw data (where programmatic)
1 Raw FILE1.R
1 Raw FILE2.R
1 Raw FILE3.R
# write files into folder
1 Raw
# Notes:
# Manually downloaded raw data files should generally keep their original names
# Programatically downloaded/created files can be named as pleased

# 2 clean data (perform basic cleaning and formatting, standardization, transformation or restructuring, subsetting, etc.) to prepare data before merge
2 Clean FILE1.R
etc
# write files into folder
2 Clean

# 3 merge data
3 Merge MERGEDFILE1.R
# write files into folder
3 Merge

# 4 analyze data (eg, quality assurance, regression results, tables and graphs, property testing, etc.)
4 Analyze MERGEDFILE1.R
# write files into folder
4 Analysis

# 5 finalize data for export / R Shiny app
5 Export MERGEDFILE1.R
# write files into folder
5 Export
```

Where a file must be run after another file in the same stage, files should be named in this style: "2 Foo", "2.1 Bar", "2.2 Baz". In general, filename alphabetical order should yield code run order.

# 0 Setup
The `0 Setup.R` file should set up anything used repeatedly in later files (such as global variables, packages, global options and helper functions). 

The setup code should provide wd_... working directory global variables based on the folder structure above, as in the example below:

```{r}
#####
# init
#####
# clear old data
# rm(list=ls(all=TRUE))

# working directories
wd = "C:/gdrive/dsa/DsaMembershipDashboard"
wd_1raw = paste0(wd, "/1 Raw")
wd_2clean = paste0(wd, "/2 Clean")
wd_3merge = paste0(wd, "/3 Merge")
wd_4analyze = paste0(wd, "/4 Analysis")
wd_5export = paste0(wd, "/5 Export")

# load packages: data management
library(data.table)
library(googlesheets4)
library(stringr)
library(lubridate)
library(qs2)
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
```

# Init
All code in all steps after 0 should start with an init section that calls the `0 Setup.R` code:

```{r}
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
```

# 1 Raw
Code files in 1 Raw obtain and write any data which can be programmatically obtained. Code files in 1 Raw should use the following general structure:

```{r}
#####
# get data
#####
# TODO code below uses nonexistent datasets and must be updated
xxxupdate = fake_download_data_function()

#####
# write data
#####
setwd(wd_1raw)
# TODO code below uses nonexistent datasets and must be updated
qs2::qs_save(xxxupdate, "xxxupdate.qs2")
```

# 2 Clean
Code files in 2 Clean should use the following general structure:

```{r}
#####
# read data
#####
setwd(wd_1raw)
# TODO code below uses nonexistent datasets and must be updated
xxxupdate = qs2::qs_read("xxxupdate.qs2")

#####
# clean data
#####
# TODO code below uses nonexistent datasets and must be updated
xxxupdate = clean_function(xxxupdate)

#####
# save data
#####
setwd(wd_2clean)
# TODO code below uses nonexistent datasets and must be updated
qs2::qs_save(xxxupdate, "xxxupdate.qs2")
```

# 3 Merge

Code files in 3 Merge should attempt to load all data files as early as possible, to identify any missing or malformed data before attempting to merge data.

```{r}
#####
# read data
#####
setwd(wd_1raw)
# TODO code below uses nonexistent datasets and must be updated
xxxupdate1 = qs2::qs_read("xxxupdate1.qs2")
xxxupdate2 = qs2::qs_read("xxxupdate2.qs2")

#####
# merge data
#####
# TODO code below uses nonexistent datasets and columns and must be updated
xxxupdate = data.table::merge(xxxupdate1, xxxupdate2, by = c("common_column_name"))

#####
# save data
#####
setwd(wd_2clean)
# TODO code below uses nonexistent datasets and must be updated
qs2::qs_save(xxxupdate, "xxxupdate.qs2")
```

# 4 Analysis

Not currently necessary for this project.

# 5 Export

Code files in 5 Export simply reformat other files for easier usage by other users (such as qs2 to CSV) or copy & reformat files as a group where necessary (such as for a Shiny dashboard, where only the merged data and final shapefiles are needed).