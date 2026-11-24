library(shiny)
library(shinyWidgets)
library(plotly)
library(data.table)
library(lubridate)
library(scales)
library(sf)
library(leaflet)

chapter_data <- as.data.table(data.table::fread("chapter_data.csv"))
# fread can infer IDate, while Shiny date inputs use base Date.
chapter_data[, `:=`(date1 = as.Date(date1), month_year = as.Date(month_year))]
if (!"vintage" %in% names(chapter_data)) {
  setnames(chapter_data, "zip_vintage", "vintage")
}
chapter_data[, vintage := as.character(vintage)]
zip_vintages <- sort(unique(na.omit(chapter_data$vintage)), decreasing = TRUE)
stopifnot(length(zip_vintages) > 0L, !anyNA(chapter_data$vintage),
          !anyDuplicated(chapter_data, by = c("vintage", "cid", "month_year")))
latest_vintage <- zip_vintages[1]
setnames(chapter_data, c("Former Members Lapsed <2 years", "Former Members Lapsed >2 years"),
         c("Lapsed <2 years", "Lapsed 2+ years"), skip_absent = TRUE)
count_cols <- intersect(c("Members", "Change", "Annual", "Monthly", "SIBD", "New", "Quits",
                          "Lapsed <2 years", "Lapsed 2+ years"), names(chapter_data))
chapter_data[, (count_cols) := lapply(.SD, function(x) {
  suppressWarnings(as.numeric(gsub(",", "", trimws(x), fixed = TRUE)))
}), .SDcols = count_cols]
chapter_data[, `:=`(
  `Monthly + SIBD` = Monthly + SIBD,
  local_population = population,
  members_as_pct_population = fifelse(total_population > 0, Members / total_population, NA_real_)
)]
setorder(chapter_data, vintage, namestate, date1)
latest_memberships <- chapter_data[vintage == latest_vintage]

lower_48_states <- setdiff(state.abb, c("AK", "HI"))
map_has_state <- function(parts, state_code) {
  vapply(parts, function(part) any(part == state_code), logical(1))
}
map_affine_transform <- function(map_sf, target_center, scale = 1, scale_x = NULL, scale_y = NULL, source_bbox = NULL) {
  if (nrow(map_sf) == 0) {
    return(map_sf)
  }
  if (is.null(scale_x)) {
    scale_x <- scale
  }
  if (is.null(scale_y)) {
    scale_y <- scale
  }
  map_bbox <- if (is.null(source_bbox)) {
    st_bbox(map_sf)
  } else {
    source_bbox
  }
  source_center <- c(
    (map_bbox["xmin"] + map_bbox["xmax"]) / 2,
    (map_bbox["ymin"] + map_bbox["ymax"]) / 2
  )
  map_geometry <- st_geometry(map_sf)
  transform_piece <- function(piece) {
    if (is.matrix(piece)) {
      piece[, 1] <- (piece[, 1] - source_center[1]) * scale_x + target_center[1]
      piece[, 2] <- (piece[, 2] - source_center[2]) * scale_y + target_center[2]
      return(piece)
    }
    if (is.list(piece)) {
      piece_class <- class(piece)
      transformed <- lapply(piece, transform_piece)
      class(transformed) <- piece_class
      return(transformed)
    }
    piece
  }
  transformed_geometry <- lapply(map_geometry, function(piece) {
    piece_class <- class(piece)
    transformed <- transform_piece(piece)
    class(transformed) <- piece_class
    transformed
  })
  st_geometry(map_sf) <- st_sfc(transformed_geometry, crs = st_crs(map_sf))
  map_sf
}
map_geometry_cache <- new.env(parent = emptyenv())
map_reference_dir <- "us_state_reference"
map_reference_path <- file.path(map_reference_dir, "us_state_reference_inset.gpkg")
map_reference_layer <- "us_state_reference_inset"
map_reference_bbox_path <- file.path(map_reference_dir, "us_state_reference_inset_bboxes.csv")
map_display_simplify_tolerance <- 0.004

simplify_map_geometry <- function(map_sf) {
  suppressWarnings(st_simplify(
    map_sf,
    dTolerance = map_display_simplify_tolerance,
    preserveTopology = TRUE
  ))
}

get_map_region_bboxes <- function() {
  if (!exists("region_bboxes", envir = map_geometry_cache, inherits = FALSE)) {
    bbox_data <- fread(map_reference_bbox_path)
    region_bboxes <- new.env(parent = emptyenv())
    for (state_code in bbox_data$map_state) {
      state_bbox <- unlist(bbox_data[map_state == state_code, .(xmin, ymin, xmax, ymax)])
      names(state_bbox) <- c("xmin", "ymin", "xmax", "ymax")
      region_bboxes[[state_code]] <- state_bbox
    }
    map_geometry_cache$region_bboxes <- region_bboxes
  }
  map_geometry_cache$region_bboxes
}

map_transform_insets <- function(map_sf) {
  if (!("map_state" %in% names(map_sf))) {
    return(map_sf)
  }
  map_region_bboxes <- get_map_region_bboxes()
  state_parts <- strsplit(map_sf$map_state %||% "", ",\\s*")
  alaska_rows <- map_has_state(state_parts, "AK")
  hawaii_rows <- map_has_state(state_parts, "HI")
  puerto_rico_rows <- map_has_state(state_parts, "PR")
  if (any(alaska_rows)) {
    map_sf[alaska_rows, ] <- map_affine_transform(
      map_sf[alaska_rows, ],
      c(-117, 27.5),
      scale_x = 0.315,
      scale_y = 0.4725,
      source_bbox = map_region_bboxes$AK
    )
  }
  if (any(hawaii_rows)) {
    map_sf[hawaii_rows, ] <- map_affine_transform(
      map_sf[hawaii_rows, ],
      c(-102.2, 25.4),
      1.2,
      source_bbox = map_region_bboxes$HI
    )
  }
  if (any(puerto_rico_rows)) {
    map_sf[puerto_rico_rows, ] <- map_affine_transform(
      map_sf[puerto_rico_rows, ],
      c(-90.5, 25.5),
      3,
      source_bbox = map_region_bboxes$PR
    )
  }
  map_sf
}

get_us_reference_map <- function() {
  if (!exists("us_reference_map", envir = map_geometry_cache, inherits = FALSE)) {
    us_reference_map <- st_read(
      map_reference_path,
      layer = map_reference_layer,
      quiet = TRUE
    )
    us_reference_map <- st_transform(us_reference_map, 4326)
    map_geometry_cache$us_reference_map <- us_reference_map[
      us_reference_map$map_state %in% c(lower_48_states, "DC", "AK", "HI", "PR"),
      c("map_state", attr(us_reference_map, "sf_column"))
    ]
  }
  map_geometry_cache$us_reference_map
}

get_chapter_map_shapes <- function(map_base_mode, vintage, variant, memberships) {
  cache_id <- paste("chapters", vintage, variant, sep = "_")
  if (is.null(map_geometry_cache[[cache_id]])) {
    prefix <- if (variant == "filled") "dsa_chapters_zcta_filled" else "dsa_chapters_zcta"
    path <- file.path("chapter_zcta_shapefiles", paste0(prefix, " v", vintage, ".shp"))
    sidecars <- paste0(tools::file_path_sans_ext(path), c(".shp", ".shx", ".dbf", ".prj"))
    validate(need(all(file.exists(sidecars)), paste("Chapter boundaries are unavailable for", vintage, variant)))
    chapter_map_shapes <- st_read(path, quiet = TRUE)
    chapter_map_shapes <- st_transform(chapter_map_shapes, 4326)
    chapter_map_state_lookup <- memberships[
      !is.na(cid) & !is.na(state),
      .SD[which.max(date1)],
      by = cid
    ][, .(cid, map_state = state)]
    chapter_map_shapes <- merge(chapter_map_shapes, chapter_map_state_lookup, by = "cid", all.x = TRUE)
    map_state_parts <- strsplit(chapter_map_shapes$map_state %||% "", ",\\s*")
    keep_map_shape <- vapply(
      map_state_parts,
      function(parts) {
        any(parts %in% c(lower_48_states, "DC", "AK", "HI", "PR"))
      },
      logical(1)
    )
    map_geometry_cache[[cache_id]] <- simplify_map_geometry(chapter_map_shapes[keep_map_shape, ])
  }
  if (identical(map_base_mode, "us_reference")) {
    inset_id <- paste0(cache_id, "_inset")
    if (is.null(map_geometry_cache[[inset_id]])) {
      map_geometry_cache[[inset_id]] <- map_transform_insets(map_geometry_cache[[cache_id]])
    }
    return(map_geometry_cache[[inset_id]])
  }
  map_geometry_cache[[cache_id]]
}

map_zoom_bbox <- function(map_bbox, zoom_factor = 1.1, y_shift_fraction = -0.05) {
  center_x <- (map_bbox["xmin"] + map_bbox["xmax"]) / 2
  center_y <- (map_bbox["ymin"] + map_bbox["ymax"]) / 2
  half_width <- (map_bbox["xmax"] - map_bbox["xmin"]) / (2 * zoom_factor)
  half_height <- (map_bbox["ymax"] - map_bbox["ymin"]) / (2 * zoom_factor)
  center_y <- center_y + (2 * half_height * y_shift_fraction)
  map_bbox["xmin"] <- center_x - half_width
  map_bbox["xmax"] <- center_x + half_width
  map_bbox["ymin"] <- center_y - half_height
  map_bbox["ymax"] <- center_y + half_height
  map_bbox
}
aip_metric_labels <- c(
  self_ideology = "Left-right ideology (survey self-placement, 1 right, 5 left) in the American Ideology Project",
  mrp_ideology = "Left-right ideology (estimated via MRP, -1 right, +1 left) in the American Ideology Project"
)
aip_metric_choices <- names(aip_metric_labels)
election_metric_choices <- c(
  "Democratic vote share in 2024 Presidential election (% of two-party vote)" = "share_dem_2p_2024",
  "Democratic vote share in 2020 Presidential election (% of two-party vote)" = "share_dem_2p_2020",
  "Democratic vote share in 2016 Presidential election (% of two-party vote)" = "share_dem_2p_2016",
  "Green vote share in 2020 Presidential election (% of all votes)" = "share_green_2020",
  "Green vote share in 2016 Presidential election (% of all votes)" = "share_green_2016"
)
chapter_acs_x_choice_groups <- list(
  "Age" = c(
    "Median age" = "estimated_median_age",
    "% age 18-24" = "pct_age_18_24",
    "% age 18-29" = "pct_age_18_29",
    "% age 18-34" = "pct_age_18_34"
  ),
  "Children and marriage" = c(
    "% nonfamily households" = "pct_nonfamily_households",
    "% households without children under 18" = "pct_households_without_children_under18",
    "% never married, age 15+" = "pct_never_married_15plus",
    "% living alone" = "pct_living_alone"
  ),
  "Commute to work" = c(
    "% commute via public transit, walk, or bike" = "pct_public_transit_walk_bike_commute",
    "% commute via public transit" = "pct_public_transit_commute",
    "% commute via walk or bike" = "pct_walk_bike_commute",
    "% no vehicle available" = "pct_no_vehicle"
  ),
  "Education" = c(
    "% graduate/professional degree, age 25+" = "pct_graduate_or_professional_degree_25plus",
    "% bachelor's degree or higher, age 25+" = "pct_bachelors_or_higher_25plus"
  ),
  "Housing" = c(
    "% renter" = "pct_renter_occupied"
  ),
  "Income" = c(
    "Median household income" = "estimated_median_household_income",
    "State median household income" = "state_median_household_income",
    "Median household income as % of state median" = "estimated_median_household_income_pct_of_state",
    "% government workers" = "pct_government_workers"
  ),
  "Ideology" = setNames(aip_metric_choices, unname(aip_metric_labels)),
  "Presidential elections" = election_metric_choices,
  "Race" = c(
    "% White" = "pct_race_white_alone",
    "% Black" = "pct_race_black_alone",
    "% American Indian / Alaska Native" = "pct_race_aian_alone",
    "% Asian / Pacific Islander" = "pct_race_asian_nhpi_alone",
    "% Some other race" = "pct_race_some_other_alone",
    "% Multiracial" = "pct_race_multiracial"
  ),
  "Race and Hispanic ethnicity" = c(
    "% Hispanic" = "pct_eth_hispanic_any_race",
    "% White, non-Hispanic" = "pct_eth_nonhispanic_white_alone",
    "% Black, non-Hispanic" = "pct_eth_nonhispanic_black_alone",
    "% American Indian / Alaska Native, non-Hispanic" = "pct_eth_nonhispanic_aian_alone",
    "% Asian / Pacific Islander, non-Hispanic" = "pct_eth_nonhispanic_asian_nhpi_alone",
    "% Some other race, non-Hispanic" = "pct_eth_nonhispanic_some_other_alone",
    "% Multiracial, non-Hispanic" = "pct_eth_nonhispanic_multiracial"
  )
)
chapter_acs_x_choice_groups <- chapter_acs_x_choice_groups[sort(names(chapter_acs_x_choice_groups))]
chapter_acs_x_choice_groups <- lapply(
  chapter_acs_x_choice_groups,
  function(choices) {
    present <- choices[choices %in% names(chapter_data)]
    present[vapply(chapter_data[, unname(present), with = FALSE],
                   function(x) any(is.finite(x)), logical(1))]
  }
)
chapter_acs_x_choice_groups <- chapter_acs_x_choice_groups[lengths(chapter_acs_x_choice_groups) > 0]
chapter_acs_metric_choices <- unname(unlist(chapter_acs_x_choice_groups, use.names = FALSE))
chapter_acs_metric_labels <- unlist(lapply(chapter_acs_x_choice_groups, names), use.names = FALSE)
names(chapter_acs_metric_labels) <- chapter_acs_metric_choices
scatterplot_presets <- data.frame(
  label = c("College", "Non-car commute", "Renters", "Income", "Relative income",
            "White non-Hisp", "Left-right ideology", "2020 Dem vote", "2016 Green vote"),
  x = c("pct_bachelors_or_higher_25plus", "pct_public_transit_walk_bike_commute",
        "pct_renter_occupied", "estimated_median_household_income",
        "estimated_median_household_income_pct_of_state", "pct_eth_nonhispanic_white_alone",
        "self_ideology", "share_dem_2p_2020", "share_green_2016"),
  scale = c("linear", "log10", "log10", "log10", "log10", "linear", "linear", "linear", "linear")
)
scatterplot_presets <- scatterplot_presets[scatterplot_presets$x %in% chapter_acs_metric_choices, ]
default_scatterplot_preset <- scatterplot_presets[scatterplot_presets$label == "Renters", ]
default_scatterplot_y <- "members_as_pct_population"
default_chapter_acs_x <- if (nrow(default_scatterplot_preset)) {
  default_scatterplot_preset$x
} else {
  chapter_acs_metric_choices[1]
}
default_scatterplot_scale <- if (nrow(default_scatterplot_preset)) default_scatterplot_preset$scale else "linear"
demographics_dates <- latest_memberships[
  is.finite(members_as_pct_population) & is.finite(get(default_chapter_acs_x)) &
    (default_scatterplot_scale != "log10" | get(default_chapter_acs_x) > 0), date1
]
demographics_default_date <- max(if (length(demographics_dates)) demographics_dates else latest_memberships$date1)
chapter_choices <- sort(unique(na.omit(chapter_data$namestate)))
chapter_state_suffixes <- regmatches(chapter_choices, regexpr("(, [A-Z]{2})+$", chapter_choices))
chapter_states <- sort(intersect(
  unique(unlist(strsplit(sub("^, ", "", chapter_state_suffixes), ", ", fixed = TRUE))),
  c(state.abb, "DC")
))
chapters_by_state <- setNames(lapply(chapter_states, function(state) {
  grep(paste0(", ", state, "(, [A-Z]{2})*$"), chapter_choices, value = TRUE)
}), chapter_states)
default_chapter <- "Total DSA Members, US"
y_variables <- c(
  "Members", "Change", "Annual", "Monthly", "SIBD", "Monthly + SIBD", "New", "Quits",
  "Lapsed <2 years", "Lapsed 2+ years"
)
metric_choices <- y_variables[y_variables %in% names(latest_memberships)]
metric_labels <- c(
  Members = "Members",
  Change = "Change in members",
  Annual = "Annual dues-payers",
  Monthly = "Monthly dues-payers",
  SIBD = "SIBD dues-payers",
  `Monthly + SIBD` = "Monthly + SIBD",
  New = "New members",
  Quits = "Quits",
  `Lapsed <2 years` = "Former members lapsed <2 years",
  `Lapsed 2+ years` = "Former members lapsed >2 years"
)
metric_choice_names <- unname(metric_labels[metric_choices])
map_y_choice_groups <- c(
  list("DSA Chapter Variables" = setNames(metric_choices, metric_choice_names)),
  chapter_acs_x_choice_groups
)
map_metric_labels <- c(
  setNames(metric_choice_names, metric_choices),
  chapter_acs_metric_labels
)

if (!(default_chapter %in% chapter_choices)) {
  default_chapter <- chapter_choices[1]
}

member_dates <- latest_memberships[
  ,
  .(date1 = date1, has_data = rowSums(!is.na(.SD)) > 0),
  .SDcols = metric_choices
][has_data == TRUE, date1]
date_min <- min(member_dates, na.rm = TRUE)
date_max <- max(member_dates, na.rm = TRUE)
available_dates <- sort(unique(latest_memberships$date1))
map_member_dates <- latest_memberships[
  !is.na(cid) &
    !grepl("Total|At-Large|Dechartered", namestate, ignore.case = TRUE),
  .(date1 = date1, has_data = rowSums(!is.na(.SD)) > 0),
  .SDcols = metric_choices
][has_data == TRUE, date1]
map_date_min <- min(map_member_dates, na.rm = TRUE)
map_date_max <- max(map_member_dates, na.rm = TRUE)
date_slider_step <- 31
date_slider_format <- "%b %Y"
date_range_tick_dates <- seq(
  floor_date(date_min, unit = "quarter"),
  ceiling_date(date_max, unit = "quarter"),
  by = "3 months"
)
date_range_tick_dates <- date_range_tick_dates[
  date_range_tick_dates >= date_min &
    date_range_tick_dates <= date_max
]
date_range_tick_positions <- 100 * as.numeric(date_range_tick_dates - date_min) /
  as.numeric(date_max - date_min)
date_range_tick_labels <- ifelse(month(date_range_tick_dates) == 1, format(date_range_tick_dates, "%b %Y"), "")
date_range_tick_classes <- ifelse(month(date_range_tick_dates) == 1, "major", "minor")

latest_member_date <- max(latest_memberships[!is.na(Members), date1], na.rm = TRUE)
chapter_ranks <- latest_memberships[
  date1 == latest_member_date &
    !is.na(Members) &
    !grepl("Total|At-Large|Dechartered", namestate, ignore.case = TRUE),
  .(namestate, Members)
]
setorder(chapter_ranks, -Members, namestate)

many_chapter_presets <- list(
  Total = default_chapter,
  `Top 1-5` = chapter_ranks[1:min(5, .N), namestate],
  `Top 6-15` = chapter_ranks[6:min(15, .N), namestate],
  `Top 16-40` = chapter_ranks[16:min(40, .N), namestate],
  `Top 15` = chapter_ranks[1:min(15, .N), namestate],
  `Top 40` = chapter_ranks[1:min(40, .N), namestate]
)

many_variable_presets <- list(
  Members = list(metric = "Members", value_mode = "absolute", comparison_date = NULL),
  `New members` = list(metric = "New", value_mode = "absolute", comparison_date = NULL),
  `% growth` = list(metric = "Change", value_mode = "percent_membership", comparison_date = NULL),
  `Growth since Oct 24` = list(metric = "Members", value_mode = "change_since", comparison_date = as.Date("2024-10-31")),
  `Growth since Oct 20` = list(metric = "Members", value_mode = "change_since", comparison_date = as.Date("2020-10-31")),
  `% monthly dues` = list(metric = "Monthly + SIBD", value_mode = "percent_membership", comparison_date = NULL),
  `% income-based dues` = list(metric = "SIBD", value_mode = "percent_membership", comparison_date = NULL),
  `Top 5 chapters as % of DSA` = list(metric = "Members", value_mode = "percent_total_dsa", chapter_preset = "Top 1-5")
)

month_gap <- function(date, previous_date) {
  (year(date) - year(previous_date)) * 12 + month(date) - month(previous_date)
}

year_range_label <- function(date_range) {
  years <- year(as.Date(date_range))
  if (years[1] == years[2]) {
    as.character(years[1])
  } else {
    paste(years, collapse = "-")
  }
}

closest_data_date <- function(date_value) {
  date_value <- as.Date(date_value)
  available_dates[which.min(abs(as.numeric(available_dates - date_value)))]
}

divide_by_baseline <- function(value, baseline) {
  if (is.na(baseline) || baseline == 0) {
    rep(NA_real_, length(value))
  } else {
    value / baseline
  }
}

chapter_palette <- c(
  "#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02",
  "#a6761d", "#666666", "#0072B2", "#CC6677", "#117733", "#882255"
)

format_percent_sig <- function(value, digits = 2) {
  vapply(
    value,
    function(single_value) {
      if (is.na(single_value)) {
        return(NA_character_)
      }
      percent_value <- single_value * 100
      formatted <- format(signif(percent_value, digits), trim = TRUE, scientific = FALSE)
      paste0(formatted, "%")
    },
    character(1)
  )
}

format_metric_value <- function(metric_value, percent_display = FALSE) {
  if (percent_display) {
    format_percent_sig(metric_value)
  } else {
    comma(metric_value)
  }
}

metric_label <- function(metric_name) {
  labels <- metric_labels[metric_name]
  labels[is.na(labels)] <- metric_name[is.na(labels)]
  unname(labels)
}

build_one_chapter_hover <- function(plot_data, selected_metrics, percent_display = FALSE) {
  selected_metric_labels <- vapply(selected_metrics, metric_label, character(1))
  metric_labels <- lapply(
    selected_metrics,
    function(metric) {
      format_metric_value(plot_data[[metric]], percent_display)
    }
  )

  metric_lines <- vapply(
    seq_len(nrow(plot_data)),
    function(row_index) {
      row_values <- vapply(metric_labels, `[`, character(1), row_index)
      paste0("<br>", selected_metric_labels, ": ", row_values, collapse = "")
    },
    character(1)
  )

  paste0(
    "<b>", plot_data$namestate, "</b>",
    "<br>Date: ", format(plot_data$date1, "%B %Y"),
    metric_lines
  )
}

default_one_chapter_metrics <- "Members"
preset_metrics <- list(
  Members = default_one_chapter_metrics,
  `Dues Types` = c("Annual", "Monthly", "SIBD"),
  `Joins & Leaves` = c("New", "Change", "Quits"),
  `% growth` = c("Change", "New", "Quits")
)

value_mode_choices <- c(
  "Numeric values" = "absolute",
  "% of chapter membership" = "percent_membership",
  "% change since [date]" = "change_since",
  "% of local population" = "percent_local_population",
  "% of total DSA" = "percent_total_dsa"
)
local_value_mode_choices <- value_mode_choices[value_mode_choices != "percent_total_dsa"]
map_presets <- list(
  "Membership % of pop" = list(metric = "Members", value_mode = "percent_local_population"),
  "New members % of pop" = list(metric = "New", value_mode = "percent_local_population"),
  "Left-right ideology" = list(metric = "self_ideology", value_mode = "absolute")
)
default_map_preset <- "Membership % of pop"
default_map_metric <- map_presets[[default_map_preset]]$metric
default_map_value_mode <- map_presets[[default_map_preset]]$value_mode

default_reference_lines <- data.frame(
  date = as.Date(c(
    "2020-03-11",
    "2020-04-08",
    "2020-05-26",
    "2020-11-03",
    "2022-11-08",
    "2024-11-05",
    "2025-06-24",
    "2026-09-01",
    "2026-06-15",
    "2026-11-03")),
  label = c(
    "CDC declares COVID pandemic",
    "Sanders exits Dem primary",
    "George Floyd protests begin",
    "General elections",
    "Midterm elections",
    "General elections",
    "Primary: Mamdani",
    "Stricter definition for membership lapse period",
    "Primaries: Rabb (May), JLG, Valdez, DAC, Kiros, McKinney (Aug)",
    "Midterm elections: AGT, Raman")
)


reference_line_row <- function(id, date = date_max, label = "") {
  enabled_input <- checkboxInput(
    paste0("referenceLineEnabled", id),
    tags$span(class = "sr-only", "Show this reference line"),
    value = TRUE, width = "auto"
  )
  wrapper_style <- "margin-bottom: 0 !important; padding-bottom: 0 !important;"
  tags$div(
    id = paste0("referenceLineRow", id),
    class = "reference-line-row",
    tagAppendAttributes(enabled_input, class = "reference-line-checkbox", style = wrapper_style),
    tagAppendAttributes(
      dateInput(paste0("referenceLineDate", id),
                tags$span(class = "sr-only", "Reference line date"),
                value = date, width = "65px"),
      style = wrapper_style
    ),
    tagAppendAttributes(
      textInput(paste0("referenceLineLabel", id),
                tags$span(class = "sr-only", "Reference line label"),
                value = label, width = "150px"),
      style = wrapper_style
    ),
    tags$button(
      type = "button", class = "btn btn-danger btn-sm",
      style = "padding: 3px 6px; font-size: 7pt;",
      onclick = sprintf("Shiny.setInputValue('removeReferenceLine', %d, {priority: 'event'})", id),
      "Remove"
    )
  )
}

ui <- fluidPage(
  tags$style(
    "
    #plotMode .btn { background: #f8d9d7; border-color: #cf8b87; color: #622823; }
    #plotMode .btn.active, #plotMode .btn:hover { background: #efb9b5; color: #622823; }
    .reference-line-editor {
      background-color: #c4c4c4;
      padding: 10px;
      border-radius: 4px;
    }
    .reference-line-row {
      display: flex;
      flex-wrap: wrap;
      align-items: center;
      gap: 8px;
    }
    .reference-line-row > .form-group,
    .reference-line-row .checkbox {
      margin: 0;
    }
    #referenceLineRows .reference-line-checkbox,
    #referenceLineRows .reference-line-checkbox .checkbox,
    #referenceLineRows .reference-line-checkbox label {
      box-sizing: border-box;
      height: 15.4px;
      min-height: 0;
      margin: 0;
      padding: 0;
    }
    #referenceLineRows .reference-line-checkbox label {
      display: flex;
      align-items: center;
    }
    .reference-line-row input,
    .reference-line-row > .btn {
      margin-bottom: 0;
    }
    .reference-line-row .control-label {
      position: absolute;
      margin: 0;
    }
    #referenceLineRows .reference-line-row .checkbox input[type='checkbox'] {
      position: static;
      margin: 0;
    }
    .reference-line-row input.form-control {
      padding: 3px 6px;
      height: 24px;
      font-size: 7pt;
    }
    #membershipPlot .annotation-text-g[transform^='rotate(-90,'] .annotation-text {
      paint-order: stroke fill;
      stroke: white;
      stroke-width: 2px;
      stroke-linejoin: round;
    }
    #membershipPlot path.reference-highlight {
      stroke: #7e2532 !important;
      opacity: 1 !important;
    }
    #membershipPlot .annotation.reference-highlight {
      opacity: 1 !important;
    }
    #membershipPlot .annotation.reference-highlight .annotation-text {
      fill: #7e2532 !important;
    }
    #chapterPreset .btn, #manyVariablePreset .btn, #yPreset .btn, #scatterplotPreset .btn, #mapPreset .btn {
      background: #3e7959; border-color: #326248; color: white;
    }
    #chapterPreset .btn.active, #chapterPreset .btn:hover,
    #manyVariablePreset .btn.active, #manyVariablePreset .btn:hover,
    #yPreset .btn.active, #yPreset .btn:hover,
    #scatterplotPreset .btn.active, #scatterplotPreset .btn:hover,
    #mapPreset .btn.active, #mapPreset .btn:hover { background: #28563e; color: white; }
    .shiny-options-group .btn-group {
      white-space: normal;
    }
    .shiny-options-group .btn {
      margin-bottom: 4px;
    }
    #valueMode .btn-group-container-sw, #mapValueMode .btn-group-container-sw {
      white-space: normal;
    }
    #valueMode .btn-group-toggle, #mapValueMode .btn-group-toggle {
      width: auto;
    }
    #valueMode .btn, #mapValueMode .btn {
      margin-bottom: 4px;
      white-space: normal;
    }
    .form-group:has(#chapterSubset) {
      margin-bottom: 4px;
    }
    .form-group:has(#valueMode), .form-group:has(#mapValueMode) {
      margin-top: 12px;
    }
    #chapterSubset-label {
      margin-bottom: 2px;
    }
    #chapterSubset + .multi-wrapper {
      margin-bottom: 0;
    }
    #addAllChapters {
      margin-top: 0;
    }
    .chapter-controls {
      display: flex;
      align-items: center;
      gap: 6px;
      margin-bottom: 0 !important;
    }
    .chapter-controls > .shiny-input-container {
      flex: 0 0 auto;
      margin-bottom: 0 !important;f
    }
    #addMatchingChapters {
      margin-bottom: 8px;
    }
    .form-group:has(#dateRange) {
      margin-bottom: 0;
    }
    .custom-date-ticks {
      position: relative;
      height: 26px;
      margin: -1px 10px 8px 10px;
      font-size: 10px;
      color: #444;
    }
    .custom-date-ticks span {
      position: absolute;
      top: 0;
      transform: translateX(-50%);
      white-space: nowrap;
      min-width: 1px;
      text-align: center;
    }
    .custom-date-ticks span::before {
      content: '';
      display: block;
      height: 6px;
      width: 1px;
      margin: 0 auto 1px auto;
      background: #777;
    }
    .custom-date-ticks span.minor::before {
      height: 4px;
      background: #aaa;
    }
    #chapterMap,
    #chapterMap .leaflet-container {
      background: #e9eef2;
    }
    #chapterMap.chapter-map-fullscreen {
      position: fixed !important;
      inset: 0 !important;
      z-index: 9999 !important;
      width: 100vw !important;
      height: 100vh !important;
    }
    #chapterMap.chapter-map-fullscreen .leaflet-container {
      width: 100vw !important;
      height: 100vh !important;
    }
    .chapter-map-fullscreen-button {
      display: block;
      width: 30px;
      height: 30px;
      line-height: 28px;
      text-align: center;
      background: #fff;
      color: #222;
      border: 1px solid #aaa;
      border-radius: 3px;
      font-size: 18px;
      font-weight: 700;
      text-decoration: none;
      box-shadow: 0 1px 4px rgba(0,0,0,0.25);
    }
    .chapter-map-fullscreen-button:hover,
    .chapter-map-fullscreen-button:focus {
      background: #f4f4f4;
      color: #111;
      text-decoration: none;
    }
    "
  ),
  tags$script(
    HTML("
    function addChapterSubsetSearchResults() {
      var searchInput = document.querySelector('#chapterSubset + .multi-wrapper .search-input');
      var searchText = searchInput ? searchInput.value : '';
      Shiny.setInputValue('chapterSubsetSearchAdd', searchText, {priority: 'event'});
    }
    function invalidateChapterMapSize(id) {
      var widget = HTMLWidgets.find('#' + id);
      var map = widget && widget.getMap();
      if (map) map.invalidateSize();
    }
    Shiny.addCustomMessageHandler('resizeLeafletMap', function(id) {
      [100, 300, 700, 1200].forEach(function(delay) {
        setTimeout(function() {
          invalidateChapterMapSize(id);
        }, delay);
      });
    });
    window.toggleChapterMapFullscreen = function() {
      var el = document.getElementById('chapterMap');
      if (!el) return false;
      el.classList.toggle('chapter-map-fullscreen');
      document.body.style.overflow = el.classList.contains('chapter-map-fullscreen') ? 'hidden' : '';
      [50, 200, 500].forEach(function(delay) {
        setTimeout(function() {
          invalidateChapterMapSize(el.id);
        }, delay);
      });
      return false;
    };
    document.addEventListener('click', function(event) {
      var button = event.target.closest && event.target.closest('.chapter-map-fullscreen-button');
      if (!button) return;
      event.preventDefault();
      event.stopPropagation();
      window.toggleChapterMapFullscreen();
    }, true);
    ")
  ),
  titlePanel("Unofficial DSA Membership Dashboard"),
  sidebarLayout(
    sidebarPanel(
      shinyWidgets::radioGroupButtons(
        inputId = "plotMode",
        label = "Plotting mode:",
        choices = c(
          "Many chapters, one variable" = "many_chapters",
          "One chapter, many variables" = "one_chapter",
          "Scatterplots" = "chapter_demographics",
          "Chapter map (wait ~6 seconds)" = "chapter_map"
        ),
        selected = "many_chapters",
        status = "primary"
      ),
      tags$hr(),
      conditionalPanel(
        condition = "input.plotMode == 'many_chapters'",
        shinyWidgets::radioGroupButtons(
          inputId = "chapterPreset",
          label = "Quick chapter groups:",
          choices = names(many_chapter_presets),
          selected = "Total",
          status = "primary"
        ),
        shinyWidgets::radioGroupButtons(
          inputId = "manyVariablePreset",
          label = "Quick variable presets:",
          choices = names(many_variable_presets),
          selected = "Members",
          status = "primary"
        ),
        tags$hr(),
        selectInput(
          inputId = "yVariable",
          label = "Y variable:",
          choices = setNames(metric_choices, metric_choice_names),
          selected = "Members"
        )
      ),
      conditionalPanel(
        condition = "input.plotMode == 'one_chapter'",
        shinyWidgets::radioGroupButtons(
          inputId = "yPreset",
          label = "Presets:",
          choices = names(preset_metrics),
          selected = "Members & dues",
          status = "primary"
        ),
        tags$hr(),
        shinyWidgets::pickerInput(
          inputId = "chapterSingle",
          label = "Chapter:",
          choices = chapter_choices,
          selected = default_chapter,
          options = list(
            `live-search` = TRUE,
            `live-search-placeholder` = "Search chapters",
            `size` = 8
          )
        ),
        shinyWidgets::multiInput(
          inputId = "yVariableSubset",
          label = "Include these Y variables:",
          choices = NULL,
          choiceNames = metric_choice_names,
          choiceValues = metric_choices,
          selected = default_one_chapter_metrics,
          options = list(enable_search = TRUE)
        )
      ),
      conditionalPanel(
        condition = "input.plotMode == 'chapter_demographics'",
        shinyWidgets::radioGroupButtons(
          inputId = "scatterplotPreset",
          label = "Presets:",
          choices = scatterplot_presets$label,
          selected = default_scatterplot_preset$label,
          status = "primary"
        ),
        sliderInput(
          inputId = "demographicsMonth",
          label = "Comparison month:",
          min = date_min,
          max = date_max,
          value = demographics_default_date,
          step = date_slider_step,
          timeFormat = date_slider_format
        ),
        shinyWidgets::radioGroupButtons(
          inputId = "demographicsY",
          label = "Y variable:",
          choices = c(
            "Members" = "Members",
            "Log10 members" = "log10_members",
            "Members as % of population" = "members_as_pct_population"
          ),
          selected = default_scatterplot_y,
          status = "primary"
        ),
        selectInput(
          inputId = "demographicsX",
          label = "X variable:",
          choices = chapter_acs_x_choice_groups,
          selected = default_chapter_acs_x
        ),
        shinyWidgets::radioGroupButtons(
          inputId = "demographicsXScale",
          label = "X value scale:",
          choices = c(
            "Linear values" = "linear",
            "Log10" = "log10"
          ),
          selected = default_scatterplot_scale,
          status = "primary"
        )
      ),
      conditionalPanel(
        condition = "input.plotMode == 'chapter_map'",
        shinyWidgets::radioGroupButtons(
          inputId = "mapPreset",
          label = "Presets:",
          choices = names(map_presets),
          selected = default_map_preset,
          status = "primary"
        ),
        selectInput("mapBoundaryVariant", "Chapter boundaries",
                    c("Filled (holes removed)" = "filled", "Original (with holes)" = "original")),
        sliderInput(
          inputId = "mapMonth",
          label = "Comparison month:",
          min = map_date_min,
          max = map_date_max,
          value = map_date_max,
          step = date_slider_step,
          timeFormat = date_slider_format
        ),
        selectInput(
          inputId = "mapYVariable",
          label = "Y variable:",
          choices = map_y_choice_groups,
          selected = default_map_metric
        ),
        shinyWidgets::radioGroupButtons(
          inputId = "mapBaseMode",
          label = "Map background:",
          choices = c(
            "OpenStreetMap locations" = "tiles",
            "US state outlines" = "us_reference"
          ),
          selected = "tiles",
          status = "primary"
        )
      ),
      conditionalPanel(
        condition = "input.plotMode == 'many_chapters' || input.plotMode == 'one_chapter'",
        shinyWidgets::radioGroupButtons(
          inputId = "valueMode",
          label = "Y value metric:",
          choices = value_mode_choices,
          selected = "absolute",
          status = "primary"
        )
      ),
      conditionalPanel(
        condition = "input.plotMode == 'chapter_map'",
        shinyWidgets::radioGroupButtons(
          inputId = "mapValueMode",
          label = "Y value metric:",
          choices = local_value_mode_choices,
          selected = default_map_value_mode,
          status = "primary"
        )
      ),
      conditionalPanel(
        condition = "((input.plotMode == 'many_chapters' || input.plotMode == 'one_chapter') && input.valueMode == 'change_since') || (input.plotMode == 'chapter_map' && input.mapValueMode == 'change_since')",
        sliderInput(
          inputId = "comparisonDate",
          label = "Comparison month:",
          min = date_min,
          max = date_max,
          value = date_min,
          step = date_slider_step,
          timeFormat = date_slider_format
        )
      ),
      conditionalPanel(
        condition = "input.plotMode == 'many_chapters'",
        shinyWidgets::multiInput(
          inputId = "chapterSubset",
          label = "Include these chapters:",
          choices = NULL,
          choiceNames = chapter_choices,
          choiceValues = chapter_choices,
          selected = default_chapter,
          options = list(enable_search = TRUE)
        ),
        actionButton(
          inputId = "addMatchingChapters",
          label = "Add all chapters in search results",
          onclick = "addChapterSubsetSearchResults();"
        ),
        tags$div(
          class = "chapter-controls",
          tags$label(`for` = "chapterState",
                     style = "margin: 0; font-weight: normal;",
                     "Add all chapters in this state:"),
          htmltools::tagQuery(selectInput(
            "chapterState", label = NULL, choices = chapter_states,
            selectize = FALSE, width = "70px"
          ))$addAttrs(style = "margin-bottom: 0 !important;")$
            find(".shiny-label-null")$remove()$allTags(),
          actionButton("addStateChapters", "Add"),
        ),
        tags$div(
          class = "chapter-controls",
          tags$label(`for` = "addAllChapters",
                     style = "margin: 0; font-weight: normal;",
                     "Add ALL chapters: (WARNING: SLOW!)"),
          actionButton(
            inputId = "addAllChapters",
            label = "⚠️ Add ⚠️",
            class = "btn-danger"
          )
        ),
        sliderInput(
          inputId = "dateRange",
          label = "Date range:",
          min = date_min,
          max = date_max,
          value = c(date_min, date_max),
          step = date_slider_step,
          timeFormat = date_slider_format,
          ticks = FALSE
        ),
        tags$div(
          class = "custom-date-ticks",
          Map(
            function(tick_label, tick_position, tick_class) {
              tags$span(
                class = tick_class,
                style = paste0("left: ", round(tick_position, 3), "%;"),
                tick_label
              )
            },
            date_range_tick_labels,
            date_range_tick_positions,
            date_range_tick_classes
          )
        )
      ),
      conditionalPanel(
        condition = "input.plotMode == 'many_chapters' || input.plotMode == 'one_chapter'",
        shinyWidgets::radioGroupButtons(
          "showReferenceLines", "Vertical reference lines",
          choices = c("Show" = "show", "Hide" = "hide"),
          selected = "show",
          status = "primary"
        ),
        conditionalPanel(
          condition = "input.showReferenceLines == 'show'",
          class = "reference-line-editor",
          tags$div(id = "referenceLineRows", lapply(seq_len(nrow(default_reference_lines)), function(id) {
            reference_line_row(id, default_reference_lines$date[id], default_reference_lines$label[id])
          })),
          actionButton("addReferenceLine", "Add line")
        )
      ),
      tags$hr(),
      tags$div(
        style = "font-size: 12px; line-height: 1.35; color: #555; margin-top: 8px;",
        tags$strong("FAQ:"),
        tags$br(),
        tags$strong("Q:"),
        " What do the \"Y value metric\" options mean? ",
        tags$strong("A:"),
        tags$ul(
          tags$li("Numeric values (N): Raw values for each chapter, like \"number of members\" or \"number of members who pay monthly dues\", etc."),
          tags$li("% of chapter membership: (N) divided by (number of members in that chapter), shows metric as % of chapter"),
          tags$li("% change since [date]: (N for each month) divided by (N at [date]), shows % change in metric"),
          tags$li("% of local population: (N) divided by (number of people in local area), shows % of local ZCCA population"),
          tags$li("% of total DSA: (N) divided by (number of members in DSA), shows chapter's % share of DSA"),
        ),
        tags$strong("Q:"),
        " What is ZCCA? ",
        tags$strong("A:"),
        " DSA defines each chapter's boundaries with a list of zipcodes, which I call the ZipCode Catchement Area (ZCCA). Many researchers release datasets at the zipcode level, using the Census' \"Zipcode Tabulation Areas\" (ZCTAs) files as a base. This allows us to merge ZCTA data with our ZCCA data.",
        tags$br(),
        tags$strong("Q:"),
        " Why multiple zipcode \"vintages\"? ",
        tags$strong("A: "),
        " These change over time, and thus change the comparison unit for population, demographics, vote share and ideology, etc.",
        tags$br(),
        tags$strong("Q:"),
        " In some months, \"Change\" is higher than \"New members\". How? ",
        tags$strong("A:"),
        " New members includes only members new to DSA. Re-joining members are not counted.",
        tags$br(),
        tags$strong("Q:"),
        " What's the data source? ",
        tags$strong("A:"),
        tags$ul(
          tags$li(
            tags$strong("DSA membership:"),
            " DSA GDC's ",
            tags$a(
              "\"DSA Membership Numbers by Chapter\"",
              href = "https://docs.google.com/spreadsheets/d/1qWH39PiSmFrj8cGtVwMVVZSM9BarvCakKRSKIStcHE0/edit",
              target = "_blank"
            ),
            "sheet. For more details, see the ",
            tags$a(
              "Member Data Reports",
              href = "https://discussion.dsausa.org/tag/gdc-membership-update/1441",
              target = "_blank"
            ),
            "on the DSA forums."),
          tags$li(
            tags$strong("Ideology:"),
            " The amazing ",
            tags$a(
              "American Ideology Project",
              href = "https://americanideologyproject.com/",
              target = "_blank"
            ),
            ", version 2022. (See also ",
            tags$a(
              "this map",
              href = "https://www.reddit.com/r/MapPorn/comments/9z747u/liberalnessconservativeness_of_us_counties_data/",
              target = "_blank"
            ),
            ") AIP aggregates a dozen huge election surveys. The 'self-assessed' measure reports how the average respondent rated themself. The 'ideal point' measure provides a statistical estimate of average ideology based on respondents' answers to policy questions. For more detail, read AIP's ",
            tags$a(
              "graypaper",
              href = "https://dataverse.harvard.edu/file.xhtml?fileId=6690214&version=1.0",
              target = "_blank"
            ),
          ),
          tags$li(
            tags$strong("Election results:"),
            " The excellent paper ",
            tags$a(
              "Fekrazad 2025",
              href = "https://www.nature.com/articles/s41597-025-05140-3",
              target = "_blank"
            ),
            ", 2026 update, land cover method (see also ",
            tags$a(
              "code",
              href = "https://github.com/fekrazad/precinct-to-census-mapping/tree/main",
              target = "_blank"
            ),
            " and ",
            tags$a(
              "data",
              href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/Z8TSH3",
              target = "_blank"
            ),
            ")."
          ),
          tags$li(
            tags$strong("Demographics:"),
            " The American Community Survey's ",
            tags$a(
              "5-year dataset",
              href = "https://www.census.gov/data/developers/data-sets/acs-5year.html",
              target = "_blank"),
            " for 2020-24.",
          ),
          tags$li(
            tags$strong("Population:"),
            " The American Community Survey's ",
            tags$a(
              "5-year datasets",
              href = "https://www.census.gov/data/developers/data-sets/acs-5year.html",
              target = "_blank"),
            " for 2012-2024. Overlapping estimates for zipcode population averaged into annual estimates. Annual estimates linearly interpolated for monthly estimates. Estimates past July 2024 are linear extrapolations from 2020-24.",
          ),
        ),
        tags$strong("Q:"),
        " This dashboard rocks! ",
        tags$strong("A:"),
        " yay :)",
        tags$br(),
        tags$strong("Q:"),
        " How can I join DSA? ",
        tags$strong("A:"),
        tags$a("dsausa.org/join", href = "http://dsausa.org/join", target = "_blank"),
      ),
      selectInput("zipVintage", "Chapter ZIP code vintage", zip_vintages, selected = latest_vintage)
    ),
    mainPanel(
      tags$script(
        "
        function downloadMembershipPlot(width, height, filename) {
          var plot = document.getElementById('membershipPlot');
          if (!plot) return;
          Plotly.toImage(plot, {
            format: 'png',
            width: width,
            height: height,
            scale: 1
          }).then(function(url) {
            var image = new Image();
            image.onload = function() {
              var captionHeight = 50;
              var canvas = document.createElement('canvas');
              canvas.width = width;
              canvas.height = height + captionHeight;
              var ctx = canvas.getContext('2d');
              ctx.fillStyle = 'white';
              ctx.fillRect(0, 0, canvas.width, canvas.height);
              ctx.drawImage(image, 0, 0);
              ctx.fillStyle = '#555';
              ctx.font = '12px Arial, sans-serif';
              ctx.textAlign = 'left';
              ctx.fillText('DSA Growth and Development Committee: https://discussion.dsausa.org/tag/1441', 12, height + 20);
              ctx.fillText('Dashboard: https://aidanb-dsa-membership-dashboard.share.connect.posit.cloud/', 12, height + 38);
              var link = document.createElement('a');
              link.href = canvas.toDataURL('image/png');
              link.download = filename + '.png';
              document.body.appendChild(link);
              link.click();
              document.body.removeChild(link);
            };
            image.src = url;
          });
        }
        "
      ),
      conditionalPanel(
        condition = "input.plotMode != 'chapter_map'",
        tags$div(
          class = "btn-group",
          role = "group",
          tags$button(
            type = "button",
            class = "btn btn-default",
            onclick = "downloadMembershipPlot(800, 600, 'dsa-membership-800x600')",
            "PNG 800x600"
          ),
          tags$button(
            type = "button",
            class = "btn btn-default",
            onclick = "downloadMembershipPlot(1200, 900, 'dsa-membership-1200x900')",
            "PNG 1200x900"
          ),
          tags$button(
            type = "button",
            class = "btn btn-default",
            onclick = "downloadMembershipPlot(1920, 1080, 'dsa-membership-1920x1080')",
            "PNG 1920x1080"
          )
        ),
        plotlyOutput("membershipPlot", height = "650px")
      ),
      conditionalPanel(
        condition = "input.plotMode == 'chapter_map'",
        leafletOutput("chapterMap", height = "650px")
      ),
      tags$div(
        style = "margin-top: 0; color: #555; font-size: 11px; line-height: 1.25;",
        "Dashboard: https://aidanb-dsa-membership-dashboard.share.connect.posit.cloud/",
        tags$br(),
        "Growth and Development Committee membership data: https://discussion.dsausa.org/tag/1441"
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$mapPreset, {
    preset <- map_presets[[input$mapPreset]]
    req(preset$metric %in% c(metric_choices, chapter_acs_metric_choices))
    updateSelectInput(session, "mapYVariable", selected = preset$metric)
    shinyWidgets::updateRadioGroupButtons(session, "mapValueMode", selected = preset$value_mode)
  }, ignoreInit = TRUE)

  reference_line_ids <- reactiveVal(seq_len(nrow(default_reference_lines)))

  observeEvent(input$addReferenceLine, {
    id <- nrow(default_reference_lines) + input$addReferenceLine
    reference_line_ids(c(reference_line_ids(), id))
    insertUI(
      selector = "#referenceLineRows", where = "beforeEnd",
      ui = reference_line_row(id)
    )
  })

  observeEvent(input$removeReferenceLine, {
    id <- input$removeReferenceLine
    if (id %in% reference_line_ids()) {
      reference_line_ids(setdiff(reference_line_ids(), id))
      removeUI(selector = paste0("#referenceLineRow", id))
    }
  })

  selected_vintage <- reactive({
    selected <- input$zipVintage %||% latest_vintage
    req(selected %in% zip_vintages)
    selected
  })
  vintage_data <- reactive({
    selected <- selected_vintage()
    chapter_data[vintage == selected]
  })

  observeEvent(input$chapterPreset, {
    selected_chapters <- many_chapter_presets[[input$chapterPreset]]
    selected_chapters <- selected_chapters[selected_chapters %in% chapter_choices]
    if (length(selected_chapters) == 0) {
      selected_chapters <- default_chapter
    }

    shinyWidgets::updateMultiInput(
      session = session,
      inputId = "chapterSubset",
      selected = selected_chapters
    )
  }, ignoreInit = TRUE)

  observeEvent(input$manyVariablePreset, {
    selected_preset <- many_variable_presets[[input$manyVariablePreset]]
    if (is.null(selected_preset)) {
      return()
    }

    if (selected_preset$metric %in% metric_choices) {
      updateSelectInput(
        session = session,
        inputId = "yVariable",
        selected = selected_preset$metric
      )
    }

    shinyWidgets::updateRadioGroupButtons(
      session = session,
      inputId = "valueMode",
      selected = selected_preset$value_mode
    )

    if (!is.null(selected_preset$chapter_preset)) {
      selected_chapters <- many_chapter_presets[[selected_preset$chapter_preset]]
      shinyWidgets::updateRadioGroupButtons(
        session, "chapterPreset", selected = selected_preset$chapter_preset
      )
      shinyWidgets::updateMultiInput(session, "chapterSubset", selected = selected_chapters)
    }

    if (!is.null(selected_preset$comparison_date)) {
      updateSliderInput(
        session = session,
        inputId = "comparisonDate",
        value = closest_data_date(selected_preset$comparison_date)
      )
    }
  }, ignoreInit = TRUE)

  observeEvent(input$addAllChapters, {
    shinyWidgets::updateMultiInput(
      session = session,
      inputId = "chapterSubset",
      selected = chapter_choices[chapter_choices != default_chapter]
    )
  })

  observeEvent(input$addStateChapters, {
    req(input$chapterState)
    matching_chapters <- chapters_by_state[[input$chapterState]]
    req(length(matching_chapters))

    if (identical(input$plotMode, "many_chapters")) {
      selected <- unique(c(input$chapterSubset, matching_chapters))
      shinyWidgets::updateMultiInput(session, "chapterSubset", selected = selected)
    } else if (identical(input$plotMode, "one_chapter")) {
      selected <- if (isTRUE(input$chapterSingle %in% matching_chapters)) {
        input$chapterSingle
      } else {
        matching_chapters[1]
      }
      shinyWidgets::updatePickerInput(
        session, "chapterSingle", selected = selected,
        choices = list("Selected state" = matching_chapters,
                       "Other chapters" = setdiff(chapter_choices, matching_chapters))
      )
    }
  })

  observeEvent(input$chapterSubsetSearchAdd, {
    match_text <- trimws(input$chapterSubsetSearchAdd %||% "")
    if (match_text == "") {
      return()
    }

    matching_chapters <- chapter_choices[
      grepl(tolower(match_text), tolower(chapter_choices), fixed = TRUE)
    ]
    selected_chapters <- input$chapterSubset %||% character()
    selected_chapters <- unique(c(selected_chapters, matching_chapters))

    shinyWidgets::updateMultiInput(
      session = session,
      inputId = "chapterSubset",
      selected = selected_chapters
    )
  })

  observeEvent(input$yPreset, {
    selected_preset <- preset_metrics[[input$yPreset]]
    selected_preset <- selected_preset[selected_preset %in% metric_choices]
    if (length(selected_preset) == 0) {
      selected_preset <- "Members"
    }

    shinyWidgets::updateMultiInput(
      session = session,
      inputId = "yVariableSubset",
      selected = selected_preset
    )
    shinyWidgets::updateRadioGroupButtons(
      session, "valueMode",
      selected = if (input$yPreset == "% growth") "percent_membership" else "absolute"
    )
  }, ignoreInit = TRUE)

  observeEvent(input$plotMode, {
    if (input$plotMode %in% c("many_chapters", "one_chapter")) {
      choices <- if (input$plotMode == "many_chapters") value_mode_choices else local_value_mode_choices
      selected <- input$valueMode %||% "absolute"
      if (!selected %in% choices) selected <- "absolute"
      shinyWidgets::updateRadioGroupButtons(
        session, "valueMode", choices = choices, selected = selected, status = "primary"
      )
    }
    if (identical(input$plotMode, "chapter_map")) {
      session$sendCustomMessage("resizeLeafletMap", "chapterMap")
    }
  }, ignoreInit = FALSE)

  plot_memberships <- reactive({
    memberships <- vintage_data()
    date_range <- as.Date(input$dateRange)
    # if input mode is one chapter, many variables
    if (identical(input$plotMode, "one_chapter")) {
      selected_chapter <- input$chapterSingle
      if (is.null(selected_chapter) || selected_chapter == "") {
        selected_chapter <- default_chapter
      }

      selected_metrics <- input$yVariableSubset
      if (is.null(selected_metrics) || length(selected_metrics) == 0) {
        selected_metrics <- default_one_chapter_metrics
      }
      title_metric <- paste(metric_label(selected_metrics), collapse = ", ")
      value_mode <- input$valueMode %||% "absolute"
      if (!value_mode %in% local_value_mode_choices) value_mode <- "absolute"
      percent_membership <- identical(value_mode, "percent_membership")
      percent_local_population <- identical(value_mode, "percent_local_population")
      percent_total_dsa <- identical(value_mode, "percent_total_dsa")
      change_since <- identical(value_mode, "change_since")
      percent_display <- percent_membership || percent_local_population || percent_total_dsa || change_since
      comparison_date <- closest_data_date(input$comparisonDate %||% date_min)

      plot_data <- memberships[
        namestate == selected_chapter &
          date1 >= date_range[1] &
          date1 <= date_range[2],
        unique(c("namestate", "date1", "Members", "local_population", selected_metrics)),
        with = FALSE
      ]

      if (percent_membership) {
        plot_data[, (selected_metrics) := lapply(
          .SD,
          function(value) fifelse(Members > 0, value / Members, NA_real_)
        ), .SDcols = selected_metrics]
      } else if (percent_local_population) {
        plot_data[, (selected_metrics) := lapply(
          .SD,
          function(value) fifelse(local_population > 0, value / local_population, NA_real_)
        ), .SDcols = selected_metrics]
      } else if (percent_total_dsa) {
        total_dsa_data <- memberships[
          namestate == default_chapter,
          c("date1", selected_metrics),
          with = FALSE
        ]
        setnames(
          total_dsa_data,
          old = selected_metrics,
          new = paste0("total_dsa_", make.names(selected_metrics))
        )
        plot_data <- merge(plot_data, total_dsa_data, by = "date1", all.x = TRUE)
        for (metric in selected_metrics) {
          denominator <- paste0("total_dsa_", make.names(metric))
          plot_data[, (metric) := fifelse(
            !is.na(get(denominator)) & get(denominator) != 0,
            get(metric) / get(denominator),
            NA_real_
          )]
        }
      } else if (change_since) {
        baseline_data <- memberships[
          namestate == selected_chapter & date1 == comparison_date,
          selected_metrics,
          with = FALSE
        ]
        baseline_values <- if (nrow(baseline_data) > 0) {
          unlist(baseline_data[1], use.names = TRUE)
        } else {
          setNames(rep(NA_real_, length(selected_metrics)), selected_metrics)
        }
        plot_data[, (selected_metrics) := Map(
          divide_by_baseline,
          .SD,
          baseline_values[selected_metrics]
        ), .SDcols = selected_metrics]
      }
      if (percent_membership && "Change" %in% selected_metrics) {
        plot_data[Change >= 1, Change := NA_real_]
      }
      plot_data[, hover_text := build_one_chapter_hover(.SD, selected_metrics, percent_display)]

      plot_data <- melt(
        plot_data,
        id.vars = c("namestate", "date1", "hover_text"),
        measure.vars = selected_metrics,
        variable.name = "metric_name",
        value.name = "metric_value"
      )
      plot_data[, metric_name := as.character(metric_name)]
      plot_data[, series_name := metric_name]
      plot_data[, series_order := match(series_name, selected_metrics)]
      legend_title <- "Y variable"
      y_title <- if (change_since) {
        paste0("% of ", format(comparison_date, "%B %Y"))
      } else if (percent_membership) {
        "% of membership"
      } else if (percent_local_population) {
        "% of local population"
      } else if (percent_total_dsa) {
        "% of total DSA"
      } else if (length(selected_metrics) == 1) {
        metric_label(selected_metrics)
      } else {
        "Value"
      }
      percent_axis <- percent_display
      show_comparison_line <- change_since
    } else {
      selected_chapters <- input$chapterSubset
      if (is.null(selected_chapters) || length(selected_chapters) == 0) {
        selected_chapters <- default_chapter
      }

      selected_metric <- input$yVariable
      if (is.null(selected_metric) || selected_metric == "") {
        selected_metric <- "Members"
      }
      title_metric <- metric_label(selected_metric)

      plot_data <- memberships[
        namestate %in% selected_chapters &
          date1 >= date_range[1] &
          date1 <= date_range[2]
      ]

      plot_data[, metric_name := selected_metric]
      plot_data[, metric_value := get(selected_metric)]
      value_mode <- input$valueMode %||% "absolute"
      percent_membership <- identical(value_mode, "percent_membership")
      percent_local_population <- identical(value_mode, "percent_local_population")
      percent_total_dsa <- identical(value_mode, "percent_total_dsa")
      change_since <- identical(value_mode, "change_since")
      comparison_date <- closest_data_date(input$comparisonDate %||% date_min)
      if (percent_membership) {
        plot_data[, metric_value := fifelse(Members > 0, metric_value / Members, NA_real_)]
        if (identical(selected_metric, "Change")) {
          plot_data[metric_value >= 1, metric_value := NA_real_]
        }
      } else if (percent_local_population) {
        plot_data[, metric_value := fifelse(local_population > 0, metric_value / local_population, NA_real_)]
      } else if (percent_total_dsa) {
        total_dsa_data <- memberships[
          namestate == default_chapter,
          .(date1, total_dsa_value = get(selected_metric))
        ]
        plot_data <- merge(plot_data, total_dsa_data, by = "date1", all.x = TRUE)
        plot_data[, metric_value := fifelse(
          !is.na(total_dsa_value) & total_dsa_value != 0,
          metric_value / total_dsa_value,
          NA_real_
        )]
      } else if (change_since) {
        baseline_data <- memberships[
          namestate %in% selected_chapters & date1 == comparison_date,
          .(series_name = namestate, baseline_value = get(selected_metric))
        ]
        plot_data <- merge(plot_data, baseline_data, by.x = "namestate", by.y = "series_name", all.x = TRUE)
        plot_data[, metric_value := fifelse(
          !is.na(baseline_value) & baseline_value != 0,
          metric_value / baseline_value,
          NA_real_
        )]
      }
      plot_data[, order_value := metric_value]
      plot_data[, series_name := namestate]
      latest_plot_date <- max(plot_data[!is.na(metric_value), date1], na.rm = TRUE)
      chapter_order <- plot_data[
        date1 == latest_plot_date & !is.na(order_value),
        .(order_value = max(order_value, na.rm = TRUE)),
        by = series_name
      ]
      setorder(chapter_order, -order_value, series_name)
      plot_data[, series_order := match(series_name, chapter_order$series_name)]
      legend_title <- "Chapter"
      y_title <- if (change_since) {
        paste0("% of ", format(comparison_date, "%B %Y"))
      } else if (percent_membership) {
        "% of membership"
      } else if (percent_local_population) {
        "% of local population"
      } else if (percent_total_dsa) {
        "% of total DSA"
      } else {
        metric_label(selected_metric)
      }
      percent_axis <- percent_membership || percent_local_population || percent_total_dsa || change_since
      show_comparison_line <- change_since
      plot_data[, hover_text := NA_character_]
    }

    # plot title
    if (identical(input$plotMode, "one_chapter")) {
      title_chapter = selected_chapter
    } else {
      title_chapter = "DSA"
    }
    plot_title <- paste0(
      title_metric,
      if (identical(value_mode, "absolute")) {
        paste0(
          " in ",
          title_chapter,
          ", ",
          year_range_label(date_range)
        )
      } else if (percent_membership) {
        paste0(
          " as % of membership in ",
          title_chapter,
          ", ",
          year_range_label(date_range)
          )
      } else if (percent_local_population) {
        paste0(
          " as % of local population for ",
          title_chapter,
          ", ",
          year_range_label(date_range)
          )
      } else if (percent_total_dsa) {
        paste0(" as % of total DSA, ", year_range_label(date_range))
      } else if (change_since) {
        paste0(
          " as % of value in ",
          format(comparison_date, "%B %Y"),
          " for ",
          title_chapter
          )
      } else {
        ""
      }
    )

    plot_data <- plot_data[!is.na(metric_value)]
    setorder(plot_data, series_order, date1)
    plot_data[, segment := cumsum(c(FALSE, month_gap(date1[-1], date1[-.N]) > 2)), by = series_name]

    list(
      data = plot_data,
      legend_title = legend_title,
      y_title = y_title,
      plot_title = plot_title,
      percent_axis = percent_axis,
      show_comparison_line = show_comparison_line,
      comparison_date = comparison_date,
      date_range = date_range
    )
  })

  observeEvent(input$scatterplotPreset, {
    preset <- scatterplot_presets[scatterplot_presets$label == input$scatterplotPreset, ]
    req(nrow(preset) == 1L)
    shinyWidgets::updateRadioGroupButtons(session, "demographicsY", selected = default_scatterplot_y)
    updateSelectInput(session, "demographicsX", selected = preset$x)
    shinyWidgets::updateRadioGroupButtons(session, "demographicsXScale", selected = preset$scale)
  }, ignoreInit = TRUE)

  chapter_demographics_plot <- reactive({
    memberships <- vintage_data()
    selected_month <- closest_data_date(input$demographicsMonth %||% demographics_default_date)
    x_var <- input$demographicsX %||% default_chapter_acs_x
    if (!(x_var %in% chapter_acs_metric_choices)) {
      x_var <- default_chapter_acs_x
    }
    x_title <- chapter_acs_metric_labels[[x_var]] %||% x_var
    y_var <- input$demographicsY %||% default_scatterplot_y
    if (!(y_var %in% c("Members", "log10_members", "members_as_pct_population"))) {
      y_var <- "members_as_pct_population"
    }
    x_scale <- input$demographicsXScale %||% default_scatterplot_scale
    if (!(x_scale %in% c("linear", "log10"))) {
      x_scale <- "linear"
    }

    plot_data <- memberships[date1 == selected_month]
    plot_data[, x_value := as.numeric(get(x_var))]
    election_share <- x_var %in% election_metric_choices
    if (election_share) {
      plot_data[, x_value := 100 * x_value]
    }
    if (identical(y_var, "log10_members")) {
      plot_data <- plot_data[Members > 0]
      plot_data[, y_value := log10(Members)]
    } else {
      plot_data[, y_value := as.numeric(get(y_var))]
    }
    if (identical(x_scale, "log10")) {
      plot_data <- plot_data[x_value > 0]
    }
    plot_data <- plot_data[!is.na(x_value) & !is.na(y_value)]
    validate(
      need(nrow(plot_data) > 0, "No chapter demographic data matches the selected month and variables.")
    )

    y_is_percent <- identical(y_var, "members_as_pct_population")
    y_title <- if (identical(y_var, "log10_members")) {
      "Log10 members"
    } else if (y_is_percent) {
      "Members as % of population"
    } else {
      "Members"
    }
    y_hover <- if (identical(y_var, "log10_members")) {
      paste0(round(plot_data$y_value, 2), " (", comma(plot_data$Members), " members)")
    } else if (y_is_percent) {
      format_percent_sig(plot_data$y_value)
    } else {
      comma(plot_data$y_value)
    }
    x_is_percent <- election_share || startsWith(x_var, "pct_") || grepl("pct_of_state$", x_var)
    x_hover <- if (x_is_percent) {
      paste0(format(signif(plot_data$x_value, 2), trim = TRUE, scientific = FALSE), "%")
    } else {
      comma(plot_data$x_value)
    }
    plot_data[, hover_text := paste0(
      "<b>", name, ", ", state, "</b>",
      "<br>Month: ", format(selected_month, "%B %Y"),
      "<br>", x_title, ": ", x_hover,
      "<br>", y_title, ": ", y_hover
    )]

    fit_data <- plot_data[is.finite(x_value) & is.finite(y_value)]
    annotation_text <- "r^2 = NA<br>p = NA<br>slope = NA"
    line_data <- NULL
    if (identical(x_scale, "log10")) {
      fit_data[, model_x_value := log10(x_value)]
    } else {
      fit_data[, model_x_value := x_value]
    }
    if (nrow(fit_data) >= 3 && uniqueN(fit_data$model_x_value) >= 2) {
      fit <- lm(y_value ~ model_x_value, data = fit_data)
      fit_summary <- summary(fit)
      f_stat <- fit_summary$fstatistic
      p_value <- if (is.null(f_stat)) {
        NA_real_
      } else {
        pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
      }
      p_stars <- if (is.na(p_value)) {
        ""
      } else if (p_value < 0.001) {
        " ***"
      } else if (p_value < 0.01) {
        " **"
      } else if (p_value < 0.05) {
        " *"
      } else {
        ""
      }
      p_text <- if (is.na(p_value)) {
        "p = NA"
      } else if (p_value < 0.001) {
        paste0("p < 0.001", p_stars)
      } else {
        paste0("p = ", signif(p_value, 2), p_stars)
      }
      slope <- coef(fit)[["model_x_value"]]
      annotation_text <- paste0(
        "r^2 = ",
        round(fit_summary$r.squared, 2),
        "<br>",
        p_text,
        "<br>",
        "slope = ",
        formatC(slope, digits = 3, format = "g")
      )
      x_line <- seq(min(fit_data$x_value), max(fit_data$x_value), length.out = 100)
      model_x_line <- if (identical(x_scale, "log10")) log10(x_line) else x_line
      line_data <- data.table(
        x_value = x_line,
        y_value = predict(fit, newdata = data.frame(model_x_value = model_x_line))
      )
    }

    x_ticks <- NULL
    if (identical(x_scale, "log10") && nrow(plot_data) > 0) {
      x_min <- min(plot_data$x_value, na.rm = TRUE)
      x_max <- max(plot_data$x_value, na.rm = TRUE)
      x_tick_values <- trans_breaks("log10", function(value) 10^value)(c(x_min, x_max))
      x_tick_values <- x_tick_values[x_tick_values >= x_min & x_tick_values <= x_max]
      if (length(x_tick_values) == 0) {
        x_tick_values <- pretty(c(x_min, x_max), n = 4)
        x_tick_values <- x_tick_values[x_tick_values > 0 & x_tick_values >= x_min & x_tick_values <= x_max]
      }
      x_tick_text <- if (x_is_percent) {
        paste0(format(signif(x_tick_values, 2), trim = TRUE, scientific = FALSE), "%")
      } else {
        comma(x_tick_values)
      }
      x_ticks <- list(values = x_tick_values, text = x_tick_text)
    }

    list(
      data = plot_data,
      line_data = line_data,
      x_title = x_title,
      x_is_percent = x_is_percent,
      x_scale = x_scale,
      x_ticks = x_ticks,
      y_title = y_title,
      y_is_percent = y_is_percent,
      title = paste0("Chapter zipcode demographics, DSA ", format(selected_month, "%B %Y")),
      annotation_text = annotation_text
    )
  })

  chapter_map_data <- reactive({
    memberships <- vintage_data()
    selected_month <- closest_data_date(input$mapMonth %||% map_date_max)
    selected_metric <- input$mapYVariable %||% default_map_metric
    if (!(selected_metric %in% c(metric_choices, chapter_acs_metric_choices))) {
      selected_metric <- default_map_metric
    }
    is_dsa_metric <- selected_metric %in% metric_choices
    is_demographic_metric <- selected_metric %in% chapter_acs_metric_choices
    value_mode <- input$mapValueMode %||% default_map_value_mode
    if (!value_mode %in% local_value_mode_choices) value_mode <- default_map_value_mode
    map_base_mode <- input$mapBaseMode %||% "us_reference"
    if (!(map_base_mode %in% c("tiles", "us_reference"))) {
      map_base_mode <- "us_reference"
    }
    percent_membership <- is_dsa_metric && identical(value_mode, "percent_membership")
    percent_total_dsa <- is_dsa_metric && identical(value_mode, "percent_total_dsa")
    change_since <- is_dsa_metric && identical(value_mode, "change_since")
    percent_local_population <- is_dsa_metric && identical(value_mode, "percent_local_population")
    demographic_percent <- is_demographic_metric && grepl("(^pct_|_pct_)", selected_metric)
    percent_display <- percent_membership || percent_total_dsa || change_since ||
      percent_local_population || demographic_percent || selected_metric %in% election_metric_choices

    if (is_dsa_metric) {
      map_metric_data <- memberships[
        date1 == selected_month,
        .(
          cid,
          namestate,
          Members,
          local_population,
          metric_value = as.numeric(get(selected_metric))
        )
      ]
      map_metric_data <- unique(map_metric_data, by = "cid")
      if (percent_membership) {
        map_metric_data[, metric_value := fifelse(Members > 0, metric_value / Members, NA_real_)]
      } else if (percent_total_dsa) {
        total_dsa_value <- memberships[
          namestate == default_chapter & date1 == selected_month,
          as.numeric(get(selected_metric))
        ]
        total_dsa_value <- total_dsa_value[!is.na(total_dsa_value)][1]
        if (!is.na(total_dsa_value) && total_dsa_value != 0) {
          map_metric_data[, metric_value := metric_value / total_dsa_value]
        } else {
          map_metric_data[, metric_value := NA_real_]
        }
      } else if (change_since) {
        comparison_date <- closest_data_date(input$comparisonDate %||% date_min)
        baseline_data <- memberships[
          date1 == comparison_date,
          .(cid, baseline_value = as.numeric(get(selected_metric)))
        ]
        baseline_data <- unique(baseline_data, by = "cid")
        map_metric_data <- merge(map_metric_data, baseline_data, by = "cid", all.x = TRUE)
        map_metric_data[, metric_value := fifelse(
          !is.na(baseline_value) & baseline_value != 0,
          metric_value / baseline_value,
          NA_real_
        )]
      } else if (percent_local_population) {
        map_metric_data[, metric_value := fifelse(
          local_population > 0,
          metric_value / local_population,
          NA_real_
        )]
      }
    } else {
      map_metric_data <- memberships[
        date1 == selected_month & !is.na(cid),
        .(cid, namestate, metric_value = as.numeric(get(selected_metric)))
      ]
    }
    if (identical(map_base_mode, "us_reference")) {
      validate(need(all(file.exists(c(map_reference_path, map_reference_bbox_path))),
                    "US state outlines are unavailable. Choose OpenStreetMap locations."))
    }
    map_shapes <- merge(
      get_chapter_map_shapes(map_base_mode, selected_vintage(),
                            input$mapBoundaryVariant %||% "filled", memberships),
      map_metric_data, by = "cid", all.x = TRUE
    )
    validate(
      need(any(!is.na(map_shapes$metric_value)), "No chapter map data matches the selected month and variable.")
    )

    metric_title <- if (change_since) {
      paste0(metric_label(selected_metric), " as % of ", format(comparison_date, "%B %Y"))
    } else if (percent_membership) {
      paste0(metric_label(selected_metric), " as % of membership")
    } else if (percent_total_dsa) {
      paste0(metric_label(selected_metric), " as % of total DSA")
    } else if (percent_local_population) {
      paste0(metric_label(selected_metric), " as % of local population")
    } else if (is_demographic_metric) {
      map_metric_labels[[selected_metric]] %||% selected_metric
    } else {
      metric_label(selected_metric)
    }

    map_shapes$metric_label <- metric_title
    map_shapes$metric_display <- if (percent_display) {
      if (isTRUE(demographic_percent)) {
        paste0(format(signif(map_shapes$metric_value, 2), trim = TRUE, scientific = FALSE), "%")
      } else {
        format_percent_sig(map_shapes$metric_value)
      }
    } else {
      comma(map_shapes$metric_value)
    }
    map_shapes$label_text <- paste0(
      map_shapes$namestate,
      "\n",
      metric_title,
      ": ",
      map_shapes$metric_display
    )
    map_shapes$popup_text <- paste0(
      "<strong>", htmltools::htmlEscape(map_shapes$namestate), "</strong>",
      "<br>Month: ", format(selected_month, "%B %Y"),
      "<br>", htmltools::htmlEscape(metric_title), ": ",
      htmltools::htmlEscape(map_shapes$metric_display)
    )

    list(
      data = map_shapes,
      metric_label = metric_title,
      selected_month = selected_month,
      percent_display = percent_display,
      demographic_percent = demographic_percent,
      map_base_mode = map_base_mode
    )
  })

  output$chapterMap <- renderLeaflet({
    req(identical(input$plotMode, "chapter_map"), cancelOutput = TRUE)
    map_config <- chapter_map_data()
    map_shapes <- map_config$data
    us_reference_map <- if (identical(map_config$map_base_mode, "us_reference")) {
      get_us_reference_map()
    } else {
      NULL
    }
    fill_domain <- map_shapes$metric_value[is.finite(map_shapes$metric_value)]
    pal <- colorNumeric(
      # palette = c("#D9DDF3", "#9DB8F2", "#4393C3", "#1A5FA8", "#08306B"),
      # palette = c("#F7F7F7", "#A6CEE3", "#2CA5A5", "#1A6FA8", "#0D3B5E"),
      # palette = c("#F7F7F7", "#A6CEE3", "#2CA5A5", "#1A6FA8", "#0D3B5E"),
      # palette = c("#fff7bc", "#fec44f", "#fe9929", "#d95f0e", "#993404"),
      # palette = c("#AED6E8", "#D4C4C0", "#C97B6A", "#A33030", "#5C1010"),
      palette = c("#CEF6FF", "#D4C4FF", "#C97BBB", "#A33030", "#4C0505"),
      # palette = c("#DDDDFF", "#D4C4FF", "#C97BBB", "#933030", "#4C0505"),
      domain = fill_domain,
      na.color = "transparent"
    )
    legend_format <- if (isTRUE(map_config$demographic_percent)) {
      labelFormat(suffix = "%")
    } else if (isTRUE(map_config$percent_display)) {
      labelFormat(transform = function(value) 100 * value, suffix = "%")
    } else {
      labelFormat(big.mark = ",")
    }
    conus_shapes <- map_shapes[
      !grepl("(^|,\\s*)(AK|HI|PR)($|,)", map_shapes$map_state %||% ""),
    ]
    map_bounds <- if (identical(map_config$map_base_mode, "us_reference")) {
      st_bbox(us_reference_map)
    } else if (nrow(conus_shapes) > 0) {
      st_bbox(conus_shapes)
    } else {
      st_bbox(map_shapes)
    }
    map_fit_bounds <- map_zoom_bbox(map_bounds)

    map_widget <- leaflet(
      map_shapes,
      options = leafletOptions(
        minZoom = 3,
        zoomSnap = 0.5,
        zoomDelta = 0.5,
        preferCanvas = TRUE,
        maxBoundsViscosity = 1
      )
    )
    if (identical(map_config$map_base_mode, "tiles")) {
      carto_key <- Sys.getenv("CARTO_BASEMAPS_API_KEY")

      map_widget <- map_widget %>% addTiles(
        urlTemplate = paste0(
          "https://basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}{r}.png?key=",
          utils::URLencode(carto_key, reserved = TRUE)
        ),
        attribution = paste(
          '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>',
          '&copy; <a href="https://carto.com/attributions">CARTO</a>'
        )
      )
    } else {
      map_widget <- map_widget %>% addPolygons(
        data = us_reference_map,
        group = "reference",
        fillColor = "#f8f8f8",
        fillOpacity = 1,
        color = "#c1c1c1",
        weight = 0.7,
        opacity = 1,
        smoothFactor = 1.5
      )
    }

    map_widget %>%
      addPolygons(
        group = "chapters",
        layerId = ~cid,
        fillColor = ~pal(metric_value),
        fillOpacity = ~ifelse(is.finite(metric_value), 0.8, 0),
        # color = "#6a6a6a",
        color = "#d1d1d1",
        weight = 0.5,
        opacity = 1,
        smoothFactor = 1.5,
        popup = ~popup_text,
        label = ~label_text,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#3a3a3a",
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = fill_domain,
        title = NULL,
        labFormat = legend_format,
        opacity = 0.75
      ) %>%
      addControl(
        html = "<a href='#' class='chapter-map-fullscreen-button' title='Toggle fullscreen' onclick='return toggleChapterMapFullscreen();'>&#x26F6;</a>",
        position = "topleft"
      ) %>%
      addControl(
        html = paste0(
          "<div style='font-weight:600; font-size:16px;'>",
          htmltools::htmlEscape(map_config$metric_label),
          ", DSA ",
          format(map_config$selected_month, "%B %Y"),
          "</div>"
        ),
        position = "topright"
      ) %>%
      fitBounds(
        lng1 = as.numeric(map_fit_bounds["xmin"]),
        lat1 = as.numeric(map_fit_bounds["ymin"]),
        lng2 = as.numeric(map_fit_bounds["xmax"]),
        lat2 = as.numeric(map_fit_bounds["ymax"])
      )
  })
  outputOptions(output, "chapterMap", suspendWhenHidden = TRUE)

  output$membershipPlot <- renderPlotly({
    if (identical(input$plotMode, "chapter_demographics")) {
      plot_config <- chapter_demographics_plot()
      plot_data <- plot_config$data

      fig <- plot_ly(
        data = plot_data,
        x = ~x_value,
        y = ~y_value,
        type = "scatter",
        mode = "markers",
        marker = list(color = "#1b9e77", size = 8, opacity = 0.75),
        text = ~hover_text,
        hoverinfo = "text",
        showlegend = FALSE
      )

      if (!is.null(plot_config$line_data)) {
        fig <- add_trace(
          fig,
          data = plot_config$line_data,
          x = ~x_value,
          y = ~y_value,
          type = "scatter",
          mode = "lines",
          line = list(color = "black", width = 1.5),
          hoverinfo = "skip",
          showlegend = FALSE,
          inherit = FALSE
        )
      }

      y_axis <- if (plot_config$y_is_percent) {
        list(title = plot_config$y_title, tickformat = ".2p", rangemode = "tozero")
      } else {
        list(title = plot_config$y_title, tickformat = ",", rangemode = "tozero")
      }
      x_axis <- if (plot_config$x_is_percent) {
        list(title = plot_config$x_title, ticksuffix = "%")
      } else {
        list(title = plot_config$x_title)
      }
      if (identical(plot_config$x_scale, "log10")) {
        x_axis$type <- "log"
        if (!is.null(plot_config$x_ticks)) {
          x_axis$tickmode <- "array"
          x_axis$tickvals <- plot_config$x_ticks$values
          x_axis$ticktext <- plot_config$x_ticks$text
        }
      }

      return(fig %>%
        layout(
          title = list(text = plot_config$title),
          margin = list(t = 40, b = 20),
          xaxis = x_axis,
          yaxis = y_axis,
          hovermode = "closest",
          annotations = list(
            list(
              x = 0.98,
              y = 0.02,
              xref = "paper",
              yref = "paper",
              text = plot_config$annotation_text,
              showarrow = FALSE,
              xanchor = "right",
              yanchor = "bottom",
              align = "right",
              bgcolor = "rgba(255,255,255,0.75)"
            )
          )
        ))
    }

    plot_config <- plot_memberships()
    plot_data <- plot_config$data

    validate(
      need(nrow(plot_data) > 0, "No membership data matches the current filters.")
    )

    if (plot_config$percent_axis) {
      plot_data[, hover_value := format_percent_sig(metric_value)]
    } else {
      plot_data[, hover_value := comma(metric_value)]
    }

    y_range <- NULL
    if (isTRUE(plot_config$show_comparison_line)) {
      y_values <- c(0, 1, plot_data$metric_value)
      y_min <- min(y_values, na.rm = TRUE)
      y_max <- max(y_values, na.rm = TRUE)
      y_padding <- max((y_max - y_min) * 0.04, 0.02)
      y_range <- c(y_min, y_max + y_padding)
    }

    y_axis <- if (plot_config$percent_axis) {
      list(title = plot_config$y_title, tickformat = ".2p", rangemode = "tozero")
    } else {
      list(title = plot_config$y_title, tickformat = ",", rangemode = "tozero")
    }
    if (!is.null(y_range)) {
      y_axis$range <- y_range
    }

    x_axis <- list(title = "")

    if (!identical(input$plotMode, "one_chapter")) {
      plot_data[, hover_text := paste0(
        "<b>", namestate, "</b>",
        "<br>Date: ", format(date1, "%B %Y"),
        "<br>", metric_label(metric_name), ": ", hover_value
      )]
    }

    fig <- plot_ly()
    selected_series <- unique(plot_data$series_name)
    plotted_dates <- plot_data$date1[is.finite(plot_data$date1) & is.finite(plot_data$metric_value)]
    plotted_date_range <- if (length(plotted_dates)) range(plotted_dates) else NULL
    date_is_plotted <- function(date) {
      length(plotted_date_range) == 2L && length(date) == 1L && !is.na(date) &&
        date >= plotted_date_range[1] && date <= plotted_date_range[2]
    }

    comparison_shapes <- list()
    if (isTRUE(plot_config$show_comparison_line)) {
      comparison_shapes <- list(
        list(
          type = "line",
          xref = "x",
          x0 = plot_config$comparison_date,
          x1 = plot_config$comparison_date,
          yref = "paper",
          y0 = 0,
          y1 = 1,
          line = list(color = "black", width = 1.5),
          layer = "between"
        ),
        list(
          type = "line",
          xref = "paper",
          x0 = 0,
          x1 = 1,
          yref = "y",
          y0 = 1,
          y1 = 1,
          line = list(color = "black", width = 1.5),
          layer = "between"
        )
      )
      if (!date_is_plotted(plot_config$comparison_date)) {
        comparison_shapes <- comparison_shapes[-1L]
      }
    }

    reference_annotations <- list()
    reference_hover_pairs <- list()
    if (identical(input$showReferenceLines, "show")) {
      for (id in reference_line_ids()) {
        date <- as.Date(input[[paste0("referenceLineDate", id)]])
        if (!isTRUE(input[[paste0("referenceLineEnabled", id)]]) ||
            !date_is_plotted(date)) next

        shape_index <- length(comparison_shapes)
        comparison_shapes[[shape_index + 1L]] <- list(
          type = "line", xref = "x", x0 = as.character(date), x1 = as.character(date),
          yref = "paper", y0 = 0, y1 = 1,
          line = list(color = "#555555", width = 1.5, dash = "dash"),
          opacity = 0.25
        )
        label <- trimws(input[[paste0("referenceLineLabel", id)]] %||% "")
        annotation_index <- -1L
        if (nzchar(label)) {
          annotation_index <- length(reference_annotations)
          reference_annotations[[annotation_index + 1L]] <- list(
            xref = "x", x = as.character(date), yref = "paper", y = 1,
            text = htmltools::htmlEscape(label), showarrow = FALSE,
            textangle = -90, align = "right",
            xanchor = "left", xshift = -3, yanchor = "top", yshift = 0,
            font = list(size = 10), opacity = 0.25
          )
        }
        reference_hover_pairs[[length(reference_hover_pairs) + 1L]] <- list(
          shape = shape_index, annotation = annotation_index
        )
      }
    }

    for (series_index in seq_along(selected_series)) {
      series_label <- selected_series[series_index]
      series_data <- plot_data[series_name == series_label]
      color <- chapter_palette[((series_index - 1) %% length(chapter_palette)) + 1]
      segments <- unique(series_data$segment)

      for (segment_index in seq_along(segments)) {
        segment_data <- series_data[segment == segments[segment_index]]
        fig <- add_trace(
          fig,
          data = segment_data,
          x = ~date1,
          y = ~metric_value,
          type = "scatter",
          mode = "lines+markers",
          name = series_label,
          legendgroup = series_label,
          showlegend = segment_index == 1,
          line = list(color = color),
          marker = list(color = color),
          text = ~hover_text,
          hoverinfo = "text"
        )
      }
    }

    fig <- fig %>%
      layout(
        title = list(text = plot_config$plot_title),
        margin = list(t = 40, b = 20),
        xaxis = x_axis,
        yaxis = y_axis,
        shapes = comparison_shapes,
        annotations = reference_annotations,
        showlegend = TRUE,
        hovermode = "closest",
        legend = list(title = list(text = plot_config$legend_title))
      )
    if (!length(reference_hover_pairs)) return(fig)

    htmlwidgets::onRender(fig, "
      function(el, x, pairs) {
        if (el.referenceHoverRefresh) {
          el.removeListener('plotly_afterplot', el.referenceHoverRefresh);
        }
        el.referenceHoverRefresh = function() {
          pairs.forEach(function(pair) {
            var line = el.querySelector('.shapelayer path[data-index=' + CSS.escape(String(pair.shape)) + ']');
            if (!line) return;
            var annotation = el.querySelector('.annotation[data-index=' + CSS.escape(String(pair.annotation)) + ']');

            // Use a solid hit target so gaps in the dashed stroke also respond.
            var target = line.nextElementSibling;
            if (!target || !target.classList.contains('reference-hover-target')) {
              target = line.cloneNode(false);
              target.classList.add('reference-hover-target');
              target.style.stroke = 'transparent';
              target.style.strokeDasharray = 'none';
              target.style.strokeWidth = (parseFloat(line.style.strokeWidth) + 4) + 'px';
              target.style.pointerEvents = 'stroke';
              line.after(target);
            }

            function highlight(active) {
              line.classList.toggle('reference-highlight', active);
              if (annotation) annotation.classList.toggle('reference-highlight', active);
            }
            target.onpointerenter = function() { highlight(true); };
            target.onpointerleave = function() { highlight(false); };
            var label = annotation && annotation.querySelector('.annotation-text');
            if (label) {
              label.style.pointerEvents = 'all';
              label.onpointerenter = function() { highlight(true); };
              label.onpointerleave = function() { highlight(false); };
            }
          });
        };
        el.on('plotly_afterplot', el.referenceHoverRefresh);
        el.referenceHoverRefresh();
      }
    ", data = reference_hover_pairs)
  })
}

shinyApp(ui = ui, server = server)
