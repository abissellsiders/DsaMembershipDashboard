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
raw_path = file.path(wd_1raw, "tigris_states_2024_500k.qs2")
output_dir = file.path(wd_2clean, "us_state_reference")
output_path = file.path(output_dir, "us_state_reference.gpkg")
inset_output_path = file.path(output_dir, "us_state_reference_inset.gpkg")
bbox_output_path = file.path(output_dir, "us_state_reference_inset_bboxes.csv")

#####
# munge data
#####
if (!file.exists(raw_path)) {
  if (!all(file.exists(c(output_path, inset_output_path, bbox_output_path)))) {
    stop("Run 1 Download Tigris State Reference.R to obtain the raw state boundaries.")
  }
  message("Using existing cleaned state reference files. Raw state boundaries are unavailable.")
} else {
  states = qs2::qs_read(raw_path)


  states = sf::st_transform(states, 4326)
  states$map_state = states$STUSPS
  states = states[
    states$map_state %in% c(state.abb, "DC", "PR"),
    c("map_state", attr(states, "sf_column"))
  ]

  alaska_rows = states$map_state == "AK"
  if (any(alaska_rows)) {
    states[alaska_rows, ] = suppressWarnings(sf::st_crop(
      states[alaska_rows, ],
      xmin = -170,
      xmax = -129,
      ymin = 51,
      ymax = 72
    ))
  }

  hawaii_rows = states$map_state == "HI"
  if (any(hawaii_rows)) {
    states[hawaii_rows, ] = suppressWarnings(sf::st_crop(
      states[hawaii_rows, ],
      xmin = -161,
      xmax = -154,
      ymin = 18.5,
      ymax = 23
    ))
  }

  states = sf::st_make_valid(states)

  map_affine_transform = function(map_sf, target_center, scale = 1, scale_x = NULL, scale_y = NULL, source_bbox = NULL) {
    if (nrow(map_sf) == 0) {
      return(map_sf)
    }
    if (is.null(scale_x)) {
      scale_x = scale
    }
    if (is.null(scale_y)) {
      scale_y = scale
    }
    map_bbox = if (is.null(source_bbox)) {
      sf::st_bbox(map_sf)
    } else {
      source_bbox
    }
    source_center = c(
      (map_bbox["xmin"] + map_bbox["xmax"]) / 2,
      (map_bbox["ymin"] + map_bbox["ymax"]) / 2
    )
    map_geometry = sf::st_geometry(map_sf)
    transform_piece = function(piece) {
      if (is.matrix(piece)) {
        piece[, 1] = (piece[, 1] - source_center[1]) * scale_x + target_center[1]
        piece[, 2] = (piece[, 2] - source_center[2]) * scale_y + target_center[2]
        return(piece)
      }
      if (is.list(piece)) {
        piece_class = class(piece)
        transformed = lapply(piece, transform_piece)
        class(transformed) = piece_class
        return(transformed)
      }
      piece
    }
    transformed_geometry = lapply(map_geometry, function(piece) {
      piece_class = class(piece)
      transformed = transform_piece(piece)
      class(transformed) = piece_class
      transformed
    })
    sf::st_geometry(map_sf) = sf::st_sfc(transformed_geometry, crs = sf::st_crs(map_sf))
    map_sf
  }

  region_bbox_rows = lapply(c("AK", "HI", "PR"), function(state_code) {
    state_bbox = sf::st_bbox(states[states$map_state == state_code, ])
    data.frame(
      map_state = state_code,
      xmin = unname(state_bbox["xmin"]),
      ymin = unname(state_bbox["ymin"]),
      xmax = unname(state_bbox["xmax"]),
      ymax = unname(state_bbox["ymax"])
    )
  })
  region_bboxes = do.call(rbind, region_bbox_rows)

  states_inset = states
  alaska_rows = states_inset$map_state == "AK"
  hawaii_rows = states_inset$map_state == "HI"
  puerto_rico_rows = states_inset$map_state == "PR"
  if (any(alaska_rows)) {
    states_inset[alaska_rows, ] = map_affine_transform(
      states_inset[alaska_rows, ],
      c(-117, 27.5),
      scale_x = 0.315,
      scale_y = 0.4725,
      source_bbox = sf::st_bbox(states[states$map_state == "AK", ])
    )
  }
  if (any(hawaii_rows)) {
    states_inset[hawaii_rows, ] = map_affine_transform(
      states_inset[hawaii_rows, ],
      c(-102.2, 25.4),
      1.2,
      source_bbox = sf::st_bbox(states[states$map_state == "HI", ])
    )
  }
  if (any(puerto_rico_rows)) {
    states_inset[puerto_rico_rows, ] = map_affine_transform(
      states_inset[puerto_rico_rows, ],
      c(-90.5, 25.5),
      3,
      source_bbox = sf::st_bbox(states[states$map_state == "PR", ])
    )
  }
  states_inset = sf::st_make_valid(states_inset)

  #####
  # save data
  #####
  dir.create(output_dir, showWarnings = FALSE)
  sf::st_write(states, output_path, layer = "us_state_reference", quiet = TRUE, delete_layer = TRUE)
  sf::st_write(states_inset, inset_output_path, layer = "us_state_reference_inset", quiet = TRUE, delete_layer = TRUE)
  write.csv(region_bboxes, bbox_output_path, row.names = FALSE, quote = TRUE)

  message("Wrote ", nrow(states), " Census cartographic state reference geometries to ", output_path)
  message("Wrote precomputed inset reference geometries to ", inset_output_path)
  message("Wrote inset source bounding boxes to ", bbox_output_path)
}
