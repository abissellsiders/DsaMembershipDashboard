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
zcta_zip = file.path(wd_1raw, "cb_2020_us_zcta520_500k.zip")
if (!file.exists(zcta_zip)) {
  stop("Run 1 Download US ZCTA Reference.R to obtain the 2020 ZCTA shapefile.")
}
zcta_path = paste0("/vsizip/", normalizePath(zcta_zip, winslash = "/"),
                   "/cb_2020_us_zcta520_500k.shp")
zctas = sf::st_read(zcta_path, quiet = TRUE)
stopifnot(is.character(zctas$ZCTA5CE20), !anyNA(zctas$ZCTA5CE20),
          !anyDuplicated(zctas$ZCTA5CE20))

chapter_metadata = qs2::qs_read(file.path(wd_1raw, "membership_chapters.qs2"))
chapter_ids = unique(chapter_metadata[
  !is.na(cid) & nzchar(trimws(cid)),
  .(chapter = original_name, state = original_state, cid)
])
setwd(wd_2clean)
zip_files = list.files(pattern = "^zipcodes v[0-9]{4}-(0[1-9]|1[0-2])[.]qs2$")
if (!length(zip_files)) {
  stop("No versioned ZIP snapshots in 2 Clean.")
}

#####
# munge data
#####
fill_polygon_holes = function(geometry, overlap_threshold = 0.05) {
  polygons = sf::st_cast(geometry, "MULTIPOLYGON")
  crs = sf::st_crs(polygons)
  area = function(x) as.numeric(sf::st_area(sf::st_transform(x, 6933)))
  shells = lapply(seq_along(polygons), function(chapter) {
    parts = lapply(polygons[[chapter]], function(rings) {
      if (length(rings) <= 1L) return(rings)
      holes = sf::st_sfc(lapply(rings[-1L], function(ring) {
        sf::st_polygon(list(ring))
      }), crs = crs)
      candidates = suppressMessages(sf::st_intersects(holes, polygons))
      keep = vapply(seq_along(holes), function(i) {
        other_chapters = setdiff(candidates[[i]], chapter)
        if (!length(other_chapters)) return(FALSE)
        covered = suppressMessages(sf::st_intersection(holes[i], polygons[other_chapters]))
        if (!length(covered)) return(FALSE)
        # Count shared coverage once, using equal-area measurements.
        covered = suppressMessages(sf::st_union(covered))
        area(covered) > overlap_threshold * area(holes[i])
      }, logical(1))
      rings[c(TRUE, keep)]
    })
    sf::st_multipolygon(parts)
  })
  filled = sf::st_sfc(shells, crs = crs)
  # A separate polygon already enclosed by a filled ring is covered by that shell.
  # Union those overlapping shells while keeping all disconnected components.
  overlaps = which(!sf::st_is_valid(filled))
  for (i in overlaps) {
    filled[i] = suppressMessages(sf::st_union(sf::st_cast(filled[i], "POLYGON")))
  }
  filled
}

output_dir = "chapter_zcta_shapefiles"
dir.create(output_dir, showWarnings = FALSE)

# Use planar topology for the Census cartographic boundary polygons.
withr::with_options(list(sf_use_s2 = FALSE), {
  sf::st_geometry(zctas) = sf::st_make_valid(sf::st_geometry(zctas))

  for (zip_file in zip_files) {
    snapshot = substr(zip_file, 11, 17)
    chapter_zips = qs2::qs_read(zip_file)
    zip_ids = merge(chapter_zips, chapter_ids, by = c("chapter", "state"), all.x = TRUE)
    if (nrow(zip_ids) != nrow(chapter_zips) || anyNA(zip_ids$cid)) {
      stop("Missing or ambiguous chapter IDs in ", zip_file)
    }
    zip_ids = unique(zip_ids[, .(cid, zipcode)])
    zip_ids[, zcta_row := match(zipcode, zctas$ZCTA5CE20)]
    matched = zip_ids[!is.na(zcta_row)]
    if (!nrow(matched)) {
      stop("No chapter ZIPs matched 2020 ZCTAs in ", zip_file)
    }
    rows_by_chapter = split(matched$zcta_row, matched$cid)
    geometry = do.call(c, lapply(rows_by_chapter, function(rows) {
      suppressMessages(sf::st_union(sf::st_geometry(zctas)[rows]))
    }))
    geometry = sf::st_collection_extract(sf::st_make_valid(geometry), "POLYGON")
    chapters = sf::st_sf(
      cid = names(rows_by_chapter),
      zcta_count = unname(lengths(rows_by_chapter)),
      geometry = geometry
    )
    filled = chapters
    sf::st_geometry(filled) = fill_polygon_holes(sf::st_geometry(chapters))

    #####
    # save data
    #####
    sf::st_write(sf::st_transform(chapters, 4326), file.path(
      output_dir, paste0("dsa_chapters_zcta v", snapshot, ".shp")
    ), delete_layer = TRUE, quiet = TRUE)
    sf::st_write(sf::st_transform(filled, 4326), file.path(
      output_dir, paste0("dsa_chapters_zcta_filled v", snapshot, ".shp")
    ), delete_layer = TRUE, quiet = TRUE)

    message(snapshot, ": wrote both variants for ", nrow(chapters), " chapters (",
            nrow(zip_ids) - nrow(matched), " unmatched chapter-ZIP pairs).")
  }
})
