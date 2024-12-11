test_that("weight_resource_by_area errors with invalid inputs", {
  nc = sf::read_sf(system.file("shape/nc.shp", package="sf"))

  # Error on non-sf object as resource
  expect_error(
    weight_resource_by_area(as.data.frame(nc))
  )

  # Error on non-POINT geometry for area
  expect_error(
    weight_resource_by_area(nc, sf::st_centroid(nc[["geometry"]]))
  )
})

test_that("weight_resource_by_area works with valid inputs", {
  nc_path <- system.file("shape/nc.shp", package="sf")
  nc = sf::read_sf(nc_path)
  nc_lines <- suppressWarnings(sf::st_cast(nc, to = "MULTILINESTRING"))

  # Works with POLYGON resource and area input
  expect_s3_class(
    weight_resource_by_area(nc, sf::st_union(nc), weight = "area"),
    "sf"
  )

  # Works with LINESTRING resource and POLYGON area input
  expect_s3_class(
    weight_resource_by_area(nc_lines, nc, weight = "length"),
    "sf"
  )
})
