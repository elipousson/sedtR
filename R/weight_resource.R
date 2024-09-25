#' Convert a sfg, sfc, or sf object to use POINT geometry
#'
#' @param x A sfg, sfc, or sf object to convert to POINT geometry.
#' @param placement If "centroid" (default) convert geometry to POINT using
#'   [sf::st_centroid()]. If "surface", use [sf::st_point_on_surface()].
#' @param allow_what Allowed types of geometry. Defaults to `c("POINT",
#'   "MULTIPOINT", "LINESTRING", "MULTILINESTRING", "POLYGON", "MULTIPOLYGON")`.
#' @keywords internal
obj_as_point <- function(
    x,
    split_multi_point = FALSE,
    placement = c("centroid", "surface"),
    allow_what = c(
      "POINT", "MULTIPOINT",
      "LINESTRING", "MULTILINESTRING",
      "POLYGON", "MULTIPOLYGON"
    )) {
  stopifnot(
    inherits_any(x, c("sfg", "sfc", "sf")),
    any(sf::st_is(x, allow_what))
  )

  if (split_multi_point && all(sf::st_is(x, c("POINT", "MULTIPOINT")))) {
    x <- sf::st_cast(x, "POINT", warn = FALSE, do_split = TRUE)
  }

  if (all(sf::st_is(x, "POINT"))) {
    return(x)
  }

  placement <- arg_match(placement)

  placement_fn <- switch(placement,
    surface = sf::st_point_on_surface,
    centroid = sf::st_centroid
  )

  suppressWarnings(placement_fn(x))
}

#' Weight a resource based on intersection with administrative areas
#'
#' [weight_resource_by_area()] uses [sf::st_intersection()] to combine
#' `resource` with `area` to derive a new weight column based on the length or
#' area of the intersected geometry. [weight_resource_by_admin_geo()] is a
#' variation that uses the `{tigris}` package to download administrative areas
#' from the U.S. Census Bureau to use as the `area` input.
#'
#' @param resource A sf object to modify.
#' @param area A sf or sfc object to intersect with resource.
#' @inheritParams obj_as_point
#' @param weight If "length", use [sf::st_length()] to assign weight. If "area",
#'   use [sf::st_area()] to assign weight.
#' @keywords internal
weight_resource_by_area <- function(
    resource,
    area,
    weight = c("length", "area"),
    placement = c("centroid", "surface", "none"),
    ...) {
  stopifnot(
    inherits(resource, "sf"),
    inherits(area, c("sfc", "sf")),
    any(sf::st_is(area, c("POLYGON", "MULTIPOLYGON")))
  )

  crs <- sf::st_crs(resource)

  if (sf::st_crs(resource) != sf::st_crs(area)) {
    area <- sf::st_transform(area, crs = crs)
  }

  # TODO: Add a "count" option to weight
  weight <- arg_match0(weight, c("length", "area"))

  if (weight == "length") {
    stopifnot(
      any(sf::st_is(resource, c("LINESTRING", "MULTILINESTRING")))
    )
    weight_fn <- sf::st_length
  } else if (weight == "area") {
    stopifnot(
      any(sf::st_is(resource, c("POLYGON", "MULTIPOLYGON")))
    )
    weight_fn <- sf::st_area
  }

  resource_intersection <- suppressWarnings(
    sf::st_intersection(
      x = resource,
      y = area
    )
  )

  stopifnot(
    !has_name(resource_intersection, weight)
  )

  resource_intersection <- dplyr::mutate(
    resource_intersection,
    "{weight}" := as.numeric(suppressWarnings(weight_fn(geometry))),
    .before = dplyr::all_of(attr(resource_intersection, "sf_column"))
  )

  placement <- arg_match(placement)

  if (placement == "none") {
    return(resource_intersection)
  }

  obj_as_point(
    resource_intersection,
    placement = placement
  )
}

#' @inheritParams tigris::tracts
#' @param geo One of "city", "county", "state", or "national".
#' @param admin_geo One of "tract", "county", or "state".
#' @param placement One of "centroid", "surface", or "none".
#' @rdname weight_resource_by_area
weight_resource_by_admin_geo <- function(
    resource,
    state = NULL,
    county = NULL,
    geo = "city",
    admin_geo = "tract",
    weight = c("length", "area"),
    placement = c("centroid", "surface", "none"),
    ...,
    call = caller_env()) {
  check_installed("tigris", call = call)

  geo <- match_geo(geo, error_call = call)

  # TODO: Use geo to determine allowed values for admin_geo
  admin_geo <- arg_match(
    admin_geo,
    c("tract", "county", "state"),
    error_call = call
  )

  admin_geo <- switch(admin_geo,
    tract = tigris::tracts(state = state, county = county, ...),
    county = tigris::counties(state = state, ...),
    state = tigris::states(...)
  )

  weight_resource_by_area(
    resource = resource,
    area = admin_geo,
    weight = weight,
    placement = placement
  )
}
