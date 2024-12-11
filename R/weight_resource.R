#' Check if object inherits any class
#' @noRd
check_is_any <- function(x, class, arg = caller_arg(x), call = caller_env()) {
  if (inherits_any(x, class)) {
    return(invisible(NULL))
  }

  cli::cli_abort(
    "{.arg {arg}} must be a {.or {class}} object,
    not {.cls {class(x)}}.",
    call = call
  )
}

#' Check if object is all geometry type
#' @noRd
check_st_is_all <- function(x,
                            type,
                            class = c("sf", "sfc"),
                            arg = caller_arg(x),
                            call = caller_env()) {
  check_is_any(
    x,
    class = class,
    arg = arg,
    call = call
  )

  is_type <- sf::st_is(x, type)

  if (all(is_type)) {
    return(invisible(NULL))
  }

  x_type <- unique(sf::st_geometry_type(x))

  cli::cli_abort(
    "{.arg {arg}} must be use only {.or {type}} geometry,
    not {.and {x_type}}.",
    call = call
  )
}

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
    allow_type = c(
      "POINT", "MULTIPOINT",
      "LINESTRING", "MULTILINESTRING",
      "POLYGON", "MULTIPOLYGON"
    ),
    arg = caller_arg(x),
    call = caller_env()) {
  check_st_is_all(
    x = x,
    type = allow_type,
    class = c("sfg", "sfc", "sf"),
    arg = arg,
    call = call
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
#' [weight_resource_by_area()] uses [sf::st_intersection()] to combine a
#' `resource` sf object with `area` geometry to derive a new weight column based
#' on the length or area of the geometry created by the intersection.
#' [weight_resource_by_admin_geo()] is a variation that uses the `{tigris}`
#' package to download administrative areas from the U.S. Census Bureau to use
#' as the `area` input.
#'
#' @param resource A `sf` object to modify.
#' @param area A `sf` or `sfc` object to intersect with resource.
#' @inheritParams obj_as_point
#' @param weight Type of function to use in calculating the weight value. If
#'   "length", use [sf::st_length()] and, if "area", use
#'   [sf::st_area()] to calculate weight values based on the geometry. `weight`
#'   can also be any other function that supports the geometry column as the
#'   only input parameter. If `weight` is a function, the results are coerced to
#'   a numeric vector (dropping the units class if available).
#' @param placement Type of function to use in processing the returned geometry.
#'   "surface" (default) uses [sf::st_point_on_surface()], "centroid" uses
#'   [sf::st_centroid()], or "none" returns geometry with the same type as the
#'   input resource modified only by the intersection with the specified
#'   administrative geography.
#' @returns A modified sf object supplied to `resource` where features are
#'   intersected with `area` and optionally transformed to use POINT geometry.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
#'
#'   roads <- tigris::roads(state = "NC", county = nc$NAME[1:5])
#'
#'   weight_resource_by_area(
#'     roads,
#'     nc[1:5, ],
#'     weight = "length"
#'   )
#' }
#' }
#' @keywords internal
#' @export
weight_resource_by_area <- function(
    resource,
    area,
    weight = c("length", "area"),
    placement = "surface",
    ...,
    call = caller_env()) {
  check_is_any(resource, "sf", call = call)
  check_st_is_all(area, c("POLYGON", "MULTIPOLYGON"), class = c("sfc", "sf"), call = call)

  crs <- sf::st_crs(resource)
  if (crs != sf::st_crs(area)) {
    area <- sf::st_transform(area, crs = crs)
  }

  if (is.character(weight)) {
    # TODO: Add a "count" option to weight
    weight <- arg_match0(weight, c("length", "area"), error_call = call)

    if (weight == "length") {
      check_st_is_all(resource, c("LINESTRING", "MULTILINESTRING"), call = call)
      weight_fn <- sf::st_length
    } else if (weight == "area") {
      check_st_is_all(resource, c("POLYGON", "MULTIPOLYGON"), call = call)
      weight_fn <- sf::st_area
    }
  } else if (rlang::is_function(weight)) {
    weight_fn <- weight
    weight <- "weight"
  }

  if (inherits(area, "sf")) {
    area <- area[, attr(area, "sf_column"), drop = FALSE]
  }

  resource_intersection <- suppressWarnings(
    sf::st_make_valid(
      sf::st_intersection(
        x = resource,
        y = area
      )
    )
  )

  stopifnot(
    !has_name(resource_intersection, weight)
  )

  sf_column <- attr(resource_intersection, "sf_column")

  resource_intersection <- dplyr::mutate(
    resource_intersection,
    # TODO: Consider if units class should be preserved and if `call_sedt_api()`
    # should handle that automatically
    "{weight}" := as.numeric(suppressWarnings(weight_fn(.data[[sf_column]]))),
    .before = dplyr::all_of(sf_column)
  )

  placement <- arg_match(placement, error_call = call)

  if (placement == "none") {
    return(resource_intersection)
  }

  obj_as_point(
    resource_intersection,
    placement = placement
  )
}

#' @param geo One of "city", "county", "state", or "national". Not yet
#'   implemented but this parameter should be used for validating the
#'   `admin_geo` value.
#' @param admin_geo Type of geometry to use for intersecting area. One of
#'   "tract" (default), "county", or "state" specifying the corresponding tigris
#'   function (`tigris::tracts`, `tigris::counties`, or `tigris::states`).
#' @param year Year of data passed to tigris functions. Uses global
#'   `"sedtR.year"` option or 2021 if option is not set.
#' @param ... Additional parameters passed to tigris function specified by
#'   `admin_geo`.
#' @rdname weight_resource_by_area
#' @name weight_resource_by_admin_geo
#' @keywords internal
#' @export
weight_resource_by_admin_geo <- function(
    resource,
    state = NULL,
    county = NULL,
    geo = "city",
    admin_geo = "tract",
    weight = c("length", "area"),
    placement = "surface",
    year = getOption("sedtR.year", 2021),
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
    tract = suppressMessages(tigris::tracts(
      year = year,
      state = state,
      county = county,
      ...
    )),
    county = suppressMessages(tigris::counties(
      year = year,
      state = state,
      ...
    )),
    state = suppressMessages(tigris::states(
      year = year,
      ...
    ))
  )

  weight_resource_by_area(
    resource = resource,
    area = admin_geo,
    weight = weight,
    placement = placement,
    call = call
  )
}
