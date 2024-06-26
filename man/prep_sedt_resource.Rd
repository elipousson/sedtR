% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep_sedt_resource.R
\name{prep_sedt_resource}
\alias{prep_sedt_resource}
\title{Prepare a resource for upload to the Spatial Equity Data Tool API}
\usage{
prep_sedt_resource(
  resource,
  coords = c("lon", "lat"),
  file = NULL,
  fileext = "csv",
  placement = "surface",
  ...,
  arg = caller_arg(resource),
  call = caller_env()
)
}
\arguments{
\item{resource}{A URL, file path, sf or sfc object, or a data frame with
\code{coords} columns. See details for more information.}

\item{coords}{A length 2 character vector with coordinate column names in
lon/lat order. Should correspond to the \code{resource_lon_column} and
\code{resource_lat_column} values for \code{\link[=call_sedt_api]{call_sedt_api()}}.}

\item{file}{File path for output file. Optional. If \code{NULL}, file path is set
to a temporary directory.}

\item{placement}{Placement of point when converting non-point input geometry.
"surface" (default) or "centroid". Only used if resource is a file path for
a spatial data file or an \code{sf} object. See details for more information.}

\item{arg}{An argument name as a string. This argument
will be mentioned in error messages as the input that is at the
origin of a problem.}

\item{call}{The execution environment of a currently
running function, e.g. \code{caller_env()}. The function will be
mentioned in error messages as the source of the error. See the
\code{call} argument of \code{\link[rlang:abort]{abort()}} for more information.}
}
\value{
A path to a CSV or TSV file (depending on value of \code{fileext}).
}
\description{
\code{\link[=prep_sedt_resource]{prep_sedt_resource()}} is used by \code{\link[=call_sedt_api]{call_sedt_api()}} to convert an object or
URL to a valid file path before uploading the file to the Spatial Equity Data
Tool API.
}
\details{
Valid resource inputs

To call the Spatial Equity Data Tool API, \code{\link[=call_sedt_api]{call_sedt_api()}} requires a CSV or
TSV input file with coordinate attribute columns. \code{\link[=prep_sedt_resource]{prep_sedt_resource()}} also allows users to alternatively supply:
\itemize{
\item A URL for a delimited file with coordinate columns or a spatial data file
\item A ArcGIS FeatureLayer or Table URL (read with \code{\link[arcgislayers:arc_read]{arcgislayers::arc_read()}})
\item A file path for a spatial or tabular data file that can be read with \code{\link[sf:st_read]{sf::read_sf()}}
\item A data frame containing the named columns in \code{coords}
\item A \code{sf} or \code{sfc} object that can be converted to a data frame using \code{\link[sf:st_coordinates]{sf::st_coordinates()}}
}

If resource is an \code{sf} object or a path to a spatial data file with POLYGON,
MULTIPOLYGON, or any non-POINT geometry, \code{placement} is used to convert the
input into points. \code{\link[sf:geos_unary]{sf::st_centroid()}} (if \code{placement = "centroid"}) or
\code{\link[sf:geos_unary]{sf::st_point_on_surface()}} (if \code{placement = "surface"} which is the default)
option.
}
\keyword{internal}
