#' @importFrom glue glue
.warn_long_calculation <- function(cells_number){
  if (cells_number > 50000) {
    message(glue::glue("The number of cells (points) in the outcomes is large (`{cells_number}`). ",
                       "The calculation may take a while. To speed it up you can use larger `cell_size`."))
  }
}

#' @importFrom rlang quo_text enquo
#' @importFrom glue glue
.validate_sf <- function(x){
  var_name <- rlang::quo_text(rlang::enquo(x))

  if (!("sf" %in% class(x))) {
    stop(glue::glue("Variable `{var_name}` must be of class `sf`, currently is has classes: ",
                    glue::glue_collapse(class(x), sep = ", "), "."))
  }
}

#' @importFrom sf st_geometry_type
#' @importFrom glue glue
.validate_points <- function(points){

  if (!(all(unique(sf::st_geometry_type(points)) == "POINT"))) {
    stop(glue::glue("`Points` must be only of sfc `POINT`. Other types were found: ",
                    glue::glue_collapse(unique(sf::st_geometry_type(points)), sep = ", "), "."))
  }
}

#' @importFrom glue glue
#' @importFrom rlang quo_text enquo
#' @importFrom sf st_is_longlat
.validate_projected <- function(x){
  var_name <- rlang::quo_text(rlang::enquo(x))

  if (sf::st_is_longlat(x)) {
    stop(glue::glue("Variable `{var_name}` layer must be projected. Cannot calculate KDE on geographical coordinates."))
  }
}

#' @importFrom glue glue
#' @importFrom rlang quo_text enquo
#' @importFrom raster isLonLat crs
.validate_raster_projected <- function(x){
  var_name <- rlang::quo_text(rlang::enquo(x))

  if (raster::isLonLat(raster::crs(x))) {
    stop(glue::glue("Raster layer `{var_name}` must be projected. Cannot calculate KDE on geographical coordinates."))
  }
}

#' @importFrom glue glue
.validate_bandwidth <- function(band_width){

  if (!is.numeric(band_width)) {
    if (band_width <= 0) {
      stop(glue::glue("Band_width parameter must be numerical and higher than zero. ",
                      "Currently it is `{class(band_width)}` with value `{band_width}`."))
    }
  }
}

#' @importFrom glue glue
.validate_cellsize <- function(cell_size){

  if (!is.numeric(cell_size) | cell_size <= 0) {
    stop(glue::glue("Cell_size parameter must be numerical and higher than zero. ",
                    "Currently it is `{class(cell_size)}` with value `{cell_size}`."))
  }
}

#' @importFrom sf st_geometry_type
.is_polygon <- function(geometry){

  all(unique(sf::st_geometry_type(geometry)) %in% c("POLYGON", "MULTIPOLYGON"))
}

#' @importFrom glue glue
.validate_sideoffset <- function(side_offset){

  if (!is.numeric(side_offset) | side_offset < 0) {
    stop(glue::glue("Side_offset parameter must be numerical and higher than zero. ",
                    "Currently it is `{class(side_offset)}` with value `{side_offset}`."))
  }
}
