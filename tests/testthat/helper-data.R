load_libraries <- function(){

  libs <- c("sf", "sp", "raster", "dplyr")

  for (lib in libs){
    suppressMessages(
      library(lib, character.only = TRUE)
    )
  }
}

test_data <- function(){
  load(system.file("data", "meuse.rda", package = "sp"))

  sp::coordinates(meuse) <- ~x+y

  test_data <- meuse %>%
    sf::st_as_sf() %>%
    sf::st_set_crs(28992)

  test_data %>%
    dplyr::select()
}

test_data_not_projected <- function(){
  test_data() %>%
    sf::st_transform(4326)
}

test_data_polygons <- function(){
  sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE) %>%
    sf::st_transform(3969)
}


test_grid <- function(){
  create_grid_rectangular(test_data(), cell_size = 100)
}

test_grid_not_projected <- function(){
  test_grid() %>%
    sf::st_transform(4326)
}

test_raster <- function(){
  create_raster(test_data(), cell_size = 100)
}

test_raster_not_projected <- function(){
  suppressWarnings(
    test_raster_not_projected <- raster::projectRaster(test_raster(),
                                                       crs = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
  )

  test_raster_not_projected
}
