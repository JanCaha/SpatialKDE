context("Test KDE")
library(SpatialKDE)
library(sp)
library(sf)
library(raster)
library(dplyr)

data(meuse)

coordinates(meuse) <- ~x+y
proj4string(meuse) <- CRS("+init=epsg:28992")

test_data <- meuse %>%
  st_as_sf() %>%
  select()

test_data_not_projected <- test_data %>%
  st_transform(4326)

test_data_polygons <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE) %>%
  st_transform(3969)

test_grid <- create_grid_rectangular(test_data, cell_size = 100)

test_grid_not_projected <- test_grid %>% st_transform(4326)

test_raster <- create_raster(test_data, cell_size = 100)

suppressWarnings(test_raster_not_projected <- projectRaster(test_raster, crs = '+init=EPSG:4326'))

test_that("kde - wrong inputs", {
  expect_error(kde("a"), regexp = "Variable `points` must be of class `sf`, currently is has classes: character.")
  expect_error(kde(test_data_not_projected), regexp = "Variable `points` layer must be projected.")
  expect_error(kde(test_data_polygons), regexp = "Other types were found: MULTIPOLYGON.")
  expect_error(kde(test_data), regexp = "Both variables `grid` and `cellsize` are not specified.")
  expect_error(kde(test_data, cell_size = -1), regexp = "Currently it is `numeric` with value `-1`.")
  expect_error(kde(test_data, cell_size = 100, band_width = -5), regexp = "Currently it is `numeric` with value `-5`.")
  expect_warning(kde(test_data, cell_size = 100, band_width = 100, kernel = "wrong"),
                 regexp = "Unknown `kernel` used.")
})

test_that("kde - wrong inputs - grid input", {
  expect_error(kde(test_data, cell_size = 100, band_width = 100, kernel = "quartic",
                   grid = test_grid_not_projected),
               regexp = "Variable `grid` layer must be projected.")
})

test_that("kde - wrong inputs - raster input", {
  expect_error(kde(test_data, cell_size = 100, band_width = 100, kernel = "quartic",
                   grid = test_raster_not_projected),
               regexp = "Raster layer `grid` must be projected.")
})

test_that("results", {
  expect_s3_class(kde(test_data, cell_size = 100, band_width = 100, kernel = "quartic",
                      grid = test_grid),
                  "sf")

  expect_s4_class(kde(test_data, cell_size = 100, band_width = 100, kernel = "quartic",
                      grid = test_raster),
                  "RasterLayer")
})


