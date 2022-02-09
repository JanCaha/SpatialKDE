test_that("kde - wrong inputs", {

  load_libraries()

  expect_error(kde("a"),
               regexp = "Variable `points` must be of class `sf`, currently is has classes: character.")

  expect_error(kde(test_data_not_projected()),
               regexp = "Variable `points` layer must be projected.")

  expect_error(kde(test_data_polygons()),
               regexp = "Other types were found: MULTIPOLYGON.")

  expect_error(kde(test_data()),
               regexp = "Both variables `grid` and `cellsize` are not specified.")

  expect_error(kde(test_data(), cell_size = -1),
               regexp = "Currently it is `numeric` with value `-1`.")

  expect_error(kde(test_data(), cell_size = 100, band_width = -5),
               regexp = "Currently it is `numeric` with value `-5`.")

  expect_error(kde(test_data(), cell_size = 100, band_width = 100, kernel = "wrong"),
                 regexp = "should be one of")
})

test_that("kde - wrong inputs - weights", {

  load_libraries()

  test_data <- test_data()

  expect_error(kde(test_data, cell_size = 100, band_width = 100, kernel = "quartic",
                   grid = test_grid, weights = c("test")),
               regexp = "All values of `weights` must be numerical and finite vector")

  expect_error(kde(test_data, cell_size = 100, band_width = 100, kernel = "quartic",
                   grid = test_grid, weights = c(5, 15, 30)),
               regexp = "All values of `weights` must be numerical and finite vector")
})

test_that("kde - wrong inputs - grid input", {

  load_libraries()

  expect_error(kde(test_data(), cell_size = 100, band_width = 100, kernel = "quartic",
                   grid = test_grid_not_projected()),
               regexp = "Variable `grid` layer must be projected.")
})

test_that("kde - wrong inputs - raster input", {

  load_libraries()

  expect_error(kde(test_data(), cell_size = 100, band_width = 100, kernel = "quartic",
                   grid = test_raster_not_projected()),
               regexp = "Raster layer `grid` must be projected.")
})

test_that("results", {

  load_libraries()

  options(SpatialKDE.suppres_message = TRUE)

  expect_message(kde(test_data(), cell_size = 100, band_width = 100, kernel = "quartic",
                     grid = test_grid() %>% sf::st_transform(crs = sf::st_crs(5514))),
                 "`points` are transformed into `grid` CRS to make coordinates match")

  expect_s3_class(kde(test_data(), cell_size = 100, band_width = 100, kernel = "quartic",
                      grid = test_grid()),
                  "sf")

  expect_s3_class(kde(test_data(), cell_size = 100, band_width = 100, kernel = "quartic",
                      grid = test_grid(), weights = rnorm(nrow(test_data()))),
                  "sf")

  expect_s4_class(suppressWarnings(
    kde(
      test_data(),
      cell_size = 100,
      band_width = 100,
      kernel = "quartic",
      grid = test_raster()
    )
  ),
  "RasterLayer")
})

test_that("weights", {

  load_libraries()

  options(SpatialKDE.suppres_message = TRUE)

  expect_s3_class(kde(test_data(), cell_size = 100, band_width = 100, kernel = "quartic",
                      weights = test_weights_double(), grid = test_grid()),
                  "sf")

  expect_s3_class(kde(test_data(), cell_size = 100, band_width = 100, kernel = "quartic",
                      weights = test_weights_integer(), grid = test_grid()),
                  "sf")

})
