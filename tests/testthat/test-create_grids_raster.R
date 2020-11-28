test_that("create grid - correct", {

  load_libraries()

  expect_s3_class(create_grid_rectangular(test_data(), cell_size = 100),
                  "sf")

  expect_s3_class(create_grid_hexagonal(test_data(), cell_size = 100),
                  "sf")
})

test_that("create grid - wrong inputs", {

  load_libraries()

  expect_error(create_grid_rectangular("a"),
               regex = "`geometry` must be of class `sf`.")

  expect_error(create_grid_rectangular(test_data(), cell_size = "-1"),
               regex = "Currently it is `character` with value `-1`.")

  expect_error(create_grid_rectangular(test_data(), cell_size = -10),
               regex = "Currently it is `numeric` with value `-10`.")

  expect_error(create_grid_rectangular(test_data(), cell_size = 1, side_offset = "-1"),
               regex = "Currently it is `character` with value `-1`.")

  expect_error(create_grid_rectangular(test_data(), cell_size = 1, side_offset = -1),
               regex =  "Currently it is `numeric` with value `-1`.")

  expect_error(create_grid_rectangular(test_data(), cell_size = 1, side_offset = 1, only_inside = "a"),
               regex =  "Currently it is of type: `character`.")
})

test_that("create raster - correct", {

  load_libraries()

  expect_s4_class(suppressWarnings(
    create_raster(
      test_data(),
      cell_size = 100)
  ),
  "RasterLayer")
})

test_that("create raster - wrong inputs", {

  load_libraries()

  expect_error(create_raster("a"),
               regex = "`geometry` must be of class `sf`.")

  expect_error(create_raster(test_data(), cell_size = "-1"),
               regex = "Currently it is `character` with value `-1`.")

  expect_error(create_raster(test_data(), cell_size = -10),
               regex = "Currently it is `numeric` with value `-10`.")

  expect_error(create_raster(test_data(), cell_size = 1, side_offset = "-1"),
               regex = "Currently it is `character` with value `-1`.")

  expect_error(create_raster(test_data(), cell_size = 1, side_offset = -1),
               regex =  "Currently it is `numeric` with value `-1`.")
})
