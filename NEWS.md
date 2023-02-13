# SpatialKDE 0.8.2

* allow supression of progress bar 

# SpatialKDE 0.8.0

* add progress bar to the calculation for better overview of calculation progress

* internal rewrite from using **Rcpp** package to **cpp11**, this change should be invisble to user, only relates to needed dependencies

# SpatialKDE 0.7.0

* global option `options(SpatialKDE.suppres_message = TRUE)` can be used to suppress using centroids message from `kde()`

# SpatialKDE 0.6.2

* fix problem with integer `weights`

* reworked tests

# SpatialKDE 0.6.0

* added `weights` parameter to `kde()`


# SpatialKDE 0.4.0

* rename of `create_raster_rectangular()` to `create_grid_rectangular()` and `create_raster_hexagonal()` to `create_grid_hexagonal()` to avoid confusion about the type of outcome.

* fix of inner workings of `create_grid_rectangular()` and `create_grid_hexagonal()`

* fix usage of kernell in function `kde()`

* Added a `NEWS.md` file to track changes to the package.

# SpatialKDE 0.3.2

* Website via pkgdown
