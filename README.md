# SpatialKDE

<!-- badges: start -->
![R-CMD-check](https://github.com/JanCaha/SpatialKDE/workflows/R-CMD-check/badge.svg)
![Windows Release](https://github.com/JanCaha/SpatialKDE/workflows/Build%20Windows%20Binaries%20and%20Create%20Release/badge.svg)
![Web deploy](https://github.com/JanCaha/SpatialKDE/workflows/Pkgdown%20-%20build%20and%20deploy%20website/badge.svg)
![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)
<!-- badges: end -->

R package to calculate spatial KDE. Inspired by the tool Heatmap tool from QGIS. Help for Heatmap tool can be found [here](https://docs.qgis.org/2.18/en/docs/user_manual/plugins/plugins_heatmap.html), the help is for older version of the tool, but the window of the tool looks relatively the same. 

## Documentation

[Available here.](https://jancaha.github.io/SpatialKDE/)

## Instalation 

If you have RTools (Windows), or any C++ compiler (Linux, macOS) installed then you can use:

``` r
remotes::install_github("JanCaha/SpatialKDE")
```

### Compiled Window binaries

For Windows the binaries are available from [release page](https://github.com/JanCaha/SpatialKDE/releases/).

The instalation can be done from R using command, where you just replace __x.x.x__ with the current version (e.g. __0.1.0__):

``` r
install.packages('SpatialKDE_x.x.x.zip', repos = NULL, type = "win.binary")
```
