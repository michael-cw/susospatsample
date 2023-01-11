
<!-- README.md is generated from README.Rmd. Please edit that file -->

# susospatsample

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

This is a repository for the Joint Development Center - Survey Solutions
Spatial Sampling Application to support survey sampling and
implementation when using Spatial Resources for the sampling Process
together with Survey Solutions Computer Assisted Survey System (CASS).

The Survey Solutions CASS allows to use spatial resources in the data
collection operations, these spatial resources can be boundary files (of
a particular area) in the form of an (ESRI) shape file, as well as
background maps in the form of either Tile map packages (.tpk) or
geo-tif files (.tif).

For further details on how to use spatial resources in Survey Solutions
please see
    here:

  - <https://docs.mysurvey.solutions/headquarters/svymanage/shapefile-maps/>

and
    here:

  - <https://docs.mysurvey.solutions/headquarters/svymanage/map-formats/>

All its modules come with a user interface and are based on R’s shiny
framework. For the visualization of the sample it uses the
<https://deck.gl> framework implemented through the mapdeck package.

## Installation

  - Install R: <https://cran.r-project.org/mirrors.html> (version 4.1.1
    or greater)

  - Install R Studio: <https://rstudio.com/products/rstudio/download/>
    (version 1.2.5001-3 or newer)

  - Make sure the *devtools* package is installed, if not install it
    with:

<!-- end list -->

``` r
install.packages("devtools")
```

  - After that install the actual package:

<!-- end list -->

``` r
devtools::install_github("michael-cw/susospatsample")
```

In case R prompts you to update/install any addtional packages, please
confirm, ideally with update all. This may take a while, however these
packages are required to run the application. **In case you are asked to
install packages from source, please refuse, as this may take very long
in particular in windows environments.**

## Start the application

Start R Studio and run the following commands:

``` r
library(susospatsample)
susospatsample::run_app()
```

## Help for runtime parameters

To learn about the individual run time parameters, please run the
following commands:

``` r
?susospatsample::run_app
```

This will give you all the information required. The most important
component is the mapbox key for the base map used in the application as
well as the Postgres Database address and credentials in case you are
using one.
