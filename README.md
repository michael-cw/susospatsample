
<!-- README.md is generated from README.Rmd. Please edit that file -->

<a href='https://docs.mysurvey.solutions/'><img src="man/img/susospatial.png" align="right" height="139"/></a>

# Survey Solutions Spatial Sampling Application

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

<div align="justify">

This repository contains the Survey Solutions Spatial Sampling
Application[^1]. It’s an essential tool for supporting survey sampling
and implementation using Spatial Resources along with Survey Solutions
[Computer Assisted Survey System
(CASS)](https://mysurvey.solutions/en/).

The Survey Solutions CASS leverages spatial resources in survey data
collection operations. These can include boundary files (specific to an
area) in the form of an ESRI shape file or background maps in the form
of Tile map packages (.tpk) or geo-tif files (.tif).

For a details on utilizing spatial resources in Survey Solutions, refer
to the following links:

- [Shapefile
  Maps](https://docs.mysurvey.solutions/interviewer/special/shape-file-overlay/)
- [Map
  Formats](https://docs.mysurvey.solutions/headquarters/mapsmanage/map-formats/)

The application’s modules, built on R’s Shiny framework, provide a
Graphical User Interface. Visualization of the sampling frame and sample
is carried out using the [deck.gl framework](https://deck.gl) through
the mapdeck package or by using leaflet maps.

Furthermore the application is part of a larger set of tools to support
the use of spatial resources in survey sampling and data collection.
Other currently available applications are:

- [Grid Sample Replacement
  Application](https://github.com/michael-cw/susogrdframe)
- [Desktop Listing
  Application](https://github.com/michael-cw/susolisting)

## Main User Features

- **Sampling Frame Flexibility**: Supports both regular grid and
  irregular clusters.
- **Population Data Integration**: Easily include or combine population
  data in raster or points format.
- **Supported Sampling Strategies**:
  - Random or Probability Proportional to Size sampling at the area
    level, stratified or unstratified.
  - Spatially balanced sampling at the points level.
- **Survey Resource Creation**:
  - Frame (csv format)
  - Sample with weights (csv format)
  - Shape file boundaries for Survey Solutions CAPI application
  - Base maps for Survey Solutions CAPI application
  - Sampling Report as word document
- **Storage Modes**:
  - Local Storage
  - PostGIS-PostgreSQL (installation not included in the package)
- **Available Map Services**:
  - Single Layer Geo Tiff
  - ESRI tile package creation (Requires [ESRI tile
    server](https://developers.arcgis.com/documentation/mapping-apis-and-services/data-hosting/services/image-tile-service/))

#### Technical Features

- **Deployment**: Operates on local installation, Shiny server and RS
  connect.
- **Modular Design**: Written in shiny modules for reusability, easy
  extension, and improved code readability.
- **High-Performance Capabilities**: Utilizes tools like:
  - [Data.table](https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html)
  - [Fst](http://www.fstpackage.org/)
  - [Fasterize](https://cran.r-project.org/web/packages/fasterize/vignettes/using-fasterize.html)
  - Multi-core computation

#### Admin Interface

- **Selection of Map Providers**:
  - ESRI World Imagery (no key)
  - Open Street Maps (no key)
  - [Mapbox (with key)](https://www.mapbox.com/)
  - ESRI map server (with key).
- **Data Storage Options**:
  - Postgis – Postgres (requires server installation)
  - local storage.
- **Survey Solutions Integration**:
  - Connect to Survey Solutions API
  - Upload base maps and boundary files
  - Assign/un-assign maps to field teams.

## Installation

The package is not released on CRAN yet, so installation has to be done
through installation from this repository.

#### Prerequisites

- [Install R, version 4.1.1 or
  greater](https://cran.r-project.org/mirrors.html)

- [Install R Studio, version 1.2.5001-3 or
  newer](https://posit.co/download/rstudio-desktop/)

- Make sure the *devtools* package is installed, if not install it with:

``` r
install.packages("devtools")
```

- After that install the actual package:

``` r
devtools::install_github("michael-cw/susospatsample")
```

In case R prompts you to update/install any addtional packages, please
confirm, ideally with update all. This may take a while, however these
packages are required to run the application. **In case you are asked to
install packages from source, please refuse, as this may take very long
in particular in windows environments.**

## Running the application interactively

There are two options to run the application. The first one is in
interactive mode. Start R Studio and run the following commands:

``` r
library(susospatsample)
## to use the leaflet map
susospatsample::runSpatSampleApp(mapwidget.option = "leaflet")

## OR to use the mapdeck map
susospatsample::runSpatSampleApp(mapwidget.option = "mapdeck")
```

## Running the application on a Shiny Server

The package also contains a shiny server function (open source or pro),
which you can run, i.e. in an Ubuntu Virtual Machine on your PC. To run
it on a shiny server, you need to create a server location (=
directory), i.e. susospatsample and then put an **app.R** script inside
of this directory. The script contains the following two lines:

``` r
library(susospatsample)
## to use the leaflet map
susospatsample::runSpatSampleAppServer(mapwidget.option = "leaflet")

## OR to use the mapdeck map
susospatsample::runSpatSampleAppServer(mapwidget.option = "mapdeck")
```

That’s all there is to do. Please make sure, you have read the [Spatial
Sampling Application
documentation](https://datanalytics.worldbank.org/SpatialSamplingManual/)
before moving on.

#### Attention - Potential issue with ‘shinyalert’

There’s a known issue with the ‘shinyalert’ package from Dean Attali,
which may cause the application to fail during start-up on certain
Windows installations running the latest version of R. For more details,
please check this issue.

This issue has been resolved in the development version of ‘shinyalert’,
but the fix is not yet available in the official CRAN release. If you
encounter this problem, please install the development version for
‘shinyalert’ using the following command:

``` r
devtools::install_github("daattali/shinyalert")
```

## Feature requests and bug reports

You can either use the standard GitHub approach by filing a bug
report/feature request
[here](https://github.com/michael-cw/SurveySolutionsAPI/issues) or you
use the Survey Solutions user forum
[here](https://forum.mysurvey.solutions/c/api/13).

Please continue to check for updates, as we are constantly working to
improve the application.

</div>

[^1]: Funding was received from the [Joint Development Data Center on
    Forced Displacement](https://www.jointdatacenter.org/) as well as
    the [World Bank’s Knowledge For Change Programm
    (KCP)](https://www.worldbank.org/en/programs/knowledge-for-change/brief/2022-knowledge-for-change-call-for-proposals-and-application-procedures).
