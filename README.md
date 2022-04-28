
<!-- README.md is generated from README.Rmd. Please edit that file -->

# surveyGEER

<!-- badges: start -->
<!-- badges: end -->

This project/package contains ready to go code templates utilizing
`rgee` and `tidyrgee` to extract remote sensing data for statistical
analysis and mapping. `rgee` is the R API for Google Earth Engine
(GEE)and includes all of the GEE functionality available in the code
editor with a syntax that parallels both the JavaScript and the Python
GEE APIs. `tidyrgee` is a package currently under rapid development that
wraps the `rgee` package to create a syntax parallel to the
`tidyverse`/`dplyr`. The goal of `tidyrgee` is to simplify alot of the
more complicated currently necessary under the JavaScript and Python
paradigms.

SurveyGEER will provide templates using `tidyrgee`, and eventually even
wrap into even larger functions specifically designed for REACH purpose.
The `tidyrgee` syntax should make remote sensing with GEE in R simple
for any r/tidyverse user, but we will aim for larger specific wrappers
where outputs can be produced from one function so that even complete
novices can produce these standardized analysis outputs.

## Installation

You can install the development version of surveyGEER like so:

``` r
devtools::install_github("")
```

This package will hold templates and data-pipelines to extract remote
sensing data using the `rgee` and new `tidyrgee` packages. Templates
will be stored as vignettes in the package. I will provide one template
in this readme to serve as an example for new users just visiting the
page for the first time:

## Hex Maps

This annotated code below will show you how to extract the data to make
maps similar to these hexagon maps below

**NDVI Standard Score Map**
<img src="man/figures/202203_som_ndvi.png" width="100%" style="display: block; margin: auto;" />

**Precipitaiton Record Low Map**
<img src="man/figures/20220420_som_record_low_rainfall_2021.png" width="100%" style="display: block; margin: auto;" />

Below is the code to extract the data relevant to your context. Once the
data is extracted it can easily be mapped in any GIS software (even here
in R). If you like how the above examples look, feel free to email
<zack.arno.ext@impact-initiatives.org> for the QGIS templates. The
output below can be put directly into the QGIS templates.

``` r
local_aoi_available <- F
local_grid_available <- F
aoi_gdb_path<-""
grid_gdb_path <- ""
aoi_layer_name <- ""
grid_layer_name<-""



if(local_grid_available){
  grid <- st_read(grid_gdb_path,aoi_layer)
}
if(local_aoi_available){
    aoi <- st_read(grid_gdb_path,aoi_layer)
}
```

``` r
library(surveyGEER)
library(rgee)
library(tidyrgee)
#> 
#> Attaching package: 'tidyrgee'
#> The following object is masked from 'package:rgee':
#> 
#>     ee_extract
#> The following object is masked from 'package:stats':
#> 
#>     filter

ee_Initialize()
#> -- rgee 1.1.2.9000 ---------------------------------- earthengine-api 0.1.295 -- 
#>  v user: not_defined
#>  v Initializing Google Earth Engine: v Initializing Google Earth Engine:  DONE!
#>  v Earth Engine account: users/zackarno 
#> --------------------------------------------------------------------------------
```
