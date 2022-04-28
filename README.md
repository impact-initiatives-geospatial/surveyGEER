
<!-- README.md is generated from README.Rmd. Please edit that file -->

# surveyGEER

<!-- badges: start -->
<!-- badges: end -->

Bring the remotely sense data straight into your survey data set

## Installation

You can install the development version of surveyGEER like so:

``` r
devtools::    
```

This package will hold templates and data-pipelines to extract remote
sensing data using the `rgee` and new `tidyrgee` packages. Templates
will be stored as vignettes in the package. I will provide one template
in this readme to serve as an example

## Hex Maps

This template will show you how to extract the data to make maps similar
to these hexagon maps below

**NDVI Standard Score Map**
<img src="man/figures/202203_som_ndvi.png" width="100%" style="display: block; margin: auto;" />

**Precipitaiton Record Low Map**
<img src="man/figures/20220420_som_record_low_rainfall_2021.png" width="100%" style="display: block; margin: auto;" />

Well below is the code to extract the data relevant to your context.
Once the data is extracted it can easily be mapped in any GIS software
(even here in R). If you like how the above examples look, feel free to
email <zack.arno.ext@impact-initiatives.org> for the QGIS templates

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

# ee_Initialize()
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:
