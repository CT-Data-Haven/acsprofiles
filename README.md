acsprofiles
================

Create profiles of ACS data for variety of geographies in R
-----------------------------------------------------------

**A far superior version** of this package is our [`cwi` package](https://github.com/CT-Data-Haven/cwi)

This R package was developed for our work in-house at [DataHaven](http://www.ctdatahaven.org/), including speeding up and making reproducible the process of generating profiles ([example](https://github.com/CT-Data-Haven/WebsiteIndicators/blob/master/NeighborhoodProfiles/2015_5Y_NH_neighborhood_Git.csv)). These functions are also useful for quickly getting neighborhood data for visualization or other projects. It includes functions to pull tables from the ACS API via the `acs` package, aggregate over custom geographies such as groups of tracts or towns, and arrange columns for output.

Data available in the package:

-   `nhv_neighborhoods`, a nested list of tracts & block groups for New Haven neighborhoods
-   `nhv_tracts`, a nested list of New Haven neighborhoods by tract only
-   `hfd_tracts` and `w_hfd_tracts`, nested lists of Hartford and West Hartford neighborhoods by tract only
-   `regions`, a list of towns for each region of Connecticut

These lists are saved here to standardize them between projects and avoid error from missing block groups, etc.

Depends on `acs`, and imports functions from `dplyr`, `purrr`, `stringr`, and `magrittr`.

**Note:** the `acs` package requires a Census API key, which you can [get from the Census Bureau](http://api.census.gov/data/key_signup.html). To save your key:

``` r
acs::api.key.install(key)
```

Also note that the Census APIs seem to crash pretty often.

To install this package:

``` r
devtools::install_github("CT-Data-Haven/acsprofiles")
```

[See the vignette](https://github.com/CT-Data-Haven/acsprofiles/blob/master/simple_profile_gh.md) for a common, simplified workflow using several of the functions here.
