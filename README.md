<!-- README.md is generated from README.Rmd. Please edit that file -->
statfitools
===========

The `statfitools` is a collection of functions to help working with a data from [Statistics Finland](http://tilastokeskus.fi). I have writen functions for my own use, but I am happy if someone else finds functions useful. Some of the functions are spesific to the data from Statistics Finland, others have more general use.

To download the data from the Statistics Finland use [pxweb](https://github.com/rOpenGov/pxweb).

Installation
------------

To install the package:

``` r
install.packages("devtools")
devtools::install_github("jhuovari/statfitools")
```

Usage
-----

``` r
library("statfitools")
library("dplyr")
```

### Preprocess data

#### Make legal names

Try to make more readable valid names than `make.names()`

-   `make_names()` to pass a strings.
-   `clean_names()` to pass a data.frame or other objects with names to change.

``` r

# install.packages("pxweb")

dat <- pxweb::get_pxweb_data(
  url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/tyokay/010_tyokay_tau_101.px",
  dims = list(
    Alue = c('SSS'),
    "Pääasiallinen toiminta" = c('*'),
    Sukupuoli = c('S'),
    "Ikä" = c('SSS'),
    Vuosi = c('*')),
  clean = TRUE)

names(dat)
#> [1] "Alue"                   "Pääasiallinen toiminta"
#> [3] "Vuosi"                  "Ikä"                   
#> [5] "Sukupuoli"              "values"

dat <- clean_names(dat)

names(dat)
#> [1] "Alue"                   "Paaasiallinen_toiminta"
#> [3] "Vuosi"                  "Ika"                   
#> [5] "Sukupuoli"              "values"
```

#### Extract code or name from a code-name string

``` r

extract_code("508 Mantta-Vilppula")
#> [1] 508
extract_name("508 Mantta-Vilppula")
#> [1] "Mantta-Vilppula"
```

### Work with classifications

-   `sf_get_class()` to download statistical classifications from the Statistics Finland.
-   `sf_test_class()` to test statistical classifications from the Statistics Finland.
-   `sf_recode()`, `sf_name2code()` and `sf_code2name()` for recoding classifications

### Work with a statfi regional data

#### Clean regional names

Statistics Finland uses different formats to present regional names. Make them uniform.

TODO

#### Recode and aggregate regional data

Available municipality based regional classifications from Statistics Finland.

``` r

names(sf_get_reg_keytable(NULL))
#>  [1] "Knro"                  "Kunta"                
#>  [3] "Kommun"                "Mkkoodi"              
#>  [5] "Maakunta"              "Landskap"             
#>  [7] "Region"                "Avi_koodi"            
#>  [9] "AVI"                   "RFV"                  
#> [11] "AVI.1"                 "Ely_koodi"            
#> [13] "ELY_keskus"            "ELY_central"          
#> [15] "ELY_Centre"            "Seutukuntakoodi"      
#> [17] "Seutukunta"            "Ekonomisk_region"     
#> [19] "Suuraluekoodi"         "Suuralue"             
#> [21] "Storomrade"            "Major_region"         
#> [23] "Kuntaryhmakoodi"       "Kuntaryhma"           
#> [25] "Kommungrup"            "Municipal_group"      
#> [27] "Kielisuhdekoodi"       "Kielisuhde"           
#> [29] "Spraklig_indelning"    "Language_distribution"
```

Aggregate to Tilastollinen kuntaryhmitys.

Download classification key and data. Join and aggregate

``` r

# Classification key
key_kuntar <- sf_get_reg_keytable("Kuntaryhmä")

# Data
dat_ku <- pxweb::get_pxweb_data(
  url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/tyokay/010_tyokay_tau_101.px",
  dims = list(
    Alue = c('*'),
    "Pääasiallinen toiminta" = c('11'),  # Työlliset
    Sukupuoli = c('S'),
    "Ikä" = c('SSS'),
    Vuosi = c('*')),
  clean = TRUE) %>% 
  clean_names() %>% 
  clean_times()

# Join and aggregate
dat_kuntar <- dat_ku %>% 
  # safer to use codes
  mutate(ku_code = sf_name2code(Alue, class = "kunta", year = 2016)) %>%   
  left_join(key_kuntar, by = c(ku_code = "Knro")) %>% 
  group_by(Kuntaryhma, time, Paaasiallinen_toiminta) %>% 
  summarise(values = sum(values, na.rm = TRUE)) %>% 
  ungroup()
#> Warning in left_join_impl(x, y, by$x, by$y, suffix$x, suffix$y): joining
#> factors with different levels, coercing to character vector
```
