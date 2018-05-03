# Introduction to statfitools
Janne Huovari  
`r Sys.Date()`  

# Aggregate municipality data

Load packages


```r
library(statfitools)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
library(forcats)
```

Get data from the Statistics Finland and clean variable names and time variable.


```r
dat_ku <- pxweb::get_pxweb_data(
    url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/tyokay/statfin_tyokay_pxt_001.px",
    dims = list(Alue = c('*'),
               "Pääasiallinen toiminta" = c('11'),
               Sukupuoli = c('S'),
               "Ikä" = c('SSS'),
               Vuosi = c('*')),
    clean = TRUE) %>% 
  clean_names() %>% 
  clean_times()
```

Aggregate data using classification "Keskuskuntaryhma" included in the package. 


```r
dat_keskuskunta <- dat_ku %>% 
  # Remove whole country group
  filter(Alue != "KOKO MAA") %>%
  # Join with codes. Codes are not included in the data.
  mutate(ku_code = sf_name2code(Alue, class = "kunta", year = 2016)) %>%  
  left_join(keskuskuntaryhma_key, by = c(ku_code = "Knro")) %>%
  # Aggregate
  group_by(Keskusryhma, time, Paaasiallinen_toiminta) %>% 
  summarise(values = sum(values, na.rm = TRUE)) %>% 
  ungroup()
```

```
## Warning: Column `ku_code`/`Knro` joining factors with different levels,
## coercing to character vector
```

Plot the data. Groups are ordered based on last value.


```r
ggplot(dat_keskuskunta, 
       aes(time, values/1000, 
           colour = fct_reorder2(Keskusryhma, time, values))) +
  geom_line(size = 1.5) +
  guides(colour = guide_legend("")) +
  ylab("1000 työllistä") + xlab("") +
  theme_light()
```

![](Introduction_files/figure-html/plot-1.png)<!-- -->

