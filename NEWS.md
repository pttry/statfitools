# statfitools 0.3.9

* `codes2names` have no argument for code and name suffix.

# statfitools 0.3.8

* `time2year` a function to convert yearly date time column to numeric year column.
* `open_statfi_qui` takes now db_list_name.


# statfitools 0.3.7

* Fix NA issue with `fp`.
* fp works with leading NAs now.
* New demetra_adj for seasonal adjustmet with the RJDemetra package. 
  Note Java ja Java jdk have to be installed. Can be used from `sa_series` and
  `trend_series` with method argument "tramoseats" and "x13"


# statfitools 0.3.6

* A new function `pp` to calculate a previous year's prices serie from current and 
  constant price series.

# statfitools 0.3.5

* Fix `clean_times` for quarterly and monthly data, again.
* A new argument rename_values to `clean_names` to rename last columns of data.frame as "values".
* `clean_times` now adds time column to previous position of year column.
* Fix vignette error due to change in Statistics Finland

# statfitools 0.3.4

* A new function `fp` to calculate a fixed prices serie from current and 
  previous year´s prices series.
* Added key-table `mk_pitka_key` for long name for maakunta ("Uudenmaan maakunta").
* Updated vignette for new StatFin links.
TODO: 
- Document data
- Function to use data?


# statfitools 0.3.3

* Fix `clean_times` for quarterly and monthly data.
* A new argument to `to_lower` for `make_names` and `clean_names` to turn 
  names lower cap.
* `seasonal_adj`, `sa_series` and `trend_series` for seasonal adjusment. 
   Wrappers for seasonal package.

# statfitools 0.3.2

* A new function `sf_get_reg_keytable()` to get regional classification keys.
* `sf_get_class()` returns code as character.
* keskuskunnat and keskuskuntaryhma_key datasets.

# statfitools 0.3.1

* `extract_name()` recognise now more complex codes and return factor for factors.
* `clean_times()` now works also for mounths and quarters
* `make_names()` return factor for factors.



# statfitools 0.3.0

* `sf_recode_ex_munic()` to recode abolished municipalities. 
  `sf_get_ex_munic()` download codes.
* `sf_get_class_key()` and `sf_get_reg_key()` to get classifications keys.


# statfitools 0.2.0

* `clean_times()` to covert time variables to numeric or dates
* `extract_name()` to extract name for code-name variable

# statfitools 0.1.0

* `sf_get_class()` to download statistical classifications from the Statistics Finland.
* `sf_test_class()` to test statistical classifications from the Statistics Finland.
* `extract_code()` to extract numerical code form a string.
* `make_names()` and `clean_names()` to make valid names.
* `sf_recode()`, `sf_name2code()` and `sf_code2name()` for recoding 
  classifications from the Statistics Finland
