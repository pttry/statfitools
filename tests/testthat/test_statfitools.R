library("statfitools")

context("extract")

test_that("extract_code returns numeric",{
  expect_equal(extract_code("508 Mantta-Vilppula"), 508)
  expect_equal(extract_code(factor("00508 Mantta-Vilppula"),
                            numbers_as_numeric = FALSE), factor("00508"))
  expect_equal(extract_code("B1GMHT/HI00 Bruttokansantuote, hintaindeksi 2010=100"),
               "B1GMHT/HI00")
  expect_equal(extract_code("G01  Yleinen julkishallinto"), "G01")
  expect_is(extract_code(factor("G01  Yleinen julkishallinto")), "factor")
})

test_that("extract_name returns name",{
  expect_equal(extract_name("508 Mantta-Vilppula"), "Mantta-Vilppula")
  expect_equal(extract_name("S1311 Valtionhallinto ee"), "Valtionhallinto ee")
  expect_equal(extract_name("Valtionhallinto"), "Valtionhallinto")
  expect_equal(extract_name("G01  Yleinen julkishallinto"), "Yleinen julkishallinto")
  expect_equal(extract_name("B1GMHT/HI00 Bruttokansantuote, hintaindeksi 2010=100"),
               "Bruttokansantuote, hintaindeksi 2010=100")
  expect_is(extract_name(factor("Valtionhallinto")), "factor")
})


# context("get_class")
#
#
# test_that("get_class functions return data.frame",{
#   expect_is(sf_get_class("kuntaryhmitys", 2014), "data.frame")
#   expect_is(sf_get_reg_keytable("Maakunta"), "data.frame")
#   expect_is(sf_get_class("maakunta", 2013), "data.frame")
# })

context("clean")



test_that("clean_times return right",{
  expect_is(clean_times(output_ind, time_format = "date")$time, "Date")
  expect_is(clean_times(public_debt)$time, "numeric")
  expect_equal(min(clean_times(public_debt)$time), 1975)
  expect_is(
    clean_times(employment_q, sub_year_col = "Ajanjakso", agg_time = "Vuosikeskiarvo")$time,
    "Date")
  expect_false(
    any(is.na(clean_times(employment_q, sub_year_col = "Ajanjakso", agg_time = "Vuosikeskiarvo")$time))
    )
})

# Link does not work anymore
# test_that("clean_times dates difference a month",{
#   # test data
#   dat_palkkasumma0 <-
#     pxweb::get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/pal/ktps/statfin_ktps_pxt_111m.px",
#                           dims = list(Vuosi = c('2016', '2017'),
#                                       Ajanjakso = c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12'),
#                                       Toimiala = c('02385'),
#                                       Sarja = c('Trendi', 'Kausitasoitettu')),
#                           clean = TRUE)
#
#   expect_true(all(diff(
#     filter(clean_times(dat_palkkasumma0,
#                        sub_year_col = "Ajanjakso"),
#            Sarja == "Trendisarja")$time) %in% c(28, 29, 30, 31))
#     )
# })
