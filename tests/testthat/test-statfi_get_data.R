test_that("statfi_get_data work", {
  expect_named(
    statfi_get_data(
      "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/tyti/statfin_tyti_pxt_11pk.px/",
      list(Vuosi = c("2010", "2011"), Sukupuoli = c("SSS", "1", "2"
      ), Tiedot = "*"))
    , c("time", "sukupuoli", "tiedot", "values"))
})
