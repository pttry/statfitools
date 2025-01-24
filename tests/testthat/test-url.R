test_that("statfi_parse_url works", {
  expect_equal(
    statfi_parse_url("https://statfin.stat.fi/PxWeb/pxweb/fi/StatFin/StatFin__muutl/statfin_muutl_pxt_119z.px/"),
    "https://pxdata.stat.fi/PXWeb/api/v1/fi/StatFin/muutl/statfin_muutl_pxt_119z.px")
})



test_that("statfi_parse_url_arch works", {
  expect_equal(
    statfi_parse_url_arch("https://statfin.stat.fi/PxWeb/pxweb/fi/StatFin_Passiivi/StatFin_Passiivi__kivih/statfinpas_kivih_pxt_001_201812_fi.px/"),
    "https://pxdata.stat.fi/PXWeb/api/v1/fi/StatFin_Passiivi/kivih/statfinpas_kivih_pxt_001_201812_fi.px")
})
