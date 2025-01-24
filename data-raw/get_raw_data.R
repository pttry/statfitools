

public_debt <- pxweb::get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/jul/jali/010_jali_tau_101.px",
                                    dims = list("Alijäämä/velka" = c('*'),
                                                Sektori = c('*'),
                                                Arvo = c('*'),
                                                Vuosi = c('*')),
                                    clean = TRUE)

names(public_debt)[1] <- "Alijäämä/velka"

usethis::use_data(public_debt, overwrite = TRUE)

output_ind <-
  get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/kan/ktkk/110_ktkk_tau_101.px",
                 dims = list(Toimiala = c('0'),
                             Tiedot = c('IND100'),
                             Sarjatyyppi = c('*'),
                             Vuosi = c('*'),
                             Kuukausi = c('*')),
                 clean = TRUE)

devtools::use_data(output_ind, overwrite = TRUE)


# Keskuskunnat

keskuskunnat <- readr::read_tsv("data-raw/Keskuskunnat.txt", col_types = readr::cols(
  Kunta = readr::col_character(),
  Keskusryhma = readr::col_character()
))

usethis::use_data(keskuskunnat, overwrite = TRUE)

library(dplyr)

key_kuntar$Kuntaryhma <- as.character(key_kuntar$Kuntaryhma)
key_kuntar[match(keskuskunnat$Kunta, key_kuntar$Kunta), "Kuntaryhma"] <- keskuskunnat$Keskusryhma

keskuskuntaryhma_key <- key_kuntar %>%
  select(Knro, Kunta, Keskusryhma = Kuntaryhma)

usethis::use_data(keskuskuntaryhma_key, overwrite = TRUE)

write.csv2(keskuskuntaryhma_key, file = "data-raw/Keskuskuntaryhma_key.csv")


# Maakunnat pitka
#  esim Uudenmaan maakunta

mk_pitka_key <- readr::read_csv2("data-raw/Maakunnat_pitka.csv",
  col_types = "ccc")

usethis::use_data(mk_pitka_key, overwrite = TRUE)
