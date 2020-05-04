## ----init---------------------------------------------------------------------

library(statfitools)
library(dplyr)
library(ggplot2)
library(forcats)

## ----data---------------------------------------------------------------------

# 115b -- Väestö alueen, pääasiallisen toiminnan, sukupuolen, iän ja vuoden mukaan, 1987-2017


dat_ku <- pxweb::pxweb_get_data(
    url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/tyokay/statfin_tyokay_pxt_115b.px",
    query = list(Tiedot = c("vaesto"),
                 Alue = c('*'),
               "Pääasiallinen toiminta" = c('11'),
               Sukupuoli = c('SSS'),
               "Ikä" = c('SSS'),
               Vuosi = c('*'))) %>% 
  clean_names(rename_values = TRUE) %>% 
  clean_times()



## ----aggregate----------------------------------------------------------------

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
  



## ----plot, fig.width=6--------------------------------------------------------

ggplot(dat_keskuskunta, 
       aes(time, values/1000, 
           colour = fct_reorder2(Keskusryhma, time, values))) +
  geom_line(size = 1.5) +
  guides(colour = guide_legend("")) +
  ylab("1000 työllistä") + xlab("") +
  theme_light()


