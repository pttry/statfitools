
# Employment from regional employment
library(dplyr)

file1 <- "C:/Users/janne.huovari/OneDrive - Pellervon Taloustutkimus PTT ry/Rwork/Aluetalous/data/Tyo_Q_1.rds"

dat1_0 <- readRDS(file1)

employment_q <- dat1_0 %>%
  filter(Tiedot == "Työlliset",
         Sukupuoli == "Sukupuolet yhteensä",
         Maakunta == "Koko maa",
         Ika == "15-64") %>%
  select(Vuosi, Ajanjakso, Tiedot, data) %>%
  mutate(Tiedot = "employment")

use_data(employment_q, overwrite = TRUE)

