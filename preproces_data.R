library(tidyverse)
library(lubridate)

lista_glosowan <- readRDS("lista_glosowan.rds")
glosowania <- readRDS("glosowania_total.RDS")

lista_glosowan2 <- lista_glosowan %>%
  mutate(g = str_match_all(link, ".*NrPosiedzenia=(\\d+)&NrGlosowania=(\\d++)")) %>%
  unnest_wider(g) %>%
  rename(link2 = ...1,
         numer_posiedzenia = ...2,
         numer_glosowania = ...3) %>%
  select(numer_posiedzenia, numer_glosowania, data_posiedzenia, czas, godzina, temat, link)

id_glosowan <- glosowania %>% distinct(id_glosowania, numer_posiedzenia, numer_glosowania)

lista_glosowan2 <- left_join(lista_glosowan2, id_glosowan,
          by = c("numer_posiedzenia" = "numer_posiedzenia",
                 "numer_glosowania" = "numer_glosowania"))

glosowania <- glosowania %>% select(id_glosowania, klub, osoba, glos)


saveRDS(glosowania, "final_glosowania.RDS")
saveRDS(lista_glosowan2, "final_lista_glosowan.RDS")
