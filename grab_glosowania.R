# lista posiedzen Sejmu
# http://sejm.gov.pl/Sejm8.nsf/agent.xsp?symbol=posglos&NrKadencji=8
# stąd bierzemy urle do głosowań
# http://sejm.gov.pl/Sejm8.nsf/agent.xsp?symbol=listaglos&IdDnia=1745
# konkretne głosowanie
# http://sejm.gov.pl/Sejm8.nsf/agent.xsp?symbol=glosowania&NrKadencji=8&NrPosiedzenia=84&NrGlosowania=67
# stąd bierzemy listę klubów, dla każdego klubu:
# http://sejm.gov.pl/Sejm8.nsf/agent.xsp?symbol=klubglos&IdGlosowania=51679&KodKlubu=Kukiz15

library(tidyverse)
library(lubridate)
library(rvest)


wez_liste_posiedzen <- function() {

  miesiace <- tibble(miesiac = c("stycznia", "lutego", "marca", "kwietnia", "maja",
                                 "czerwca", "lipca", "sierpnia", "września",
                                 "października", "listopada", "grudnia"),
                     numer = 1:12)

  # weź listę posiedzeń sejmu
  url <- "http://sejm.gov.pl/Sejm8.nsf/agent.xsp?symbol=posglos&NrKadencji=8"

  page <- read_html(url)

  posiedzenia <- page %>%
    html_node("div#contentBody") %>%
    html_node("table") %>%
    html_nodes("a")

  return(tibble(link = html_attr(posiedzenia, "href"),
         data = html_text(posiedzenia)) %>%
    mutate(link = paste0("http://sejm.gov.pl/Sejm8.nsf/", link)) %>%
    mutate(data = str_replace_all(data, " r.", "")) %>%
    separate(col = data, into = c("d", "m", "y"), sep = " ") %>%
    mutate_at(c("d", "y"), as.numeric) %>%
    left_join(miesiace, by = c("m" = "miesiac")) %>%
    select(-m) %>%
    rename(m = numer) %>%
    mutate(data = make_date(y,m,d)) %>%
    select(data, link))

}

wez_liste_glosowan <- function(posiedzenie_url, data) {

  page <- read_html(posiedzenie_url)

  glosowania <- page %>%
    html_node("div#contentBody") %>%
    html_node("table") %>%
    html_nodes("td")

  glosowania_df <- tibble()

  for(i in seq(1, length(glosowania), 3)) {
    glosowania_df <- bind_rows(glosowania_df,
                               tibble(link = glosowania[i] %>%
                                        html_node("a") %>%
                                        html_attr("href") %>%
                                        paste0("http://sejm.gov.pl/Sejm8.nsf/", .),
                                      czas = glosowania[i+1] %>%
                                        html_text(),
                                      temat = glosowania[i+2] %>%
                                        html_text()) %>%
                                 mutate(godzina = as.numeric(str_sub(czas, 1, 2)))
    )
  }

  glosowania_df <- glosowania_df %>% mutate(data_posiedzenia = data)

  return(glosowania_df)
}

wez_liste_klubow <- function(glosowanie_url) {

  page <- read_html(glosowanie_url)

  kluby <- page %>%
    html_node("div#contentBody") %>%
    html_node("table") %>%
    html_node("tbody") %>%
    html_nodes("tr") %>%
    html_node("td") %>%
    html_node("a")

  return(tibble(
    klub = kluby %>% html_text(),
    link = kluby %>% html_attr("href") %>% paste0("http://sejm.gov.pl/Sejm8.nsf/", .)))
}

wez_glosowanie_klubu <- function(glosowanie_klubu_url) {

  page <- read_html(glosowanie_klubu_url)

  # wyniki konkretnego głosowania
  wyniki_glosowania <- page %>%
    html_node("div#contentBody") %>%
    html_node("table") %>%
    html_node("tbody") %>%
    html_nodes("td") %>%
    html_text() %>%
    str_squish()

  wyniki_glosowania_df <- tibble()

  for(i in seq(1, length(wyniki_glosowania), 3)) {
    wyniki_glosowania_df <- bind_rows(wyniki_glosowania_df,
                                      tibble(osoba = wyniki_glosowania[i+1],
                                             glos = wyniki_glosowania[i+2]))
  }

  wyniki_glosowania_df <- wyniki_glosowania_df %>%
    mutate(id_glosowania = as.numeric(str_match(glosowanie_klubu_url, "IdGlosowania=(\\d+)&KodKlubu=(.*)")[1,2]),
           klub = str_match(glosowanie_klubu_url, "IdGlosowania=(\\d+)&KodKlubu=(.*)")[1,3])

  return(wyniki_glosowania_df)

}




posiedzenia <- wez_liste_posiedzen()




lista_glosowan <- tibble()

for(i in 1:nrow(posiedzenia)) {

  cat(paste0("\r ", i, " / ", nrow(posiedzenia)))

  lista_glosowan <- bind_rows(lista_glosowan, wez_liste_glosowan(posiedzenia$link[i], posiedzenia$data[i]))

  Sys.sleep(runif(1, 2, 8)) # czekamy 2-8 sekund
}

cat("\n\nLista głosowań pobrana\n")

saveRDS(lista_glosowan, "lista_glosowan.rds")




glosowania_total <- tibble()

for(i in 1:nrow(lista_glosowan)) {

  cat(paste0("\r ", i, " / ", nrow(lista_glosowan)))

  glosowanie_kluby_url <- wez_liste_klubow(lista_glosowan$link[i])
  posiedzenie <- str_match(lista_glosowan$link[i], "NrPosiedzenia=(\\d+)&NrGlosowania=(\\d+)")
  data_posiedzenia <- lista_glosowan$data_posiedzenia[i]
  godzina_glosowania <- lista_glosowan$godzina[i]

  for(j in 1:nrow(glosowanie_kluby_url)) {

    glosy_klubu <- wez_glosowanie_klubu(glosowanie_kluby_url$link[j]) %>%
      mutate(data_posiedzenia = data_posiedzenia,
             godzina_glosowania = godzina_glosowania,
             numer_posiedzenia = posiedzenie[1, 2],
             numer_glosowania = posiedzenie[1, 3])

    glosowania_total <- bind_rows(glosowania_total, glosy_klubu)

    Sys.sleep(runif(1, 2, 8)) # czekamy 2-8 sekund
  }

  saveRDS(glosowania_total, "part_glosowania_total.RDS")
}

cat("\n\nLista głosów pobrana.\n")

saveRDS(glosowania_total, "glosowania_total.RDS")
