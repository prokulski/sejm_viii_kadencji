library(tidyverse)
library(lubridate)
library(widyr)
library(Rtsne)
library(dbscan)


lista_glosowan <- readRDS("final_lista_glosowan.RDS")
glosowania <- readRDS("final_glosowania.RDS")

100 * n_distinct(glosowania$id_glosowania) / nrow(lista_glosowan)


theme_set(theme_minimal())


lista_glosowan %>%
  mutate(data_posiedzenia = floor_date(data_posiedzenia, "weeks")) %>%
  count(data_posiedzenia) %>%
  ggplot() +
  geom_area(aes(data_posiedzenia, n), color = "blue", fill = "lightblue") +
  labs(title = "Liczba głosowań Sejmu VIII kadencji w kolejnych tygodniach",
       x = "Data posiedzenia", y = "Liczba głosowań w tygodniu",
       caption = "(c) Łukasz Prokulski, fb.com/DaneAnalizy")


lista_glosowan %>%
  count(godzina) %>%
  ggplot() +
  geom_col(aes(godzina, n), color = "blue", fill = "lightblue") +
  labs(title = "Liczba głosowań Sejmu VIII kadencji w zależności od godziny głosowania",
       x = "Godzina głosowania", y = "Liczba głosowań",
       caption = "(c) Łukasz Prokulski, fb.com/DaneAnalizy")




lista_glosowan %>%
  count(data_posiedzenia, godzina) %>%
  ggplot() +
  geom_point(aes(data_posiedzenia, godzina, size = n, color = n), alpha = 0.9) +
  scale_color_distiller(palette = "YlOrRd", direction = 1) +
  scale_y_reverse() +
  guides(size = "none") +
  theme(legend.position = "right") +
  labs(title = "Liczba głosowań Sejmu VIII kadencji w zależności od pory dnia",
       x = "Data posiedzenia", y = "Godzina głosowania",
       color = "Liczba\ngłosowań",
       caption = "(c) Łukasz Prokulski, fb.com/DaneAnalizy")



lista_glosowan %>%
  mutate(wday = wday(data_posiedzenia, week_start = 1, label = TRUE)) %>%
  count(wday, godzina) %>%
  ggplot() +
  geom_tile(aes(wday, godzina, fill = n), color = "gray30") +
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  scale_y_reverse() +
  theme(legend.position = "right") +
  labs(title = "Liczba głosowań Sejmu VIII kadencji w zależności od dnia tygodnia i pory dnia",
       x = "", y = "Godzina", fill = "Liczba\ngłosowań",
       caption = "(c) Łukasz Prokulski, fb.com/DaneAnalizy")




glosowania %>%
  count(klub, id_glosowania) %>%
  left_join(lista_glosowan %>% select(id_glosowania, data_posiedzenia),
            by = "id_glosowania") %>%
  mutate(data_posiedzenia = floor_date(data_posiedzenia, "week")) %>%
  group_by(klub, data_posiedzenia) %>%
  summarise(n=mean(n)) %>%
  ungroup() %>%
  mutate(klub = fct_reorder(klub, n)) %>%
  ggplot() +
  geom_point(aes(data_posiedzenia, klub, size = n)) +
  scale_size_continuous(range = c(0.5, 3)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Liczność klubów Sejmu VIII kadencji",
       x = "", y = "", size = "Liczba członków klubu",
       caption = "(c) Łukasz Prokulski, fb.com/DaneAnalizy")


# sanky diagram?
# https://www.r-graph-gallery.com/321-introduction-to-interactive-sankey-diagram-2.html


# czy tutaj trzeba grupować po zliczeniu?
glosowania %>%
  count(glos, klub) %>%
  group_by(klub) %>%
  mutate(p = 100*n/sum(n)) %>%
  ungroup() %>%
  filter(glos == "Nieobecny") %>%
  mutate(klub = fct_reorder(klub, p)) %>%
  ggplot() +
  geom_col(aes(klub, p), fill = "lightblue", color = "blue") +
  geom_text(aes(klub, p, label = sprintf("%.1f%%", p)), color = "gray20", hjust = 1, vjust = 0.5, size = 3) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Procent absencji na głosowaniach Sejmu VIII kadencji wg klubów",
       x = "Klub", y = "% nieobecnych posłów",
       caption = "(c) Łukasz Prokulski, fb.com/DaneAnalizy")


# czy tutaj trzeba grupować po zliczeniu?
# sprawdzenie: Szczypińska - około 30.6% nieobecności
# Zembala - 53.9% nieobecności
glosowania %>%
  count(osoba, glos) %>%
  group_by(osoba) %>%
  mutate(p = 100*n/sum(n)) %>%
  ungroup() %>%
  filter(glos == "Nieobecny") %>%
  top_n(30, p) %>%
  mutate(osoba = fct_reorder(osoba, p)) %>%
  ggplot() +
  geom_col(aes(osoba, p), fill = "lightblue", color = "blue") +
  geom_text(aes(osoba, p, label = sprintf("%.1f%%", p)), color = "gray20", hjust = 1, vjust = 0.5, size = 3) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Procent absencji na głosowaniach Sejmu VIII kadencji wg posłów",
       subtitle = sprintf("Dane szczątkowe (%d głosowań z %d ~= %.1f%%)", n_distinct(glosowania$id_glosowania), nrow(lista_glosowan), 100 * n_distinct(glosowania$id_glosowania) / nrow(lista_glosowan)),
       x = "", y = "% nieobecności",
       caption = "(c) Łukasz Prokulski, fb.com/DaneAnalizy")


# dodać frekwencję w poszczególnych miesiąch
wybrana_osoba <- "Kaczyński Jarosław"

glosowania %>%
  filter(osoba == wybrana_osoba) %>%
  distinct(id_glosowania, glos) %>%
  left_join(lista_glosowan %>% select(id_glosowania, data_posiedzenia),
            by = "id_glosowania") %>%
  mutate(miesiac = floor_date(data_posiedzenia, "week")) %>%
  count(glos, miesiac) %>%
  group_by(miesiac) %>%
  mutate(p = 100*n/sum(n)) %>%
  ungroup() %>%
  filter(glos == "Nieobecny") %>%
  ggplot() +
  geom_line(aes(miesiac, p)) +
  geom_point(aes(miesiac, p)) +
  labs(title = paste0(wybrana_osoba, ": procent absencji na głosowaniach Sejmu VIII kadencji w kolejnych tygodniach"),
       x = "", y = "% nieobecności",
       caption = "(c) Łukasz Prokulski, fb.com/DaneAnalizy")


glosowania %>%
  count(glos, id_glosowania) %>%
  left_join(lista_glosowan %>% select(id_glosowania, godzina),
            by = "id_glosowania") %>%
  group_by(godzina, glos) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  group_by(godzina) %>%
  mutate(p = 100*n/sum(n)) %>%
  ungroup() %>%
  filter(glos == "Nieobecny") %>%
  ggplot() +
  geom_col(aes(godzina, p), fill = "lightblue", color = "blue") +
  labs(title = "Absencja na głosowaniach Sejmu VIII kadencji wg godziny głosowania",
       x = "", y = "% nieobecności",
       caption = "(c) Łukasz Prokulski, fb.com/DaneAnalizy")



glosowania %>%
  filter(osoba == wybrana_osoba) %>%
  left_join(lista_glosowan %>% select(id_glosowania, godzina, data_posiedzenia),
            by = "id_glosowania") %>%
  mutate(ym = floor_date(data_posiedzenia, "1 month")) %>%
  count(ym, godzina, glos) %>%
  group_by(ym, godzina) %>%
  mutate(p = 100*n/sum(n)) %>%
  ungroup() %>%
  filter(glos == "Nieobecny") %>%
  ggplot() +
  geom_point(aes(ym, godzina, size = p, color = p), alpha = 0.9) +
  scale_color_distiller(palette = "YlOrRd", direction = 1) +
  scale_y_reverse() +
  guides(size = "none") +
  labs(title = paste0(wybrana_osoba, ": procent absencji na głosowaniach Sejmu VIII kadencji wg godziny głosowania"),
       x = "", y = "Godzina głosowania", color = "% nieobecności",
       caption = "(c) Łukasz Prokulski, fb.com/DaneAnalizy")






glosowania %>%
  select(id_glosowania, klub, osoba, glos) %>%
  left_join(glosowania %>%
              count(id_glosowania, klub, glos) %>%
              filter(glos != "Nieobecny") %>%
              group_by(id_glosowania, klub) %>%
              top_n(1, n) %>%
              ungroup() %>%
              select(-n) %>%
              rename(glos_klubu = glos),
            by = c("id_glosowania", "klub")
  ) %>%
  group_by(osoba) %>%
  mutate(n_glosowan = n()) %>%
  ungroup() %>%
  filter(glos != glos_klubu) %>%
  filter(glos != "Nieobecny") %>%
  filter(klub != "niez.") %>%
  count(klub, osoba, n_glosowan, sort = TRUE) %>%
  mutate(p_roznic = 100*n/n_glosowan) %>%
  group_by(klub) %>%
  top_n(3, p_roznic) %>%
  # filter(p_roznic > quantile(p_roznic, 0.95)) %>%
  ungroup() %>%
  mutate(osoba = fct_reorder(osoba, p_roznic)) %>%
  ggplot() +
  geom_col(aes(osoba, p_roznic, fill = klub), show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  facet_wrap(~klub, scales = "free_y") +
  labs(title = "Osoby, które głosowały inaczej niż klub",
       subtitle = sprintf("Dane szczątkowe (%d głosowań z %d ~= %.1f%%)", n_distinct(glosowania$id_glosowania), nrow(lista_glosowan), 100 * n_distinct(glosowania$id_glosowania) / nrow(lista_glosowan)),
       x = "", y = "% głosowań z odmiennym głosem",
       caption = "(c) Łukasz Prokulski, fb.com/DaneAnalizy")




# głosowania klubów i osób
glosowania_kluby <- glosowania %>%
  count(id_glosowania, klub, glos) %>%
  group_by(id_glosowania, klub) %>%
  mutate(p = 100*n/sum(n)) %>%
  filter(glos != "Nieobecny") %>%
  top_n(1, n) %>%
  mutate(rn = row_number()) %>%
  ungroup() %>%
  filter(rn == 1) %>%
  select(-rn)

klub_osoba <- glosowania %>% distinct(klub, osoba)

glosowania_osoba <- glosowania %>%
  filter(osoba == wybrana_osoba) %>%
  select(id_glosowania, glos_osoby=glos) %>%
  left_join(glosowania_kluby %>%
              filter(klub %in% (klub_osoba %>%
                                  filter(osoba == wybrana_osoba) %>%
                                  distinct(klub) %>%
                                  pull(klub))) %>%
              rename(glos_klubu = glos),
            by = "id_glosowania")




plot_data <- glosowania_osoba %>%
  count(klub, glos_osoby, glos_klubu) %>%
  mutate(p = 100*n/sum(n))

zgodnosc <- plot_data %>%
  mutate(same = glos_osoby == glos_klubu) %>%
  group_by(same) %>%
  summarise(p = sum(p)) %>%
  ungroup() %>%
  filter(same) %>%
  pull(p) %>%
  round(.,1)

plot_data %>%
  ggplot() +
  geom_tile(aes(glos_osoby, glos_klubu, fill = p), color = "gray", show.legend = FALSE) +
  geom_text(aes(glos_osoby, glos_klubu, label=sprintf("%.2f%%", p)), color = "black") +
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  labs(title = paste0(wybrana_osoba, ": zgodność z głosowaniem klubu: ", zgodnosc, "%"),
       x = paste0("Głos: ", wybrana_osoba), y = "Uśredniony głos Klubu",
       caption = "(c) Łukasz Prokulski, fb.com/DaneAnalizy")



# korelacja w głosowaniu między osobami i klubami
corr_plot_data_osoba <- glosowania %>%
  filter(glos != "Nieobecny") %>%
  count(osoba, glos) %>%
  pairwise_cor(osoba, glos, n, use = "complete.obs") %>%
  filter(item1 < item2)

corr_plot_data_klub <- glosowania %>%
  filter(glos != "Nieobecny") %>%
  count(id_glosowania, klub, glos) %>%
  pairwise_cor(klub, glos, n,  use = "complete.obs") %>%
  filter(item1 < item2)



corr_plot_data_klub %>%
  ggplot() +
  geom_tile(aes(item1, item2, fill = correlation), color = "black") +
  geom_text(aes(item1, item2, label = sprintf("%.2f", correlation)), color = "black") +
  scale_fill_gradient2(low = "skyblue3", mid = "yellow", high = "firebrick2",  midpoint = 0, limits = c(-1, 1)) +
  theme(legend.position = "bottom") +
  labs(title = "Podobieństwo klubów Sejmu VIII kadencji na podstawie głosowań",
       x = "", y = "", fill = "Współczynnik korelacji",
       caption = "(c) Łukasz Prokulski, fb.com/DaneAnalizy")




corr_plot_data_osoba %>%
  ggplot() +
  geom_point(aes(item1, item2, color = correlation)) +
  scale_color_gradient2(low = "skyblue3", mid = "yellow", high = "firebrick2",  midpoint = 0, limits = c(-1, 1)) +
  theme(legend.position = "bottom") +
  labs(title = "Podobieństwo klubów Sejmu VIII kadencji na podstawie głosowań",
       x = "", y = "", fill = "Współczynnik korelacji",
       caption = "(c) Łukasz Prokulski, fb.com/DaneAnalizy")



corr_plot_data_osoba %>%
  filter(item1 == wybrana_osoba) %>%
  left_join(klub_osoba, by = c("item2" = "osoba")) %>%
  rename(klub2 = klub) %>%
  filter(klub2 %in% (klub_osoba %>%
                       filter(osoba == wybrana_osoba) %>%
                       distinct(klub) %>%
                       pull(klub))) %>%
  select(item2, klub2, correlation) %>%
  filter(correlation <= 0.99) %>%
  arrange(correlation) %>%
  mutate(item2 = fct_inorder(item2)) %>%
  ggplot() +
  geom_col(aes(item2, correlation, fill = klub2)) +
  coord_flip() +
  labs(title = paste0(wybrana_osoba, ": podobieństwo z posłami z jej/jego klubu/ów\nw głosowaniach Sejmu VIII kadencji"),
       subtitle = "Bez posłów o podobieństwie (współczynniku korelacji) >= 0.99",
       x = "", y = "Współczynnik korelacji", fill = "Klub",
       caption = "(c) Łukasz Prokulski, fb.com/DaneAnalizy")




# t-SNE
set.seed(2019)
osoby_glosy <- glosowania %>% count(osoba, glos) %>% spread(glos, n, fill = 0)

tsne <- Rtsne(osoby_glosy[, 2:5], pca = FALSE, perplexity = 15, theta = 0.25)

tsne_df <- as_tibble(tsne$Y) %>%
  mutate(osoba = osoby_glosy$osoba) %>%
  left_join(klub_osoba, by = "osoba")

tsne_df %>%
  ggplot() +
  geom_point(aes(V1, V2, color = klub))


tsne_df$kmeans_cluster <- kmeans(tsne_df[,1:2], 8)$cluster

tsne_df %>%
  ggplot() +
  geom_point(aes(V1, V2, size = as.factor(kmeans_cluster), color = klub), alpha = 0.5)


tsne_df %>%
  count(klub, kmeans_cluster) %>%
  group_by(klub) %>%
  mutate(p = 100*n/sum(n)) %>%
  #  top_n(1, p) %>%
  ungroup() %>%
  ggplot() +
  geom_tile(aes(kmeans_cluster, klub, fill = p), color = "black") +
  geom_text(aes(kmeans_cluster, klub, label = sprintf("%d (%.1f%%)", n, p))) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1)



tsne_df$dbscan_cluster <- dbscan(tsne_df[,1:2], eps = 3, minPts = 2)$cluster

tsne_df %>%
  ggplot() +
  geom_point(aes(V1, V2, size = as.factor(dbscan_cluster), color = klub), alpha = 0.5)


tsne_df %>%
  count(klub, dbscan_cluster) %>%
  group_by(klub) %>%
  mutate(p = 100*n/sum(n)) %>%
  #  top_n(1, p) %>%
  ungroup() %>%
  ggplot() +
  geom_tile(aes(dbscan_cluster, klub, fill = p), color = "black") +
  geom_text(aes(dbscan_cluster, klub, label = sprintf("%d (%.1f%%)", n, p))) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1)



# przygotowanie tabeli korelacji pomiędzy dwoma osobami po kolejnych głosowaniach
para <- c("Majka Robert", "Rzepecki Łukasz")

glosowania_tmp <- glosowania %>%
  filter(glos != "Nieobecny") %>%
  filter(osoba %in% para)

nr_glosowan <- sort(unique(glosowania$id_glosowania))

corr_total <- tibble()
j <- 0
l <- length(nr_glosowan)


for(i in nr_glosowan) {

  j <- j + 1
  cat(paste0("\r", j, " / ", l))

  corr_osoba_temp <- glosowania_tmp %>% filter(id_glosowania <= i)

  if(nrow(corr_osoba_temp) > 0) {
    corr_osoba_temp <- corr_osoba_temp %>%
      count(osoba, glos) %>%
      pairwise_cor(osoba, glos, n, use = "complete.obs") %>%
      filter(item1 < item2) %>%
      mutate(id_glosowania = i)

    corr_total <- bind_rows(corr_total, corr_osoba_temp)
  }

}


corr_total %>%
  left_join(lista_glosowan %>%
              select(id_glosowania, data_posiedzenia),
            by = "id_glosowania") %>%
  group_by(data_posiedzenia) %>%
  summarise(m_correlation = mean(correlation, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(data_posiedzenia, m_correlation))

