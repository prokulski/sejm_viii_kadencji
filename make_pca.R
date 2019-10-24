library(tidyverse)
library(lubridate)


glosowania <- readRDS("final_glosowania.RDS")
lista_glosowan <- readRDS("final_lista_glosowan.RDS") %>% select(id_glosowania, data_posiedzenia, czas)


# przygotowanie tabeli z wartościami t-SNE dla wszystkich posłów po kolejnych głosowaniach
set.seed(2019)
nr_glosowan <- sort(unique(glosowania$id_glosowania))


j <- 0
l <- length(nr_glosowan)


for(i in nr_glosowan) {

  j <- j + 1
  cat(paste0("\r", j, " / ", l))

  df <- glosowania %>%
    filter(id_glosowania <= i) %>%
    count(klub, osoba, glos) %>%
    spread(glos, n, fill = 0)

  df <- bind_cols(df, prcomp(df[, 3:ncol(df)],
                             center = TRUE,
                             scale. = TRUE)$x %>%
                    as_tibble() %>%
                    set_names(c("V1", "V2", "V3", "V4"))) %>%
    select(klub, osoba, V1, V2)

  czas_glosowania <- lista_glosowan %>% filter(id_glosowania == i) %>% select(data_posiedzenia, czas)
  czas_glosowania <- paste0("Głosowanie ", czas_glosowania$data_posiedzenia, " @ ", czas_glosowania$czas)

  p <- df %>%
    ggplot() +
    geom_point(aes(V1, V2, color = klub)) +
    scale_x_continuous(limits = c(-6, 6)) +
    scale_y_continuous(limits = c(-6, 6)) +
    theme_minimal() +
    labs(title = czas_glosowania,
         x = "PCA 1", y = "PCA 2", color = "Klub")

  ggsave(filename =  sprintf("anim/%05d.png", j), plot = p,
         width = 12, height = 12, dpi = 90, units = "in", device = "png")
}

system('ffmpeg -r 25 -i anim/%05d.png -c:v libx264 -b:v 1500k -vf fps=25 -pix_fmt yuv420p glosowania_pca.mp4')
