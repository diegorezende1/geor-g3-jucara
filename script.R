##Diego - 31/10/2021
#'Espécies escolhidas 
#'Eciton burchelli > CTmáx= 40º
#'Solenopsis invicta > CTmáx= 45.3°C
#'Ectatomma brunneum > CTmáx= 40º


#install packages (Bianca-01/11/2021)
install.packages("tidyverse")
install.packages("here")
install.packages("tidyr")
install.packages("dplyr")
install.packages("dismo")
install.packages("maptools")
install.packages("rgdal")
install.packages("raster")
install.packages("sp")



#library packages (Bianca-01/11/2021)
library(tidyverse)
library(here)
library(tidyr)
library(dplyr)
library("sp")
library("raster")
library("maptools")
library("rgdal")
library("dismo")
library(tmap)


# conferir diretório
here::here()

# criar um arquivo .here
here::set_here()


#verificar diretório (Bianca-01/11/2021)
getwd()

#definir diretório de trabalho (Bianca-01/11/2021)
# setwd("C:/github/geor-g3-jucara/Atlantic_Ants-main/DATASET")

#importar tabela, extraí o zip da pasta DATASET pra ter o txt. (Bianca-01/11/2021)
#read.table("ATLANTIC_ANTS_dataset.txt", header = TRUE)

# importar tabela e ler com tidyverse (Bianca-01/11/2021)
ants <- readr::read_tsv(here::here("Atlantic_Ants-main", "DATASET", "ATLANTIC_ANTS_dataset.txt"))
ants

spec(ants)

#selecionar colunas (Bianca-01/11/2021)

ants_select <- ants %>%
dplyr::select(Latitude.y, Longitude.x, Subfamily, Genus, Species, Start.year, Habitat.Type)
ants_select

#Selecionar os gêneros (Bianca-01/11/2021)
ants_genus_select <- ants_select %>%
dplyr::filter(Genus %in% c("Solenopsis", "Ectatomma", "Eciton"))
ants_genus_select

#Ordenar por ordem alfabética os gêneros (Bianca-01/11/2021)
ants_arrange <- ants_genus_select %>%
dplyr::arrange(Genus)

#Exportar tabela com gêneros selecionados em ordem (Bianca-01/11/2021)
write.table(ants_arrange, "AANTS_genus_selected.txt", 
            row.names = FALSE, col.names = TRUE, quote = FALSE)

#importar tabela com os gêneros selecionados (Diego-02/11/2021)

ants_genus_select  <- readr::read_delim(here::here("Atlantic_Ants-main", "DATASET", 
                                                 "AANTS_genus_selected.txt"))
spec(ants_genus_select)
view(ants_genus_select)
ncol(ants_genus_select)
nrow(ants_genus_select)
summary(ants_genus_select)
str(ants_genus_select)


#Selecionar colunas de interesses e montando os data.frame (Diego-02/11/2021)

ants_genus_select <- ants_genus_select %>% 
       dplyr::select(Genus, Species, Latitude.y, Longitude.x)
view(ants_genus_select)

#unir as colunas Genus e Speceies (Diego-02/11/2021)

ants_genus_select  <- tidyr::unite(data = ants_genus_select, 
                                   col = "sp",
                                   Genus:Species, 
                                   sep = " ",
                                   remove = FALSE)
view(ants_genus_select )

#Selecionar as espécies (Diego-03/11/2021)
ants_sp_select <- ants_genus_select %>%
  dplyr::filter(sp %in% c("Eciton burchellii", "Solenopsis invicta", "Ectatomma brunneum"))
ants_sp_select

view(ants_sp_select)

#criar um objeto sf (Diego-03/11/2021)


# criar geometria a partir dos dados de latlong (Luan 03-11)

ants_sp_sf <- ants_sp_select %>% sf::st_as_sf(coords = c("Longitude.x", "Latitude.y"), crs = 4326)
ants_sp_sf

plot(ants_sp_sf$geometry, pch = 20)

#=======


#Baixando os dados de temperatura do worldclim (Maria Alice-02/11/2021)


download.file(url = "https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_5m_tmax.zip",
              destfile = "worldclim/wc2.1_5m_tmax.zip", mode = "wb")

download.file(url = "https://biogeo.ucdavis.edu/data/worldclim/v2.1/fut/5m/wc2.1_5m_tmax_CanESM5_ssp245_2081-2100.zip",
              destfile = "worldclim/wc2.1_5m_tmax_CanESM5_ssp245_2081-2100.zip", mode = "wb")

download.file(url = "https://biogeo.ucdavis.edu/data/worldclim/v2.1/fut/5m/wc2.1_5m_tmax_CanESM5_ssp585_2081-2100.zip",
              destfile = "worldcim/wc2.1_5m_tmax_CanESM5_ssp585_2081-2100.zip", mode = "wb")

download.file(url = "https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_5m_elev.zip",
              destfile = "wc2.1_5m_elev.zip", mode = "wb")

unzip("worldclim/wc2.1_5m_tmax.zip", exdir = "worldclim")
unzip("worldclim/wc2.1_5m_tmax_CanESM5_ssp245_2081-2100.zip", exdir = "worldclim")
unzip("worldclim/wc2.1_5m_tmax_CanESM5_ssp585_2081-2100.zip", exdir = "worldclim")
unzip("worldclim/wc2.1_5m_elev.zip", exdir = "worldclim")

# import ------------------------------------------------------------------

# raster
# Presente
tmax_presente <- dir(path = here::here("worldclim"), pattern = "tmax", full.names = TRUE) %>% 
  grep(".tif", ., value = TRUE) %>% 
  raster::stack()
tmax_presente_mean <- tmax_presente %>% mean()
tmax_presente_mean
tmax_presente_max <- tmax_presente %>% max()
tmax_presente_max

elev <- raster::raster("worldclim/wc2.1_5m_elev.tif")
elev

# Previsao 1 (2100 cenario de acao)
tmax_fut_1 <- raster::stack(here::here("worldclim", "wc2.1_5m_tmax_CanESM5_ssp245_2081-2100.tif"))
tmax_fut_1_mean <- tmax_fut_1 %>% 
  mean()
tmax_fut_1_mean
tmax_fut_1_max <- tmax_fut_1 %>% 
  max()


# Previsao 2 (2100 cenario de continuar aquecendo)
tmax_fut_2 <- raster::stack(here::here("worldclim", "wc2.1_5m_tmax_CanESM5_ssp585_2081-2100.tif"))
tmax_fut_2_mean <- tmax_fut_2 %>% 
  mean()
tmax_fut_2_mean
tmax_fut_2_max <- tmax_fut_2 %>% 
  max()
tmax_fut_2_max

# extrair -----------------------------------------------------------------

# resumo
ants_sp_sf_values <- ants_sp_sf %>% 
  dplyr::mutate(tmax_presente_mean = raster::extract(tmax_presente_mean, .),
                tmax_fut_1_mean = raster::extract(tmax_fut_1_mean, .),
                tmax_fut_2_mean = raster::extract(tmax_fut_2_mean, .),
                tmax_presente_max = raster::extract(tmax_presente_max, .),
                tmax_fut_1_max = raster::extract(tmax_fut_1_max, .),
                tmax_fut_2_max = raster::extract(tmax_fut_2_max, .),
                elev = raster::extract(elev, .)) %>% 
  na.omit()
ants_sp_sf_values

glimpse(ants_sp_sf_values)

# Media de temperatura maxima ao longo dos meses
ants_sp_sf_values <- ants_sp_sf %>% 
  dplyr::mutate(tmax_presente_mean = raster::extract(tmax_presente_mean, .))
ants_sp_sf_values

ants_sp_sf_values <- ants_sp_sf_values %>% 
  dplyr::mutate(tmax_fut_1_mean = raster::extract(tmax_fut_1_mean, .))
ants_sp_sf_values

ants_sp_sf_values <- ants_sp_sf_values %>% 
  dplyr::mutate(tmax_fut_2_mean = raster::extract(tmax_fut_2_mean, .))
ants_sp_sf_values

ants_sp_sf_values <- ants_sp_sf_values %>% 
  dplyr::mutate(elev = raster::extract(elev, .))
ants_sp_sf_values

# Maxima de temperatura maxima no ano todo
ants_sp_sf_values <- ants_sp_sf_values %>% 
  dplyr::mutate(tmax_presente_max = raster::extract(tmax_presente_max, .))
ants_sp_sf_values

ants_sp_sf_values <- ants_sp_sf_values %>% 
  dplyr::mutate(tmax_fut_1_max = raster::extract(tmax_fut_1_max, .))
ants_sp_sf_values

ants_sp_sf_values <- ants_sp_sf_values %>% 
  dplyr::mutate(tmax_fut_2_max = raster::extract(tmax_fut_2_max, .))
ants_sp_sf_values


# mapas -------------------------------------------------------------------


# Visualizar minimo e maximo (da temp max media ou max total) 
# para cada spp em um periodo do tempo
ants_sp_sf_values %>% 
  group_by(sp) %>% 
  summarise(min = min(tmax_presente_max, na.rm = TRUE),
            max = max(tmax_presente_max, na.rm = TRUE))

tm_shape(ants_sp_sf_values) +
  tm_bubbles(col = "tmax_presente_max", size = .3, breaks = c(20, 25, 30, 35, 40, 50)) +
  tm_layout(legend.position = c("left", "bottom"))

tm_shape(ants_sp_sf_values) +
  tm_bubbles(col = "tmax_fut_1_max", size = .3)

tm_shape(ants_sp_sf_values) +
  tm_bubbles(col = "tmax_fut_2_max", size = .3, alpha = 0.5)


# calcular variacao No de sitios--------- (Luan)

# Duvida (Luan): pontos de ocorrencia muito proximos -> nao e preciso
# controlar para autocorrelacao espacial??


# Numero de ocorrencias sobreviventes

# Presente
ants_sp_sf_values %>% group_by(sp) %>% summarize(occ = n())

# Futuro 1 (numero de locais que a temperatura se manteria aceitavel)
ants_sp_sf_values %>% 
  group_by(sp) %>% 
  summarize(surv_Ectatomma = sum(tmax_fut_1_max < 40 & sp == "Ectatomma brunneum", na.rm = T),
            surv_Eciton = sum(tmax_fut_1_max < 40 & sp == "Eciton burchellii", na.rm = T),
            surv_Solenopsis = sum(tmax_fut_1_max < 45.3 & sp == "Solenopsis invicta", na.rm = T))

# Futuro 2 (numero de locais que a temperatura se manteria aceitavel)
ants_sp_sf_values %>% 
  group_by(sp) %>% 
  summarize(surv_Ectatomma = sum(tmax_fut_2_max < 40 & sp == "Ectatomma brunneum", na.rm = T),
            surv_Eciton = sum(tmax_fut_2_max < 40 & sp == "Eciton burchellii", na.rm = T),
            surv_Solenopsis = sum(tmax_fut_2_max < 45.3 & sp == "Solenopsis invicta", na.rm = T))


ants_sp_sf_values_eb <- ants_sp_sf_values %>% 
  sf::st_drop_geometry() %>% 
  filter(sp == "Ectatomma brunneum") %>% 
  mutate(sobreviveu_present = ifelse(tmax_presente_max > 40, 0, 1),
         sobreviveu_fut1 = ifelse(tmax_fut_1_max > 40, 0, 1),
         sobreviveu_fut2 = ifelse(tmax_fut_2_max > 40, 0, 1)) %>% 
  dplyr::select(elev, sobreviveu_present, sobreviveu_fut1, sobreviveu_fut2) %>% 
  pivot_longer(cols = c(sobreviveu_present, sobreviveu_fut1, sobreviveu_fut2), 
               names_to = "cenario", values_to = "sobrevivencia")
ants_sp_sf_values_eb

ggplot(ants_sp_sf_values_eb, aes(x = elev, y = sobrevivencia, color = cenario)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))


## biomas (Diego)
biomas <- geobr::read_biomes(showProgress = FALSE) %>%
  dplyr::filter(name_biome != "Sistema Costeiro")
biomas


## combinar biomas com cenários de temperatura

tm_shape(biomas) +
  tm_polygons() +
  tm_shape(ants_sp_sf_values) +
  tm_bubbles(col = "tmax_presente_max", size = .1) +
  tm_facets(by = "sp", free.coords = FALSE) +
  tm_grid(lines = FALSE, 
          labels.format = list(big.mark = ""), 
          labels.rot = c(0, 90))


tm_shape(biomas) +
  tm_polygons() +
  tm_shape(ants_sp_sf_values) +
  tm_bubbles(col = "tmax_fut_1_max", size = .1) +
  tm_grid(lines = FALSE, 
          labels.format = list(big.mark = ""), 
          labels.rot = c(0, 90))


tm_shape(biomas) +
  tm_polygons() +
  tm_shape(ants_sp_sf_values) +
  tm_bubbles(col = "tmax_fut_2_max", size = .1, alpha = 0.5)+
  tm_grid(lines = FALSE, 
          labels.format = list(big.mark = ""), 
          labels.rot = c(0, 90))


# mapa com x --------------------------------------------------------------

ants_sp_sf_values_eb <- ants_sp_sf_values %>% 
  filter(sp == "Ectatomma brunneum") %>% 
  mutate(sobreviveu_present = ifelse(tmax_presente_max > 40, 0, 1),
         sobreviveu_fut1 = ifelse(tmax_fut_1_max > 40, 0, 1),
         sobreviveu_fut2 = ifelse(tmax_fut_2_max > 40, 0, 1)) %>% 
  dplyr::select(elev, sobreviveu_present, sobreviveu_fut1, sobreviveu_fut2) %>% 
  tidyr::gather(c(sobreviveu_present, sobreviveu_fut1, sobreviveu_fut2), 
                key = "cenario", value = "sobrevivencia") %>% 
  mutate(sobrevivencia = as.factor(sobrevivencia))
ants_sp_sf_values_eb

tm_shape(ants_sp_sf_values) +
  tm_symbols(size = .1, shape = "sobrevivencia", shapes = c(20, 4)) +
  tm_facets("cenario")
