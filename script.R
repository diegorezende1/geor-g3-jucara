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
library(sf)
library(maxnet)
library(glmnet)
library("sp")
library("raster")
library("maptools")
library("rgdal")
library(dismo)
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
ants_genus_select
spec(ants_genus_select)
view(ants_genus_select)
ncol(ants_genus_select)
nrow(ants_genus_select)
summary(ants_genus_select)
str(ants_genus_select)


#Selecionar colunas de interesses e montando os data.frame (Diego-02/11/2021)

ants_genus_select <- ants_genus_select %>% 
       dplyr::select(Genus, Species, Latitude.y, Longitude.x)
ants_genus_select
view(ants_genus_select)

#unir as colunas Genus e Speceies (Diego-02/11/2021)

ants_genus_select  <- tidyr::unite(data = ants_genus_select, 
                                   col = "sp",
                                   Genus:Species, 
                                   sep = " ",
                                   remove = FALSE)
ants_genus_select
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

tm_shape(ants_sp_sf) +
  tm_bubbles(col = "sp", size = .3)

#=======


#Baixando os dados de temperatura do worldclim (Maria Alice-02/11/2021)


download.file(url = "https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_5m_tmax.zip",
              destfile = "worldclim/wc2.1_5m_tmax.zip", mode = "wb")

download.file(url = "https://biogeo.ucdavis.edu/data/worldclim/v2.1/fut/5m/wc2.1_5m_tmax_CanESM5_ssp245_2081-2100.zip",
              destfile = "worldclim/wc2.1_5m_tmax_CanESM5_ssp245_2081-2100.zip", mode = "wb")

download.file(url = "https://biogeo.ucdavis.edu/data/worldclim/v2.1/fut/5m/wc2.1_5m_tmax_CanESM5_ssp585_2081-2100.zip",
              destfile = "worldcim/wc2.1_5m_tmax_CanESM5_ssp585_2081-2100.zip", mode = "wb")

unzip("worldclim/wc2.1_5m_tmax.zip", exdir = "worldclim")
unzip("worldclim/wc2.1_5m_tmax_CanESM5_ssp245_2081-2100.zip", exdir = "worldclim")
unzip("worldclim/wc2.1_5m_tmax_CanESM5_ssp585_2081-2100.zip", exdir = "worldclim")
# depois de unzip, pegar o arquivo .tif futuro lah no final das pastas e jogar para uma 
# pasta chamada "future" no diretorio "worldclim"

# import ------------------------------------------------------------------

# raster
tmax_presente <- raster::stack(dir(path = "worldclim", pattern = ".tif"))
tmax_presente

tmax_fut_sonho_meu <- raster::raster(dir(path = "worldclim/future", pattern = "CanESM5_ssp245_2081-2100")) %>% 
  mean()
tmax_fut_sonho_meu

tmax_fut_armagedom <- raster::stack(dir(pattern = "CanESM5_ssp245_2081-2100")) %>% 
  mean()
tmax_fut_armagedom

# extrair -----------------------------------------------------------------

ants_sp_sf_values <- ants_sp_sf %>% 
  dplyr::mutate(tmax_presente = raster::extract(tmax_presente, .))
ants_sp_sf_values

ants_sp_sf_values %>% 
  group_by(sp) %>% 
  summarise(min = min(tmax_presente, na.rm = TRUE),
            max = max(tmax_presente, na.rm = TRUE))

tm_shape(ants_sp_sf_values) +
  tm_bubbles(col = "tmax_presente", size = .3)


# outra parte do script ---------------------------------------------------

# Ler dados

obs.data <- read.csv(file = "Atlantic_Ants-main.csv")

#Verificar dados

summary(obs.data)

#Determinando a extensão geográfica dos dados (Maria Alice-02/11/2021)

max.lat <- ceiling(max(obs.data$latitude))
min.lat <- floor(min(obs.data$latitude))
max.lon <- ceiling(max(obs.data$longitude))
min.lon <- floor(min(obs.data$longitude))
geographic.extent <- extent(x = c(min.lon, max.lon, min.lat, max.lat))

#Carregando os dados para mapa de base(world_simpl) (Maria Alice-02/11/2021)

#Plotando mapa base
plot(wrld_simpl, xlim = c(min.lon, max.lon), ylim = c(min.lat, max.lat), axes = TRUE, col = "grey")

# Adicionar pontos para observação individual (Maria Alice-02/11/2021)
(x = obs.data$longitude, y = obs.data$latitude, col = "olive", pch = 20, cex = 0.75)

# Desenhar caixa ao redor do gráfico (Maria Alice-02/11/2021)
box()

# Projeção para o futuro (condições ambientais mais extremas) 

futureEnv <- getData('CMIP5', var='bio', res=2.5, rcp=85, model='HE', year=50)
names(futureEnv)=names(currentEnv)

# Limitando o conjunto de temperaturas
currentEnv=dropLayer(currentEnv, c("bio2", "bio3", "bio4", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19"))
futureEnv=dropLayer(futureEnv, c("bio2", "bio3", "bio4", "bio10", "bio11", "bio012", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19"))

#Temperaturas ecolhidas:
#BIO1 = Annual Mean Temperature
#BIO5 = Max Temperature of Warmest Month
#BIO6 = Min Temperature of Coldest Month
#BIO7 = Temperature Annual Range (BIO5-BIO6)
#BIO8 = Mean Temperature of Wettest Quarter
#BIO9 = Mean Temperature of Driest Quarter
