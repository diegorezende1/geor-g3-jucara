##Diego - 31/10/2021


#install packages (Bianca-01/11/2021)
install.packages("tidyverse")
install.packages("here")
install.packages("tidyr")
install.packages("dplyr")


#library packages (Bianca-01/11/2021)
library(tidyverse)
library(here)
library(tidyr)
library(dplyr)

# conferir diretório
here::here()

# criar um arquivo .here
here::set_here()


#verificar diretório (Bianca-01/11/2021)
getwd()

#definir diretório de trabalho (Bianca-01/11/2021)
setwd("C:/github/geor-g3-jucara/Atlantic_Ants-main/DATASET")

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

##Install packages (Maria Alice-02/11/2021) 

install.packages("dismo")
install.packages("maptools")
install.packages("rgdal")
install.packages("raster")
install.packages("sp")

#Library packages (Maria Alice-02/11/2021)

library("sp")
library("raster")
library("maptools")
library("rgdal")
library("dismo")

#Baixando os dados de temperatura do worldclim (temperatura média) (Maria Alice-02/11/2021)

bioclim.data <- getData(name = "worldclim",
                        var = "bio1",
                        res = 2.5,
                        path = "data/"Atlantic_Ants-main"
ants))

#Ler dados
obs.data <- read.csv(file = "data/Atlantic_Ants-main.csv"

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
plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey"

# Adicionar pontos para observação individual (Maria Alice-02/11/2021)
(x = obs.data$longitude, 
       y = obs.data$latitude, 
       col = "olive", 
       pch = 20, 
       cex = 0.75)

# Desenhar caixa ao redor do gráfico (Maria Alice-02/11/2021)
box()
