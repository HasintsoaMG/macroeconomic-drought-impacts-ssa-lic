
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyterra) 

#Chargement du shapefiles gadm
world <- vect("01_raw_data/gadm_410-levels/gadm_410-levels.gpkg",layer ="ADM_0")
plot(world)
names(world)
head(world)
world$GID_0
data_world <- as.data.frame(world)
View(data_world)

#Chargement des rasters SPEI et SPI
spei_spi_rast <- list.files("01_raw_data/SPI_SPEI_Era5", full.names = TRUE) %>%
  rast()
spei_spi_rast 

head(names(spei_spi_rast), 10) #dix premières lignes
tail(names(spei_spi_rast), 10) #dix dernières lignes
varnames(spei_spi_rast)[c(1, 1021, 1022, 2042)]

#création de deux objets disctincts SPI et SPEI (séparer le spei_spi_rast)
spei_rast <- spei_spi_rast[[1:1021]]
spi_rast <- spei_spi_rast[[1022:2042]]
names(spei_rast) <- format(time(spei_rast), "%Y-%m") #renommer les fichiers en année et mois
names(spi_rast)  <- format(time(spi_rast), "%Y-%m")

#création du sous-ensemble gadm ssalic
ssalic <- world[world$GID_0 %in% c(
  "BDI",
  "BFA",
  "CAF",
  "COD",
  "ERI",
  "ETH",
  "GMB",
  "GNB",
  "LBR",
  "MDG",
  "MLI",
  "MOZ",
  "MWI",
  "NER",
  "RWA",
  "SDN",
  "SLE",
  "SOM",
  "SSD",
  "TCD",
  "UGA"
), ]
nrow(ssalic)
ssalic$GID_0

# EXTRACT SPEI-------------------------------------------------------------------------------------------------------------------
#vérification de la projection: les objets spatiaux (rasters et shapefile) ont-ils le même système de coordonnées 
same.crs(ssalic, spei_rast)

#vérification de la superposition
plot(spei_rast[[500]])
plot(ssalic, add = TRUE, border = "yellow", lwd = 0.25)

#calcul de la moyenne spatiale mensuelle pour chaque pays
spei_mean <- extract(
  spei_rast,
  ssalic,
  fun = mean,
  na.rm = TRUE
)
dim(spei_mean)
head(spei_mean[,1:10])
spei_mean$GID_0 <- ssalic$GID_0
spei_mean$country <- ssalic$COUNTRY
head(spei_mean[,c("GID_0","country")])

spei_mean[spei_mean$GID_0=="MDG",
          c("GID_0","country","1940-12","1941-01")] #vérification pour Madagascar

# EXTRACT SPI-------------------------------------------------------------------------------------------------------------------
#vérification de la projection: les objets spatiaux (rasters et shapefile) ont-ils le même système de coordonnées 
same.crs(ssalic, spi_rast)

#vérification de la superposition
plot(spi_rast[[500]])
plot(ssalic, add = TRUE, border = "yellow", lwd = 0.25)

#calcul de la moyenne spatiale mensuelle pour chaque pays
spi_mean <- extract(
  spi_rast,
  ssalic,
  fun = mean,
  na.rm = TRUE
)
dim(spi_mean)
head(spi_mean[,1:10])
spi_mean$GID_0 <- ssalic$GID_0
spi_mean$country <- ssalic$COUNTRY
head(spi_mean[,c("GID_0","country")])

spi_mean[spi_mean$GID_0=="MDG",
          c("GID_0","country","1940-12","1941-01")] #vérification pour Madagascar
