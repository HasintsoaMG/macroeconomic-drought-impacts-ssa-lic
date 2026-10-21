
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyterra) 
library(stringr)

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

#Garder les 8 ssalic ok 
pays_retenus <- c("BDI", "ETH", "GNB", "MDG", "MLI", "MWI", "NER", "UGA")

ssalic8 <- world[world$GID_0 %in% pays_retenus, ]

# Contrôles
nrow(ssalic8)                      # doit valoir 8
sort(ssalic8$GID_0)
setdiff(pays_retenus, ssalic8$GID_0)   # doit être vide

# EXTRACT SPEI-------------------------------------------------------------------------------------------------------------------
#vérification de la projection: les objets spatiaux (rasters et shapefile) ont-ils le même système de coordonnées 
same.crs(ssalic8, spei_rast)

#vérification de la superposition
plot(spei_rast[[500]])
plot(ssalic8, add = TRUE, border = "yellow", lwd = 0.25)

#calcul de la moyenne spatiale mensuelle pour chaque pays
spei_mean <- extract(
  spei_rast,
  ssalic8,
  fun = mean,
  na.rm = TRUE
)
dim(spei_mean)
head(spei_mean[,1:10])
spei_mean$GID_0 <- ssalic8$GID_0
spei_mean$country <- ssalic8$COUNTRY
head(spei_mean[,c("GID_0","country")])

spei_mean[spei_mean$GID_0=="MDG",
          c("GID_0","country","1940-12","1941-01")] #vérification pour Madagascar

# EXTRACT SPI-------------------------------------------------------------------------------------------------------------------
#vérification de la projection: les objets spatiaux (rasters et shapefile) ont-ils le même système de coordonnées 
same.crs(ssalic8, spi_rast)

#vérification de la superposition
plot(spi_rast[[500]])
plot(ssalic8, add = TRUE, border = "yellow", lwd = 0.25)

#calcul de la moyenne spatiale mensuelle pour chaque pays
spi_mean <- extract(
  spi_rast,
  ssalic8,
  fun = mean,
  na.rm = TRUE
)
dim(spi_mean)
head(spi_mean[,1:10])
spi_mean$GID_0 <- ssalic8$GID_0
spi_mean$country <- ssalic8$COUNTRY
head(spi_mean[,c("GID_0","country")])

spi_mean[spi_mean$GID_0=="MDG",
          c("GID_0","country","1940-12","1941-01")] #vérification pour Madagascar

#DIAGNOSTIC sur 1994-2024, décembre seulement---------------------------------------------------------------------------
library(dplyr); library(tidyr); library(stringr)

pays_retenus <- c("BDI", "ETH", "GNB", "MDG", "MLI", "MWI", "NER", "UGA")

#SPI*******************
spi_dec <- spi_mean %>%
  select(-any_of("ID")) %>%
  filter(GID_0 %in% pays_retenus) %>%
  pivot_longer(-c(GID_0, country), names_to = "ym", values_to = "spi") %>%
  mutate(year  = as.integer(str_sub(ym, 1, 4)),
         month = as.integer(str_sub(ym, 6, 7))) %>%
  filter(month == 12, year >= 1994, year <= 2024)

nrow(spi_dec)   # doit valoir 8 pays x 31 années = 248 OK

# Global
spi_dec %>% summarise(moy = mean(spi), sd = sd(spi), min = min(spi), max = max(spi))

# Par pays
spidec_statdes <- spi_dec %>%
  group_by(GID_0) %>%
  summarise(moy = mean(spi), sd = sd(spi), min = min(spi), max = max(spi),
            part_hors_3 = mean(abs(spi) > 3)) %>%
  arrange(desc(abs(moy)))

paysspi3plus <- spi_dec %>% filter(abs(spi) > 3) %>% select(GID_0, year, spi) 

#SPEI*******************
spei_dec <- spei_mean %>%
  select(-any_of("ID")) %>%
  filter(GID_0 %in% pays_retenus) %>%
  pivot_longer(-c(GID_0, country), names_to = "ym", values_to = "spei") %>%
  mutate(year  = as.integer(str_sub(ym, 1, 4)),
         month = as.integer(str_sub(ym, 6, 7))) %>%
  filter(month == 12, year >= 1994, year <= 2024)

nrow(spei_dec)   # doit valoir 8 pays x 31 années = 248 OK

# Global
spei_dec %>% summarise(moy = mean(spei), sd = sd(spei), min = min(spei), max = max(spei))

# Par pays
speidec_statdes <- spei_dec %>%
  group_by(GID_0) %>%
  summarise(moy = mean(spei), sd = sd(spei), min = min(spei), max = max(spei),
            part_hors_3 = mean(abs(spei) > 3)) %>%
  arrange(desc(abs(moy)))

paysspi3plus <- spi_dec %>% filter(abs(spi) > 3) %>% select(GID_0, year, spi)

#CONSTRUCTION DES TABLES ANNUELLES---------------------------------------------------------------------
make_annual <- function(df_mean, nm) {
  df_mean %>%
    select(-any_of("ID")) %>%
    filter(GID_0 %in% pays_retenus) %>%
    pivot_longer(-c(GID_0, country), names_to = "ym", values_to = "v") %>%
    mutate(year  = as.integer(substr(ym, 1, 4)),
           month = as.integer(substr(ym, 6, 7))) %>%
    filter(month == 12, year >= 1993, year <= 2024) %>%
    arrange(GID_0, year) %>%
    group_by(GID_0) %>%
    mutate(v_l1 = lag(v)) %>%
    ungroup() %>%
    transmute(`Country Code` = GID_0, year,
              !!nm                := v,
              !!paste0(nm, "_l1") := v_l1)
}

spei_annual <- make_annual(spei_mean, "spei")
spi_annual  <- make_annual(spi_mean,  "spi")

