#packages
library(tidyverse)
library(terra)
library(sf)
install.packages("exactextractr")
library(exactextractr)
library(SPEI)
library(ncdf4)
library(lubridate)
install.packages("countrycode")
library(countrycode)
install.packages("janitor")
library(janitor)
install.packages("here")
library(here)

#liste des pays 
countries <- c(
  "Burkina Faso",
  "Burundi",
  "Central African Republic",
  "Chad",
  "Congo RD",
  "Eritrea",
  "Ethiopia",
  "Gambia",
  "Guinea-Bissau",
  "Liberia",
  "Madagascar",
  "Malawi",
  "Mali",
  "Mozambique",
  "Niger",
  "Rwanda",
  "Sierra Leone",
  "Somalia RF",
  "South Sudan",
  "Sudan",
  "Uganda"
)

#extraction des données sur ERA 5

#1 installation de ecmwfr
install.packages("ecmwfr")
library(ecmwfr)
if(!require(remotes)){install.packages("remotes")}
remotes::install_github("bluegreen-labs/ecmwfr")
library("ecmwfr")

#clé API
wf_get_key()

#envoi de la demande
request <- list(
  dataset_short_name = "derived-drought-historical-monthly",
  variable = c("standardised_precipitation_index", "standardised_precipitation_evapotranspiration_index"),
  accumulation_period = "12",
  version = "1_0",
  product_type = "reanalysis",
  dataset_type = "consolidated_dataset",
  year = c("1940", "1941", "1942", "1943", "1944", "1945", "1946", "1947", "1948", "1949", "1950", "1951", "1952", "1953", "1954", "1955", "1956", "1957", "1958", "1959", "1960", "1961", "1962", "1963", "1964", "1965", "1966", "1967", "1968", "1969", "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", 
"2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025"),
  month = "12",
  area = c(41.66, -18.13, -49.28, 78.47),
  target = "TMPFILE"
)
file <- wf_request(
  request = request,
  transfer = TRUE,
  path = "."
)

#extraction des fichiers 
file.exists("TMPFILE.zip")
unzip(
  "TMPFILE.zip",
  exdir = "01_raw_data/SPI_SPEI_Era5"
)
