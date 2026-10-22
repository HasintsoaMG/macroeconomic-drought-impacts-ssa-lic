library(ecmwfr)

request_T <- list(
  dataset_short_name = "satellite-land-surface-temperature",
  variable = "land_surface_temperature",
  observation_time = "day",
  year = c("1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024"),
  month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
  version = "v3_00",
  area = c(42.15, -27.51, -45.04, 68.16),
  target = "TMPFILE"
)
