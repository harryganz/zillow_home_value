library(sf)
library(tidyverse) |> suppressPackageStartupMessages()
library(stars) |> suppressPackageStartupMessages()
library(tigris)
library(scales)
library(gstat)

setwd('~/projects/zillow_home_values')

## Read ZHVI for single family home and condos, seasonally adjusted, per county
home_values <- read_csv('./County_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv') |>
  select(RegionID, RegionName, StateName, StateCodeFIPS, MunicipalCodeFIPS, starts_with('20')) |>
  pivot_longer(cols = starts_with('20'), names_to = 'Date', values_to = 'ZHVI') |>
  mutate(Date = as.Date(Date))

# Get State data
st <- tigris::states(year = 2021, cb = TRUE)
original_crs <- st_crs(st) # NAD83
# Continuous US boundaries as a bbox
cus_boundaries <- st_bbox(c(xmin = -125, ymin = 20, xmax = -67,  ymax = 50), crs = original_crs)

# Get state and county data and transform to CRS
st <- st |> 
  filter(STATEFP %in% unique(home_values$StateCodeFIPS))

co <- tigris::counties(year = 2021, cb = TRUE) |> 
  filter(STATEFP %in% unique(home_values$StateCodeFIPS) & COUNTYFP %in% unique(home_values$MunicipalCodeFIPS))
  

co_home_values <- co |>
  left_join(home_values, by = c('STATEFP' = 'StateCodeFIPS', 'COUNTYFP' = 'MunicipalCodeFIPS'))


ggplot() +
  geom_sf(data = co_home_values |> filter(Date == as.Date('2024-06-30')), 
          aes(fill = ZHVI), linewidth = 0.05) +
  geom_sf(data = st |> st_cast("MULTILINESTRING")) +
  coord_sf(crs = original_crs, xlim = c(-125, -67), ylim = c(24, 50)) +
  scale_fill_stepsn(breaks = seq(2e5, 1e6, 2e5), colors = terrain.colors(3), 
                    labels = scales::label_currency(scale_cut = cut_short_scale()))