library(sf)
library(tidyverse) |> suppressPackageStartupMessages()
library(stars) |> suppressPackageStartupMessages()
library(tigris)
library(scales)
library(gstat)

setwd('~/projects/zillow_home_values')

home_values <- read_csv('./Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv') |> 
  filter(RegionType == 'msa') |>
  pivot_longer(starts_with("20"), names_to = 'Date', values_to = 'ZHVI') |>
  mutate(Date = as.Date(Date))

home_values_20240531 <- home_values |> 
  filter(Date == as.Date('2024-05-31')) |>
  select(RegionID, RegionName, StateName, ZHVI)

bcoord <- c(-125.0, 24.3, -66.9,  49.4)
crs <- st_crs(6350) # NAD83 with units as meters
st <- tigris::states(year = 2021, filter_by = bcoord, cb = TRUE) |> st_transform(crs)
pl <- tigris::places(state = unique(st$STUSPS), year = 2021, cb = TRUE) |> st_transform(crs)
  
  

pl_zl <- pl |>
  st_centroid() |>
  mutate(PlaceName = paste(NAME, STUSPS, sep = ", ")) |>
  inner_join(home_values_20240531 |> select(RegionID, RegionName, ZHVI), by = c('PlaceName' = 'RegionName'))

grd <- st_bbox(st) |>
  st_as_stars(dx = units::set_units(20, km)) |>
  st_crop(st)

i_w <- idw(ZHVI~1, pl_zl, grd)
## 11 cities excluded due to naming differences between Zillow and Census data,
## e.g. Winton, NC in Zillow is Winston-Salem, NC in the Census data
excluded <- home_values_20240531 |> filter(!(RegionID %in% pl_zl$RegionID) & StateName != 'AK' & StateName != 'HI')

ggplot() +
  geom_stars(data = i_w, aes(fill = var1.pred, x = x, y = y)) +
  geom_sf(data = st_cast(st, "MULTILINESTRING")) +
  scale_fill_stepsn("Typical Home Value", na.value = "transparent", colors = terrain.colors(11), breaks = seq(0, 1e6, 2e5), labels = scales::label_currency(scale_cut = cut_short_scale())) +
  theme_void() +
  theme(legend.box.margin = margin(0, 1, 0, 0, unit = "cm"), plot.title = element_text(hjust = 0.5))

ggsave('./figures/zhvi_05312024.png', width = 12, height = 8, units = "in", dpi = 150)
