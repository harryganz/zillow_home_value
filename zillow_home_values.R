library(sf)
library(tidyverse) |> suppressPackageStartupMessages()
library(tigris)
library(scales)
library(sp)
library(lubridate)
library(magick)

setwd('~/projects/zillow_home_values')

## Read ZHVI for single family home and condos, seasonally adjusted, per county
home_values <- read_csv('./County_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv') |>
  select(RegionID, RegionName, StateName, StateCodeFIPS, MunicipalCodeFIPS, starts_with('20')) |>
  pivot_longer(cols = starts_with('20'), names_to = 'Date', values_to = 'ZHVI') |>
  mutate(Date = as.Date(Date)) |>
  filter(Date >= as.Date('2010-01-01')) |>
  group_by(floor_date(Date, 'year')) |>
  arrange(Date) |>
  filter(Date == first(Date)) |>
  ungroup()

# Get State data
st <- tigris::states(year = 2021, cb = TRUE)
original_crs <- st_crs(st)
new_crs <- st_crs(6350) # Albers Equal Area projection using NAD83

# Get state and county data and transform to CRS
st <- st |> 
  filter(STATEFP %in% unique(home_values$StateCodeFIPS)) |>
  st_transform(new_crs)

co <- tigris::counties(year = 2021, cb = TRUE) |> 
  filter(STATEFP %in% unique(home_values$StateCodeFIPS) & COUNTYFP %in% unique(home_values$MunicipalCodeFIPS)) |>
  st_transform(new_crs)
  

co_home_values <- co |>
  left_join(home_values, by = c('STATEFP' = 'StateCodeFIPS', 'COUNTYFP' = 'MunicipalCodeFIPS'))

# Select only contiguous U.S.
st_cus <- st |> filter(STUSPS != 'AK' & STUSPS != 'HI')
co_home_values_cus <- co_home_values  |> filter(STUSPS != 'AK' & STUSPS != 'HI')

# Select AK
st_ak <- st |> filter(STUSPS == 'AK')
co_home_values_ak <- co_home_values |> filter(STUSPS == 'AK')

# Select HI
st_hi <- st |> filter(STUSPS == 'HI')
co_home_values_hi <- co_home_values |> filter(STUSPS == 'HI')

# Rotate, Scale, and slide a Simple Feature object in the 
# units of the original
trans <- function(sf, degree = 0, scale = 1.0 , dx = 0, dy = 0) {
  crs <- st_crs(sf)
  geom <- st_geometry(sf)
  rad <- degree * pi/180
  rot <- matrix(c(cos(rad), sin(rad), -sin(rad), cos(rad)), 2, 2)
  cnt <- st_centroid(st_union(geom))
  t_geom <- ((geom - cnt) * rot + cnt + c(dx, dy)) * scale
  t_sf <- st_set_geometry(sf, t_geom)
  st_crs(t_sf) <- crs
  t_sf
}

ak_trans <- function(x)trans(x, -10, 0.4, 0.75e6, -5.2e6)
hi_trans <- function(x)trans(x, 0, 1, 3.5e6, -1.5e6)

lapply(unique(home_values$Date), function(date) {
  cu_dat <- co_home_values_cus |> filter(Date == date)
  ak_dat <- co_home_values_ak |> filter(Date == date)
  hi_dat <- co_home_values_hi |> filter(Date == date)
  yr <- year(date)
  p <- ggplot() +
    ggtitle(yr) +
    geom_sf(data = st_cus, fill = "grey70") +
    geom_sf(data = ak_trans(st_ak), fill = "grey70") +
    geom_sf(data = hi_trans(st_hi), fill = "grey70") +
    geom_sf(data = cu_dat, aes(fill = ZHVI), linewidth = 0.05) +
    geom_sf(data = ak_trans(ak_dat), aes(fill = ZHVI)) +
    geom_sf(data = hi_trans(hi_dat) ,aes(fill = ZHVI)) +
    coord_sf(crs = original_crs, xlim = c(-124, -68)) +
    scale_fill_stepsn("Typical Home Value", breaks = seq(2e5, 1e6, 2e5), 
                      colors = terrain.colors(5), na.value = "grey70",
                      labels = scales::label_currency(scale_cut = cut_short_scale())) +
    theme_void() +
    theme(plot.title = element_text(size = unit("16", "pt"), hjust = 1.0), plot.margin = unit(rep(0.25, 4), "in"))
  fp <- file.path(paste0('./figures/zhvi_', yr, ".png"))
  ggsave(plot = p, filename = fp, device = "png")
})

imgs <- list.files('./figures', full.names = TRUE)
img_animated <- lapply(imgs, image_read) |>
  image_join() |>
  image_animate(fps = 2)





