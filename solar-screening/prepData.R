#Script to clean data before loading into shiny app
#Data source: U.S. Department of Energy (DOE)/NREL/ALLIANCE FEMP Screening Map

#clear environment
rm(list = ls())

#load packages
library(tidyverse)
library(units)
library(sf)
library(leaflet)

#https://github.com/r-spatial/sf/issues/1762
#https://grantmcdermott.com/ds4e/spatial-analysis.html
sf_use_s2(FALSE)

#read in the data
inputDir <- "solar-screening/data/input"
outputDir <- "solar-screening/data/clean"
shpName1 <- "femp_pv_no_incentives_sir_10000kw_view/femp_pv_no_incentives_sir_10000kw_view.shp"
shpName2 <- "cb_2018_us_county_20m/cb_2018_us_county_20m.shp"
shpPath1 <- paste0(inputDir, "/", shpName1) #savings to investment ratio data
shpPath2 <- paste0(inputDir, "/", shpName2) #us county boundaries

#set sf precision (to speed up processing time) and project to commmon CRS for Oregon (2992)
shp1 <- st_set_precision(st_transform(st_read(shpPath1), crs = 2992), 1e8)
shp2 <- st_set_precision(st_transform(st_read(shpPath2), crs = 2992), 1e8)

#filter out county boundaries in oregon
shp2OR <- shp2 %>% filter(STATEFP == 41)

#check that coordinate systems match
st_crs(shp1) == st_crs(shp2OR)

#fix geometries
shp1Valid <- st_make_valid(shp1)
shp2ORValid <- st_make_valid(shp2OR)

#all(st_is_valid(shp1Valid))
#all(st_is_valid(shp2ORValid))

#fix geometries by applying zero buffer
#shp1ValidBuf <- st_buffer(shp1Valid, 0.0)
#shp2ORValidBuf <- st_buffer(shp2ORValid, 0.0)

#get quantiles for solar savings to investment ratios for 10kw systems
breaks <- quantile(shp1Valid$pvsir10kw)

#classify each feature based on which 10kw SIR quantile it falls into
shp1ValidBinned <- shp1Valid %>%
  mutate(pv10kwBin = cut(pvsir10kw, breaks))

#add unique IDs and calculate area of features for later analysis
# shp1Valid <- shp1Valid %>% mutate(pvID = row_number(), 
#                                   areaSqMi = x)
# 
# shp2ORValid <- shp2ORValid %>% mutate(countyID = row_number(),
#                                       areaSqMi = set_units(st_area(.), "mi^2"))

#head(shp2OR)
#plot(shp2OR["NAME"])

#intersect solar screening data and Oregon county boundaries
#shp1ValidBufOR <- st_intersection(shp1ValidBuf, shp2ORValidBuf)
shp1ValidOR <- st_intersection(shp1ValidBinned, shp2ORValid)

#dissolve intersected data by county to get area (mi^2) of 10kw SIR (by quantile) by county
shp1ValidOR <- shp1ValidOR %>% 
  mutate(areaSqMi = set_units(st_area(.), "mi^2")) %>%
  group_by(NAME, pv10kwBin) %>% 
  summarise(sumAreaSqMi = sum(areaSqMi)) %>% 
  st_cast()

#transform to EPSG 4326 to avoid leaflet error(?) see: https://github.com/rstudio/leaflet/issues/505
#shp1FixProjWGS <- st_transform(shp1ValidBufOR, 4326) #introduces geometry errors(?)
shp1ORSummarizeProjWGS <- st_transform(shp1ValidOR, 4326) #introduces geometry errors(?)
#shp1CleanWGS <- shp1FixProjWGS %>% filter( !(st_geometry_type(.) %in% c("GEOMETRYCOLLECTION")) ) #filter out geometry collection --> not needed if summarize areas first
#shp2ORFixProjWGS <- st_transform(shp2ORValidBuf, 4326)
#shp2ORFixProjWGS <- st_transform(shp2ORValid, 4326) #don't actually need the county data currently as we summarized data by county above

#plot the result (note: switching to AGG graphics device may speed up sf plotting)

#plot with base r:
#plot(shp1CleanWGS["pvsir10kw"])

#plot with ggplot:
# ggplot(data = shp2OR) +
#   geom_sf() +
#   title("Solar Screening Data") +
#   geom_sf(data = shp1CleanWGS, aes(fill = pvsir10kw)) +
#   scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
#   geom_sf_label(aes(label = NAME))

#plot with leaflet: (have to make sure input doesn't contain geometry collection and that crs of inputs is WGS84)

#colors for solar screening data
# shp1Pal <- colorQuantile(
#   palette = "magma", 
#   domain = shp1CleanWGS$pvsir10kw)
# 
# leaflet() %>%
#   addPolygons(data = shp1CleanWGS,
#               fillColor = ~shp1Pal(pvsir10kw),
#               color = NA)
              

#write data to output folder
st_write(shp1ORSummarizeProjWGS, paste0(outputDir, "/pv_sir_10000kw_summaryByCounty_ProjWGS.shp"))
st_write(shp2ORFixProjWGS, paste0(outputDir, "/2018_us_county_20m_FixGeom_FilterOR_ProjWGS.shp"))

