library(sf)
library (ggplot2)
library(rnaturalearth)
library(ggrepel)

#Mapping shapefiles
EEZ <- st_read(here::here("", "Data/Mapping_shapefiles/EEZ.shp"))
land <- st_read(here::here("", "Data/Mapping_shapefiles/poly_NAD83.shp")) #eastern Can only
land_canada <- ne_countries(country = "Canada", scale = "medium", returnclass = "sf")
contours <- st_read(here::here("", "Data/Mapping_shapefiles/GEBCO_DepthContours.shp"))
NAFO <- st_read(here::here("", "Data/Mapping_shapefiles/Divisions.shp"))
Hague <- st_read(here::here("", "Data/Mapping_shapefiles/HagueLine.shp"))

crs<-st_crs(land_canada)

EEZ <- st_transform (EEZ, crs)
land <- st_transform (land, crs)
contours <- st_transform (contours, crs)
Hague <- st_transform (Hague, crs)

# Clip large shapefiles shapefile to  bounding box
sf::sf_use_s2(FALSE)   # revert to GEOS-based operations

# Plot study area
CAMAP<-ggplot() +
  #geom_sf(data = contours, color="lightblue") +
  geom_sf(data = Hague, color="navy") +
  geom_sf(data = EEZ, color="navy", linetype = "dashed", size = 1.2) +
  geom_sf(data = NAFO, color="darkgrey", fill = NA) +
  geom_sf(data = land, fill="cornsilk") +
 labs(title="")+
  xlim(-72.5, -48) + ylim(39.355, 50)+
  theme_bw()+
  theme(axis.text = element_text(angle = 0, vjust = 0.2, hjust=1,size=8,family="serif"))
CAMAP

### Grab CRIB data from GoC Open Data web ###
require("readr")
url <- "https://api-proxy.edh-cde.dfo-mpo.gc.ca/catalogue/records/264692e5-7b51-4ab9-bb1f-da65f6fc0875/attachments/EN_ClimateRiskIndex_Spatial-CanEEZ_145Spp.csv"
crib.df <- read_csv(url)
head(crib.df) #file too large - do not try to save

#################################################################################################
################################ Pollock & Herring Data #########################################
#################################################################################################
#select pollock and herring data (replace species names with whatever species you want from the list in unique(crib.df$`common name`))
unique(crib.df$`common name`)
polcrib <- crib.df[crib.df$`common name` == "Pollock",]
hercrib <- crib.df[crib.df$`common name` == "Atlantic Herring",]
head(polcrib)
str(polcrib)
write.csv(polcrib, "Data/CRIB/crib_pollock.csv")
write.csv(hercrib, "Data/CRIB/crib_herring.csv")

#################################################################################################
################################ Cod & Lobster Data #############################################
#################################################################################################
#select atl. cod and am. lobster data (replace species names with whatever species you want from the list in unique(crib.df$`common name`))
unique(crib.df$`common name`)
codcrib <- crib.df[crib.df$`common name` == "Atlantic Cod",]
alcrib <- crib.df[crib.df$`common name` == "American Lobster",]
write.csv(codcrib, "Data/CRIB/crib_atlcod.csv")
write.csv(alcrib, "Data/CRIB/crib_amlobster.csv")

#################################################################################################
################################## MAP ATL HALIBUT ##############################################
#################################################################################################
#select only halibut data
halcrib<-read.csv("Data/CRIB/crib_halibut.csv")

# Convert dataframe to an `sf` object
library(sf)
library(terra)
library(gstat)
halv<-halcrib[,c("longitude","latitude","ssp","Vulnerability")]
#haltsm<-halcrib[,c("longitude","latitude","ssp","S.Thermal.safety.margin")]
#haltoe<-halcrib[,c("longitude","latitude","ssp","ToE.year")]
#halcv<-halcrib[,c("longitude","latitude","ssp","E.Climate.velocity")]
#halthv<-halcrib[,c("longitude","latitude","ssp","AC.Thermal.habitat.availability")]
# Convert to a spatial object
spatial_points1 <- vect(halv[halv$ssp=="SSP5-8.5",], geom = c("longitude", "latitude"), crs = "WGS84")
# Create an empty raster (set resolution and extent as needed)
r <- rast(extent=spatial_points1, resolution = 0.25, crs = "WGS84") # Adjust resolution
# Rasterize the points into a grid
raster_data1 <- rasterize(spatial_points1, r, field = "Vulnerability", fun = mean)
spatial_points2 <- vect(halv[halv$ssp=="SSP1-2.6",], geom = c("longitude", "latitude"), crs = "WGS84")
# Create an empty raster (set resolution and extent as needed)
r <- rast(extent=spatial_points2, resolution = 0.25, crs = "WGS84") # Adjust resolution
# Rasterize the points into a grid
raster_data2 <- rasterize(spatial_points2, r, field = "Vulnerability", fun = mean)

#plot(raster_data)

#add halibut vulnerability rasters to NAFO map
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis) 
library(cowplot)
raster_df<-as.data.frame(raster_data1, xy=TRUE)
colnames(raster_df) <- c("longitude", "latitude", "Vulnerability") 

raster_df2<-as.data.frame(raster_data2, xy=TRUE)
colnames(raster_df2) <- c("longitude", "latitude", "Vulnerability")

# ── Compute shared vulnerability range across both rasters ────────────────────
shared_limits <- c(
  min(raster_df$Vulnerability,  raster_df2$Vulnerability, na.rm = TRUE),
  max(raster_df$Vulnerability,  raster_df2$Vulnerability, na.rm = TRUE)
)

NAFO2<-NAFO[NAFO$ZONE!="6A",]

VMAP <- ggplot() +
  geom_raster(data = raster_df, aes(x = longitude, y = latitude, fill = `Vulnerability`)) +
  #geom_sf(data = All_region_df, fill = NA) +
  geom_sf(data = NAFO2, color = "darkgrey", size = 0.6, fill = NA) +
  geom_sf(data = land, fill = "cornsilk") +
  geom_sf(data = EEZ, color = "lightblue", linetype = "dashed", size = 1) +
  geom_sf(data = Hague, color = "red", size = 0.8) +
  geom_sf_label(
    data          = NAFO2,
    aes(label     = ZONE),
    size          = 2.5,
    colour        = "black",
    fontface      = "bold",
    fill          = alpha("grey90", 0.7),   # semi-transparent grey box
    label.size    = 0.2,                    # border thickness around box
    label.padding = unit(0.1, "lines"),     # padding inside box
    check_overlap = TRUE
  ) +
  scale_fill_viridis_c(
    option    = "D",
    name      = "Vulnerability",
    direction = 1,
    limits    = shared_limits   # add this
  ) +
  xlim(-80, -46) + ylim(40, 70) +
  theme_bw() +
  theme(axis.text = element_text(angle = 0, vjust = 0.2, hjust = 1, size = 8, family = "serif")) +
  labs(title = "a) High emissions (SSP5-8.5)", x = "Longitude", y = "Latitude", color = "Vulnerability SSP5-8.5")
print(VMAP) 

VMAP2 <- ggplot() +
  geom_raster(data = raster_df2, aes(x = longitude, y = latitude, fill = `Vulnerability`)) +
  #geom_sf(data = All_region_df, fill = NA) ++
  geom_sf(data = NAFO2, color = "darkgrey", size = 0.6, fill = NA) +
  geom_sf(data = land, fill = "cornsilk") +
  geom_sf(data = EEZ, color = "lightblue", linetype = "dashed", size = 1) +
  geom_sf(data = Hague, color = "red", size = 0.8)+
  geom_sf_label(
    data          = NAFO2,
    aes(label     = ZONE),
    size          = 2.5,
    colour        = "black",
    fontface      = "bold",
    fill          = alpha("grey90", 0.7),   # semi-transparent grey box
    label.size    = 0.2,                    # border thickness around box
    label.padding = unit(0.1, "lines"),     # padding inside box
    check_overlap = TRUE
  ) +
  scale_fill_viridis_c(
    option    = "D",
    name      = "Vulnerability",
    direction = 1,
    limits    = shared_limits   # add this
  ) +
  xlim(-80, -46) + ylim(40, 70) +
  theme_bw() +
  theme(axis.text = element_text(angle = 0, vjust = 0.2, hjust = 1, size = 8, family = "serif")) +
  labs(title = "b) Low emissions (SSP1-2.6)", x = "Longitude", y = "Latitude", color = "Vulnerability SSP5-8.5")
print(VMAP2)

library(grid)
library(gtable)

# Clean versions
VMAP_clean <- VMAP +
  theme(
    legend.position = "none",
    plot.margin     = margin(0, 0, 0, 0)   # no right margin to close the gap
  )

VMAP2_clean <- VMAP2 +
  theme(
    legend.position  = "right",
    axis.title.y     = element_blank(),
    axis.text.y      = element_blank(),
    axis.ticks.y     = element_blank(),
    plot.margin      = margin(0, 0, 0, 2)  # small left margin adds tiny gap
  ) +
  guides(fill = guide_colourbar(
    title          = "Vulnerability",
    barwidth       = unit(0.5, "cm"),
    barheight      = unit(6, "cm"),
    title.position = "top",
    title.hjust    = 0.5
  ))

g1 <- ggplotGrob(VMAP_clean)
g2 <- ggplotGrob(VMAP2_clean)

# Bind side by side
final_grob <- cbind(g1, g2, size = "first")

ggsave(
  filename = "CRIB results/NAFO_AH_5Zto0A_2scenarios.png",
  plot     = final_grob,
  width    = 10,
  height   = 5,
  dpi      = 400,
  bg       = "white"
)

# Save as RDS
saveRDS(final_grob, "CRIB results/NAFO_AH_5Zto0A_2scenarios.rds")

#################################################################################################
######################################## MAP GH #################################################
#################################################################################################
#select only halibut data
halcrib<-read.csv("Data/CRIB/crib_greenland_halibut.csv")

# Convert dataframe to an `sf` object
library(sf)
library(terra)
library(gstat)
halv<-halcrib[,c("longitude","latitude","ssp","Vulnerability")]
#haltsm<-halcrib[,c("longitude","latitude","ssp","S.Thermal.safety.margin")]
#haltoe<-halcrib[,c("longitude","latitude","ssp","ToE.year")]
#halcv<-halcrib[,c("longitude","latitude","ssp","E.Climate.velocity")]
#halthv<-halcrib[,c("longitude","latitude","ssp","AC.Thermal.habitat.availability")]
# Convert to a spatial object
spatial_points1 <- vect(halv[halv$ssp=="SSP5-8.5",], geom = c("longitude", "latitude"), crs = "WGS84")
# Create an empty raster (set resolution and extent as needed)
r <- rast(extent=spatial_points1, resolution = 0.25, crs = "WGS84") # Adjust resolution
# Rasterize the points into a grid
raster_data1 <- rasterize(spatial_points1, r, field = "Vulnerability", fun = mean)
spatial_points2 <- vect(halv[halv$ssp=="SSP1-2.6",], geom = c("longitude", "latitude"), crs = "WGS84")
# Create an empty raster (set resolution and extent as needed)
r <- rast(extent=spatial_points2, resolution = 0.25, crs = "WGS84") # Adjust resolution
# Rasterize the points into a grid
raster_data2 <- rasterize(spatial_points2, r, field = "Vulnerability", fun = mean)

#plot(raster_data)

#add halibut vulnerability rasters to NAFO map
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis) 
library(cowplot)
raster_df<-as.data.frame(raster_data1, xy=TRUE)
colnames(raster_df) <- c("longitude", "latitude", "Vulnerability") 

raster_df2<-as.data.frame(raster_data2, xy=TRUE)
colnames(raster_df2) <- c("longitude", "latitude", "Vulnerability")

# ── Compute shared vulnerability range across both rasters ────────────────────
shared_limits <- c(
  min(raster_df$Vulnerability,  raster_df2$Vulnerability, na.rm = TRUE),
  max(raster_df$Vulnerability,  raster_df2$Vulnerability, na.rm = TRUE)
)

NAFO2<-NAFO[NAFO$ZONE!="6A",]

VMAP <- ggplot() +
  geom_raster(data = raster_df, aes(x = longitude, y = latitude, fill = `Vulnerability`)) +
  #geom_sf(data = All_region_df, fill = NA) +
  geom_sf(data = NAFO2, color = "darkgrey", size = 0.6, fill = NA) +
  geom_sf(data = land, fill = "cornsilk") +
  geom_sf(data = EEZ, color = "lightblue", linetype = "dashed", size = 1) +
  geom_sf(data = Hague, color = "red", size = 0.8) +
  geom_sf_label(
    data          = NAFO2,
    aes(label     = ZONE),
    size          = 2.5,
    colour        = "black",
    fontface      = "bold",
    fill          = alpha("grey90", 0.7),   # semi-transparent grey box
    label.size    = 0.2,                    # border thickness around box
    label.padding = unit(0.1, "lines"),     # padding inside box
    check_overlap = TRUE
  ) +
  scale_fill_viridis_c(
    option    = "D",
    name      = "Vulnerability",
    direction = 1,
    limits    = shared_limits   # add this
  ) +
  xlim(-80, -46) + ylim(40, 73) +
  theme_bw() +
  theme(axis.text = element_text(angle = 0, vjust = 0.2, hjust = 1, size = 8, family = "serif")) +
  labs(title = "a) High emissions (SSP5-8.5)", x = "Longitude", y = "Latitude", color = "Vulnerability SSP5-8.5")
print(VMAP) 

VMAP2 <- ggplot() +
  geom_raster(data = raster_df2, aes(x = longitude, y = latitude, fill = `Vulnerability`)) +
  #geom_sf(data = All_region_df, fill = NA) ++
  geom_sf(data = NAFO2, color = "darkgrey", size = 0.6, fill = NA) +
  geom_sf(data = land, fill = "cornsilk") +
  geom_sf(data = EEZ, color = "lightblue", linetype = "dashed", size = 1) +
  geom_sf(data = Hague, color = "red", size = 0.8)+
  geom_sf_label(
    data          = NAFO2,
    aes(label     = ZONE),
    size          = 2.5,
    colour        = "black",
    fontface      = "bold",
    fill          = alpha("grey90", 0.7),   # semi-transparent grey box
    label.size    = 0.2,                    # border thickness around box
    label.padding = unit(0.1, "lines"),     # padding inside box
    check_overlap = TRUE
  ) +
  scale_fill_viridis_c(
    option    = "D",
    name      = "Vulnerability",
    direction = 1,
    limits    = shared_limits   # add this
  ) +
  xlim(-80, -46) + ylim(40, 73) +
  theme_bw() +
  theme(axis.text = element_text(angle = 0, vjust = 0.2, hjust = 1, size = 8, family = "serif")) +
  labs(title = "b) Low emissions (SSP1-2.6)", x = "Longitude", y = "Latitude", color = "Vulnerability SSP5-8.5")
print(VMAP2)

library(grid)
library(gtable)

# Clean versions
VMAP_clean <- VMAP +
  theme(
    legend.position = "none",
    plot.margin     = margin(0, 0, 0, 0)   # no right margin to close the gap
  )

VMAP2_clean <- VMAP2 +
  theme(
    legend.position  = "right",
    axis.title.y     = element_blank(),
    axis.text.y      = element_blank(),
    axis.ticks.y     = element_blank(),
    plot.margin      = margin(0, 0, 0, 2)  # small left margin adds tiny gap
  ) +
  guides(fill = guide_colourbar(
    title          = "Vulnerability",
    barwidth       = unit(0.5, "cm"),
    barheight      = unit(6, "cm"),
    title.position = "top",
    title.hjust    = 0.5
  ))

g1 <- ggplotGrob(VMAP_clean)
g2 <- ggplotGrob(VMAP2_clean)

# Remove the y-axis column from g2 to eliminate the gap
# Find and zero out the axis-l (left axis) width in g2
g2$widths[g2$widths == max(g2$widths)] <- unit(2, "cm")

# Bind side by side
final_grob <- cbind(g1, g2, size = "first")

ggsave(
  filename = "CRIB results/NAFO_GH_5Zto0A_2scenarios.png",
  plot     = final_grob,
  width    = 10,
  height   = 5,
  dpi      = 400,
  bg       = "white"
)

# Save as RDS
saveRDS(final_grob, "CRIB results/NAFO_GH_5Zto0A_2scenarios.rds")

#################################################################################################
##################################### MAP Lobster ###############################################
#################################################################################################
#select only halibut data
halcrib<-read.csv("Data/CRIB/crib_amlobster.csv")

# Convert dataframe to an `sf` object
library(sf)
library(terra)
library(gstat)
halv<-halcrib[,c("longitude","latitude","ssp","Vulnerability")]
#haltsm<-halcrib[,c("longitude","latitude","ssp","S.Thermal.safety.margin")]
#haltoe<-halcrib[,c("longitude","latitude","ssp","ToE.year")]
#halcv<-halcrib[,c("longitude","latitude","ssp","E.Climate.velocity")]
#halthv<-halcrib[,c("longitude","latitude","ssp","AC.Thermal.habitat.availability")]
# Convert to a spatial object
spatial_points1 <- vect(halv[halv$ssp=="SSP5-8.5",], geom = c("longitude", "latitude"), crs = "WGS84")
# Create an empty raster (set resolution and extent as needed)
r <- rast(extent=spatial_points1, resolution = 0.25, crs = "WGS84") # Adjust resolution
# Rasterize the points into a grid
raster_data1 <- rasterize(spatial_points1, r, field = "Vulnerability", fun = mean)
spatial_points2 <- vect(halv[halv$ssp=="SSP1-2.6",], geom = c("longitude", "latitude"), crs = "WGS84")
# Create an empty raster (set resolution and extent as needed)
r <- rast(extent=spatial_points2, resolution = 0.25, crs = "WGS84") # Adjust resolution
# Rasterize the points into a grid
raster_data2 <- rasterize(spatial_points2, r, field = "Vulnerability", fun = mean)

#plot(raster_data)

#add halibut vulnerability rasters to NAFO map
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis) 
library(cowplot)
raster_df<-as.data.frame(raster_data1, xy=TRUE)
colnames(raster_df) <- c("longitude", "latitude", "Vulnerability") 

raster_df2<-as.data.frame(raster_data2, xy=TRUE)
colnames(raster_df2) <- c("longitude", "latitude", "Vulnerability")

# ── Compute shared vulnerability range across both rasters ────────────────────
shared_limits <- c(
  min(raster_df$Vulnerability,  raster_df2$Vulnerability, na.rm = TRUE),
  max(raster_df$Vulnerability,  raster_df2$Vulnerability, na.rm = TRUE)
)

NAFO2<-NAFO[NAFO$ZONE!="6A",]

VMAP <- ggplot() +
  geom_raster(data = raster_df, aes(x = longitude, y = latitude, fill = `Vulnerability`)) +
  #geom_sf(data = All_region_df, fill = NA) +
  geom_sf(data = NAFO2, color = "darkgrey", size = 0.6, fill = NA) +
  geom_sf(data = land, fill = "cornsilk") +
  geom_sf(data = EEZ, color = "lightblue", linetype = "dashed", size = 1) +
  geom_sf(data = Hague, color = "red", size = 0.8) +
  geom_sf_label(
    data          = NAFO2,
    aes(label     = ZONE),
    size          = 2.5,
    colour        = "black",
    fontface      = "bold",
    fill          = alpha("grey90", 0.7),   # semi-transparent grey box
    label.size    = 0.2,                    # border thickness around box
    label.padding = unit(0.1, "lines"),     # padding inside box
    check_overlap = TRUE
  ) +
  scale_fill_viridis_c(
    option    = "D",
    name      = "Vulnerability",
    direction = 1,
    limits    = shared_limits   # add this
  ) +
  xlim(-80, -46) + ylim(40, 55) +
  theme_bw() +
  theme(axis.text = element_text(angle = 0, vjust = 0.2, hjust = 1, size = 8, family = "serif")) +
  labs(title = "a) High emissions (SSP5-8.5)", x = "Longitude", y = "Latitude", color = "Vulnerability SSP5-8.5")
print(VMAP) 

VMAP2 <- ggplot() +
  geom_raster(data = raster_df2, aes(x = longitude, y = latitude, fill = `Vulnerability`)) +
  #geom_sf(data = All_region_df, fill = NA) ++
  geom_sf(data = NAFO2, color = "darkgrey", size = 0.6, fill = NA) +
  geom_sf(data = land, fill = "cornsilk") +
  geom_sf(data = EEZ, color = "lightblue", linetype = "dashed", size = 1) +
  geom_sf(data = Hague, color = "red", size = 0.8)+
  geom_sf_label(
    data          = NAFO2,
    aes(label     = ZONE),
    size          = 2.5,
    colour        = "black",
    fontface      = "bold",
    fill          = alpha("grey90", 0.7),   # semi-transparent grey box
    label.size    = 0.2,                    # border thickness around box
    label.padding = unit(0.1, "lines"),     # padding inside box
    check_overlap = TRUE
  ) +
  scale_fill_viridis_c(
    option    = "D",
    name      = "Vulnerability",
    direction = 1,
    limits    = shared_limits   # add this
  ) +
  xlim(-80, -46) + ylim(40, 55) +
  theme_bw() +
  theme(axis.text = element_text(angle = 0, vjust = 0.2, hjust = 1, size = 8, family = "serif")) +
  labs(title = "b) Low emissions (SSP1-2.6)", x = "Longitude", y = "Latitude", color = "Vulnerability SSP5-8.5")
print(VMAP2)

library(grid)
library(gtable)

# Clean versions
VMAP_clean <- VMAP +
  theme(
    legend.position = "none",
    plot.margin     = margin(0, 0, 0, 0)   # no right margin to close the gap
  )

VMAP2_clean <- VMAP2 +
  theme(
    legend.position  = "right",
    axis.title.y     = element_blank(),
    axis.text.y      = element_blank(),
    axis.ticks.y     = element_blank(),
    plot.margin      = margin(0, 0, 0, 2)  # small left margin adds tiny gap
  ) +
  guides(fill = guide_colourbar(
    title          = "Vulnerability",
    barwidth       = unit(0.5, "cm"),
    barheight      = unit(6, "cm"),
    title.position = "top",
    title.hjust    = 0.5
  ))

g1 <- ggplotGrob(VMAP_clean)
g2 <- ggplotGrob(VMAP2_clean)

# Bind side by side
final_grob <- cbind(g1, g2, size = "first")

ggsave(
  filename = "CRIB results/NAFO_AmLobster_5Zto0A_2scenarios.png",
  plot     = final_grob,
  width    = 10,
  height   = 5,
  dpi      = 400,
  bg       = "white"
)

# Save as RDS
saveRDS(final_grob, "CRIB results/NAFO_AmLobster_5Zto0A_2scenarios.rds")

#################################################################################################
######################################## MAP Cod ################################################
#################################################################################################
#select only halibut data
halcrib<-read.csv("Data/CRIB/crib_atlcod.csv")

# Convert dataframe to an `sf` object
library(sf)
library(terra)
library(gstat)
halv<-halcrib[,c("longitude","latitude","ssp","Vulnerability")]
#haltsm<-halcrib[,c("longitude","latitude","ssp","S.Thermal.safety.margin")]
#haltoe<-halcrib[,c("longitude","latitude","ssp","ToE.year")]
#halcv<-halcrib[,c("longitude","latitude","ssp","E.Climate.velocity")]
#halthv<-halcrib[,c("longitude","latitude","ssp","AC.Thermal.habitat.availability")]
# Convert to a spatial object
spatial_points1 <- vect(halv[halv$ssp=="SSP5-8.5",], geom = c("longitude", "latitude"), crs = "WGS84")
# Create an empty raster (set resolution and extent as needed)
r <- rast(extent=spatial_points1, resolution = 0.25, crs = "WGS84") # Adjust resolution
# Rasterize the points into a grid
raster_data1 <- rasterize(spatial_points1, r, field = "Vulnerability", fun = mean)
spatial_points2 <- vect(halv[halv$ssp=="SSP1-2.6",], geom = c("longitude", "latitude"), crs = "WGS84")
# Create an empty raster (set resolution and extent as needed)
r <- rast(extent=spatial_points2, resolution = 0.25, crs = "WGS84") # Adjust resolution
# Rasterize the points into a grid
raster_data2 <- rasterize(spatial_points2, r, field = "Vulnerability", fun = mean)

#plot(raster_data)

#add halibut vulnerability rasters to NAFO map
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis) 
library(cowplot)
raster_df<-as.data.frame(raster_data1, xy=TRUE)
colnames(raster_df) <- c("longitude", "latitude", "Vulnerability") 

raster_df2<-as.data.frame(raster_data2, xy=TRUE)
colnames(raster_df2) <- c("longitude", "latitude", "Vulnerability")

# ── Compute shared vulnerability range across both rasters ────────────────────
shared_limits <- c(
  min(raster_df$Vulnerability,  raster_df2$Vulnerability, na.rm = TRUE),
  max(raster_df$Vulnerability,  raster_df2$Vulnerability, na.rm = TRUE)
)

NAFO2<-NAFO[NAFO$ZONE!="6A",]

VMAP <- ggplot() +
  geom_raster(data = raster_df, aes(x = longitude, y = latitude, fill = `Vulnerability`)) +
  #geom_sf(data = All_region_df, fill = NA) +
  geom_sf(data = NAFO2, color = "darkgrey", size = 0.6, fill = NA) +
  geom_sf(data = land, fill = "cornsilk") +
  geom_sf(data = EEZ, color = "lightblue", linetype = "dashed", size = 1) +
  geom_sf(data = Hague, color = "red", size = 0.8) +
  geom_sf_label(
    data          = NAFO2,
    aes(label     = ZONE),
    size          = 2.5,
    colour        = "black",
    fontface      = "bold",
    fill          = alpha("grey90", 0.7),   # semi-transparent grey box
    label.size    = 0.2,                    # border thickness around box
    label.padding = unit(0.1, "lines"),     # padding inside box
    check_overlap = TRUE
  ) +
  scale_fill_viridis_c(
    option    = "D",
    name      = "Vulnerability",
    direction = 1,
    limits    = shared_limits   # add this
  ) +
  xlim(-80, -46) + ylim(40, 70) +
  theme_bw() +
  theme(axis.text = element_text(angle = 0, vjust = 0.2, hjust = 1, size = 8, family = "serif")) +
  labs(title = "a) High emissions (SSP5-8.5)", x = "Longitude", y = "Latitude", color = "Vulnerability SSP5-8.5")
print(VMAP) 

VMAP2 <- ggplot() +
  geom_raster(data = raster_df2, aes(x = longitude, y = latitude, fill = `Vulnerability`)) +
  #geom_sf(data = All_region_df, fill = NA) ++
  geom_sf(data = NAFO2, color = "darkgrey", size = 0.6, fill = NA) +
  geom_sf(data = land, fill = "cornsilk") +
  geom_sf(data = EEZ, color = "lightblue", linetype = "dashed", size = 1) +
  geom_sf(data = Hague, color = "red", size = 0.8)+
  geom_sf_label(
    data          = NAFO2,
    aes(label     = ZONE),
    size          = 2.5,
    colour        = "black",
    fontface      = "bold",
    fill          = alpha("grey90", 0.7),   # semi-transparent grey box
    label.size    = 0.2,                    # border thickness around box
    label.padding = unit(0.1, "lines"),     # padding inside box
    check_overlap = TRUE
  ) +
  scale_fill_viridis_c(
    option    = "D",
    name      = "Vulnerability",
    direction = 1,
    limits    = shared_limits   # add this
  ) +
  xlim(-80, -46) + ylim(40, 70) +
  theme_bw() +
  theme(axis.text = element_text(angle = 0, vjust = 0.2, hjust = 1, size = 8, family = "serif")) +
  labs(title = "b) Low emissions (SSP1-2.6)", x = "Longitude", y = "Latitude", color = "Vulnerability SSP5-8.5")
print(VMAP2)

library(grid)
library(gtable)

# Clean versions
VMAP_clean <- VMAP +
  theme(
    legend.position = "none",
    plot.margin     = margin(0, 0, 0, 0)   # no right margin to close the gap
  )

VMAP2_clean <- VMAP2 +
  theme(
    legend.position  = "right",
    axis.title.y     = element_blank(),
    axis.text.y      = element_blank(),
    axis.ticks.y     = element_blank(),
    plot.margin      = margin(0, 0, 0, 2)  # small left margin adds tiny gap
  ) +
  guides(fill = guide_colourbar(
    title          = "Vulnerability",
    barwidth       = unit(0.5, "cm"),
    barheight      = unit(6, "cm"),
    title.position = "top",
    title.hjust    = 0.5
  ))

g1 <- ggplotGrob(VMAP_clean)
g2 <- ggplotGrob(VMAP2_clean)

# Bind side by side
final_grob <- cbind(g1, g2, size = "first")

ggsave(
  filename = "CRIB results/NAFO_AtlCod_5Zto0A_2scenarios.png",
  plot     = final_grob,
  width    = 10,
  height   = 5,
  dpi      = 400,
  bg       = "white"
)

# Save as RDS
saveRDS(final_grob, "CRIB results/NAFO_AtlCod_5Zto0A_2scenarios.rds")
