#Plot 1: Map of Core areas 
  # make colours match CAPlot
  #re-do all folder connections
  #add new core areas
  #Increase box area 
#Plot 2: A look at the distribution of RV Survey data
  #add NF
  #Plot presence only 
library(sf)
library (ggplot2)
library(rnaturalearth)

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

library(sf)
library(ggrepel)

# Plot CoreArea
CAMAP<-ggplot() +
  #geom_sf(data = contours, color="lightblue") +
  #geom_sf(data = All_region_df,  fill = NA) +
  geom_sf(data = Hague, color="navy") +
  geom_sf(data = EEZ, color="navy", linetype = "dashed", size = 1.2) +
  geom_sf(data = NAFO, color="darkgrey", fill = NA) +
  geom_sf(data = land, fill="cornsilk") +
  #scale_fill_manual(values = region_colours)+
  #scale_fill_manual(name = " ", values = c("#E41A1C","#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628","#F781BF", "#999999")) +
  labs(title="")+
  xlim(-72.5, -48) + ylim(39.355, 50)+
  theme_bw()+
  theme(axis.text = element_text(angle = 0, vjust = 0.2, hjust=1,size=8,family="serif"))
CAMAP

### Add CRIB data ###
require("readr")
url <- "https://api-proxy.edh-cde.dfo-mpo.gc.ca/catalogue/records/264692e5-7b51-4ab9-bb1f-da65f6fc0875/attachments/EN_ClimateRiskIndex_Spatial-CanEEZ_145Spp.csv"
crib.df <- read_csv(url)
head(crib.df)
#write.csv(crib.df, "Data/CRIB/cribspeciesdata.csv") #file too large

#################################################################################################
##################################### HALIBUT ###################################################
#################################################################################################
#select only halibut data
halcrib <- crib.df[crib.df$`common name` == "Atlantic Halibut",]
head(halcrib)
str(halcrib)
#write.csv(halcrib, "Data/CRIB/crib_halibut.csv")
halcrib<-read.csv("Data/CRIB/crib_halibut.csv")
halcrib2<-read.csv("Data/CRIB/crib_greenland_halibut.csv")
halcrib$ToE.year<-2015+(-log(halcrib$E.Time.of.climate.emergence)/0.033) #calculate raw ToE's from standardized
halcrib2$ToE.year<-2015+(-log(halcrib2$E.Time.of.climate.emergence)/0.033) #calculate raw ToE's from standardized

# Convert dataframe to an `sf` object
library(sf)
library(terra)
library(gstat)
halv<-halcrib[,c("longitude","latitude","ssp","Vulnerability")]
halv2<-halcrib2[,c("longitude","latitude","ssp","Vulnerability")]
haltsm<-halcrib[,c("longitude","latitude","ssp","S.Thermal.safety.margin")]
haltoe<-halcrib[,c("longitude","latitude","ssp","ToE.year")]
halcv<-halcrib[,c("longitude","latitude","ssp","E.Climate.velocity")]
halthv<-halcrib[,c("longitude","latitude","ssp","AC.Thermal.habitat.availability")]
# Convert to a spatial object
spatial_points1 <- vect(halv[halv$ssp=="SSP5-8.5",], geom = c("longitude", "latitude"), crs = "WGS84")
# Create an empty raster (set resolution and extent as needed)
r <- rast(extent=spatial_points1, resolution = 0.25, crs = "WGS84") # Adjust resolution
# Rasterize the points into a grid
raster_data1 <- rasterize(spatial_points1, r, field = "Vulnerability", fun = mean)
spatial_points2 <- vect(halv2[halv2$ssp=="SSP5-8.5",], geom = c("longitude", "latitude"), crs = "WGS84")
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
VMAP <- ggplot() +
  geom_raster(data = raster_df, aes(x = longitude, y = latitude, fill = `Vulnerability`)) +
  #geom_sf(data = All_region_df, fill = NA) +
  geom_sf(data = Hague, color = "navy") +
  geom_sf(data = land, fill = "cornsilk") +
  geom_sf(data = EEZ, color = "lightblue", linetype = "dashed", size = 1) +
  geom_sf(data = NAFO, color = "darkgrey", size = 0.9, fill = NA) +
  geom_sf_label(
    data          = NAFO,
    aes(label     = ZONE),
    size          = 4,
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
    direction = 1
  ) +
  xlim(-80, -46) + ylim(39.6, 68) +
  theme_bw() +
  theme(axis.text = element_text(angle = 0, vjust = 0.2, hjust = 1, size = 8, family = "serif")) +
  labs(title = "a) NAFO Divisions", x = "Longitude", y = "Latitude", color = "Vulnerability SSP5-8.5")
print(VMAP) 

regions <- st_read(here::here("", "Data/Mapping_shapefiles/FederalMarineBioregions_SHP/FederalMarineBioregions.shp"))
regions1 <- regions[regions$LABEL != "999", ]
raster_df2<-as.data.frame(raster_data2, xy=TRUE)
colnames(raster_df2) <- c("longitude", "latitude", "Vulnerability") 
VMAP2 <- ggplot() +
  geom_raster(data = raster_df2, aes(x = longitude, y = latitude, fill = `Vulnerability`)) +
  #geom_sf(data = All_region_df, fill = NA) +
  geom_sf(data = Hague, color = "navy") +
  geom_sf(data = land_canada, fill = "cornsilk") +
  geom_sf(data = regions1, color = "darkgrey", size = 0.9, fill = NA) +
  geom_sf(data = EEZ, color = "lightblue", linetype = "dashed", size = 1) +
  geom_sf_label(
    data          = regions1,
    aes(label     = LABEL),
    size          = 4,
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
    direction = 1
  ) +
  xlim(-138, -50) + ylim(42, 82) +
  theme_bw() +
  theme(axis.text = element_text(angle = 0, vjust = 0.2, hjust = 1, size = 8, family = "serif")) +
  labs(title = "b) Marine Bioregions", x = "Longitude", y = "Latitude", color = "Vulnerability SSP5-8.5")
print(VMAP2)

# Clean versions — just remove legends, no extra layers
VMAP_clean <- VMAP +
  theme(
    legend.position  = "none",
    axis.title.y       = element_text(size = 14),
    plot.title       = element_text(size = 15)
  )

VMAP2_clean <- VMAP2 +
  theme(
    legend.position = "none",
    axis.title.y    = element_blank(),
    plot.title       = element_text(size = 15),
    plot.margin     = margin(5, 0, 5, -15)  # negative left margin pulls it closer to VMAP
  )

# Extract shared legend
shared_legend <- get_legend(
  VMAP +
    theme(
      legend.position = "right",
      legend.title    = element_text(size = 11),
      legend.text     = element_text(size = 9),
      legend.margin   = margin(0, 0, 0, -15)   # negative left margin pulls legend left
    ) +
    guides(fill = guide_colourbar(
      title          = "Vulnerability",
      barwidth       = unit(0.5, "cm"),
      barheight      = unit(6, "cm"),
      title.position = "top",
      title.hjust    = 0.5
    ))
)

# Combine maps side-by-side
maps_row <- plot_grid(
  VMAP_clean,
  VMAP2_clean,
  ncol       = 2,
  align      = "h",
  rel_widths = c(1, 1.4)
)

# Add shared legend to the right
final_map <- plot_grid(
  maps_row,
  shared_legend,
  ncol       = 2,
  rel_widths = c(1, 0.13)
)

print(final_map)
# ── Save ──────────────────────────────────────────────────────────────────────
ggsave(
  filename = "CRIB results/NAFO_and_Bioregions.png",
  plot     = final_map,
  width    = 10,
  height   = 5,
  dpi      = 400,
  bg       = "white"
)

message("Done! Plot saved")

### Calculate average + SD for each indicator in zones 4X, 4VW, and 3KL:###
library(sf)      # For spatial data handling
library(dplyr)   # For summarizing grouped data
library(here)
# Convert halcrib data frame to a spatial object
halcrib1 <- st_as_sf(halcrib, coords = c("longitude", "latitude"), crs = "WGS84")
# Reproject both datasets to align their CRS (e.g., EPSG:4326)
halcrib1 <- st_transform(halcrib1, crs = st_crs(NAFO))
# Perform a spatial join to add the NAFO zone info to each point in halcrib
halcrib_with_zones <- st_join(halcrib1, NAFO, na.rm=TRUE)

# Check the first few rows of the resulting dataset
head(halcrib_with_zones)
names(halcrib_with_zones)
unique(halcrib_with_zones$ZONE)
# Remove rows where NAFO_ID or ZONE are NA
halcrib_with_zones_clean <- halcrib_with_zones[] %>%
  filter(!is.na(NAFO_ID), !is.na(ZONE))
unique(halcrib_with_zones_clean$ZONE)
head(halcrib_with_zones_clean)

# Add new NAFO groups
halcrib_with_zones_clean1 <- halcrib_with_zones_clean %>%
  mutate(
    NAFO_Zones = case_when(
      ZONE %in% c("4Vn", "4Vs", "4W") ~ "4VW",    # If ZONE is one of these values, assign "4VW"
      ZONE %in% c("4X") ~ "4X",
      ZONE %in% c("3N","3O","3Pn","3Ps") ~ "3NOPs",
      ZONE %in% c("3K","3L") ~ "3KL",
      ZONE %in% c("2J","2H","2G") ~ "2JHG",
      ZONE %in% c("5Y","5Ze", "5Zw", "6A") ~ "5YZ6A",
      ZONE %in% c("4R","4S","4T") ~ "4RST"
    )
  )%>%
  filter(!is.na(NAFO_Zones))  # Remove rows where NAFO_Zones is NA
head(halcrib_with_zones_clean1)


####################################
### Calculate summary statistics ###
####################################
summary_stats <- halcrib_with_zones_clean1 %>%
  group_by(NAFO_Zones, ssp) %>%
  summarise(
    mean_vulnerability = mean(Vulnerability, na.rm = TRUE),
    sd_vulnerability = sd(Vulnerability, na.rm = TRUE),
    mean_s_thermal_safety_margin = mean(`S.Thermal.safety.margin`, na.rm = TRUE),
    sd_s_thermal_safety_margin = sd(`S.Thermal.safety.margin`, na.rm = TRUE),
    ci_s_thermal_safety_margin = 1.96 * (sd(S.Thermal.safety.margin, na.rm = TRUE) / sqrt(n())),
    mean_e_climate_velocity = mean(`E.Climate.velocity`, na.rm = TRUE),
    sd_e_climate_velocity = sd(`E.Climate.velocity`, na.rm = TRUE),
    mean_yr_climate_emergence = mean(`ToE.year`, na.rm = TRUE),
    sd_yr_climate_emergence = sd(`ToE.year`, na.rm = TRUE),
    ci_yr_climate_emergence = 1.96 * (sd(ToE.year, na.rm = TRUE) / sqrt(n())),
    mean_e_time_climate_emergence = mean(`E.Time.of.climate.emergence`, na.rm = TRUE),
    sd_e_time_climate_emergence = sd(`E.Time.of.climate.emergence`, na.rm = TRUE),
    ci_e_time_climate_emergence = 1.96 * (sd(E.Time.of.climate.emergence, na.rm = TRUE) / sqrt(n())),
    mean_ac_thermal_habitat_variability = mean(`AC.Thermal.habitat.availability`, na.rm = TRUE),
    sd_ac_thermal_habitat_variability = sd(`AC.Thermal.habitat.availability`, na.rm = TRUE),
    ci_ac_thermal_habitat_variability = 1.96 * (sd(AC.Thermal.habitat.availability, na.rm = TRUE) / sqrt(n()))
  )

# View the resulting summarized data
print(summary_stats)
head(summary_stats)
# Remove geometry column
non_spatial_data <- st_drop_geometry(summary_stats)
head(non_spatial_data)
df<-as.data.frame(non_spatial_data)
write.csv(df, "CRIB results/crib_AtlHalibut_byNAFO_SSPs.csv", row.names = FALSE)

# Group halcrib_with_zones by NAFO zone and calculate summary risks
risk_summary <- halcrib_with_zones_clean1 %>%
  group_by(NAFO_Zones, ssp, Overall.Risk) %>% # Group by NAFO_Zones, SSP, and Overall.Risk
  summarise(
    count = n(),                              # Count the number of rows in each group
    .groups = "drop_last"                     # Keep grouping by NAFO_Zones and ssp
  ) %>%
  mutate(
    percentage = (count / sum(count)) * 100  # Calculate percentage for each Overall.Risk
  ) %>%
  ungroup() # Ungroup to return ungrouped data

# View the summary
print(risk_summary)

# Remove geometry column
non_spatial_data <- st_drop_geometry(risk_summary)
head(non_spatial_data)
non_spatial_data<-as.data.frame(non_spatial_data)
write.csv(non_spatial_data, "CRIB results/crib_risksummary_AtlHalibut_byNAFO_SSPs.csv", row.names = FALSE)

#################################################################################################
##################################### Greenland halibut ###################################################
#################################################################################################
#select Greenland halibut data
unique(crib.df$`common name`)
halcrib <- crib.df[crib.df$`common name` == "Greenland Halibut",]
head(halcrib)
str(halcrib)
#write.csv(halcrib, "Data/CRIB/crib_greenland_halibut.csv")
halcrib<-read.csv("Data/CRIB/crib_greenland_halibut.csv")
halcrib$ToE.year<-2015+(-log(halcrib$E.Time.of.climate.emergence)/0.033) #calculate raw ToE's from standardized

# Convert dataframe to an `sf` object
library(sf)
library(terra)
library(gstat)
halv<-halcrib[,c("longitude","latitude","ssp","Vulnerability")]
haltsm<-halcrib[,c("longitude","latitude","ssp","S.Thermal.safety.margin")]
haltoe<-halcrib[,c("longitude","latitude","ssp","ToE.year")]
halcv<-halcrib[,c("longitude","latitude","ssp","E.Climate.velocity")]
halthv<-halcrib[,c("longitude","latitude","ssp","AC.Thermal.habitat.availability")]
# Convert to a spatial object
spatial_points1 <- vect(haltoe[haltoe$ssp=="SSP1-2.6",], geom = c("longitude", "latitude"), crs = "WGS84")
# Create an empty raster (set resolution and extent as needed)
r <- rast(extent=spatial_points1, resolution = 0.25, crs = "WGS84") # Adjust resolution
# Rasterize the points into a grid
raster_data1 <- rasterize(spatial_points1, r, field = "ToE.year", fun = mean)
spatial_points2 <- vect(haltoe[haltoe$ssp=="SSP5-8.5",], geom = c("longitude", "latitude"), crs = "WGS84")
# Create an empty raster (set resolution and extent as needed)
r <- rast(extent=spatial_points2, resolution = 0.25, crs = "WGS84") # Adjust resolution
# Rasterize the points into a grid
raster_data2 <- rasterize(spatial_points2, r, field = "ToE.year", fun = mean)

#plot(raster_data)

#add halibut vulnerability rasters to NAFO map
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis) 
raster_df<-as.data.frame(raster_data1, xy=TRUE)
colnames(raster_df) <- c("longitude", "latitude", "ToE.year") 
VMAP <- ggplot() +
  geom_raster(data = raster_df, aes(x = longitude, y = latitude, fill = `ToE.year`)) +
  geom_sf(data = All_region_df, fill = NA) +
  geom_sf(data = Hague, color = "navy") +
  geom_sf(data = EEZ, color = "navy", linetype = "dashed", size = 1.2) +
  geom_sf(data = NAFO, color = "darkgrey", size=0.9, fill = NA) +
  geom_sf(data = land, fill = "cornsilk") +
  scale_fill_viridis_c(
    option = "D", # Keep the "D" viridis colour palette
    name = "Time of Climate Emergence",
    direction = -1 # Reverse the direction of the colour scale
  ) +
  xlim(-72.5, -45) + ylim(39, 60) +
  theme_bw() +
  theme(axis.text = element_text(angle = 0, vjust = 0.2, hjust = 1, size = 8, family = "serif")) +
  labs(title = "Time of Climate Emergence SSP1-2.6", x = "Longitude", y = "Latitude", color = "Time of Climate Emergence")
print(VMAP) 

### Calculate average + SD for each indicator in zones 4X, 4VW, and 3KL:###
library(sf)      # For spatial data handling
library(dplyr)   # For summarizing grouped data
library(here)
# Convert halcrib data frame to a spatial object
halcrib1 <- st_as_sf(halcrib, coords = c("longitude", "latitude"), crs = "WGS84")
# Reproject both datasets to align their CRS (e.g., EPSG:4326)
halcrib1 <- st_transform(halcrib1, crs = st_crs(NAFO))
# Perform a spatial join to add the NAFO zone info to each point in halcrib
halcrib_with_zones <- st_join(halcrib1, NAFO, na.rm=TRUE)

# Check the first few rows of the resulting dataset
head(halcrib_with_zones)
names(halcrib_with_zones)
unique(halcrib_with_zones$ZONE)
# Remove rows where NAFO_ID or ZONE are NA
halcrib_with_zones_clean <- halcrib_with_zones[] %>%
  filter(!is.na(NAFO_ID), !is.na(ZONE))
unique(halcrib_with_zones_clean$ZONE)
head(halcrib_with_zones_clean)

# Add new NAFO groups
halcrib_with_zones_clean1 <- halcrib_with_zones_clean %>%
  mutate(
    NAFO_Zones = case_when(
      ZONE %in% c("4Vn", "4Vs", "4W") ~ "4VW",    # If ZONE is one of these values, assign "4VW"
      ZONE %in% c("4X") ~ "4X",
      ZONE %in% c("3N","3O","3Pn","3Ps") ~ "3NOPs",
      ZONE %in% c("3K","3L") ~ "3KL",
      ZONE %in% c("2J","2H","2G") ~ "2JHG",
      ZONE %in% c("5Y","5Ze", "5Zw", "6A") ~ "5YZ6A",
      ZONE %in% c("4R","4S","4T") ~ "4RST"
    )
  )
head(halcrib_with_zones_clean1)

# Group halcrib_with_zones by NAFO zone and calculate summary statistics
summary_stats <- halcrib_with_zones_clean1 %>%
  group_by(NAFO_Zones, ssp) %>%
  summarise(
    mean_vulnerability = mean(Vulnerability, na.rm = TRUE),
    sd_vulnerability = sd(Vulnerability, na.rm = TRUE),
    mean_s_thermal_safety_margin = mean(`S.Thermal.safety.margin`, na.rm = TRUE),
    sd_s_thermal_safety_margin = sd(`S.Thermal.safety.margin`, na.rm = TRUE),
    ci_s_thermal_safety_margin = 1.96 * (sd(S.Thermal.safety.margin, na.rm = TRUE) / sqrt(n())),
    mean_e_climate_velocity = mean(`E.Climate.velocity`, na.rm = TRUE),
    sd_e_climate_velocity = sd(`E.Climate.velocity`, na.rm = TRUE),
    mean_yr_climate_emergence = mean(`ToE.year`, na.rm = TRUE),
    sd_yr_climate_emergence = sd(`ToE.year`, na.rm = TRUE),
    ci_yr_climate_emergence = 1.96 * (sd(ToE.year, na.rm = TRUE) / sqrt(n())),
    mean_e_time_climate_emergence = mean(`E.Time.of.climate.emergence`, na.rm = TRUE),
    sd_e_time_climate_emergence = sd(`E.Time.of.climate.emergence`, na.rm = TRUE),
    ci_e_time_climate_emergence = 1.96 * (sd(E.Time.of.climate.emergence, na.rm = TRUE) / sqrt(n())),
    mean_ac_thermal_habitat_variability = mean(`AC.Thermal.habitat.availability`, na.rm = TRUE),
    sd_ac_thermal_habitat_variability = sd(`AC.Thermal.habitat.availability`, na.rm = TRUE),
    ci_ac_thermal_habitat_variability = 1.96 * (sd(AC.Thermal.habitat.availability, na.rm = TRUE) / sqrt(n()))
  )

# View the resulting summarized data
head(summary_stats)
# Remove geometry column
non_spatial_data <- st_drop_geometry(summary_stats)
head(non_spatial_data)
df<-as.data.frame(non_spatial_data)
write.csv(df, "CRIB results/crib_GreenlandHalibut_byNAFO_SSPs.csv", row.names = FALSE)

# Group halcrib_with_zones by NAFO zone and calculate summary risks
risk_summary <- halcrib_with_zones_clean1 %>%
  group_by(NAFO_Zones, ssp, Overall.Risk) %>% # Group by NAFO_Zones, SSP, and Overall.Risk
  summarise(
    count = n(),                              # Count the number of rows in each group
    .groups = "drop_last"                     # Keep grouping by NAFO_Zones and ssp
  ) %>%
  mutate(
    percentage = (count / sum(count)) * 100  # Calculate percentage for each Overall.Risk
  ) %>%
  ungroup() # Ungroup to return ungrouped data

# View the summary
print(risk_summary)

# Remove geometry column
non_spatial_data <- st_drop_geometry(risk_summary)
head(non_spatial_data)
non_spatial_data<-as.data.frame(non_spatial_data)
write.csv(non_spatial_data, "CRIB results/crib_risksummary_GreenlandHalibut_byNAFO_SSPs.csv", row.names = FALSE)
