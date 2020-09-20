# ------------------------------------------------------------------------------------------------------
#
# Raster calculations and visualization of herbicide hotspots
# 
# Author: Egina Malaj, Ph.D.
#
# Part of the publication:
#
# Malaj, E., Liber, K., and Morrissey, C. (2020). Spatial distribution of agricultural pesticide use and 
# predicted wetland exposure in the Canadian Prairie Pothole Region. Sci. Total Environ. 718, 
# doi.org/10.1016/j.scitotenv.2019.134765.
#
# ------------------------------------------------------------------------------------------------------
#
lspg<- c("ggplot2","rgdal","raster","classInt","RColorBrewer","ggspatial")
lapply(lspg, require, character.only = TRUE)
#
# Data 
# 
# look at data file provided with the code
#
herb_kg<-read.csv("herbicide_kg.csv")
#
# Shapefile
prov<- readOGR("prov_bound.shp","prov_bound")
#
# Land use data
#
load(file="landUseRaster.RData")
#
#
applct<-cbind('herb'= herb_kg[-1])
#
#
prov@data <- merge(x=prov@data, y=applct, 
                   by.x="PRUID", by.y="herb.country", all.x=TRUE)
#
# Reprojected - the same as the land use file 
prov<- spTransform(prov, CRS("+proj=aea +lat_1=44.75 +lat_2=55.75 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
#
# Resolution 1000m x1000m (1kmx1km) - this is calculated based on nrow and ncols. Check out this link for the method to 
# calculate it.
# See: https://stackoverflow.com/questions/9542039/resolution-values-for-rasters-in-r
#
r <- raster(as(prov, "Spatial"), nrows=1440, ncols=1982)
#
as.integer(prov@data$PRUID)->prov@data$PRUID
rprov <- rasterize(prov, r, field='PRUID')
#
# 
# Convert raster into a raster brick wih 10 columns for each crop
crop_brck <- subs(rprov, data.frame(prov), by='PRUID', which=6:ncol(prov)) 
crop_brck
#
#
# Herbicide
h_bar <- overlay(crop_brck[["herb.Barley"]], lu_rs[["lu_barl"]], fun=function(x,y){(x*y)} )
h_can<-overlay(crop_brck[["herb.Canola"]], lu_rs[["lu_can"]], fun=function(x,y){(x*y)})
h_bea<-overlay(crop_brck[["herb.Beans"]], lu_rs[["lu_bean"]], fun=function(x,y){(x*y)})
h_pea<-overlay(crop_brck[["herb.Peas"]], lu_rs[["lu_pea"]], fun=function(x,y){(x*y)})
h_len<-overlay(crop_brck[["herb.Lentils"]], lu_rs[["lu_len"]], fun=function(x,y){(x*y)})
h_oat<-overlay(crop_brck[["herb.Oats"]], lu_rs[["lu_oat"]], fun=function(x,y){(x*y)})
h_whe<-overlay(crop_brck[["herb.Wheat"]], lu_rs[["lu_whe"]], fun=function(x,y){(x*y)})
h_whe2<-overlay(crop_brck[["herb.Wheat"]], lu_rs[["lu_whe2"]], fun=function(x,y){(x*y)})
h_cor<-overlay(crop_brck[["herb.Corn"]], lu_rs[["lu_corn"]], fun=function(x,y){(x*y)})
h_soy<-overlay(crop_brck[["herb.Soybeans"]], lu_rs[["lu_soy"]], fun=function(x,y){(x*y)})
#
#
# Put rasters files together - sum them up 
#
# Herb
h_pest <- overlay(h_bar, h_can, h_bea, h_pea, h_len, h_oat, h_whe, h_whe2, h_cor, h_soy, 
                  fun=function(x) sum(x, na.rm=TRUE))
#
# Prepare for Plotting
# 
# Change projection to utm zone 13 and NAD83 - Projection: Transverse_Mercator
# Check:https://mgimond.github.io/Spatial/coordinate-systems-in-r.html for guidance on how to find the proj4 symbols from 
# GIS syntax
#
aea.proj <-"+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
# 
# Extent - remove Peace River and zoom in the map
e<-extent(-200000,1250000,5000000,6200000)
#
# Fortify polygon for ggplotting
#
pol <- spTransform(prov, CRS(aea.proj))
#pol_crop<- crop(pol,e) # crop the map to the area of interest
pol_fort <- fortify(pol, region = 'PRUID')
#
# Herb
h_pest_p<-projectRaster(h_pest, crs = aea.proj)
h_pest_c<- crop(h_pest_p,e)
h_spdf<-as.data.frame(as(h_pest_c, "SpatialPixelsDataFrame"))
colnames(h_spdf) <- c("value", "x", "y")
h_map<-h_spdf[h_spdf$value>0,] 
#
#
values(h_pest_c)[values(h_pest_c) < 0] = NA
#
####
#### ----------------------------------------------------------------------------####
####
#### Plotting
####
#### ----------------------------------------------------------------------------####
#
#
# Create class intervals for the colours - Quantile works best for fungicides
# as quantiles are good for data with a skewed distribution
# kmeans - jenks are a special case - reduced the variability within classes and 
# maximizes the variance between classes.
#
#
brk_h <- classIntervals(h_map$value, n=5, style="kmeans")
X11()
pal1 <- rev(brewer.pal(5,"RdYlBu")) # colours code similar to the ones that will be displayed in the map
plot(classIntervals(h_map$value, n=5, style="kmeans"), cex.lab=1.5,cex.axis=1.5,cex.main=2.5,
     pal=pal1, main="", xlab= expression("Herbicide Use Intensity (kg/km" ^ 2 * ")"), ylab="Frequency Distribution")
text(x=0, y=0.95,"", pos=2, cex=2)
#
#
# Map of Canada for the plot grob
cadata <- getData("GADM", country = "canada", level = 1)
ca <- fortify(cadata)
#
ca$positive <- ifelse(ca$id==1|ca$id==4|ca$id==7, "Yes", "No")
#
pr<-ca[ca$positive=="Yes",]
#
# Prairies - ID: 1,3,14 or 1,4,7
ca1 <- ggplotGrob(
  ggplot() +
    geom_blank(data = ca, aes(x = long, y = lat)) +
    geom_map(data = ca, map = ca,
             aes(group = group, map_id = id),
             fill = "white", color = "black", size = 0.3)+
    geom_polygon(data = pr, aes(x=long, y=lat, group = group),fill="#b2b2b2", color = "black", size = 0.3)+
    theme_map()+
    theme(panel.background = element_rect(fill = NULL), panel.grid.major = element_blank())
)
#
#
#
####
#### ----------------------------------------------------------------------------
####
#### Herbicide
####
#### ----------------------------------------------------------------------------
#
#
# Theme for main plot
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "bottom",plot.title = element_text(size = 25, face = "bold"),
      legend.title=element_text(size=22),
      legend.text=element_text(size=18),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(), 
      panel.background = element_blank(), 
      legend.background = element_blank(),
      panel.border = element_blank(),
      ...
    )
}
#
#
h_lab<-c()
h_brks<- brk_h$brks
for(z in 1:length(h_brks)){
  h_lab <- c(h_lab,round(h_brks[z + 1], 0))
}
h_lab <- h_lab[1:length(h_lab)-1]
#
h_map$brks_h <- cut(h_map$value, 
                    breaks = h_brks, 
                    include.lowest = TRUE, 
                    labels = h_lab)
h_brks_scale <- levels(h_map$brks_h)
h_labels_scale <- rev(h_brks_scale)
#
h1<-
  ggplot() +  
  geom_tile(data=h_map, aes(x=x, y=y, fill=brks_h), alpha=0.8) + 
  geom_path(data=pol_fort, aes(x=long, y=lat, group=group), 
            alpha=1.0,size=0.5)+
  coord_fixed(xlim = c(-220000, 1150000), ylim = c(5450000, 6200000))+
  annotate("text", x = 0, y = 6220000, label = "Alberta", size = 7)+
  annotate("text", x = 450000, y = 6220000, label = "Saskatchewan", size = 7)+
  annotate("text", x = 950000, y = 6220000, label = "Manitoba", size = 7)+
  theme_map() +
  annotation_scale(location = "bl", width_hint = 0.15,
                   pad_x = unit(0.25, "cm"), pad_y = unit(0.045, "cm"), text_cex = 0.8)+
  annotation_north_arrow(location = "bl", 
                         pad_x = unit(-0.3, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_fancy_orienteering)+
  labs(x = NULL, 
       y = NULL) +
  scale_fill_manual(
    values = brewer.pal(5,"RdYlBu"),
    breaks = rev(h_brks_scale),
    name = expression(PUD[Herbicide]~(kg/km^2)),
    drop = FALSE,
    labels = h_labels_scale,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(5, units = "mm"),
      keywidth = unit(70 / length(h_lab), units = "mm"),
      title.position = 'left', # use 'top' for name on top 
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = T,
      label.position = "bottom"
    ))

h2<- h1 +
  annotation_custom(grob = ca1, xmin = 930000, xmax = 1230000,
                    ymin = 5900000, ymax = 6200000)
#
#
X11(width = 10, height = 7)
h2
#
#ggsave("Herbicide.tiff", width = 10, height = 7,plot = h2, dpi=500, compression = "lzw")






