
# 19jun2024 LMY
#
# PURPOSE: Plot elevation and soil type, along with selected NDVI and yield maps.
# NDVI images are computed from Sentinel-2 data taken around the time the regular
# cash crop was planted, and they illustrate the location of cover crops.  These 
# specific examples show cases in which we had cover crops on part of the field, allowing
# us to measure yields in the cover area and in a control (the rest of the field).
# The code also plots a timeseries showing the yield impact of the cover crops.

# TO DO:
# - convert from raster to terra.  looks like sf is ok, but raster will go away.
# - are there better color schemes?


# ------------------- functions, definitions, setup ----------------------

library(tidyverse)
library(janitor)
library(sf)
library(raster)
library(RColorBrewer)
library(gridExtra)
library(scales)

readsatdata <- function(date, myraster) {
   # Satellite NDVI data from `date` will be read and cropped to the extent of myraster
   redname <- paste('../satellite data/datadirs/',date,'_EPSG32616/',date,'-Sentinel-2_L2A_B04_Raw.tiff',sep='')
   nirname <- paste('../satellite data/datadirs/',date,'_EPSG32616/',date,'-Sentinel-2_L2A_B08_Raw.tiff',sep='')
   nir <- raster(nirname)
   red <- raster(redname)
   ndvi <- (nir-red)/(nir+red)
   ndvi <- raster::shift(ndvi, dx=-5, dy=12) # units: m. satellite-based coords often need a little tweak
   ndvi_crop <- raster::crop(ndvi, myraster)  # copies the extent info from the yield file
   ndvi_crop_df <- as.data.frame(ndvi_crop, xy=TRUE) %>%
      rename(NDVI='layer')
   return(ndvi_crop_df)
}

readyielddata <- function(filetoread, target_raster, aoi){
   print(filetoread)
   data <- sf::read_sf(filetoread) %>%
      dplyr::select('Yld_Vol_Dr') %>%
      mutate(yldnorm = Yld_Vol_Dr/median(Yld_Vol_Dr)) %>%
      mutate(AOI_flag = 0.0)  %>%   # fill later
      sf::st_transform(crs=st_crs(target_raster))
   # select data points within the area of interest (cover crop region)
   blah <- st_within(data, aoi$geometry[1]) %>% lengths > 0
   data[blah, 'AOI_flag'] <- 1.0 # points where there was cover crop in 2018 and 2019
   data_ras <- rasterize(x=data, y=target_raster, field=c('yldnorm','AOI_flag'), fun=median, na.rm=TRUE)
   # 3x3 boxcar smoothing is not bad, maybe helpful
   data_ras$yldnorm <- focal(data_ras$yldnorm, w3, median, na.rm=TRUE, pad=TRUE) # NAonly=TRUE if you don't want to touch most of the original values
   data_ras_df <- as.data.frame(data_ras, xy=TRUE) 
   data_ras_df$AOI_flag <- as.factor(data_ras_df$AOI_flag) # most values will be 0 or 1; occasionally 0.5
   return(data_ras_df)
}

plotndvi <- function(ndvi_crop_df, satdate, ccname){
   p3 <- ggplot(ndvi_crop_df) +
      geom_raster(aes(x=x, y=y, fill=NDVI)) +
      scale_fill_viridis_c(direction=-1, label = function(x) sprintf("%.1f", x)) +  # magic incantation to control formatting on colorbar labels
      # geom_sf(data=cropsoilshapes, fill=NA, linewidth=0.5) +
      labs(x='', y='', title=paste(satdate,ccname, sep=' ')) + 
      theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
      theme(legend.margin = margin(0, 0, 0, 0)) +
      coord_sf()
   return(p3)
}

plotyielddata <- function(yield_ras_df, harvestyear, cashcrop) {
   p4 <- ggplot(yield_ras_df) +
      geom_raster(aes(x=x, y=y, fill=yldnorm)) +
      scale_fill_distiller(palette='YlGn', limits=c(0.85,1.15), oob=squish, direction='horizontal') + 
      {if (!is.null(aoi)) geom_sf(data=aoi, fill=NA, linewidth=1., color='blue', linetype='21')} +
      labs(x='', y='', title=paste(harvestyear,cashcrop,sep=' ')) +
      guides(fill = guide_colorbar(title='Yield')) +
      theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
      theme(legend.margin = margin(0, 0, 0, 0)) +
      # geom_sf(data=cropsoilshapes, fill=NA, linewidth=0.5) +
      coord_sf(crs=st_crs(elev_ras))
   return(p4)
}


median.function <- function(x, index) {
   # used by bootstrapping routine
   d <- x[index]     
   return(median(d, na.rm=TRUE))  
}


setwd(this.path::here())

w5 <- matrix(1, 5, 5)  # for smoothing.  
w3 <- matrix(1, 3, 3)  # if you want to skip the corners, use c(0,1,0,1,0,1,0,1,0), nrow=3

mycolors <- c("dodgerblue","green","orange",'gold','orchid','pink')
names(mycolors) <- c('152A', '154A', '171B', '198A', '679B', '56B')
mapkeycolors <- c("dodgerblue","green","orange",'gold','orchid','pink')
names(mapkeycolors) <- c(242963, 242965, 242966, 242997, 243024, 242969)

# ----------------- plot elevation and soil type ---------------------

# elevation files are actually just regular yield monitor files from a year that had good elevation data.
# soil type files come from SSURGO data.
elevfile <- '../elevation+soiltype data/HarrysS80_2022.shp'
soiltypefile <- '../elevation+soiltype data/HarrysS80 Soil Types.shp'

# read elevation and soil type data
elevdata <- sf::read_sf(elevfile) %>%
   dplyr::select(c('Elevation_','Yld_Vol_Dr')) %>%
   rename(elevation='Elevation_') %>%
   sf::st_transform(crs='EPSG:32616')   # x and y units will be m from now on
soilshapes <- sf::read_sf(soiltypefile) %>%
   sf::st_transform(crs=st_crs(elevdata))
soilshapes$Name <- as.factor(soilshapes$Name)
# crop soil type polygons to the extent of the raster data
cropsoilshapes <- st_crop(soilshapes, extent(elevdata))
# clean up: this elevation file has a few bad data points
elevdata$elevation <- replace(elevdata$elevation, elevdata$elevation<685.3, NA)
# rasterize and smooth elevation
r <- raster(extent(elevdata), res=7) # units: m. res=0.00007 degrees works well too
raster::crs(r) <- crs(elevdata)
elev_ras <- rasterize(elevdata, r, field='elevation', fun=median, na.rm=TRUE)
elev_ras_sm <- focal(elev_ras, w5, median, na.rm=TRUE, pad=TRUE)

# plot elevation
elev_ras_df <- as.data.frame(elev_ras_sm, xy=TRUE) %>%
   rename(Elev='layer')
p1 <- ggplot(elev_ras_df) +
   geom_raster(aes(x=x, y=y, fill=Elev)) +
   scale_fill_viridis_c(label = function(x) sprintf("%.0f", x)) +
   coord_sf() +
   guides(fill = guide_colorbar('Elev\nft')) +
   geom_sf(data=cropsoilshapes, fill=NA, linewidth=0.5) +
   theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
   labs(x='', y='')
# plot soil type polygons
p2 <- ggplot() +
   geom_sf(data=cropsoilshapes, aes(fill=Name)) +
   scale_fill_manual(values = alpha(mycolors, 0.5)) + 
   guides(fill = guide_legend('Soil\nType')) +
   theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
   coord_sf()

# -----------------  plot NDVI and yield maps -----------------------

aoi <- sf::read_sf('../satellite data/regions of interest/HarrysS80_2018_AOI.shp') %>%
   sf::st_transform(crs=st_crs(elevdata))   

satdate <- '2018-04-11'
ccname <- 'rye'
ndvi_crop_df <- readsatdata(satdate, elev_ras)
p3 <- plotndvi(ndvi_crop_df, satdate, ccname)

ylddir <- 'linktoSMSData/Harrys S80/Harrys S80/'
yldfile <- paste(ylddir, 'Grain Harvest_2018/Kids s80_P37T09L-PB46_1.shp', sep='')
harvestyear <- '2018'
cashcrop <- 'soy'
yield_ras_df <- readyielddata(yldfile, elev_ras, aoi)
p4 <- plotyielddata(yield_ras_df, harvestyear, cashcrop)

# 2nd NDVI
satdate <- '2019-04-26'
ccname <- 'rye'
ndvi_crop_df <- readsatdata(satdate, elev_ras)
p5 <- plotndvi(ndvi_crop_df, satdate, ccname)

# 2nd yield
yldfile <- paste(ylddir, 'Grain Harvest_2019/Kids s80_P1306W_1.shp', sep='')
harvestyear <- '2019'
cashcrop <- 'corn'
yield_ras_df <- readyielddata(yldfile, elev_ras, aoi)
p6 <- plotyielddata(yield_ras_df, harvestyear, cashcrop)

# end: wrap up, save figure
bigfig <- grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2, top='NDVI and yield maps')
ggsave('NDVI+yield.pdf', bigfig)
ggsave('NDVI+yield.png', bigfig)


# ----------------- timeseries on cover crop yield impact -----------------

# see info in yield_experiments_3.R

yieldfiles <- c(
   "Grain Harvest_2010/Harry's S80_2YC_1.shp",
   "Grain Harvest_2011/Harry's S80_P93Y41 Paren_1.shp",
   "Grain Harvest_2014/Harry's S80_P93Y41 P 2200052_1.shp",
   "Grain Harvest_2015/Harry's S80_P 1221 AMXT_1.shp",
   "Grain Harvest_2016/Harry's S80_IP 3902_1.shp",
   'Grain Harvest_2017/kids S80_P1257AMXT_1.shp',
   'Grain Harvest_2018/Kids s80_P37T09L-PB46_1.shp',
   'Grain Harvest_2019/Kids s80_P1306W_1.shp',
   'Grain Harvest_2020/S80_5035ax06-02_1.shp',
   'Grain Harvest_2021/Kids s80_P1306W_1.shp',
   'Grain Harvest_2022/Harry_s S80_PE3401_1.shp',
   'Grain Harvest_2023/harvest.shp'
)
crops <- c('corn',
   'soy',
   'soy',
   'corn',
   'soy',
   'corn',
   'soy',
   'corn',
   'soy',
   'corn',
   'soy',
   'corn'
)

timeseries <- NULL # initialize

#  loop over multiple years for one field 
for (i in 1:length(yieldfiles)){
   
   filename <- paste(ylddir, yieldfiles[i], sep='')
   blah <- str_split_fixed(filename,'/',n=5)
   year <- str_split_fixed(blah[[4]],'_',n=2)[[2]]
   
   yield_ras_df <- readyielddata(filename, elev_ras, aoi)

   # compute medians for the yields in the treatment areas
   rastmeds <- yield_ras_df %>%
      drop_na() %>%
      group_by(AOI_flag) %>%
      dplyr::summarize(medyld = median(yldnorm, na.rm=TRUE),
                       stdev = sd(yldnorm, na.rm=TRUE))
   m1 <- rastmeds$medyld[rastmeds$AOI_flag==1.0][1]
   m2 <- rastmeds$medyld[rastmeds$AOI_flag==0.0][1]
   # estimate uncertainty on median from bootstrapping
   cover <- filter(yield_ras_df, AOI_flag==1.0)
   control <- filter(yield_ras_df, AOI_flag==0.0)
   BootDist_cov <- boot(data = cover$yldnorm, statistic = median.function, R=5000)
   BootDist_con <- boot(data = control$yldnorm, statistic = median.function, R=5000)
   BootDist_rat <- BootDist_cov$t/BootDist_con$t # by hand distribution mimicking the ratio
   range_rat <- quantile(BootDist_rat, probs=c(.025, .975)) #  95%
   newresults <- data.frame(year = as.numeric(year), 
                            cashcrop = crops[i], 
                            med_cover = m1,
                            med_control = m2,
                            ratio = m1/m2,
                            rat_lo = range_rat[1],  # from bootstrapping
                            rat_hi = range_rat[2] )
   timeseries <- rbind(timeseries, newresults)
   
}   # end of loop over multiple years for one field

# plot timeseries
barmean <- mean(timeseries$ratio)
barwidth <- sd(timeseries$ratio)  # dispersion in the ratios for this field
subset <- filter(timeseries, (year > 2017) & (year < 2020)) # the two years that are shown in the other figure
ts <- ggplot(timeseries, aes(x=year, y=ratio)) +
   geom_line() +
   geom_hline(yintercept=1.0, color='black', linetype='21') +
   # shaded area whose width is the dispersion in the timseries ratios
   annotate("rect", xmin=-Inf, xmax=Inf, ymin=(barmean-barwidth), ymax=(barmean+barwidth), alpha=0.25) +
   geom_point(aes(color=cashcrop), size=4) +
   scale_color_manual(values=c('dodgerblue','darkorange')) + 
   geom_errorbar(aes(color=cashcrop, ymin=rat_lo, ymax=rat_hi)) +
   # circle the years that are plotted in the other figure
   geom_point(data=subset, aes(x=year, y=ratio), shape=21, size=4, stroke=2) +
   ylim(0.9, 1.1) +
   theme_gray(base_size=16) +
   guides(color = guide_legend('cash crop')) +
   labs(x='Year', y='Yield ratio covercrops / control area') +
   labs(title='Yield impacts of cover crops')
print(ts)
ggsave('cc+yield_timeseries.pdf', ts)
ggsave('cc+yield_timeseries.png', ts)

