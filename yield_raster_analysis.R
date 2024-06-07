# 06jun 2024 LMY
# extracted and simplified from plot_yield_rasters.R
#
# PURPOSE: Plot normalized yield data for all available years, for one field;
# stack it all up into one rasterbrick and compute median and standard deviation.
# Areas with unusually high variability may not be economic to continue to farm,
# as discussed in Fowler et al 2024 (https://www.nature.com/articles/s41598-024-51155-y)


# TO DO:

library(tidyverse)
library(janitor)
library(sf)
library(raster)
library(RColorBrewer)
library(gridExtra)
library(scales)

setwd(this.path::here())

w <- matrix(1, 3, 3)  # for boxcar smoothing.  if you want to skip the corners, use c(0,1,0,1,0,1,0,1,0), nrow=3)
pixelsize <- 20.0 # resolution of raster grid, in feet
pixelarea <- (pixelsize**2)/43560  # area of one raster cell, in acres

readme <- function(filetoread, target_crs){
   data <- sf::read_sf(filetoread) %>%
      dplyr::select('Yld_Vol_Dr') %>%
      # normalize yield: divide by the median of this year's data
      mutate(yldnorm = Yld_Vol_Dr/median(Yld_Vol_Dr)) %>%
      # make sure we're always using the same coordinate system
      sf::st_transform(crs=target_crs)
   return(data)
}

# -------------- work starts here -------------------

# figure out which files we can use
processme <- list()
dirstoread <- list.files(path='./linktoSMSData/E100/E100', pattern='Grain Harvest', recursive=FALSE, full.names=TRUE)
for (dirname in dirstoread){
   filestoread <- list.files(path=dirname, pattern='.shp', full.names=TRUE)
   for (filetoread in filestoread){
      # usually there will be only one item in filestoread so this loop will be unnecessary
      processme <- c(processme, filetoread)
   }
}

# use most recent available dataset to define coordinate system and raster grid
nfiles <- length(processme)
data <- sf::read_sf(processme[nfiles])
bd <- extent(data)
target_crs <- sprintf('+proj=tmerc +lon_0=%f +lat_0=%f +units=ft', bd@xmin, bd@ymin) 
data_f <- sf::st_transform(data, crs=target_crs)
r <- raster(data_f, res=pixelsize, crs=target_crs)

# big loop over individual data files; plot each year and build rasterbrick
p <- list()
i <- 0 # initialize counter
for (filetoread in processme){
   i <- i + 1
   blah <- str_split_fixed(filetoread, '/', n=6)[[5]]
   thisyr <- str_split_fixed(blah, '_', n=2)[[2]] # extracting a string label for the year
   data <- readme(filetoread, target_crs)
   # rasterize the normalized yield
   data_ras1 <- rasterize(x=data, y=r, field="yldnorm", fun=median, na.rm=TRUE) # unitless (bu/acre)/median
   # 3x3 boxcar smoothing is not bad, maybe helpful
   data_ras1$layer <- focal(data_ras1$layer, w, median, na.rm=TRUE, pad=TRUE) 
   # add this year's raster to the multipanel grid image
   data_ras1_df <- as.data.frame(data_ras1, xy=TRUE) %>%
      rename(yld_norm = 'layer')
   p[[i]] <- ggplot(data_ras1_df) +
      geom_raster(aes(x=x, y=y, fill=yld_norm)) +
      scale_fill_distiller(palette='YlGn', limits=c(0.85,1.15), direction='horizontal', oob=squish) + 
      coord_sf(datum=target_crs) +
      labs(x='', y='', title=thisyr) +
      theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
      theme(plot.margin=margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))
   # add the data_ras1 into a rasterbrick with all the years (for plotting outside the loop)
   names(data_ras1$layer) <- thisyr
   if (i == 1){
      # it's the first year for this file
      bigrb <- data_ras1 
   } else {
      bigrb <- addLayer(bigrb, data_ras1)
   }

}
# finished looping over years
# spit out the big multipanel plot of yield maps
bigfig <- wrap_plots(p, nrow=3, ncol=5) +
   plot_layout(guides="collect") + 
   plot_annotation('Normalized yields', theme=theme(plot.title = element_text(hjust = 0.5)))
print(bigfig)
ggsave('Yield_rasters.png', plot=bigfig, path='.')

# statistics on the rasterbrick with multiple years
bigmedian <- calc(bigrb, fun=median, na.rm=TRUE) %>%
   as.data.frame(xy=TRUE) %>% # needed for ggplot
   rename(yldnorm='layer')
bigstdev <- calc(bigrb, fun=StdDev, na.rm=TRUE) %>%
   as.data.frame(xy=TRUE) %>%
   rename(yield_stdev='layer')
p0 <- ggplot(bigmedian) +
   geom_raster(aes(x=x, y=y, fill=yldnorm)) +
   scale_fill_distiller(palette='YlGn', limits=c(0.85,1.15), direction='horizontal', oob=squish) + 
   coord_sf(datum=target_crs) +
   theme_gray(base_size=16) +
   labs(x='ft', y='ft', title='All years median')
print(p0)
ggsave('Yield_median.png', plot=p0, path='.')
p0b <- ggplot(bigstdev) +
   geom_raster(aes(x=x, y=y, fill=yield_stdev)) +
   scale_fill_distiller(palette='YlOrRd', limits=c(0,0.35), direction='horizontal', oob=squish) + 
   coord_sf(datum=target_crs) +
   theme_gray(base_size=16) +
   labs(x='ft', y='ft', title='All years StdDev') +
   annotate('text', label='ponds', x=1000, y=600, color='blue', size=6) +
   annotate("segment", x=1300, xend=1700, y=600, yend=1300, color="blue", linewidth=1, arrow=arrow()) +
   annotate("segment", x=1300, xend=1650, y=600, yend=800, color="blue", linewidth=1, arrow=arrow()) +
   annotate("segment", x=1300, xend=2400, y=600, yend=200, color="blue", linewidth=1, arrow=arrow())
print(p0b)
ggsave('Yield_stdev.png', plot=p0b, path='.')