# 08jan2023 LMY initial version
#
# Purpose: to run the sulfur/starter yield experiment from 2019.  
# Learning how to process those files, make images & do analysis on the shape files.

# This code reads a combine yield monitor shape file and does some basic 
# analysis on the three different planting-time fertilizer treatments recorded
# in that yield data.
# This case is easy to process as the operator got the planting-time fertilizer application data
# loaded up where the combine yield monitor could incorporate it real-time into 
# the yield monitor output.  (Getting things set up this way is nontrivial.)
# You can exclude field edges and waterways from the study, either interactively drawing a 
# polygon around the region of interest or reading one from a shape file.

# TO DO:
# - for report, look up the analysis of the starter and sulfur so can report that as well.
# - can you figure out how to make a multipanel figure, maybe 2 on top of each other 



library(tidyverse)
library(janitor)
library(sf)
library(terra)
library(scales)
library(gridExtra)

# setup information
setwd(this.path::here())

w <- matrix(1, 3, 3)  # for smoothing.  if you want to skip the corners, use c(0,1,0,1,0,1,0,1,0), nrow=3)

myfile <- 'starter_vs_yield_2019_data.shp'  # yield data with product codes
dobyhand <- FALSE # switch for whether you want to do the interactive region drawing or just read a file on disk
shpfile <- 'E100_2019_startertestAOI.shp' # polygon shape file describing region to study



# read data
data <- sf::read_sf(myfile)
print('This file has unique product codes:')
print(unique(data$Product))

data <- data %>%
   dplyr::select(Field, Dataset, Product, Obj__Id, Yld_Vol_Dr, Area__ac_)  %>%  # only keep interesting columns
   # extra manipulation cleaning up one of the product names, which is just unwieldy and long
   rename(product_orig = 'Product') %>%
   separate(product_orig, c('Product','product_tail'), sep=':', remove=TRUE)
# clarifying the labels on these products
data$Product[(data$Product == '32')] <- '32% UAN' 
data$Product[(data$Product == '32 and starter')] <- '32% with starter' 
data$Product[(data$Product == 'Sulfur starter')] <- '32+S+starter'

# acreage totals and grouped by the 3 products.
acres <- data %>%
   group_by(Product) %>%
   dplyr::summarize(sum = sum(Area__ac_, na.rm=TRUE))
print(sprintf('%15s %s', 'Product','Acres'))
for (i in 1:length(acres)) {
   print(sprintf('%15s %.2f', acres$Product[i], acres$sum[i]))
}
print(sprintf('%15s %.2f', 'Total',sum(acres$sum)))

# prep a rasterized version of the normalized yield data.
# new coordinate system will be projected, in ft measured from SW corner.
bd <- ext(data)
target_crs <- sprintf('+proj=tmerc +lon_0=%f +lat_0=%f +units=ft', xmin(bd), ymin(bd)) 
data_f <- sf::st_transform(data, crs=target_crs)
r <- rast(extent=ext(data_f), resolution=10, crs=target_crs) # units here in ft.  this gives 10 ft pixels.
data_ras1 <- rasterize(x=data_f, y=r, 'Yld_Vol_Dr', fun=median, na.rm=TRUE)
# 3x3 boxcar smoothing is not bad, maybe helpful
data_ras1 <- focal(data_ras1, w, median, na.rm=TRUE, pad=TRUE) # na.policy='only' if you don't want to touch most of the original values
# ggplot wants dataframes, not rasters
data_ras1_df <- as.data.frame(data_ras1, xy=TRUE) %>%
   rename(Yld_Vol_Dr = 'focal_median')


# if requested: interactive setting of polygon for region to study
if (dobyhand==TRUE){
   plot(data_ras1)
   np <- 1   # one polygon
   nPoint <- 4 # 4 corners
   print(paste("Select ", nPoint, " points around polygon #", np, " in the plots space.", sep = ""))
   coords1 <- NULL
   for (i in 1:nPoint) {
      c1.1 <- locator(type = "p", n = 1, col = np, pch = 19)
      coords1 <- rbind(coords1, c(c1.1$x, c1.1$y))
   }
   coords1 <- rbind(coords1, coords1[1, ])  # tacks the first point back on again to close the polygon
   colnames(coords1) <- c("x", "y")  # for the geometry codes to interpret the numbers
   lines(coords1, col = np, type = "l", lty = 2, lwd = 3) # this just draws the polygon on the plot
   mypoly <- st_polygon(list(coords1)) %>%
      st_sfc(crs=st_crs(data_ras1))
   
   # write shape file for use later if desired
   st_write(mypoly, shpfile, append=FALSE)
} else {
   # read from file
   mypoly <- sf::read_sf(shpfile) %>%
      sf::st_transform(crs=target_crs)
}




# plot yield data with polygon region
p <- ggplot(data_ras1_df) +
   geom_raster(aes(x=x, y=y, fill=Yld_Vol_Dr)) +
   scale_fill_distiller(palette='YlGn', limits=c(120,270), direction='horizontal', oob=squish) + # squish is in the scales package and it pegs OOB values at the ends of the color scale (rather than NA)
   labs(x='Ft', y='Ft', title='Yield') +
   theme_gray(base_size=16) +
   geom_sf(data=mypoly, fill=NA, linewidth=1., color='blue', linetype='21') +
   coord_sf(datum=target_crs)
print(p)
ggsave('starter_vs_yield_2019_a.png', width=7.5, height=4) # mess around with width= and height= to fix whitespace


# plot different treatment types with polygon region
mycolors = c('skyblue','orange','forestgreen')
p2 <- ggplot(data_f) +
   geom_sf(aes(color=Product), size=0.2) +
   scale_color_manual(values=mycolors) +
   geom_sf(data=mypoly, fill=NA, linewidth=1., color='blue', linetype='21') +
   labs(x='Ft', y='Ft', title='Product Map') +
   theme_gray(base_size=16) +
   theme(plot.margin = margin(0,0,0,0)) +
   guides(colour = guide_legend(override.aes = list(size=2))) +
   coord_sf(datum=target_crs)
print(p2)
ggsave('starter_vs_yield_2019_b.png', width=7.5, height=4)


# crop the big data frame to retain only the yield data within the boundary
blah <- st_within(data_f, mypoly) %>% lengths > 0
data_f_crop <- dplyr::filter(data_f, blah)


# plot the histograms of the yields in those 3 treatments and overlay their medians.
xlabs = c(200, 200, 200)
ylabs = c(0.03, 0.035, 0.04)
mycolors = c('dodgerblue','orange','forestgreen')
meds.crop <- data_f_crop %>%
   group_by(Product) %>%
   dplyr::summarize(medyld = median(Yld_Vol_Dr, na.rm=TRUE))
p3 <- ggplot(data_f_crop, aes(x = Yld_Vol_Dr, fill=Product)) +    
   geom_histogram(alpha=0.65, binwidth=1, position='identity', aes(y=after_stat(density))) +   
   scale_fill_manual(values=mycolors) +
   geom_vline(xintercept = meds.crop$medyld, color=mycolors, linewidth=1) +
   xlim(190,260) +
   labs(title='Yield vs Product', x='Yld_Vol_Dr (bu/ac)') +
   theme_gray(base_size=16) +
   annotate('text', label=sprintf('%.1f b/Ac', meds.crop$medyld), x=xlabs, y=ylabs, color=mycolors, size=6)
print(p3)
ggsave('starter_vs_yield_2019_c.png', width=7, height=3.5)

# nesting like this works, but I decided I don't like the output
# bigfig2 <- grid.arrange(p, p2, nrow=2, ncol=1, top='Starter experiment 2019')
# bigfig3 <- grid.arrange(bigfig2, p3, nrow=1, ncol=2)
# print(bigfig3)
# ggsave('starter_vs_yield_2019_2.png', bigfig3)
