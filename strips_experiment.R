
# LMY 12 Aug 2024
#
# PURPOSE: This code carries out the analysis for an experiment on strip-till in
# our cover crops.  We wanted to know whether clearing some strips for the subsequent
# corn crop would significantly affect the biomass of the cover and the cash crops,
# and ultimately the corn yield.  This code plots NDVI images and histograms for the
# regions with the strips and the control regions without strips.
# Older Sentinel-2 images are registered to a more recent image that seems to have
# better coordinates.
# In April (prime time for the cover crops to be putting on biomass) the regions
# with cleared strips have about 80% as much biomass as the control regions.  In
# June, the corn in strip-tilled regions has 5% to 20% more biomass than the corn
# in the control regions.
# This corn was planted mid-May and the cover crops were a mix of barley, oats, 
# several brassica species and crimson clover.
# Harvest data will be coming soon.


# TO DO:
# - eventually you may want to read NDVI data once and build a huge df which you can
#   subset to make the histogram plots with.  maybe images too?  that would probably
#   be more efficient in terms of disk access but would require storing a fairly big df
#   holding NDVI for all of the fields and all of the dates


# DONE:


library(tidyverse)
library(janitor)
library(sf)
library(terra)
library(RColorBrewer)
library(gridExtra)
library(patchwork)
library(scales)
library(RNiftyReg)  
library(RStoolbox)

setwd(this.path::here())

satdir <- '../../Farm stuff misc/satellite data/'
satdir2 <- paste(satdir,'img_registration_expts',sep='')
yielddir <- '../../FarmBackups/SMSData'

mycolors <- c('forestgreen', 'darkorange')

# locations of the strips and polygons around them
stripfiles <- list.files(path=yielddir, pattern=glob2rx('*fallstrips.shp'), recursive=TRUE, full.names=TRUE)
polyfiles <- list.files(path=yielddir, pattern=glob2rx('*mypolygon.shp'), recursive=TRUE, full.names=TRUE)

# field boundaries
allboundaries <- sf::read_sf('../../Farm stuff misc/satellite data/CorrectedBoundaries2023.shp')
allboundaries$Field[allboundaries$Field=='N50'] <- 'Home N50'
allboundaries$Field[allboundaries$Field=='Kids N 80'] <- 'Kids N80'
allboundaries$Field[allboundaries$Field=='Kids N78'] <- 'Harrys N78'

# satellite image data
# use this first one for cross-correlating reference
sat_reference <- paste(satdir2, '/2024-07-13-00-00_2024-07-13-23-59_Sentinel-2_L2A_True_color.tiff', sep='') 
sat_reference_img <- rast(sat_reference)
# all the others
truecfiles <- list.files(path=satdir2, pattern=glob2rx('*True_color.tiff'), recursive=FALSE, full.names=TRUE)
shortfilenames <- list.files(path=satdir2, pattern=glob2rx('*True_color.tiff'), recursive=FALSE, full.names=FALSE)
allboundaries_m <- st_transform(allboundaries, crs=st_crs(sat_reference_img))


# one time: loop over dates and find the shifts that align all the images
datelist <- list()
shiftxlist <- list()
shiftylist <- list()
for (i in 1:length(truecfiles)){
   filen <- truecfiles[i]
   date <- substr(shortfilenames[i], 1, 10)
   datelist <- append(datelist, date[[1]])
   
   print('')
   print(filen)
   # read this satellite image
   colorimg <- rast(filen)
   # find the translation shift compared to the satellite reference image
   myshift <- niftyreg(as.array(colorimg), as.array(sat_reference_img))
   # niftyreg returns shift values in pixels.  now extract shifts and convert to image units (deg or m); mult by pixel size.
   shift_dx <- myshift$forwardTransforms[[1]][2,4] * xres(colorimg)
   shift_dy <- myshift$forwardTransforms[[1]][1,4] * yres(colorimg)
   print(sprintf('Shift = %.2f, %.2f (m) ', shift_dx, shift_dy))
   shiftxlist <- append(shiftxlist, shift_dx)
   shiftylist <- append(shiftylist, shift_dy)
}
# end of loop over date

# store measured image alignment parameters here
shiftresults <- tibble(date=datelist, shiftx=shiftxlist, shifty=shiftylist)

# initialize a structure for storing test area/control NDVI ratios
fieldlist <- c('KidsN80', 'HomeN50', 'HarrysN78', 'Hett106')
ratios <- as.data.frame(tibble(date=datelist, 'KidsN80'=1.0, 'HomeN50'=1.0, 'HarrysN78'=1.0, 'Hett106'=1.0))


# now loop over fields, creating 2 big multipanel figures for each field
# it is inefficient to be rereading the files all the time but makes it easier to manage the multipanel plots
for (j in 1:length(stripfiles)){
   
   imgrobs <- list() # initialize new plots
   histgrobs <- list()
   
   # stuff that should be done once, one field at a time
   stripdata <- sf::read_sf(stripfiles[j]) %>%
      clean_names()
   mypoly <- sf::read_sf(polyfiles[j])
   fieldname <- str_split(stripfiles[j], '/')[[1]][6]
   fieldnm <- gsub(" ", "", fieldname) # remove the space... was doing this a lot, so shorthand
   justonebound <- filter(allboundaries, Field == fieldname)
   
   # project the strip info to match the satellite data coordinate system (in case they're different or it changes or something)
   stripdata_m <- st_transform(stripdata, crs=st_crs(sat_reference_img))  
   mypoly_m <- st_transform(mypoly, crs=st_crs(sat_reference_img))
   justonebound_m <- st_transform(justonebound, crs=st_crs(sat_reference_img))
   e <- ext(justonebound_m)
   
   # one time: plot the 2023-11-11 color image as a sanity check.
   # Strip-till operation was completed slightly before 2023-11-11 so you can see
   # the tilled region in the RGB image.
   # The field boundary is shown in a solid magenta line and the polygon boundary
   # around the tilled region is shown in a dotted magenta line.
   date <- '2023-11-11'
   maxcol <- 1.0 # for plot color table, but sqrt scaling (below) is more effective
   colorimgname <- paste(satdir2, '/', date, '-00-00_', date, '-23-59_Sentinel-2_L2A_True_color.tiff', sep='') 
   colorimg <- rast(colorimgname)
   cropimg <- crop(colorimg, extend(e, 70)) # meters
   idx <- which(shiftresults$date==date)
   cropimg_sh <- terra::shift(cropimg, dx=shiftresults$shiftx[[idx]], dy=shiftresults$shifty[[idx]])
   blah <- names(cropimg_sh)
   shift_df <- as.data.frame(cropimg_sh, xy= TRUE) %>%
      rename(red = blah[[1]], green = blah[[2]], blue = blah[[3]]) %>%
      # sqrt scaling makes better contrast during the summer after canopy
      mutate(red = sqrt(red), green = sqrt(green), blue = sqrt(blue))
   imgrobs[[1]] <- ggplot() +
      # shifted color image
      geom_raster(data = shift_df, aes(x=x, y=y), 
                  fill = rgb(r=shift_df$red, g=shift_df$green, b = shift_df$blue, maxColorValue=maxcol), show.legend = FALSE) +
      geom_sf(data=mypoly_m, fill=NA, color='magenta', linewidth=0.5, linetype='dotted') +
      geom_sf(data=justonebound_m, fill=NA, color='magenta', linewidth=0.5) +
      {if (fieldnm=='Hett106') annotate('text', label='strips', x=-Inf, y=-Inf, hjust=-1.0, vjust=-4.0, color='white')} +
      {if (fieldnm=='Hett106') annotate('text', label='control', x=-Inf, y=-Inf, hjust=-2.5, vjust=-8.0, color='white')} +
      coord_sf() +
      labs(x='', y='', title=date) + 
      theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
   
   # stuff that is both field- and date- dependent from here on down
   
   # now loop over dates, work with NDVI data
   for (i in 1:length(truecfiles)){

      filen <- truecfiles[i]
      date <- substr(shortfilenames[i], 1, 10)

      print('')
      print(filen)
      # find and read the NDVI data for this date
      blah <- str_split(filen, '/img_registration_expts/')[[1]][2]
      blah2 <- str_split(blah, 'True_color.tiff')[[1]][1]
      blah3 <- paste(satdir, 'datadirs/', date, '_EPSG32616/', blah2, sep='')
      redname <- paste(blah3, 'B04_(Raw).tiff', sep='')
      nirname <- paste(blah3, 'B08_(Raw).tiff', sep='')
      redimg <- rast(redname)
      nirimg <- rast(nirname)
      ndviimg <- (nirimg-redimg)/(nirimg+redimg)
      names(ndviimg) <- 'NDVI'
      # for display overlay with strip regions: just a cutout of the whole satellite image
      ndvi_sh <- ndviimg %>%
                  crop(extend(e, 70)) %>%
                  terra::shift(dx=shiftresults$shiftx[[i]], dy=shiftresults$shifty[[i]])
      
      # prep for displaying maps
      ndvi_df <- as.data.frame(ndvi_sh, xy=TRUE)
      field_ndvi <- terra::mask(ndvi_sh, justonebound_m) # extract just our field NDVI for setting display colorscale
      scalefac <- 1.0 # tweaks the color table for the NDVI images, see below
      mylimits=as.vector(minmax(field_ndvi))*scalefac
      imgrobs[[i+1]] <- ggplot() +
         # shifted NDVI
         geom_raster(data = ndvi_df, aes(x=x, y=y, fill=NDVI)) +
         scale_fill_distiller(palette='YlGn', limits=mylimits, oob=squish, direction='horizontal', guide='none') + 
         geom_sf(data=mypoly_m, fill=NA, color='magenta', linewidth=0.5, linetype='dotted') +
         geom_sf(data=justonebound_m, fill=NA, color='magenta', linewidth=0.5) +
         coord_sf() +
         labs(x='', y='', title=date) + 
         theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

      
      # histograms of the NDVI values inside and outside the treatment region.
      # build the NDVI dataframe with a flag for pixels within
      # the treatment region so that I can do "groupby" before histogram
      # note field_ndvi (see above) sets the pixels outside our field boundary to NA
      stripmask <- terra::mask(field_ndvi, mypoly_m, inverse=TRUE, updatevalue=1.0) # pixels with strips applied become 1.0
      stripmask <- terra::mask(stripmask, mypoly_m, updatevalue=0.0) # control regions become 0.0
      names(stripmask) <- 'Treatment'
      add(field_ndvi) <- stripmask # add the mask layer to the raster
      field_ndvi_df <- as.data.frame(field_ndvi, xy=TRUE) %>%
         mutate(Treatment = factor(Treatment, levels=c('1','0'), labels=c('strips','no strips')))
      mysummary <- field_ndvi_df %>%
         group_by(Treatment) %>%
         # here I'm picking out the medians of the two groups
         dplyr::summarize(medndvi = median(NDVI, na.rm=TRUE)) #,
      # store ratio of medians in the ratios dataframe
      ratio <- filter(mysummary, Treatment=='strips')$medndvi / filter(mysummary, Treatment=='no strips')$medndvi
      ratios[[which(names(ratios)==fieldnm)]][[which(ratios$date==date)]] <- ratio
      # this part is still klutzy but I haven't figured out how to
      # get the summarize above to read off the peak of the smoothed density estimator
      nostrips_dens <- density(filter(field_ndvi_df, Treatment=='no strips')$NDVI, na.rm=TRUE)
      strips_dens <- density(filter(field_ndvi_df, Treatment=='strips')$NDVI, na.rm=TRUE)
      nostrips_peak <- nostrips_dens$x[which(nostrips_dens$y==max(nostrips_dens$y))]
      strips_peak <- strips_dens$x[which(strips_dens$y==max(strips_dens$y))]
      # custom limits for histograms
      blah1 <- min(strips_peak, nostrips_peak, mysummary$medndvi) - 0.1
      blah2 <- min(1.0, max(strips_peak, nostrips_peak, mysummary$medndvi) + 0.1)
      xlims <- c(blah1, blah2)
      binwid <- if(fieldname=='Home N50') 0.01 else 0.002
      histgrobs[[i]] <- ggplot(field_ndvi_df, aes(NDVI)) +
         geom_histogram(aes(fill=Treatment, y=after_stat(density)), binwidth=binwid, position='identity') + 
         scale_fill_manual(values = alpha(mycolors, 0.5)) +
         xlim(xlims) +
         # put median lines on
         geom_vline(xintercept=mysummary$medndvi, color=mycolors) +
         # smoothed versions and their peaks
         geom_density(data=field_ndvi_df, aes(group=Treatment, color=Treatment)) + # redundant with the density estimators above but can't figure out how to get the peak otheriwse
         scale_color_manual(values = mycolors) +
         geom_vline(xintercept=c(strips_peak, nostrips_peak), color=mycolors) +
         labs(title=date)
   }
   # end of loop over dates
   # arrange the grobs into panels for this field.
   # one panel for color & NDVI images; one panel for histograms.
   imsfig <- wrap_plots(imgrobs, ncol = 3, nrow=2) +
      plot_annotation(paste(fieldname, 'NDVI images', sep=' '), theme=theme(plot.title = element_text(hjust = 0.5)))
   histfig <- wrap_plots(histgrobs, ncol = 3, nrow = 2, axis_titles='collect') +  # collecting axis_titles doesn't seem to be working
      plot_layout(guides='collect') +
      plot_annotation(paste(fieldname, 'NDVI histograms',sep=' '), theme=theme(plot.title = element_text(hjust = 0.5)))
   print(imsfig)
   print(histfig)
   ggsave(paste('strips_expt_ims_',fieldnm,'.png', sep=''), plot=imsfig, path='.')
   ggsave(paste('strips_expt_hist_',fieldnm,'.png', sep=''), plot=histfig, path='.')
   
   # need coordinates?
   if (fieldnm=='Hett106') {
      plot(ndvi_sh)
   }
}
# end of loop over fields

print('')
print('NDVI ratios, strips / no strips')
print(ratios)

   
   
