Some examples of codes that I have found useful in analyzing the data we produce on our farm.
- [Starter fertilizer experiment](#starter-fertilizer-experiment)
- [Stacked yield rasters](#stacked-yield-rasters)

### [Starter Fertilizer Experiment] 
 
The code in starter_vs_yield_2019.R reads a combine yield monitor shape file and does some basic analysis on the three different planting-time fertilizer treatments recorded in that yield data.  Analysis is easy since the operator got the planting-time fertilizer application data loaded up where the combine yield monitor could incorporate it into the yield monitor output.  (Getting things set up this way is nontrivial.)  You can exclude field edges and waterways from the study, either interactively drawing a polygon around the region of interest or reading one from a shape file.

The three treatments in this experiment were applied with a strip-till bar a day or so prior to planting corn.  All areas were treated with UAN 32%.  Some areas had in addition a 7-22-5 liquid as a starter and some had the 32%, starter, and ATS.  In this trial the starter produced a yield gain of about 3 bu/ac and the ATS gave an additional 2 bu/ac.  It's impossible to see those gains in the yield data until you make the histograms.  This is food-grade non-GMO white corn in east central IL. 

![Yield map](starter_vs_yield_2019_a.png)
![Product map](starter_vs_yield_2019_b.png)
![Yield histograms](starter_vs_yield_2019_c.png)

### [Stacked Yield Rasters] 

Our collections of combine yield monitor data allow some time-series analysis of yield variability at different locations in a field.  Areas with low and/or unstable yields may not be economic to farm; see, e.g., [Fowler et al 2024](https://www.nature.com/articles/s41598-024-51155-y).  I carried out this kind of analysis on our fields, where we have 12 to 14 years worth of yield maps.  The code yield_raster_analysis.R plots the yield maps and computes the yield standard deviation map.

In our case, areas of high variability turn out to be the field ends, waterways, and some ponds that we already knew about.  Further quantitative analysis could reveal the probability of making or losing money on those areas.

![Stacked yield map](Yield_median.png)
![StdDev map](Yield_stdev.png)


<!-- comment 
<p align="center">
  <img src="starter_vs_yield_2019_a.png" width="400" height="330">
  <img src="starter_vs_yield_2019_b.png" width="400" height="330">
  <img src="starter_vs_yield_2019_c.png" width="400" height="300">
</p>
-->
