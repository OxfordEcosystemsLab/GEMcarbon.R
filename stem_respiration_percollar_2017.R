setwd("~/Github/GEMcarbon.R/GEMcarbon.R") 
source("allometric_equations_2014.R")


# two options we need to test:

# calculate stem respiration per whole tree, then regress with growht rate
# OR get stem resp per unit stem area (per collar), then regress with growth rate


# 1. Get: 
# stem respiration flux per unit collar: raw to EGM.
# stem area index for each tree for that date (that year) using census data & Chambers2004_surfaceArea(diameter=diametersA).
# if dying trees - do we assume they respire 1/2 of that year? (make this assumption flexible)
# if new recruits - respiring 1/2 of the year.

# 2. growth rate upscaling:
# regression: woody growth x ~ stem respiration (per collar area or per tree?) y
# estimate stem respiration for all trees in the plot by applying the regression results.
# end product plot_code, sub_plot, tree_tag, rstem_MgC_ha_yr, stem_area_cm2 -- you get total respiration per hectare by summing up the rstems. 
