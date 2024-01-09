# Veg/soil/HF processing

This package contains function that take as input the attributes from the backfilled veg/soil/HF data and produces output in long and wide format (also deals with unknown ages) for species modeling.

# Usage

There are 4 functions that can be used consecutively as part of a workflow to obtain Veg, HF, and Soil summaries.

+ `read_summary()` is a simple wrapper to read in GIS data provided by the GC into R.
+ `make_veghf_long()` processes the summaries into the long format and deal with issues such as age corrections and burning of harvest areas.
+ `make_veghf_wide()` processes the long format into the wide format, can deal with unknown ages if age corrections are not applied. 
+ `clean_landcover()` processes the wide format data to align with the landcover types used in the ABMI species models. 

```R

# Install package
remotes::install_github("ABbiodiversity/veghfsoil")

library(veghfsoil)

# Read in GIS data from the Geospatial Centre
s_drive <- "S:/GC_eric/FromEric/Sites_summaries/"
df <- read_summary(
  summary.path = paste0(s_drive, "Round2022/Modelling2022/CAM&ARU/Marcus_Selection_survey_year_2019_to_2022/summaries_20221101_rev00.sqlite"),
  table = "landscape_summary_camaru_2019_2022_rmVegNull"
)

# Obtain long summary
d.long <- make_veghf_long(
  landcover = df,
  col.label = "Site_ID",
  col.baseyear = 2017,
  col.hfyear = "YEAR",
  col.veg = "Combined_ChgByCWCS",
  col.soil = "Soil_Type_1",
  hf_fine = TRUE,
  burn.cc = TRUE,
  age.correction = TRUE,
  ver.id = "V7.0"
)

# Obtain wide summary
d.wide <- make_veghf_wide(long.output = d.long,
  col.label = "Site_ID",
  col.area = "Shape_Area",
  hf.fine = TRUE,
  tol = 0,
  sparse = TRUE,
  assign.unknown.ages = TRUE,
  age.data = "Maltman.Old",
  ver.id = "V7.0",
  rm0 = TRUE
)

# Clean the wide summary to match ABMI species models
landcover.out <- clean_landcover(data.in = d.wide,
  landscape.lookup = landcover.coef.lookup,
  type = "vegetation",
  class.in = "ID",
  class.out = "COEF")

```

# Note

This package is intended to supercede the `ABbiodiversity/veg-hf-soil-summaries` repository created by Peter S. It uses (virtually) identical code, but is just intended to help with ease of use. Thanks to Peter for the heavy lifting of creating the initial summaries code.   
