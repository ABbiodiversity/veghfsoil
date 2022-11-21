# Veg/soil/HF processing

This package contains function that take as input the attributes from the backfilled veg/soil/HF data and produces output in long and wide format (also deals with unknown ages) for species modeling.

# Usage

There are 3 functions that can be used consecutively as part of a workflow to obtain Veg, HF, and Soil summaries.

+ `read_summary()` is a simple wrapper to read in GIS data provided by the GC into R.
+ `make_veghf_long()` processes the summaries into the long format.
+ `make_veghf_wide()` processes the long format into the wide format, as well as deals with unknown ages. 

```R

# Install package
remotes::install_github("ABbiodiversity/veghfsoil")

library(veghfsoil)

# Read in GIS data from the Geospatial Centre
s_drive <- "S:/GC_eric/FromEric/Sites_summaries/"
df <- read_summary(
  summary_path = paste0(s_drive, "Round2022/Modelling2022/CAM&ARU/Marcus_Selection_survey_year_2019_to_2022/summaries_20221101_rev00.sqlite"),
  table = "landscape_summary_camaru_2019_2022_rmVegNull"
)

# Obtain long summary
d_long <- make_veghf_long(
  d = df,
  col.label = "Site_ID",
  col.veg = "Combined_ChgByCWCS",
  col.baseyear = 2019,
  col.hfyear = "YEAR",
  col.soil = "Soil_Type_1",
  unround = FALSE,
  hf_fine = TRUE
)

# Obtain wide summary
d_wide <- make_veghf_wide(
  d = df,
  long_output = d_long,
  col.label = "Site_ID",
  col.area = "Shape_Area",
  hf_fine = TRUE,
  tol = 0,
  sparse = TRUE
)

```
