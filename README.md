<!--
<img src="https://drive.google.com/uc?id=1fgYuG7jpnekZrkoL_PdVUnSiUFBFX-vI" alt="Logo" width="150" style="float: left; margin-right: 10px;">
-->

<img src="https://drive.google.com/uc?id=1szqLViKqTX5C1XF8uV7HbIst0i6Xvv7g" alt="Logo" width="300">


# Veg/soil/HF processing

![Maintenance](https://img.shields.io/badge/Status-Maintenance-green)
![Languages](https://img.shields.io/badge/Languages-R-blue)

> [!IMPORTANT]
> This has been developed for **internal use within the ABMI Science Centre**.
> Version 2.0 2026-06-02

This package contains function that take as input the attributes from the backfill veg/soil/HF data and produces output in long and wide format (also deals with unknown ages) for species modeling.

---

# Changes since version 1.0
There are three important changes that have been implemented in version 2.0.

- Improved documentation of functions and alignment with the example provided in the ReadMe.
- Updated the lookup tables association with human footprint categories to include feature types present in HFI 2022 and 2023.
- Changed the behaviour of the *burn.cc* option in the *make_landcover_long* and *clean_landcover* functions. Harvest areas that have burned are now tracked separately (i.e., BurnCCPineR) with the age of harvest areas reset if a fire occurs in the polygon (e.g., CCPine3 -> BurnCCPineR). Users can use the *clean_landcover* function to group these burned areas into harvest polygons (e.g., BurnCCPineR -> CCPineR) or the natural vegetation (e.g., BurnCCPineR -> PineR) depending on their application.

---

# Usage

There are 5 functions that can be used consecutively as part of a workflow to obtain Veg, HF, and Soil summaries.

+ `read_summary()` is a simple wrapper to read in GIS data provided by the GC into R.
+ `impute_unknown_ages()` imputes unknown ages into the long form. This is useful for tracking change over time.
+ `make_landcover_long()` processes the summaries into the long format and deal with issues such as age corrections and burning of harvest areas.
+ `make_landcover_wide()` processes the long format into the wide format, can deal with unknown ages if age corrections are not applied. 
+ `clean_landcover()` processes the wide format data to align with the landcover types used in the ABMI species models. 

```R

# Install package
remotes::install_github("ABbiodiversity/veghfsoil")

library(veghfsoil)

# Read in GIS data from the Geospatial Centre
df <- read_summary(
  summary.path = "SQLITE_PATH",
  table = "TABLE_NAME"
)

# Impute the unknown ages
landcover <- impute_unknown_ages(landcover = landcover,
                                 ages.list = ages.list,
                                 col.baseyear = 2023,
                                 decadal.fix = TRUE,
                                 origin.fix = TRUE,
                                 unknown.fix = TRUE,
                                 age.data = "Maltman.Old",
                                 ver.id = "V7.0")

# Obtain long summary
d.long <- make_landcover_long(landcover = landcover,
                              col.label = "Site_ID",
                              col.baseyear = 2023,
                              col.hfyear = "YEAR",
                              col.veg = "Combined_ChgByCWCS",
                              col.soil = "Soil_Type_1",
                              hf.fine = TRUE,
                              burn.cc = TRUE,
                              unround = FALSE,
                              age.correction = "Age_Correction",
                              ver.id = "V7.0")
                              

# Obtain wide summary
d.wide <- make_landcover_wide(long.output = d.long,
                              col.label = "Site_ID",
                              col.area = "Shape_Area",
                              hf.fine = TRUE,
                              tol = 0,
                              sparse = TRUE,
                              assign.unknown.ages = FALSE,
                              age.data = "Maltman.Old",
                              rm0 = TRUE,
                              ver.id = "V7.0")

# Clean the wide summary to match ABMI species models
# Vegetation data
clean_landcover(data.in = as.matrix(d.wide$veg.current),
                                      landscape.lookup = landcover.coef.lookup, 
                                      type = "Vegetation",
                                      class.in = "ID", 
                                      class.out = "COEF")

# Soil data                                      
clean_landcover(data.in = as.matrix(d.wide$soil.current),
                                       landscape.lookup = landcover.coef.lookup, 
                                       type = "Soil",
                                       class.in = "ID", 
                                       class.out = "COEF")

```

# Note

This package is intended to supercede the `ABbiodiversity/veg-hf-soil-summaries` repository created by Peter Solymos. It uses that repository as the foundation for the package, but is simplified to help with ease of use. Thanks to Peter for the heavy lifting of creating the initial summaries code.   
