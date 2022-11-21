## code to prepare `recl` dataset goes here

recl <- structure(
  list(
    V6_COMB = structure(
      1:30,
      .Label = c("Alkali", "AlpineLarch", "Bare", "Conif", "Decid", "Fir", "GraminoidFen","GrassHerb", "Marsh",
                 "Mixedwood", "Pine", "Shrub", "ShrubbyBog", "ShrubbyFen", "ShrubbySwamp", "SnowIce", "Spruce",
                 "TreedBog-BSpr","TreedFen-BSpr", "TreedFen-Decid", "TreedFen-Larch", "TreedFen-Mixedwood",
                 "TreedSwamp-Conif", "TreedSwamp-Decid", "TreedSwamp-Fir", "TreedSwamp-Forest","TreedSwamp-Mixedwood",
                 "TreedSwamp-Spruce", "TreedWetland-Mixedwood", "Water"),
      class = "factor"),
    MERGED = structure(
      c(4L, 13L, 1L, 13L, 2L, 13L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 15L, 15L, 15L, 16L, 16L,
        16L, 16L, 16L, 16L, 15L, 17L),
      .Label = c("Bare", "Decid", "GraminoidFen", "GrassHerb", "Marsh", "Mixedwood", "Pine", "Shrub", "ShrubbyBog",
                 "ShrubbyFen", "ShrubbySwamp", "SnowIce", "Spruce", "TreedBog", "TreedFen", "TreedSwamp", "Water"),
      class = "factor")),
  class = "data.frame",
  row.names = c(NA, -30L))

usethis::use_data(recl, overwrite = TRUE)
