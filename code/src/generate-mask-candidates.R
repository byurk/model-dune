# Generate candidate polygons to EXTEND the complex mask (clean_data/ortho/grass-mask/
# mask_grass_paper.shp) with the water / wetland / wooded / tree-and-bush areas the 3-class
# surface-composition model does not cover. These are PROPOSALS for human review in QGIS, not a
# finished mask. Three complementary detectors, because no single one catches everything:
#
#   1. dense canopy (woodland)  : sustained high NDVI over large contiguous patches. Catches the
#      big eastern woodland block that a canopy-height top-hat MISSES (its interior has no nearby
#      open ground to measure height against).
#   2. tall vegetation (trees)  : DEM top-hat (surface minus a morphological opening) > 1.5 m AND
#      high NDVI. Catches isolated trees/bushes — smaller than the opening window — that dense-NDVI
#      contiguity would merge into the background. Marram grass is low, so it does NOT trip this.
#   3. water                    : NDVI < 0 (water absorbs NIR). Catches any missed lake/pond/shore.
#
# Run from the project root:  R -f code/src/generate-mask-candidates.R
library(terra)
library(sf)

ortho <- rast("raw_data/drone_sitched/ortho.tif")
dem   <- rast("raw_data/ortho/dem.tif")
mask  <- vect("clean_data/ortho/grass-mask/mask_grass_paper.shp") |> project(crs(ortho))
outdir <- "clean_data/ortho/mask-candidates-auto"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# work at ~1 m (trees/water/woodland are all >> 1 m); NDVI from R = band 3, NIR = band 5
o1   <- aggregate(ortho[[c(3, 5)]], fact = round(1/res(ortho)[1]), fun = "mean", na.rm = TRUE)
ndvi <- (o1[[2]] - o1[[1]]) / (o1[[2]] + o1[[1]]); names(ndvi) <- "ndvi"
d1   <- aggregate(dem, fact = round(1/res(dem)[1]), fun = "mean", na.rm = TRUE) |>
        resample(ndvi, method = "bilinear")
mrast <- rasterize(mask, ndvi, field = 1, background = 0)   # 1 = already masked

# 1. woodland: smoothed NDVI > 0.45, large contiguous patches outside the mask
dense <- (focal(ndvi, w = 5, fun = "mean", na.rm = TRUE) > 0.45) & (mrast == 0)
dense[dense == 0] <- NA
wood <- as.polygons(dense, dissolve = TRUE) |> disagg()
wood <- wood[expanse(wood) >= 200]
writeVector(wood, file.path(outdir, "woodland_candidates.shp"), overwrite = TRUE)

# 2. isolated trees/bushes: DEM top-hat (open with ~15 m window) > 1.5 m AND NDVI > 0.25, outside mask
w      <- 15
ground <- focal(focal(d1, w = w, fun = "min", na.rm = TRUE), w = w, fun = "max", na.rm = TRUE)
canopy <- d1 - ground
tall   <- (canopy > 1.5) & (ndvi > 0.25) & (mrast == 0)
tall[tall == 0] <- NA
trees <- as.polygons(tall, dissolve = TRUE) |> disagg()
trees <- trees[expanse(trees) >= 4]
writeVector(trees, file.path(outdir, "tallveg_candidates.shp"), overwrite = TRUE)

# 3. water: NDVI < 0 outside the mask
wet <- (ndvi < 0) & (mrast == 0)
wet[wet == 0] <- NA
water <- as.polygons(wet, dissolve = TRUE) |> disagg()
water <- water[expanse(water) >= 10]
writeVector(water, file.path(outdir, "water_candidates.shp"), overwrite = TRUE)

cat(sprintf("woodland: %d patches, %d m^2 (largest %d m^2)\n",
            nrow(wood), round(sum(expanse(wood))), round(max(expanse(wood)))))
cat(sprintf("trees:    %d patches, %d m^2\n", nrow(trees), round(sum(expanse(trees)))))
cat(sprintf("water:    %d patches, %d m^2\n", nrow(water), round(sum(expanse(water)))))
cat("wrote candidates to", outdir, "— review in QGIS, merge into mask_grass_paper.shp\n")
