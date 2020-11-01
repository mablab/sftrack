## Code to prepare `raccoons` dataset:
library("sftrack")


## Export CSV from orginal 'raccoon' dataset (prepared by Matt):
## data(raccoon)
## raccoon$timestamp <- as.character(raccoon$timestamp)
## write.csv(raccoon, file = "../inst/extdata/raccoons.csv", row.names = FALSE)


## To access the file from within R, after 'sftrack' is installed:
## system.file("extdata", "raccoons.csv", package = "sftrack")


## Prepare `raccoons` dataset:
raccoons <- read.csv("../inst/extdata/raccoons.csv", stringsAsFactors = TRUE) # Load CSV file into R
raccoons$timestamp <- as.POSIXct(raccoon$timestamp, tz = "UTC") # Original time is UTC
attr(raccoons$timestamp, "tzone") <- "EST5EDT"                  # Converted to EST5EDT
head(raccoons)

## sftrack object
racc_track <- as_sftrack(
    data = raccoons,
    coords = c("longitude", "latitude"),
    time = "timestamp",
    group = "animal_id",
    crs = "+init=epsg:4326")
head(racc_track)

## sftraj object
racc_traj <- as_sftraj(racc_track)
head(racc_traj)


## Export 'raccoons' dataset (raw, sftrack and sftraj):
## usethis::use_data(raccoons, overwrite = TRUE)
save(raccoons, racc_track, racc_traj, file = "../data/raccoons.RData", compress = "bzip2")


## Test in empty environment:
## load("../data/raccoons.RData")
