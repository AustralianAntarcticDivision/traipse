## Golden outputs from trip 1.10.0 for the TRACK model tests.
## Run from the package root: Rscript data-raw/golden-trip.R
## Writes tests/testthat/ref/trip-1.10.0.rds
suppressPackageStartupMessages(library(trip))
stopifnot(packageVersion("trip") == "1.10.0")
load("data/trips0.rda")
d <- as.data.frame(trips0)

## trip's two-segment distance for longitude and latitude is measured along
## the wrong pairs of fixes (trackDistance.default with four arguments uses
## sequential distances). References for longitude and latitude filters are
## made with that one line corrected; projected references use trip as is.
fixed_td <- function(x1, y1, x2, y2, longlat = TRUE, prev = FALSE) {
  if (missing(y1)) {
    x2 <- x1[-1, 1]; y1 <- x1[-nrow(x1), 2]; y2 <- x1[-1, 2]; x1 <- x1[-nrow(x1), 1]
  }
  if (longlat) {
    geodist::geodist(cbind(lon = x1, lat = y1), cbind(lon = x2, lat = y2),
                     paired = TRUE, measure = "geodesic") / 1000
  } else sqrt((x2 - x1)^2 + (y2 - y1)^2)
}
orig_td <- trip:::trackDistance.default

ll <- trip(d, c("date", "id"))
sp::proj4string(ll) <- sp::CRS("+proj=longlat +datum=WGS84", doCheckCRSArgs = FALSE)

pr <- d
pr$x <- pr$x * 1e5; pr$y <- pr$y * 1e5
pr <- trip(pr, c("date", "id"))
sp::proj4string(pr) <- sp::CRS("+proj=laea +lon_0=100 +lat_0=-45 +datum=WGS84", doCheckCRSArgs = FALSE)

## time spent (planar segments, degrees)
r <- rasterize(ll)
v <- raster::values(r); v[is.na(v)] <- 0
time_spent <- list(dimension = c(ncol(r), nrow(r)),
                   extent = as.vector(raster::extent(r)),
                   .cell = which(v > 0) - 1, time = v[v > 0])

## filters
speed_projected <- speedfilter(pr, max.speed = 1e5)
assignInNamespace("trackDistance.default", fixed_td, "trip")
speed_longlat <- speedfilter(ll, max.speed = 100)
sda_longlat <- sda(ll, smax = 100)
assignInNamespace("trackDistance.default", orig_td, "trip")
speed_longlat_trip <- speedfilter(ll, max.speed = 100)

ref <- list(trip_version = "1.10.0", time_spent = time_spent,
            speed_projected = speed_projected, speed_longlat = speed_longlat,
            speed_longlat_trip = speed_longlat_trip, sda_longlat = sda_longlat,
            args = list(speed_projected = 1e5, speed_longlat = 100, sda = 100))
saveRDS(ref, "tests/testthat/ref/trip-1.10.0.rds", version = 2)
str(lapply(ref[c("speed_projected", "speed_longlat", "speed_longlat_trip", "sda_longlat")], table))
