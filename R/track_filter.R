#' Speed and angle filters for track models
#'
#' `track_speedfilter()` flags fixes that imply a speed of travel above
#' `max_speed`, with the iterative running root mean square of McConnell
#' et al. (1992). `track_sda()` adds the distance and angle tests of
#' Freitas et al. (2008). Both are ports of `speedfilter()` and `sda()` from
#' the 'trip' package; the fixes are not removed, the result says which
#' pass.
#'
#' Speeds are in km/h and distances in km for longitude and latitude
#' (geodesic distance), as in 'trip'; otherwise in coordinate units per
#' hour and coordinate units. `track_sda()` needs longitude and latitude.
#'
#' Two differences from 'trip' 1.10.0. The speed over two segments (from a
#' fix to the one after next) uses the distance between those two fixes;
#' 'trip' measured it along the wrong pairs of fixes for longitude and
#' latitude data, so its flags can differ there. And where 'trip' could
#' loop forever on a track whose remaining fixes are too few for the
#' running mean, the filter stops for that track.
#'
#' @param x a track model, or anything [as_track()] accepts
#' @param max_speed,smax maximum speed (km/h for longitude and latitude)
#' @param ang minimum turning angles in degrees, one per pass
#' @param distlim maximum step lengths in km, one per pass
#' @param pre optional logical vector, one per fix, of fixes already
#'   rejected (`FALSE`) by another filter
#' @param ... unused
#' @return A data frame with `.fix` and the logical `ok`, `FALSE` where
#'   the fix fails.
#' @references
#' McConnell, B. J., Chambers, C. and Fedak, M. A. (1992) Foraging ecology
#' of southern elephant seals in relation to the bathymetry and productivity
#' of the Southern Ocean. Antarctic Science 4:393-398.
#'
#' Freitas, C., Lydersen, C., Fedak, M. A. and Kovacs, K. M. (2008) A simple
#' new algorithm to filter marine mammal Argos locations. Marine Mammal
#' Science 24:315-325. \doi{10.1111/j.1748-7692.2007.00180.x}
#' @export
#' @examples
#' tr <- as_track(trips0)
#' table(track_speedfilter(tr, max_speed = 20)$ok)
#' table(track_sda(tr, smax = 20)$ok)
track_speedfilter <- function(x, max_speed, ...) {
  x <- as_track(x)
  f <- x$fixes
  ok <- unlist(lapply(split(seq_len(nrow(f)), f$.track), function(i) {
    speedfilter_one(f$x[i], f$y[i], as.numeric(unclass(f$date[i])),
                    max_speed, x$meta$longlat)
  }), use.names = FALSE)
  data.frame(.fix = f$.fix, ok = ok)
}

#' @rdname track_speedfilter
#' @export
track_sda <- function(x, smax, ang = c(15, 25), distlim = c(2.5, 5.0), pre = NULL, ...) {
  x <- as_track(x)
  if (!x$meta$longlat) stop("track_sda() needs longitude and latitude")
  f <- x$fixes
  if (!is.null(pre) && !length(pre) == nrow(f)) stop("pre must have one value per fix")
  ok <- unlist(lapply(split(seq_len(nrow(f)), f$.track), function(i) {
    sda_one(f$x[i], f$y[i], as.numeric(unclass(f$date[i])), smax, ang, distlim,
            if (is.null(pre)) NULL else pre[i])
  }), use.names = FALSE)
  data.frame(.fix = f$.fix, ok = ok)
}

## distance between fixes i and j (km for longlat)
pair_distance <- function(x0, y0, x1, y1, longlat) {
  if (!length(x0)) return(numeric())
  if (longlat) {
    geodist::geodist(cbind(lon = x0, lat = y0), cbind(lon = x1, lat = y1),
                     paired = TRUE, measure = "geodesic") / 1000
  } else {
    sqrt((x1 - x0)^2 + (y1 - y0)^2)
  }
}

## speedfilter() from trip, for one track; t in seconds
speedfilter_one <- function(x, y, t, max_speed, longlat) {
  pprm <- 3
  offset <- pprm - 1
  npts <- length(x)
  ok <- rep(TRUE, npts)
  if (npts < (pprm + 1)) return(ok)
  RMS <- rep(max_speed + 1, npts)
  index <- seq_len(npts)
  while (any(RMS > max_speed, na.rm = TRUE)) {
    k <- which(ok)
    n <- length(k)
    if (n < pprm + 1) break
    xk <- x[k]; yk <- y[k]; tk <- t[k]
    speed1 <- pair_distance(xk[-n], yk[-n], xk[-1L], yk[-1L], longlat) /
      (diff(tk) / 3600)
    speed2 <- pair_distance(xk[-c(n - 1, n)], yk[-c(n - 1, n)], xk[-(1:2)], yk[-(1:2)], longlat) /
      ((tk[-c(1, 2)] - tk[-c(n - 1, n)]) / 3600)
    thisIndex <- index[k]
    ns <- length(speed1)
    sub1 <- rep(1:2, ns - offset) + rep(1:(ns - offset), each = 2)
    sub2 <- rep(c(0, 2), ns - offset) + rep(1:(ns - offset), each = 2)
    rmsRows <- cbind(matrix(speed1[sub1], ncol = offset, byrow = TRUE),
                     matrix(speed2[sub2], ncol = offset, byrow = TRUE))
    RMS <- c(rep(0, offset), sqrt(rowSums(rmsRows^2) / ncol(rmsRows)))
    RMS[length(RMS)] <- 0
    bad <- RMS > max_speed
    segs <- cumsum(c(0, abs(diff(bad))))
    rmsFlag <- unlist(lapply(split(RMS, segs), function(r) seq_along(r) == which.max(r)),
                      use.names = FALSE)
    rmsFlag[!bad] <- FALSE
    RMS[rmsFlag] <- -10
    ok[thisIndex][rmsFlag > 0] <- FALSE
  }
  ok
}

## spherical azimuth, as trip's internal .abdali()
abdali <- function(x0, y0, x1, y1) {
  rad <- pi / 180
  res <- rep(NA_real_, length(x0))
  keep <- !(x0 == x1 & y0 == y1)
  if (!any(keep)) return(res)
  lon0 <- x0[keep] * rad; lat0 <- y0[keep] * rad
  lon1 <- x1[keep] * rad; lat1 <- y1[keep] * rad
  dlon <- lon1 - lon0
  azm <- atan2(sin(dlon), cos(lat0) * tan(lat1) - sin(lat0) * cos(dlon)) / rad
  res[keep] <- (azm + 360) %% 360
  res
}

## trip's trackAngle() for one track: NA at the ends
turn_abs <- function(x, y) {
  n <- length(x)
  if (n < 3) return(rep(NA_real_, n))
  i <- 2:(n - 1)
  a <- abdali(x[i], y[i], x[i + 1], y[i + 1]) - abdali(x[i], y[i], x[i - 1], y[i - 1])
  c(NA, abs((a + 180) %% 360 - 180), NA)
}

## step distances in km with 0 for the first fix, as trip's trackDistance()
step_km <- function(x, y) {
  n <- length(x)
  if (n < 2) return(rep(0, n))
  c(0, pair_distance(x[-n], y[-n], x[-1L], y[-1L], TRUE))
}

## sda0() from trip, for one track
sda_one <- function(x, y, t, smax, ang, distlim, pre = NULL) {
  speed_ok <- speedfilter_one(x, y, t, smax, TRUE)
  dprev <- step_km(x, y)
  ok <- speed_ok | dprev <= distlim[2L]
  if (!is.null(pre)) ok <- ok & pre
  rows <- which(ok)
  for (i in seq_along(distlim)) {
    xi <- x[rows]; yi <- y[rows]
    dprev <- step_km(xi, yi)
    dnext <- c(dprev[-1L], 0)
    angs <- turn_abs(xi, yi)
    angs[is.na(angs)] <- 180
    keep <- (dprev <= distlim[i] | dnext <= distlim[i]) | angs > ang[i]
    nk <- length(keep)
    keep[unique(pmax(1, pmin(nk, c(1:2, (nk - 1):nk))))] <- TRUE
    rows <- rows[keep]
  }
  out <- rep(FALSE, length(x))
  out[rows] <- TRUE
  out
}
