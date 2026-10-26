#' Track model
#'
#' `TRACK()` builds a track model from columns of coordinates, date-times
#' and track ids. A track model is a list of keyed tables: `tracks` (one
#' row per track, key `.track`) and `fixes` (one row per location, key
#' `.fix`, ordered by date-time within track). Segments between
#' consecutive fixes are derived on demand with [track_segments()]. Ids are
#' 0-based, as in the mesh models of 'meshcore'.
#'
#' A fix is identified by its track and its date-time, not by its
#' position: two fixes at the same coordinates are different fixes. This
#' is recorded as `rung = "sequential"` in `meta`.
#'
#' The rules are those of the 'trip' package: no missing values in the
#' coordinates, date-times or ids, records ordered by date-time within
#' track, no duplicated records, and no duplicated date-times within a
#' track. `policy = "error"` stops when a rule is broken. `policy =
#' "repair"` removes records with missing values, orders the records,
#' removes duplicated records, and moves duplicated date-times forward by
#' one second until they are unique, and records what it did in
#' `meta$log`.
#'
#' `longlat` says whether the coordinates are longitude and latitude,
#' which decides how distances are measured. When it is `NULL` it is taken
#' from `crs`: `TRUE` when `crs` is missing or names a longitude and
#' latitude system, `FALSE` otherwise. `segment` says what the line between
#' two fixes is: `"geodesic"` (the default for longitude and latitude) or
#' `"planar"` (straight in the coordinate system, the default otherwise).
#' [track_cells()] needs planar segments.
#'
#' @param x,y coordinates
#' @param date date-times (converted with [as.POSIXct()])
#' @param id track ids (a single value is used for every fix)
#' @param data optional data frame of other columns, one row per fix
#' @param crs coordinate reference system, carried as a string
#' @param longlat logical, are `x` and `y` longitude and latitude? See Details.
#' @param segment `"geodesic"` or `"planar"`, see Details
#' @param policy `"error"` or `"repair"`, see Details
#' @return A track model: a list with `meta`, `tracks` and `fixes`, with
#'   class `"TRACK"`.
#' @seealso [as_track()], [track_fixes()], [track_segments()],
#'   [track_cells()]
#' @export
#' @examples
#' tr <- TRACK(trips0$x, trips0$y, trips0$date, trips0$id)
#' tr
#' head(track_segments(tr))
TRACK <- function(x, y, date, id, data = NULL, crs = NA_character_,
                  longlat = NULL, segment = NULL,
                  policy = c("error", "repair")) {
  policy <- match.arg(policy)
  if (is.null(longlat)) longlat <- crs_is_longlat(crs)
  if (is.null(segment)) segment <- if (longlat) "geodesic" else "planar"
  segment <- match.arg(segment, c("geodesic", "planar"))
  n <- length(x)
  if (length(id) == 1L) id <- rep(id, n)
  if (!length(y) == n || !length(date) == n || !length(id) == n) {
    stop("x, y, date and id must have the same length")
  }
  if (!inherits(date, "POSIXct")) date <- as.POSIXct(date)
  fx <- data.frame(id = id, date = date, x = x, y = y, stringsAsFactors = FALSE)
  if (!is.null(data)) {
    data <- as.data.frame(data)
    if (!nrow(data) == n) stop("data must have one row per fix")
    data <- data[setdiff(names(data), names(fx))]
    fx <- cbind(fx, data)
  }
  log <- character()
  bad <- is.na(fx$x) | is.na(fx$y) | is.na(fx$date) | is.na(fx$id)
  if (any(bad)) {
    if (policy == "error") stop("missing values in x, y, date or id")
    fx <- fx[!bad, , drop = FALSE]
    log <- c(log, sprintf("removed %i records with missing values", sum(bad)))
  }
  o <- order(fx$id, fx$date)
  if (is.unsorted(o)) {
    if (policy == "error") stop("records are not ordered by date within id")
    fx <- fx[o, , drop = FALSE]
    log <- c(log, "ordered records by id, then date")
  }
  dup <- duplicated(fx)
  if (any(dup)) {
    if (policy == "error") stop("duplicated records")
    fx <- fx[!dup, , drop = FALSE]
    log <- c(log, sprintf("removed %i duplicated records", sum(dup)))
  }
  dt <- duplicated(fx[c("id", "date")])
  if (any(dt)) {
    if (policy == "error") stop("duplicated date-times within id")
    nudged <- sum(dt)
    while (any(dt)) {
      fx$date[dt] <- fx$date[dt] + 1
      dt <- duplicated(fx[c("id", "date")])
    }
    fx <- fx[order(fx$id, fx$date), , drop = FALSE]
    log <- c(log, sprintf("moved %i duplicated date-times forward by 1 s", nudged))
  }
  ids <- unique(fx$id)
  tracks <- data.frame(.track = seq_along(ids) - 1, id = ids, stringsAsFactors = FALSE)
  fx$.track <- match(fx$id, ids) - 1
  fx$.fix <- seq_len(nrow(fx)) - 1
  fx <- fx[c(".fix", ".track", setdiff(names(fx), c(".fix", ".track", "id")))]
  rownames(fx) <- NULL
  structure(list(meta = list(model = "TRACK", crs = crs, longlat = longlat,
                             segment = segment, rung = "sequential",
                             policy = policy, log = log),
                 tracks = tracks, fixes = fx),
            class = "TRACK")
}

crs_is_longlat <- function(crs) {
  if (is.null(crs) || length(crs) < 1 || is.na(crs[1L]) || !nzchar(crs[1L])) return(TRUE)
  grepl("longlat|lonlat|latlong|EPSG:4326|OGC:CRS84|GEOGCRS|GEOGCS", crs[1L],
        ignore.case = TRUE)
}

#' @export
print.TRACK <- function(x, ...) {
  m <- x$meta
  cat(sprintf("<TRACK> %i fixes in %i tracks; segments %s%s\n",
              nrow(x$fixes), nrow(x$tracks), m$segment,
              if (m$longlat) " (longitude, latitude)" else ""))
  if (!is.na(m$crs)) cat(sprintf("crs: %s\n", m$crs))
  if (length(m$log)) cat(paste0("repaired: ", m$log, collapse = "\n"), "\n")
  invisible(x)
}

#' Convert to a track model
#'
#' `as_track()` makes a [TRACK()] from a data frame or a 'trip' object.
#'
#' For a data frame, give the names of the coordinate, date-time and id
#' columns; every other column is kept as fix data. For a 'trip' object the
#' columns are read from the object, without loading 'sp'.
#'
#' @param x a data frame, or a 'trip' object
#' @param ... passed to [TRACK()] (`crs`, `longlat`, `segment`, `policy`)
#' @param xcol,ycol,date,id names of the columns
#' @return A track model, see [TRACK()].
#' @export
#' @examples
#' as_track(trips0)
as_track <- function(x, ...) UseMethod("as_track")

#' @rdname as_track
#' @export
as_track.TRACK <- function(x, ...) x

#' @rdname as_track
#' @export
as_track.data.frame <- function(x, ..., xcol = "x", ycol = "y", date = "date", id = "id") {
  x <- as.data.frame(x)
  cols <- c(xcol, ycol, date, id)
  miss <- setdiff(cols, names(x))
  if (length(miss)) stop(sprintf("columns not found: %s", paste(miss, collapse = ", ")))
  other <- x[setdiff(names(x), cols)]
  TRACK(x[[xcol]], x[[ycol]], x[[date]], x[[id]],
        data = if (ncol(other)) other else NULL, ...)
}

#' @rdname as_track
#' @export
as_track.trip <- function(x, ...) {
  tor <- methods::slot(x, "TOR.columns")
  xy <- methods::slot(x, "coords")
  d <- methods::slot(x, "data")
  crs <- methods::slot(methods::slot(x, "proj4string"), "projargs")
  other <- d[setdiff(names(d), tor)]
  TRACK(xy[, 1L], xy[, 2L], d[[tor[1L]]], d[[tor[2L]]],
        data = if (ncol(other)) other else NULL, crs = crs, ...)
}

#' Tables of a track model
#'
#' `track_fixes()` and `track_tracks()` return the stored tables of a
#' [TRACK()]. `track_segments()` derives the segments: one row per pair of
#' consecutive fixes within a track, keyed by `.seg`, with the two fix keys
#' (`.fix0`, `.fix1`), their coordinates and date-times, and the duration
#' `dt` in seconds.
#'
#' @param x a track model, or anything [as_track()] accepts
#' @param ... unused
#' @return A data frame.
#' @export
#' @examples
#' tr <- as_track(trips0)
#' head(track_fixes(tr))
#' track_tracks(tr)
#' head(track_segments(tr))
track_fixes <- function(x, ...) as_track(x)$fixes

#' @rdname track_fixes
#' @export
track_tracks <- function(x, ...) as_track(x)$tracks

#' @rdname track_fixes
#' @export
track_segments <- function(x, ...) {
  f <- as_track(x)$fixes
  n <- nrow(f)
  if (n < 2) {
    i0 <- integer()
  } else {
    i0 <- which(f$.track[-1L] == f$.track[-n])
  }
  i1 <- i0 + 1L
  data.frame(.seg = seq_along(i0) - 1, .track = f$.track[i0],
             .fix0 = f$.fix[i0], .fix1 = f$.fix[i1],
             x0 = f$x[i0], y0 = f$y[i0], x1 = f$x[i1], y1 = f$y[i1],
             date0 = f$date[i0], date1 = f$date[i1],
             dt = as.numeric(unclass(f$date[i1])) - as.numeric(unclass(f$date[i0])))
}

#' Segment metrics
#'
#' Distance, duration, speed and bearing for every segment of a track
#' model. For longitude and latitude, distance is geodesic in metres and
#' bearing is the initial geodesic azimuth in degrees, as from
#' [track_distance()] and [track_bearing()]. Otherwise distance is
#' Euclidean in the units of the coordinates, and bearing is the planar
#' direction clockwise from the y axis.
#'
#' @param x a track model, or anything [as_track()] accepts
#' @param ... unused
#' @return [track_segments()] with columns `distance`, `speed` (distance
#'   per second) and `bearing`.
#' @export
#' @examples
#' head(track_metrics(trips0))
track_metrics <- function(x, ...) {
  x <- as_track(x)
  s <- track_segments(x)
  if (!nrow(s)) {
    s$distance <- s$speed <- s$bearing <- numeric()
    return(s)
  }
  if (x$meta$longlat) {
    s$distance <- geodist::geodist(cbind(lon = s$x0, lat = s$y0), cbind(lon = s$x1, lat = s$y1),
                                   paired = TRUE, measure = "geodesic")
    s$bearing <- geographiclib::geodesic_inverse(cbind(s$x0, s$y0),
                                                 cbind(s$x1, s$y1))[["azi1"]]
  } else {
    s$distance <- sqrt((s$x1 - s$x0)^2 + (s$y1 - s$y0)^2)
    s$bearing <- (atan2(s$x1 - s$x0, s$y1 - s$y0) * 180 / pi) %% 360
  }
  s$speed <- s$distance / s$dt
  s
}
