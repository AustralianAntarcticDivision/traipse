#' Track segments in grid cells, and time spent
#'
#' `track_cells()` overlays the segments of a track model on a regular
#' grid. It returns one row per piece of a segment inside one cell, with
#' the segment parameters where the piece starts and ends (`p0`, `p1`, from
#' 0 at the first fix to 1 at the second) and the time spent in the piece
#' (`dt`, seconds), assuming constant speed along the segment. A segment
#' with no movement puts all its time in the cell of its fix.
#'
#' `track_time_spent()` sums that time by cell.
#'
#' The overlay is exact: the pieces come from where each segment crosses
#' the grid lines, which is arithmetic on the extent and resolution. It
#' needs planar segments (`segment = "planar"` in [TRACK()]); for longitude
#' and latitude data that means straight lines in degrees, which is what
#' the 'trip' package has always assumed for time spent.
#'
#' The grid is given as for [track_grid()] by `dimension` (ncol, nrow) and
#' `extent` (xmin, xmax, ymin, ymax), or as a 'meshcore' `GRID` model in
#' `dimension`. Cell ids in `.cell` are 0-based and row-major from the top
#' left cell (the GDAL and 'meshcore' order), so `.cell + 1` is the index
#' that [track_grid()] returns. Pieces outside the grid are dropped.
#'
#' @param x a track model, or anything [as_track()] accepts
#' @param dimension grid size `c(ncol, nrow)`, or a 'meshcore' `GRID`
#' @param extent grid extent `c(xmin, xmax, ymin, ymax)`, the range of the
#'   fixes if `NULL`
#' @param ... unused
#' @return `track_cells()`: a data frame with `.seg`, `.cell`, `p0`, `p1`
#'   and `dt`. `track_time_spent()`: a data frame with `.cell` and `time`
#'   (seconds), one row per cell with time in it.
#' @export
#' @examples
#' tr <- TRACK(trips0$x, trips0$y, trips0$date, trips0$id, segment = "planar")
#' tc <- track_cells(tr, c(50, 35))
#' head(tc)
#' ts <- track_time_spent(tr, c(50, 35))
#' v <- numeric(50 * 35)
#' v[ts$.cell + 1] <- ts$time
#' m <- matrix(v, 35, 50, byrow = TRUE)  ## first row is the top of the grid
#' image(t(m[nrow(m):1, ]))
track_cells <- function(x, dimension, extent = NULL, ...) {
  x <- as_track(x)
  if (!identical(x$meta$segment, "planar")) {
    stop("track_cells() needs planar segments, use TRACK(..., segment = \"planar\")")
  }
  if (inherits(dimension, "GRID")) {
    extent <- dimension$meta$extent
    dimension <- dimension$meta$dim
  }
  f <- x$fixes
  extent <- extent %||% ext_trip(f$x, f$y)
  .check_args(dimension, extent)
  nc <- dimension[1L]; nr <- dimension[2L]; ex <- extent
  rx <- (ex[2L] - ex[1L]) / nc
  ry <- (ex[4L] - ex[3L]) / nr
  s <- track_segments(x)
  n <- nrow(s)
  dx <- s$x1 - s$x0
  dy <- s$y1 - s$y0
  ## segment parameters where the segment crosses grid lines on one axis
  cross <- function(a0, d, lo, res) {
    a <- ceiling((pmin(a0, a0 + d) - lo) / res)
    b <- floor((pmax(a0, a0 + d) - lo) / res)
    k <- ifelse(d != 0, pmax(b - a + 1, 0), 0)
    i <- rep(seq_len(n), k)
    g <- lo + res * (rep(a, k) + sequence(k) - 1)
    list(i = i, p = (g - a0[i]) / d[i])
  }
  cx <- cross(s$x0, dx, ex[1L], rx)
  cy <- cross(s$y0, dy, ex[3L], ry)
  i <- c(seq_len(n), seq_len(n), cx$i, cy$i)
  p <- c(rep(0, n), rep(1, n), cx$p, cy$p)
  keep <- p >= 0 & p <= 1
  i <- i[keep]; p <- p[keep]
  o <- order(i, p)
  i <- i[o]; p <- p[o]
  u <- !duplicated(cbind(i, p))
  i <- i[u]; p <- p[u]
  ## consecutive parameters within one segment bound a piece; a segment
  ## with no movement keeps the single piece [0, 1]
  j <- which(c(i[-1L], NA) == i)
  seg <- i[j]
  p0 <- p[j]
  p1 <- p[j + 1L]
  mid <- (p0 + p1) / 2
  px <- s$x0[seg] + mid * dx[seg]
  py <- s$y0[seg] + mid * dy[seg]
  col <- floor((px - ex[1L]) / rx)
  row <- floor((ex[4L] - py) / ry)
  ## points on the right and bottom edges belong to the last column and row
  col[px == ex[2L]] <- nc - 1
  row[py == ex[3L]] <- nr - 1
  cell <- ifelse(col < 0 | col >= nc | row < 0 | row >= nr, NA_real_, row * nc + col)
  out <- data.frame(.seg = s$.seg[seg], .cell = cell, p0 = p0, p1 = p1,
                    dt = (p1 - p0) * s$dt[seg])
  out <- out[!is.na(out$.cell), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' @rdname track_cells
#' @export
track_time_spent <- function(x, dimension, extent = NULL, ...) {
  tc <- track_cells(x, dimension, extent)
  if (!nrow(tc)) return(data.frame(.cell = numeric(), time = numeric()))
  agg <- tapply(tc$dt, tc$.cell, sum)
  data.frame(.cell = as.numeric(names(agg)), time = as.vector(agg))
}
