t0 <- ISOdatetime(2001, 1, 1, 0, 0, 0, tz = "UTC")

test_that("one segment splits exactly across cells", {
  ## 2 x 1 grid over [0, 2] x [0, 1]; segment from x = 0.5 to x = 1.5 in 100 s
  tr <- TRACK(c(0.5, 1.5), c(0.5, 0.5), t0 + c(0, 100), 1, longlat = FALSE)
  tc <- track_cells(tr, c(2, 1), c(0, 2, 0, 1))
  expect_equal(tc$.cell, c(0, 1))
  expect_equal(tc$dt, c(50, 50))
  expect_equal(tc$p0, c(0, 0.5))
  ## a diagonal through a corner of a 2 x 2 grid
  tr <- TRACK(c(0.5, 1.5), c(1.5, 0.5), t0 + c(0, 10), 1, longlat = FALSE)
  ts <- track_time_spent(tr, c(2, 2), c(0, 2, 0, 2))
  expect_equal(ts$.cell, c(0, 3))
  expect_equal(ts$time, c(5, 5))
})

test_that("a fix with no movement keeps its time in its cell", {
  tr <- TRACK(c(0.5, 0.5, 1.5), c(0.5, 0.5, 0.5), t0 + c(0, 30, 130), 1, longlat = FALSE)
  ts <- track_time_spent(tr, c(2, 1), c(0, 2, 0, 1))
  expect_equal(ts$time, c(80, 50))
})

test_that("time is conserved inside the grid", {
  tr <- TRACK(trips0$x, trips0$y, trips0$date, trips0$id, segment = "planar")
  ts <- track_time_spent(tr, c(50, 35))
  expect_equal(sum(ts$time), sum(track_segments(tr)$dt))
})

test_that("geodesic segments are refused", {
  expect_error(track_cells(as_track(trips0), c(10, 10)), "planar")
})

test_that("track_cells agrees with track_grid on cell ids", {
  tr <- TRACK(trips0$x, trips0$y, trips0$date, trips0$id, segment = "planar")
  ex <- c(range(trips0$x), range(trips0$y))
  tc <- track_cells(tr, c(50, 35), ex)
  first <- tc[!duplicated(tc$.seg) & tc$p0 == 0, ]
  s <- track_segments(tr)
  g <- track_grid(s$x0, s$y0, c(50, 35), ex)
  expect_equal(first$.cell + 1, g[first$.seg + 1])
})

test_that("time spent matches trip 1.10.0 rasterize()", {
  ref <- readRDS(test_path("ref", "trip-1.10.0.rds"))$time_spent
  tr <- TRACK(trips0$x, trips0$y, trips0$date, trips0$id, segment = "planar")
  ts <- track_time_spent(tr, ref$dimension, ref$extent)
  ts <- ts[ts$time > 0, ]
  expect_equal(ts$.cell, ref$.cell)
  expect_equal(ts$time, ref$time, tolerance = 1e-9)
})

test_that("a meshcore-style GRID gives the grid", {
  g <- structure(list(meta = list(dim = c(50, 35), extent = c(range(trips0$x), range(trips0$y)))),
                 class = c("GRID", "meshcore_model"))
  tr <- TRACK(trips0$x, trips0$y, trips0$date, trips0$id, segment = "planar")
  expect_equal(track_time_spent(tr, g), track_time_spent(tr, g$meta$dim, g$meta$extent))
})
