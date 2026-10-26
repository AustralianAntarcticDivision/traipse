test_that("TRACK builds keyed tables", {
  tr <- TRACK(trips0$x, trips0$y, trips0$date, trips0$id)
  expect_s3_class(tr, "TRACK")
  expect_equal(nrow(track_fixes(tr)), nrow(trips0))
  expect_equal(track_tracks(tr)$.track, c(0, 1, 2))
  expect_equal(track_fixes(tr)$.fix, seq_len(nrow(trips0)) - 1)
  expect_true(tr$meta$longlat)
  expect_equal(tr$meta$segment, "geodesic")
  expect_equal(tr$meta$rung, "sequential")
  s <- track_segments(tr)
  expect_equal(nrow(s), nrow(trips0) - 3)
  expect_true(all(s$.fix1 == s$.fix0 + 1))
  expect_true(all(s$dt > 0))
  expect_equal(as_track(trips0)$fixes, tr$fixes)
})

test_that("crs decides longlat and segment", {
  tr <- TRACK(1:3, 1:3, Sys.time() + 1:3, 1, crs = "EPSG:3031")
  expect_false(tr$meta$longlat)
  expect_equal(tr$meta$segment, "planar")
  expect_true(TRACK(1:3, 1:3, Sys.time() + 1:3, 1, crs = "+proj=longlat")$meta$longlat)
})

test_that("policy error stops and repair records what it did", {
  t0 <- ISOdatetime(2001, 1, 1, 0, 0, 0, tz = "UTC")
  d <- data.frame(x = c(1, 2, 3, 3, 4, 5), y = c(1, 2, 3, 3, 4, NA),
                  date = t0 + c(3, 1, 2, 2, 2, 5), id = "a")
  expect_error(as_track(d), "missing values")
  tr <- as_track(d, policy = "repair")
  expect_equal(nrow(track_fixes(tr)), 4)
  expect_false(anyDuplicated(as.numeric(track_fixes(tr)$date)) > 0)
  expect_false(is.unsorted(track_fixes(tr)$date))
  expect_length(tr$meta$log, 4)
  expect_output(print(tr), "repaired")
})

test_that("extra columns are kept as fix data", {
  d <- trips0
  d$lc <- rep(c("A", "B", "0"), length.out = nrow(d))
  expect_equal(track_fixes(as_track(d))$lc, d$lc)
})

test_that("metrics agree with the vector kernels", {
  tr <- as_track(trips0)
  m <- track_metrics(tr)
  i <- trips0$id == "1"
  expect_equal(m$distance[m$.track == 0], track_distance(trips0$x[i], trips0$y[i])[-1])
  expect_equal(m$bearing[m$.track == 0], track_bearing(trips0$x[i], trips0$y[i])[-sum(i)])
  expect_equal(m$speed, m$distance / m$dt)
  p <- track_metrics(TRACK(c(0, 3), c(0, 4), Sys.time() + c(0, 10), 1, longlat = FALSE))
  expect_equal(p$distance, 5)
  expect_equal(p$bearing, atan2(3, 4) * 180 / pi)
})
