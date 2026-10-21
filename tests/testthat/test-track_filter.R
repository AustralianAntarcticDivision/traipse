ref <- readRDS(test_path("ref", "trip-1.10.0.rds"))

test_that("speed filter matches trip 1.10.0 on projected data", {
  d <- trips0
  tr <- TRACK(d$x * 1e5, d$y * 1e5, d$date, d$id, longlat = FALSE)
  expect_equal(track_speedfilter(tr, ref$args$speed_projected)$ok, ref$speed_projected)
})

test_that("speed filter matches trip 1.10.0 (distance pairs corrected) on longlat", {
  ok <- track_speedfilter(as_track(trips0), ref$args$speed_longlat)$ok
  expect_equal(ok, ref$speed_longlat)
  ## trip as released measures the two-segment speed on the wrong pairs
  expect_false(identical(ok, ref$speed_longlat_trip))
})

test_that("sda matches trip 1.10.0 (distance pairs corrected)", {
  expect_equal(track_sda(as_track(trips0), ref$args$sda)$ok, ref$sda_longlat)
  expect_error(track_sda(TRACK(1:4, 1:4, Sys.time() + 1:4, 1, longlat = FALSE), 10),
               "longitude")
})

test_that("short tracks pass", {
  tr <- TRACK(c(0, 10, 0), c(0, 0, 0), Sys.time() + c(0, 1, 2), 1)
  expect_true(all(track_speedfilter(tr, 1)$ok))
})
