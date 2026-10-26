# traipse dev

* New track model: `TRACK()` and `as_track()` build keyed tables of tracks and
  fixes (0-based `.track` and `.fix`, ordered by date-time within track), with
  the validity rules of the 'trip' package and an explicit `policy` ("error"
  or "repair", which records what it changed). `track_fixes()`,
  `track_tracks()`, `track_segments()` and `track_metrics()` return the tables.

* New `track_cells()` and `track_time_spent()`: exact overlay of planar track
  segments on a regular grid, and time spent per cell. Matches
  `trip::rasterize()` (trip 1.10.0) without 'sp', 'raster' or 'spatstat'.

* New `track_speedfilter()` and `track_sda()`, ports of `trip::speedfilter()`
  and `trip::sda()` that return a flag per fix. The two-segment speed now uses
  the right pairs of fixes for longitude and latitude data, which 'trip'
  1.10.0 did not.

* The pipe `%>%` is no longer re-exported (follows the removal of
  'R/utils-pipe.R'), and 'magrittr' is no longer imported.

# traipse 0.4.0

* Package repository has changed to now https://github.com/AustralianAntarcticDivision/traipse, for issues and information.

* traipse now depends on CRAN package geographiclib. 

* Entirely removed geosphere dependency. 

* Now using geographiclib, have equivalence for bearings, angles, and turns. Better accuracy now for
'track_intermediate()' on exact ellipsoidal calculations.
 
* Fixed: track_angle and track_turn now return all-NA for fewer than 3 input points. 
 
# traipse 0.3.0

* New function 'track_grid()' to compute cell index for points for a grid. 

* Fixed test failing as newer R allows raw numeric input to date-time. 

# traipse 0.2.5

* Patch fix for new {geosphere} >= 1.5.14, thanks to CRAN notification pretest reverse
 dependency checking. 

# traipse 0.2.0

* New function `track_query()` similar to `track_intermediate()` but for arbitrary
 points in time. In-development. 
 
* New function `track_intermediate()` to perform interpolation along a great circle
 between input locations. The interpolation step may be set by `distance` or `duration`
 and date-time is interpolated if included. 
 
* Metres, seconds, degrees, everywhere. 

# traipse 0.1.0

* Fixed Value documentation for various functions, thanks to CRAN feedback. 

* New functions `track_time`, `track_speed`,`track_distance_to()` and `track_bearing_to()`. 

* Added basic functions `track_distance`, `track_angle`, `track_turn`, and `track_bearing`. 
