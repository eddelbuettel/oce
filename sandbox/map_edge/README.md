---
title: map_edge: try to detect edge of earth
author: Dan Kelley
date: Feb 2019
---

Code in this directory tests methods for finding the edge-of-the-world for
mapPlot() views. The hope is to translate this boundary (replaced by plot edges
when they are on land) from xy space to lon-lat space, so that this can be used
to clip the coastline, thus yielding a solution to the UHL (ugly horizontal
line) and other mapping problems.

* `01.R` scans through y. A good proof of concept, for detecting points that
  cannot be inverted, and for using uniroot() to find edges based on this as an
edge criterion. This scans through y, finding a left-hand and right-hand
limits. Negative features of method: resolution (for globe views, which will be
typical) varies with latitude; code complexity with left-right searches. The
green line traces a problematic coastline element (detected with a mouse
click).

* `02.R` scans along radial lines passing through centre of image. The code is
  a lot simpler. I still do not detect box edges, but that will be simple. The
polygon for the earth is in transparent red, and the outline (with the dots
that define it) is red. Also, as a check on the transformation to lon-lat, the
outline in that system is shown in a blue dotted line.

* `03.R` checks the intersection of Russia and the edge. The RH panel suggests
  that this method holds some promise.

* `04.R` checks with the whole world. All looks good, including our dear old
  friend, Antarctica; see the right-hand panel of 04.pdf

* `05.R` traces box border, if necessary

* `06.R` better tracing but problems if pole or dateline in view

* `07.R` better on pole- and dateline-showing views. More tests. NOTE: if we
  shift the lon_0 we can get missing Eurasia ... I never thought about that
properly before.

**Next steps:**
- [x] test the intersection (done in 03.R)
- [x] trace box border
- [ ] think about interrupted projections (maybe ignore for now)
- [ ] code as oce::mapPlotTest() for testing (always show 2-panel plots in tests)



