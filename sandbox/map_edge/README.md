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

**Next steps:**
- trace box edges
- test the intersection
- think about interrupted projections (maybe ignore for now)
- code as oce::mapPlotTest() for testing (always show 2-panel plots in tests)



