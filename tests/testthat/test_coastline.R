## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2

library(oce)

context("coastline")

test_that("various methods of coastline creation", {
          lon <- c(1,2,1)
          lat <- c(0,1,0)
          cl1 <- as.oce(list(longitude=lon, latitude=lat))
          cl2 <- as.oce(data.frame(longitude=lon, latitude=lat))
          cl3 <- as.coastline(longitude=lon, latitude=lat)
          cl4 <- new("coastline")
          cl4 <- oceSetData(cl4, "longitude", lon)
          cl4 <- oceSetData(cl4, "latitude", lat)
          expect_equal(cl1[["latitude"]], cl2[["latitude"]])
          expect_equal(cl1[["latitude"]], cl3[["latitude"]])
          expect_equal(cl1[["latitude"]], cl4[["latitude"]])
          expect_equal(cl1[["longitude"]], cl2[["longitude"]])
          expect_equal(cl1[["longitude"]], cl3[["longitude"]])
          expect_equal(cl1[["longitude"]], cl4[["longitude"]])
})

##> ## This really should work, but travisCI complains that it has
##> ## no package named rgeos, even though it is named in the
##> ## .travis.yml file. I give up. This functionality is checked
##> ## with local tests, and I don't see any reason for a dozen tests
##> ## that take 1/2 hour each, to find the magic incantation that
##> ## will shut travisCI up.
##> test_that("coastlineCut", {
##>           data(coastlineWorld)
##>           ## NOTE: the warning is silenced in normal usage because
##>           ## coastlineCut() sets options(warn=-1), but we still need
##>           ## to anticipate warnings, because testthat tricks the system
##>           ## into ignoring the options(warn) setting.
##>           cw100 <- expect_warning(coastlineCut(coastlineWorld, lon_0=100),
##>                                   "polygons do not intersect")
##> })

