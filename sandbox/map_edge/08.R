rm(list=ls())
DEBUG <- 1
epsilon <- 0.01
## References
## https://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf
## ?"plot,SpatialPolygons,missing-method"

datelineMethod1 <- FALSE
closeup <- !TRUE
showWork <- 0
library(oce)
library(raster)
library(sp)
data(coastlineWorld)
##cwr <- coastlineCut(coastlineWorld, -100)

lonfix <- function(lon)
    ifelse(lon > 180, lon - 360, lon)

xy2SpatialPolygons <- function(x, y, name="a")
{
    sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(cbind(x, y))), name)))
}

# returns list
polygonCoords <- function(p)
{
    if (!inherits(p, "Polygon"))
        stop("p must be a Polygon object")
    xy <- p@coords
    list(x=res[,1], y=res[,2])
}

polygonsCoords <- function(p)
{
    if (1 != length(p@polygons))
        stop("Polygons has ", length(p@polygons), " polygons (should have 1)")
    if (1 != length(p@polygons[[1]]@Polygons))
        stop("Plygons first item has ", length(p@polygons[[1]]@Polygons), " polygons (should have 1)")
    res <- p@polygons[[1]]@Polygons[[1]]@coords
    list(x=res[,1], y=res[,2])
}


projlist <- list(stereN="+proj=stere +lat_0=90",
                 stereNE100="+proj=stere +lat_0=90 +lon_0=100",
                 stereNW100="+proj=stere +lat_0=90 +lon_0=-100",
                 ortho="+proj=ortho",
                 orthoE20="+proj=ortho +lon_0=20",
                 orthoE40="+proj=ortho +lon_0=40",
                 orthoE60="+proj=ortho +lon_0=60",
                 orthoE80="+proj=ortho +lon_0=80",
                 orthoE100="+proj=ortho +lon_0=100",
                 orthoE120="+proj=ortho +lon_0=120",
                 orthoE140="+proj=ortho +lon_0=140",
                 orthoE160="+proj=ortho +lon_0=160 +lat_0=20", # FIXME: N.Amer. missing
                 orthoE180="+proj=ortho +lon_0=180",
                 orthoW20="+proj=ortho +lon_0=-20",
                 orthoW40="+proj=ortho +lon_0=-40",
                 orthoW60="+proj=ortho +lon_0=-60",
                 orthoW80="+proj=ortho +lon_0=-80",
                 orthoW100="+proj=ortho +lon_0=-100",
                 orthoW120="+proj=ortho +lon_0=-120",
                 orthoW140="+proj=ortho +lon_0=-140",
                 orthoW160="+proj=ortho +lon_0=-160",
                 orthoW180="+proj=ortho +lon_0=-180",
                 robin="+proj=robin",
                 moll="+proj=moll",
                 mollE100="+proj=moll +lon_0=100", # errors
                 mollW100="+proj=moll +lon_0=-100")
namelist <- names(projlist)

for(iproj in seq_along(projlist)) {
    ##if (!length(grep("ortho", namelist[iproj]))) next
    ##if (!length(grep("stereN", namelist[iproj]))) next
    if (DEBUG==1 && namelist[iproj] != "mollE100") next
    message(namelist[iproj])
    if (!interactive())
        pdf(paste("08_", names(projlist)[iproj], ".pdf", sep=""), height=3, pointsize=9)
    projection <- projlist[[iproj]]
    ## projection <- paste("+proj=", proj, " +lon_0=50", sep="")
    message("projection=\"",projection, "\"", sep="")
    if (FALSE && names(projlist)[iproj] %in% c("stere", "moll3")) {
        lonlat <- list(lon=cwr[["longitude"]], lat=cwr[["latitude"]])
        lonlat$lon <- c(lonlat$lon, NA)
        lonlat$lat <- c(lonlat$lat, NA)
        cl <- cwr
    } else {
        lonlat <- list(lon=coastlineWorld[["longitude"]], lat=coastlineWorld[["latitude"]])
        lonlat$lon <- c(lonlat$lon, NA)
        lonlat$lat <- c(lonlat$lat, NA)
        cl <- coastlineWorld
    }

    par(mfrow=c(1, 3), mar=c(1.5, 1.5, 0.5, 0.5), mgp=c(2, 0.7, 0))
    if (closeup) {
        mapPlot(cl, projection=projection, col="gray",
                longitudelim=c(0, 180), latitudelim=c(-90,90))
    } else {
        if (length(grep("stere", names(projlist)[iproj]))) {
            mapPlot(cl, projection=projection, col="gray",
                    longitudelim=c(-130, 50), latitudelim=c(70, 110))
        } else {
            mapPlot(cl, projection=projection, col="gray")
        }
    }
    xy <- lonlat2map(lonlat$lon, lonlat$lat)

    nas <- which(is.na(lonlat$lon))

    usr <- par("usr")
    x0 <- mean(usr[1:2])
    y0 <- mean(usr[3:4])
    lx <- usr[2] - x0
    ly <- usr[4] - y0
    ldiag <- sqrt(lx^2 + ly^2)
    offworld <- function(x, y, debug=0) {
        if (debug) message("x=", x, " y=", y)
        ll <- map2lonlat(x, y)
        if (debug) print(ll)
        if (debug) message("ll=", paste(ll, collapse=" "))
        if (!is.finite(ll$longitude) || !is.finite(ll$latitude))
            return(1)
        xy <- lonlat2map(ll$longitude, ll$latitude)
        if (debug) print(xy)
        mismatch <- abs(x - xy$x) + abs(y - xy$y)
        if (debug) message("mismatch=", mismatch, " mismatch/ldiag=", mismatch/ldiag)
        if (mismatch < ldiag / 1e4) -1 else 1
    }
    tol <- (usr[2] - usr[1]) / 1e5
    N <- 128
    N <- 64
    ##N <- 32
    dtheta <- 2 * pi / N
    thetas <- seq(0, 2*pi-dtheta, dtheta)
    R <- 10 * sqrt(lx^2 + ly^2) # make very long; we trim later
    edger <- rep(NA, N)

    usrsps <- as(raster::extent(usr[1], usr[2], usr[3], usr[4]), "SpatialPolygons")
    ##. if (FALSE) { # testing
    ##.     par(xpd=NA)

    ##.     plot(usrsps)
    ##.     for (itheta in seq_along(thetas)) {
    ##.         theta <- thetas[itheta]
    ##.         rl <- sp::Line(cbind(x0 + c(0, R * cos(theta)), y0 + c(0, R * sin(theta))))
    ##.         rls <- sp::Lines(list(rl), "radial")
    ##.         rsls <- sp::SpatialLines(list(rls))
    ##.         i <- raster::intersect(rsls, usrsps)
    ##.         lines(rsls, col=2)
    ##.         ixy <- i@lines[[1]]@Lines[[1]]@coords
    ##.         points(ixy[2,1], ixy[2,2], col=3)
    ##.         span <- sqrt((ixy[2,1]-ixy[1,1])^2+ (ixy[2,2]-ixy[1,2])^2)
    ##.         ## message("theta=", theta, " span=", span)
    ##.     }
    ##. }

    for (itheta in seq_along(thetas)) {
        theta <- thetas[itheta]
        rl <- sp::Line(cbind(x0 + c(0, R * cos(theta)), y0 + c(0, R * sin(theta))))
        rls <- sp::Lines(list(rl), "radial")
        rsls <- sp::SpatialLines(list(rls))
        i <- raster::intersect(rsls, usrsps)
        ixy <- i@lines[[1]]@Lines[[1]]@coords
        span <- sqrt((ixy[2,1]-ixy[1,1])^2+ (ixy[2,2]-ixy[1,2])^2)
        ##> message(sprintf("theta=%.1f span=%.0f", theta, span))
        if (showWork) lines(x0+c(0,span*cos(theta)), y0+c(0,span*sin(theta)), col="pink", lty=3)
        points(x0+span*cos(theta), y0+span*sin(theta), col=4, pch=20)
        ##message("about to uniroot (theta=", theta, ")")
        ##points(x0, y0)
        points(x0+span*cos(theta), y0+span*sin(theta))
        prod <- offworld(x0,y0) * offworld(x0+span*cos(theta), y0+span*sin(theta))
        if (prod < 0) {
            ##message("computing with uniroot")
            edge <- uniroot(function(s) offworld(x0+s*cos(theta), y0+s*sin(theta)), c(0, span), tol=tol/100)
        } else {
            ##message("both points are on-world or off-world")
            edge <- list(r=span)
        }
        points(x0+edge$r * cos(theta), y0+edge$r * sin(theta), col="pink", pch=20)
        ## NOTE: move a bit inside to avoid problems at horizon, e.g. with
        ## ortho that shows the dateline.
        edger[itheta] <- edge$r * (1 - epsilon)
    }
    ldiag <- sqrt(lx^2 + ly^2)
    edgex <- x0 + (1-tol/ldiag)*edger * cos(thetas)
    edgey <- y0 + (1-tol/ldiag)*edger * sin(thetas)
    polygon(edgex, edgey, col=rgb(1, 0, 0, alpha=0.05), border="red")

    ll <- map2lonlat(edgex, edgey)
    res <- data.frame(x=edgex, y=edgey, lon=ll$longitude, lat=ll$latitude)

    ## If a pole is in the bounding, we need to add endpoints so that
    ## the geometry catches locations poleward of the bounding region.
    dateline <- lonlat2map(rep(180, 19), seq(-90, 90, length.out=19))
    datelineInFocus <- any(sp::point.in.polygon(dateline$x, dateline$y, edgex, edgey))
    message("datelineInFocus=", datelineInFocus)
    northPole <- lonlat2map(0, 90)
    northPoleInFocus <- sp::point.in.polygon(northPole$x, northPole$y, edgex, edgey)
    message("northPoleInFocus=", northPoleInFocus)
    if (datelineInFocus) {
        mapPoints(180, 0, col="green")
        mapPoints(res$lon[1:2], res$lat[1:2], col=1:2)
        ##. plot(res$x, res$y, type="o", col="red", pch=20)
        ##. polygon(res$x, res$y, col=rgb(1, 0, 0, alpha=0.05))
        ##> points(res$x[1:2], res$y[1:2], pch=20, col=1:2, cex=2)
        ##> points(dateline$x, dateline$y, pch=20, col="green", cex=2)
        if (datelineMethod1) {
            message("datelineMethod1=", datelineMethod1, " (old lon shift)")
            plot(res$lon, res$lat, type="o", xlim=c(-181,181), ylim=c(-90,90))
            ## Fix up shifts of approx 360 deg
            for (ii in 2:length(res$lon)) {
                ##> message("ii=", ii, " OLD=", res$lon[ii])
                lonminus <- res$lon[ii] - 360
                lonplus <- res$lon[ii] + 360
                ##> message("check lonminus=", lonminus)
                if (abs(lonminus - res$lon[ii-1]) < abs(res$lon[ii]- res$lon[ii-1]))
                    res$lon[ii] <- lonminus
                ##> message("check lonplus=", lonplus)
                if (abs(lonplus - res$lon[ii-1]) < abs(res$lon[ii] - res$lon[ii-1]))
                    res$lon[ii] <- lonplus
                ##> message(" NEW=", res$lon[ii])
            }
        } else {
            message("datelineMethod1=", datelineMethod1, " (multi-polygon scheme)")
            res$lon <- 360 + ifelse(res$lon > 0, res$lon-360, res$lon)
            resp <- sp::Polygon(cbind(res$lon, res$lat))
            resps <- sp::Polygons(list(resp), "resp")
            ressps <- sp::SpatialPolygons(list(resps))
            ## Hemispheres, used for clipping
            EHp <- sp::Polygon(cbind(c(0, 0, 180, 180), c(-90, 90, 90, -90)))
            EHps <- sp::Polygons(list(EHp), "EHp")
            EHsps <- sp::SpatialPolygons(list(EHps))
            WHp <- sp::Polygon(cbind(c(180, 180, 360, 360), c(-90, 90, 90, -90)))
            WHps <- sp::Polygons(list(WHp), "WHp")
            WHsps <- sp::SpatialPolygons(list(WHps))
            ##plot(ressps);box();axis(1);axis(2)
            ##rgeos::gIsValid(ressps)
            ##XY <- polygonsCoords(ressps)
            ##plot(XY$y)
            ## gBuffer() has been suggested as a way to avoid self-intersection,
            ## but it yields weird results
            #ressps2 <- rgeos::gBuffer(ressps, width=0)
            #plot(ressps2)
            if (DEBUG==1) {
                ##plot(ressps, col='lightgray')
                plot(res$lon, res$lat, type="o")
                points(res$lon[1:2], res$lat[1:2], col=1:2, pch=7, cex=2)
                abline(v=180)
                abline(v=360, col='red')
                w<-which.max(abs(diff(res$lon)))
                res$lon[w+seq(-1,1,1)]
                median(abs(diff(res$lon)))
                browser()
                ##plot(res$lon, type="o")
                ##points(res$lon[1:2], col=1:2, pch=7, cex=2)
                ##plot(res$lat, type="o")
                ##points(res$lat[1:2], col=1:2, pch=7, cex=2)
            }
            focusE <- raster::intersect(ressps, EHsps) # Eastern focus SpatialPolygons
            focusW <- raster::intersect(ressps, WHsps) # Western focus SpatialPolygons
            if (FALSE) {               # shows that W needs to shift by -360 deg
                plot(focusE);box();axis(1);axis(2)
                plot(focusW);box();axis(1);axis(2)
            }
            ## Shift the western polygon to negative longitude, to match
            ## coastline convention.
            tmp <- polygonsCoords(focusW)
            tmp$x <- tmp$x - 360
            focusW <- xy2SpatialPolygons(tmp$x, tmp$y, "a")

            ##Wxy <- polygonCoords(WHp)
            ##> plot(Wxy$x, Wxy$y, asp=1, type="o", main="Wsps")
            ##> Exy <- polygonsCoords(resEsps)
            ##> plot(Exy$x, Exy$y, asp=1, type="o", main="Esps")
            ##> WWp <- sp::Polygon(cbind(Wxy$x-360, Wxy$y))
            ## WWps <- sp::Polygons(list(WWp), "WWp")
            ## WWsps <- sp::SpatialPolygons(list(WWps))
            ## WWxy <- polygonsCoords(WWsps)
            ##> plot(WWxy$x, WWxy$y, asp=1, type="o", main="WWsps")
            ##> ## Join the two polygons
            ##> Wp <- WWsps@polygons[[1]]@Polygons[[1]]
            ##> Ep <- Esps@polygons[[1]]@Polygons[[1]]
            focusList <- list()
            i <- 1
            if (!is.null(focusW)) {
                message("focusW is not null")
                focusList[[i]] <- focusW@polygons[[1]]@Polygons[[1]]
                i <- i + 1
            }
            if (!is.null(focusE)) {
                message("focusE is not null")
                focusList[[i]] <- focusE@polygons[[1]]@Polygons[[1]]
                i <- i + 1
            }
            if (i == 1)
                stop("no data are in the focus region")
            ##. focus <- sp::SpatialPolygons(list(sp::Polygons(list(focusW@polygons[[1]]@Polygons[[1]],
            ##.                                                     focusE@polygons[[1]]@Polygons[[1]]), "focus")))
            focus <- sp::SpatialPolygons(list(sp::Polygons(focusList, "focus")))
            plot(focus, col=rgb(1,0,0,alpha=0.05), border="red")
            box()
            axis(1)
            axis(2)
            lines(lonlat$lon, lonlat$lat)
            ##> browser()
            ##> EWps <- sp::Polygons(list(Wp, Ep), "WEp")
            ##> focusRegion <- sp::SpatialPolygons(list(EWps))
            ##> plot(lonlat$lon, lonlat$lat, type="l", xlim=c(-185,185),ylim=c(-95, 95))
            ##> plot(focusRegion, col=rgb(1,0,0,alpha=0.05), border="red", add=TRUE)
            ##> plot(focusRegion)
            ##> lines(lonlat$lon, lonlat$lat)
            ##> box()
            ##> axis(1)
            ##> axis(2)
            ##> browser()
        }
        ##> lines(res$lon, res$lat, type="o", col="red")
        ##> points(res$lon[1:2], res$lat[1:2], type="o", pch=20, col=1:2, cex=2)
        points(-180, 0, col="green") # show equatorial dateline
        ##> polygon(res$lon, res$lat, col=rgb(1, 0, 0, alpha=0.05))
        ##> ##> browser()
        points(lonfix(res$lon[1:2]), res$lat[1:2], col=1:2)
    } else {
        ## Dateline is not visible, so we need a simpler focus
        ## res <- data.frame(x=edgex, y=edgey, lon=ll$longitude, lat=ll$latitude)
        focus <- xy2SpatialPolygons(res$lon, res$lat, name="focus")
        plot(focus, col=rgb(1,0,0,alpha=0.05), border="red")
        box()
        axis(1)
        axis(2)
        lines(lonlat$lon, lonlat$lat)
    }
    if (northPoleInFocus) {
        message("**adding the North pole**")
        o <- order(res$lon)
        res <- res[o, ]
        res <- as.list(res)
        res$x <- c(res$x[1], res$x[1], res$x, tail(res$x, 1), tail(res$x, 1))
        res$y <- c(northPole$y, res$y[1], res$y, tail(res$y, 1), northPole$y)
        ##res$lon <- c(res$lon[1], res$lon, tail(res$lon,1))
        res$lon <- c(-180, -180, res$lon, 180, 180)
        res$lat <- c(90, res$lat[1], res$lat, tail(res$lat, 1), 90)
        focus <- xy2SpatialPolygons(res$lon, res$lat, name="focus")
    }
    ## mapLines(res$lon, res$lat, col="blue", lty="dotted", lwd=2)
    ## polygon(res$lon, res$lat, col="blue", lty="dotted", lwd=2)

    ##.browser()
    ##.l<-locator(1);loc<-map2lonlat(l$x,l$y);message("l$x=", l$x, " l$y=", l$y, " lon=", loc$longitude, " lat=", loc$latitude); mapPoints(loc$lon, loc$lat)

    ## RH panel in longlat projection
    ##. plot(lonlat$lon, lonlat$lat, type="l", xlab="", ylab="", xlim=c(-181,181), ylim=c(-90,90))
    ##. lines(res$lon, res$lat, col="red")
    ##. points(res$lon, res$lat, col="red", type="o", pch=20, cex=1/2)
    ##. points(res$lon[1:2], res$lat[1:2], pch=20, cex=2, col=1:2)
    ##. points(-180, 0, pch=20, col="green", cex=2)
    ##. polygon(res$lon, res$lat, col=rgb(1, 0, 0, alpha=0.05))
    ##>Ep <- sp::Polygon(cbind(res$lon, res$lat))
    ##>Eps <- sp::Polygons(list(Ep), "Ep")
    ##>Esps <- sp::SpatialPolygons(list(Eps))
    ##. if (datelineInFocus && !datelineMethod1) {
    ##.     message("combined")
    ##.     ##???? Esps <- EWsps
    ##.     Exy <- polygonCoords(Ep)
    ##.     Ell <- map2lonlat(Exy$x, Exy$y)
    ##.     lines(Ell$longitude, Ell$latitude, col=2)
    ##.     Wxy <- polygonCoords(Wp)
    ##.     Wll <- map2lonlat(Wxy$x, Wxy$y)
    ##.     lines(Wll$longitude, Wll$latitude, col=3)
    ##. }
    nas <- which(is.na(lonlat$lon))
    nnas <- length(nas)
    LON <- LAT <- NULL
    for (iseg in 2:nnas) {
        ##. message("iseg=", iseg)
        look <- seq.int(nas[iseg-1]+1, nas[iseg]-1)
        lon <- lonlat$lon[look]
        lat <- lonlat$lat[look]
        if (length(look) > 2) {
            CLp <- sp::Polygon(cbind(lon, lat))
            CLps <- sp::Polygons(list(CLp), "CLp")
            CLsps <- sp::SpatialPolygons(list(CLps))

            ##?? if (iseg==148) browser()
            capture.output(i <- raster::intersect(focus, CLsps))
            if (!is.null(i)) {
                outlon <- outlat <- NULL
                for (j in seq_along(i@polygons)) {
                    for (k in seq_along(i@polygons[[1]]@Polygons)) {
                        xy <- i@polygons[[j]]@Polygons[[k]]@coords
                        seglon <- xy[, 1]
                        seglat <- xy[, 2]
                        outlon <- c(outlon, NA, seglon)
                        outlat <- c(outlat, NA, seglat)
                        ##> oceDebug(debug > 1, "iseg=", iseg, ", j=", j, ", k=", k, "\n", sep="")
                    }
                }
                LON <- c(LON, NA, outlon)
                LAT <- c(LAT, NA, outlat)
                ##> message("added ", length(outlon), " points")
                if (!is.null(outlon)) {
                    polygon(outlon, outlat, col=rgb(0, 0, 0, alpha=0.05), border="black")
                }
            } else {
                ##. message("no intersection for iseg=", iseg)
            }
        } else {
            ##> message("short segment iseg=", iseg)
        }
    }
    if (names(projlist)[iproj] == "stere") {
        mapPlot(as.coastline(LON, LAT), projection=projection, col="gray",
                longitudelim=c(-130, 50), latitudelim=c(70, 110))
    } else {
        mapPlot(as.coastline(LON, LAT), projection=projection, col="gray")
    }
    ##. plot(LON, LAT, type="l", xaxs="i")
    ##. abline(v=c(180, -180), col="green")
    if (!interactive()) dev.off()
}

