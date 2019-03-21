closeup <- !TRUE
showWork <- 0
library(oce)
library(raster)
library(sp)
data(coastlineWorld)
cwr <- coastlineCut(coastlineWorld, -100)

projlist <- list(stere="+proj=stere +lat_0=90 +lon_0=-100",
                 ##ortho="+proj=ortho +lon_0=-50",
                 ##ortho="+proj=ortho +lon_0=-70",
                 ##ortho="+proj=ortho +lon_0=-80",
                 ##ortho="+proj=ortho +lon_0=-90",
                 ortho="+proj=ortho +lon_0=-95",
                 ortho2="+proj=ortho +lon_0=-100",
                 robin="+proj=robin",
                 moll="+proj=moll",
                 moll2="+proj=moll +lon_0=-100",
                 moll3="+proj=moll")

for(iproj in seq_along(projlist)) {
    message(names(projlist)[iproj])
    if (!interactive()) pdf(paste("07_", names(projlist)[iproj], ".pdf", sep=""), pointsize=9)
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

    par(mfrow=c(2, 2), mar=c(1.5, 1.5, 0.5, 0.5), mgp=c(2, 0.7, 0))
    if (closeup) {
        mapPlot(cl, projection=projection, col="gray",
                longitudelim=c(0, 180), latitudelim=c(-90,90))
    } else {
        if (names(projlist)[iproj] %in% c("stere", "moll3")) {
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
    thetas <- seq(0, 2*pi, length.out=N)
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
            E <- uniroot(function(s) offworld(x0+s*cos(theta), y0+s*sin(theta)), c(0, span), tol=tol/100)
        } else {
            ##message("both points are on-world or off-world")
            E <- list(r=span)
        }
        points(x0+E$r * cos(theta), y0+E$r * sin(theta), col="pink", pch=20)
        ## NOTE: move a bit inside to avoid problems at horizon, e.g. with
        ## ortho that shows the dateline.
        edger[itheta] <- E$r * 0.99
    }
    ldiag <- sqrt(lx^2 + ly^2)
    edgex <- x0 + (1-tol/ldiag)*edger * cos(thetas)
    edgey <- y0 + (1-tol/ldiag)*edger * sin(thetas)
    polygon(edgex, edgey, col=rgb(1, 0, 0, alpha=0.05), border="red")

    ll <- map2lonlat(edgex, edgey)
    res <- data.frame(x=edgex, y=edgey, lon=ll$longitude, lat=ll$latitude)

    ## If a pole is in the bounding, we need to add endpoints so that
    ## the geometry catches locations poleward of the bounding region.
    dateline <- lonlat2map(180, 0)
    if (sp::point.in.polygon(dateline$x, dateline$y, edgex, edgey)) {
        message("**dateline is in the focus region**")
        mapPoints(180, 0, pch=20, col="green", cex=2)
        mapPoints(res$lon[1:2], res$lat[1:2], pch=20, col=1:2, cex=2)
        ##. plot(res$x, res$y, type="o", col="red", pch=20)
        ##. polygon(res$x, res$y, col=rgb(1, 0, 0, alpha=0.05))
        ##> points(res$x[1:2], res$y[1:2], pch=20, col=1:2, cex=2)
        ##> points(dateline$x, dateline$y, pch=20, col="green", cex=2)
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
        lines(res$lon, res$lat, type="o", col="red")
        points(res$lon[1:2], res$lat[1:2], type="o", pch=20, col=1:2, cex=2)
        points(-180, 0, pch=20, col="green", cex=2)
        polygon(res$lon, res$lat, col=rgb(1, 0, 0, alpha=0.05))
        ##> browser()
        ##> points(res$lon[1:2], res$lat[1:2], pch=20, col=1:2, cex=1:2)
    }
    northPole <- lonlat2map(0, 90)
    if (sp::point.in.polygon(northPole$x, northPole$y, edgex, edgey)) {
        o <- order(res$lon)
        res <- res[o, ]
        message("**adding the North pole**")
        res <- as.list(res)
        res$x <- c(res$x[1], res$x[1], res$x, tail(res$x, 1), tail(res$x, 1))
        res$y <- c(northPole$y, res$y[1], res$y, tail(res$y, 1), northPole$y)
        ##res$lon <- c(res$lon[1], res$lon, tail(res$lon,1))
        res$lon <- c(-180, -180, res$lon, 180, 180)
        res$lat <- c(90, res$lat[1], res$lat, tail(res$lat, 1), 90)
        res <- as.data.frame(res)
        ##browser()
        ##plot(res$lon, res$lat)
        ##polygon(res$lon, res$lat)
    }
    mapLines(res$lon, res$lat, col="blue", lty="dotted", lwd=2)
    ## polygon(res$lon, res$lat, col="blue", lty="dotted", lwd=2)

    ##.browser()
    ##.l<-locator(1);loc<-map2lonlat(l$x,l$y);message("l$x=", l$x, " l$y=", l$y, " lon=", loc$longitude, " lat=", loc$latitude); mapPoints(loc$lon, loc$lat)

    ## RH panel in longlat projection
    plot(lonlat$lon, lonlat$lat, type="l", xlab="", ylab="", xlim=c(-181,181), ylim=c(-90,90))
    lines(res$lon, res$lat, col="red")
    points(res$lon, res$lat, col="red", type="o", pch=20, cex=1/2)
    points(res$lon[1:2], res$lat[1:2], pch=20, cex=2, col=1:2)
    points(-180, 0, pch=20, col="green", cex=2)
    polygon(res$lon, res$lat, col=rgb(1, 0, 0, alpha=0.05))
    Ep <- sp::Polygon(cbind(res$lon, res$lat))
    Eps <- sp::Polygons(list(Ep), "Ep")
    Esps <- sp::SpatialPolygons(list(Eps))

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
            i <- raster::intersect(Esps, CLsps)
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

