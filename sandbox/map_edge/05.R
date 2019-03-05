closeup <- !TRUE
showWork <- 0
projection <- "+proj=ortho"
library(oce)
data(coastlineWorld)
if (!interactive()) pdf("05.pdf", pointsize=9)

lonlat <- list(lon=coastlineWorld[["longitude"]], lat=coastlineWorld[["latitude"]])
lonlat$lon <- c(lonlat$lon, NA)
lonlat$lat <- c(lonlat$lat, NA)

par(mfrow=c(2, 2), mar=c(1.5, 1.5, 0.5, 0.5), mgp=c(2, 0.7, 0))
if (closeup) {
    mapPlot(coastlineWorld, projection=projection, col="gray",
            longitudelim=c(0, 180), latitudelim=c(-90,90))
} else {
    mapPlot(coastlineWorld, projection=projection, col="gray")
}
xy <- lonlat2map(lonlat$lon, lonlat$lat)

## Show a bad spot (Russia), for reference
##> loc <- locator(1)
loc <- list(x=3296475, y=4514420)
near <- function(x, y) which.min(abs(xy$x - x) + abs(xy$y-y))
points(loc$x, loc$y, col=2)
n <- near(loc$x, loc$y)
mapPoints(lonlat$lon[n], lonlat$lat[n], pch=20)
## this is the segment containing the odd point
nas <- which(is.na(lonlat$lon))
start <- tail(which(n > nas), 1)
brokenBorder <- seq(nas[start]+1, nas[start+1]-1)
lines(xy$x[brokenBorder], xy$y[brokenBorder], col="green", lwd=2)


usr <- par("usr")
x0 <- mean(usr[1:2])
y0 <- mean(usr[3:4])
lx <- usr[2] - x0
ly <- usr[4] - y0
offworld <- function(x, y) {
    if (is.finite(map2lonlat(x, y)[[1]])) -1 else 1
}
tol <- (usr[2] - usr[1]) / 1e5
N <- 32
thetas <- seq(0, 2*pi, length.out=N)
R <- 10 * sqrt(lx^2 + ly^2) # make very long; we trim later
edger <- rep(NA, N)

usrsps <- as(raster::extent(usr[1], usr[2], usr[3], usr[4]), "SpatialPolygons")
if (FALSE) { # testing
    par(xpd=NA)

    plot(usrsps)
    for (itheta in seq_along(thetas)) {
        theta <- thetas[itheta]
        rl <- sp::Line(cbind(x0 + c(0, R * cos(theta)), y0 + c(0, R * sin(theta))))
        rls <- sp::Lines(list(rl), "radial")
        rsls <- sp::SpatialLines(list(rls))
        i <- raster::intersect(rsls, usrsps)
        lines(rsls, col=2)
        ixy <- i@lines[[1]]@Lines[[1]]@coords
        points(ixy[2,1], ixy[2,2], col=3)
        span <- sqrt((ixy[2,1]-ixy[1,1])^2+ (ixy[2,2]-ixy[1,2])^2)
        ## message("theta=", theta, " span=", span)
    }
}

for (itheta in seq_along(thetas)) {
    theta <- thetas[itheta]
    rl <- sp::Line(cbind(x0 + c(0, R * cos(theta)), y0 + c(0, R * sin(theta))))
    rls <- sp::Lines(list(rl), "radial")
    rsls <- sp::SpatialLines(list(rls))
    i <- raster::intersect(rsls, usrsps)
    ixy <- i@lines[[1]]@Lines[[1]]@coords
    span <- sqrt((ixy[2,1]-ixy[1,1])^2+ (ixy[2,2]-ixy[1,2])^2)
    message(sprintf("theta=%.1f span=%.0f", theta, span))
    if (showWork > 1) lines(x0+c(0,span*cos(theta)), y0+c(0,span*sin(theta)), col="pink")
    ##points(x0+span*cos(theta), y0+span*sin(theta), col=4, pch=20)
    E <- uniroot(function(s) offworld(x0+s*cos(theta),
                                      y0+s*sin(theta)), c(0, span), tol=tol/100)
    points(x0+E$r * cos(theta), y0+E$r * sin(theta), col="red", pch=20)
    edger[itheta] <- E$r
}
ldiag <- sqrt(lx^2 + ly^2)
edgex <- x0 + (1-tol/ldiag)*edger * cos(thetas)
edgey <- y0 + (1-tol/ldiag)*edger * sin(thetas)
polygon(edgex, edgey, col=rgb(1, 0, 0, alpha=0.05), border="red")

ll <- map2lonlat(edgex, edgey)
res <- data.frame(x=edgex, y=edgey, lon=ll$longitude, lat=ll$latitude)
print(res)
mapLines(res$lon, res$lat, col="blue", lty="dotted", lwd=2)

## RH panel: see if Russia is cut by the earth's edge in lonlat space
plot(lonlat$lon, lonlat$lat, type="l", xlab="", ylab="", asp=1,
     xlim=c(-180, 180), ylim=c(-90,90))
lines(res$lon, res$lat, col="red", type="o", pch=20, cex=1/2)
lines(lonlat$lon[brokenBorder], lonlat$lat[brokenBorder], col="green", lwd=2)
Ep <- sp::Polygon(cbind(res$lon, res$lat))
Eps <- sp::Polygons(list(Ep), "Ep")
Esps <- sp::SpatialPolygons(list(Eps))

nnas <- length(nas)
LON <- LAT <- NULL
for (iseg in 2:nnas) {
    look <- seq.int(nas[iseg-1]+1, nas[iseg]-1)
    lon <- lonlat$lon[look]
    lat <- lonlat$lat[look]
    CLp <- sp::Polygon(cbind(lon, lat))
    CLps <- sp::Polygons(list(CLp), "CLp")
    CLsps <- sp::SpatialPolygons(list(CLps))

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
        if (!is.null(outlon)) {
            polygon(outlon, outlat, col=rgb(0, 0, 0, alpha=0.1), border="black")
        }
    }
}

mapPlot(as.coastline(LON, LAT), projection=projection, col="gray")

if (!interactive()) dev.off()
