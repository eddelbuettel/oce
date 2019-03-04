library(oce)
data(coastlineWorld)
if (!interactive()) pdf("02.pdf")
par(mar=rep(1, 4))
mapPlot(coastlineWorld, proj="+proj=ortho", col="gray")
lonlat <- list(lon=coastlineWorld[["longitude"]], lat=coastlineWorld[["latitude"]])
xy <- lonlat2map(lonlat$lon, lonlat$lat)

usr <- par("usr")
x0 <- mean(usr[1:2])
y0 <- mean(usr[3:4])
points(x0, y0)
text(x0, y0, "(x0,y0)", pos=1, cex=2/3)
lx <- usr[2] - x0
ly <- usr[3] - y0
offworld <- function(x, y) {
    if (is.finite(map2lonlat(x, y)[[1]])) -1 else 1
}
tol <- (usr[2] - usr[1]) / 1e4
N <- 100
thetas <- seq(0, 2*pi, length.out=N)
l <- max(lx, ly)
edger <- rep(NA, N)
for (itheta in seq_along(thetas)) {
    theta <- thetas[itheta]
    E <- uniroot(function(s) offworld(x0+s*cos(theta), y0+s*sin(theta)), c(0, l), tol=tol)
    points(x0+E$r * cos(theta), y0+E$r * sin(theta), col="red", pch=20)
    edger[itheta] <- E$r
}
edgex <- x0 + edger * cos(thetas)
edgey <- y0 + edger * sin(thetas)
polygon(edgex, edgey, col=rgb(1, 0, 0, alpha=0.05), border="red")

# index near xy point
near <- function(x, y) {
    which.min(abs(xy$x - x) + abs(xy$y-y))
}

## Find a bad spot, for reference
## loc <- locator(1)
loc <- list(x=3296475, y=4514420)
points(loc$x, loc$y, col=2)
n <- near(loc$x, loc$y)
mapPoints(lonlat$lon[n], lonlat$lat[n], pch=20)
## this is the segment containing the odd point
nas <- which(is.na(lonlat$lon))
start <- tail(which(n > nas), 1)
look <- seq(nas[start]+1, nas[start+1]-1)
lines(xy$x[look], xy$y[look], col="green", lwd=2)
if (!interactive()) dev.off()

