library(oce)
data(coastlineWorld)
if (!interactive()) pdf("01.pdf")
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
N <- 50
xs <- seq(x0 - lx, x0 + lx, length.out=N)
ys <- seq(y0 - ly, y0 + ly, length.out=N)
L <- rep(NA, N)
R <- rep(NA, N)
for (iy in seq_along(ys)) {
    y <- ys[iy]
    someLand <- FALSE
    ## points(xs, rep(y, N), pch=20, cex=1/2)
    for (x in seq(x0-lx, x0+lx, length.out=50)) {
        ll <- map2lonlat(x, y)
        if (is.finite(ll[[1]])) {
            someLand <- TRUE
            break
        }
    }
    if (someLand) {
        Lr <- uniroot(function(x) offworld(x, y), c(x0-lx, x0), tol=tol)
        Rr <- uniroot(function(x) offworld(x, y), c(x0, x0+lx), tol=tol)
        points(Lr$root, y, col=2)
        points(Rr$root, y, col=2)
        L[iy] <- Lr$root
        R[iy] <- Rr$root
    }
}

message("FIXME: handle case where whole line is on-earth")

message("FIXME: angled tests")

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
lines(xy$x[look], xy$y[look], col=3, lwd=2)
lines(L, ys, col=2, lwd=3)
lines(R, ys, col=4, lwd=3)
LL <- map2lonlat(L+tol, ys) # add tol to avoid NA (but how to set tol?)
LL$longitude[is.na(L)] <- NA
LL$latitude[is.na(L)] <- NA
RR <- map2lonlat(R-tol, ys) # add tol to avoid NA (but how to set tol?)
RR$longitude[is.na(R)] <- NA
RR$latitude[is.na(R)] <- NA
print(data.frame(L, R, ys, lonL=LL$longitude, lonR=RR$longitude, lat=LL$latitude))
if (!interactive()) dev.off()

