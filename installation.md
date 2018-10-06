---
permalink: installation.html
layout: default
title: Installation
submenu: installation
---

**aCRAN version (cautious users)**

The stable version of oce is provided on the R archives.  It is installed
from within R with

```r
install.packages("oce")
```

## Pre-compiled development version

Occasionally, `oce` is compiled for MSWindows and for OS X.
The results are stored at [a Dropbox
site](https://www.dropbox.com/sh/awlz31v1jj7w0ct/620Tf2ZkGz).  Visit that site
and the appropriate subdirectory (`osx` or `windows`),
and download both `oce` and `ocedata`. Then install as R
normally installs packages, e.g. on an OS X machine, type the following in a
terminal (console) window (adjusting version numbers, as appropriate):

```bash
R CMD install oce_1.0-1.tgz
R CMD install ocedata_0.1.4.tgz
```

Note that if your system uncompresses the files, you will have to change the
suffix `.tgz` to `.tar` to make the above-stated commands work.

Users of the [RStudio](http://www.rstudio.com) IDE can install the package from
the downloaded package files by using the menu item *Install Packages* under
the *Tools* menu, and selecting *Install from: Package archive file.* A similar
approach can be used with the R.app GUI under the *Packages & Data* menu.

## Source-code development version

Users who are set up to build packages (which requires a C compiler, etc; see
note 1) may stay at the cutting edge by installing a development version.  This
may be done in several ways, of which the third is the best for most users.

1.  Download the tarball sources
    ([oce](http://github.com/dankelley/oce/tarball/develop) and
[ocedata](http://github.com/dankelley/ocedata/tarball/master)), expand the
files, rename the resultant directories to `oce` and `ocedata` and then execute
the OS commands stated below (adjusting version numbers, as appropriate).

    ```bash
R CMD build oce
R CMD install oce_1.0-1.tar.gz
R CMD build ocedata
R CMD install ocedata_0.1.4.gz
```

    A similar procedure works for the zip files for
    [oce](http://github.com/dankelley/oce/zipball/develop)
    and
    [ocedata](http://github.com/dankelley/ocedata/zipball/master).

2.  Clone the sources with Git

    ```bash
git clone https://dankelley@github.com/dankelley/oce.git
git clone https://dankelley@github.com/dankelley/ocedata.git
```
    and then build as described above.

3.  Build with `devtools::install_github()`

    ```R
library(devtools)
install_github("dankelley/oce", ref="develop")
install_github("dankelley/ocedata", ref="master")
```

The first method is included here for convenience of users who do not have git
on their computers. The second method is the one chosen by most users who have
programming experience.  The third method is recommended for most users who
simply want to build an up-to-date version of oce.

**Note.** Getting ready to compile (by installing a `C` compiler, etc.)
is easy on linux, although the method depends on the system. On MSWindows,
install [Rtools from CRAN](http://cran.r-project.org/bin/windows/Rtools).
On OSX, it can be fairly complicated, as explained in a
[wiki page](https://github.com/dankelley/oce/wiki/Configuring-a-system-to-be-able-to-build-oce-from-source).

