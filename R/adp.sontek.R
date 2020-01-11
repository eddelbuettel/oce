## vim: tw=120 shiftwidth=4 softtabstop=4 expandtab:

sontekDateTime <- function(bytes, tz="UTC")
{
    ## Note the odd ordering of elements. See e.g. page 87 of [1]
    ISOdatetime(readBin(bytes[1:2], "integer", size=2, endian="little"),
                as.integer(bytes[4]), # month
                as.integer(bytes[3]), # day
                as.integer(bytes[6]), # hour
                as.integer(bytes[5]), # minute
                as.integer(bytes[8]) + as.integer(bytes[7])/100.0, # second + sec100
                tz=tz)
}

#' Read a SonTek ADP File
#'
#' Read a SonTek acoustic-Doppler profiler file (see reference 1).
#'
#' @param despike if `TRUE`, [despike()] will be used to clean
#' anomalous spikes in heading, etc.
#'
#' @param type optional character string indicating the type of instrument.
#' The permitted values are `"adp"`, `"argonaut_adp"` and `"adcp"`. If not
#' `type` is not provided, the first four bytes will be checked,
#' with `"adp"` being inferred if the bytes are `0x10 0x02 0x60 0x00`
#' and `"argonaut_adp"` being inferred if thy are `0x40 0x02 0x60 0x00`;
#' note that `"pcadp"` files start with the same four bytes as `"adp"`
#' files, so the user ought to specify `type` to distinguish between
#' the two.
#'
#' @template adpTemplate
#'
#' @references
#' 1. Information about SonTek instruments is available at https://www.sontek.com, but manuals are behind a login wall.
#' 2. SonTek/YSI Incorporated. "ADP (Acoustic Doppler Profiler) Operation Manual Firmware Version 7.1." Sontek/YSI, March 2001.
#' 3. SonTek/YSI Incorporated. "Argonaut Acoustic Doppler Current Meter Operation Manual Firmware Version 7.9."
#'    SonTek/YSI, May 1, 2001.
#'    https://eng.ucmerced.edu/snsjho/files/San_Joaquin/Sensors_and_Loggers/SonTek/SonTek_Argonaut/ArgonautXR.pdf
#'
#' @author Dan Kelley and Clark Richards
#'
#' @family things related to adp data
read.adp.sontek <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                            longitude=NA, latitude=NA, type,
                            monitor=FALSE, despike=FALSE, processingLog,
                            debug=getOption("oceDebug"),
                            ...)
{
    oceDebug(debug, "read.adp.sontek(...,",
             argShow(from),
             argShow(to),
             argShow(type),
             "...) {\n", unindent=1, sep="", style="bold")
    if (missing(file))
        stop("must supply 'file'")
    if (is.character(file) && !file.exists(file))
        stop("cannot open '", file, "' because there is no such file or directory")
    if (is.character(file) && 0 == file.info(file)$size)
        stop("empty file")
    missing.to <- missing(to)
    ## In this function, comments in [] refer to logical page number of ADPManual_v710.pd; add 14 for file page number
    profileStart <- NULL # prevent scope warning from rstudio; defined later anyway
    bisectSontekAdp<- function(buf, t.find, add=0, debug=0)
    {
        oceDebug(debug, "bisectSontekAdp(t.find=", format(t.find), ", add=", add, ", debug=", debug, ")\n")
        len <- length(profileStart)
        lower <- 1
        upper <- len
        passes <- floor(10 + log(len, 2)) # won't need this many; only do this to catch coding errors
        for (pass in 1:passes) {
            middle <- floor( (upper + lower) / 2 )
            oceDebug(debug-1, "pass=", pass, "middle=", middle, "\n")
            oceDebug(debug-1, "data=", buf[profileStart[middle]+seq(0, 100, 1)], "\n")
            ##oceDebug(debug-1, "profileStart=", profileStart[1:5], "...; profileStart2=", profileStart2[1:5], "...\n")
            oceDebug(debug-1, "profileStart=", profileStart[1:5], "\n")
            ##year <- readBin(buf[profileStart2[middle]+18], "integer", size=2, signed=FALSE, endian="little")
            year <- readBin(buf[profileStart[middle]+18:19], "integer", size=2, signed=FALSE, endian="little")
            day <- as.integer(buf[profileStart[middle]+20])
            month <- as.integer(buf[profileStart[middle]+21])
            min <- as.integer(buf[profileStart[middle]+22])
            hour <- as.integer(buf[profileStart[middle]+23])
            ## SIG p82 C code suggests sec100 comes before second.
            sec100 <- as.integer(buf[profileStart[middle]+24])     # FIXME: determine whether this is 1/100th second
            sec <- as.integer(buf[profileStart[middle]+25])
            t <- ISOdatetime(year, month, day, hour, min, sec+sec100/100, tz=tz)
            oceDebug(debug, "t=", format(t),
                      " [year=", year, " month=", month, " day=", day, " hour=", hour, " sec=", sec, "sec100=", sec100, "]\n")
            if (t.find < t)
                upper <- middle
            else
                lower <- middle
            if (upper - lower < 2)
                break
            oceDebug(debug, "middle=", middle, " lower=", lower, " upper=", upper, " pass=", pass, " of max=", passes, "\n")
        }
        middle <- middle + add          # may use add to extend before and after window
        if (middle < 1)
            middle <- 1
        if (middle > len)
            middle <- len
        t <- ISOdatetime(readBin(buf[profileStart[middle]+18:19], "integer", size=2, signed=FALSE, endian="little"),  # year
                         as.integer(buf[profileStart[middle]+21]), # month
                         as.integer(buf[profileStart[middle]+20]), # day
                         as.integer(buf[profileStart[middle]+23]), # hour
                         as.integer(buf[profileStart[middle]+22]), # min
                         as.integer(buf[profileStart[middle]+25]), # sec
                         tz=tz)
        oceDebug(debug, "result: t=", format(t), " at profileStart[", middle, "]=", profileStart[middle], "\n")
        return(list(index=middle, time=t)) # index is within vsd
    }
    ##parameters <- list(profile.byte1 = 0xa5, profile.byte2=0x10, profile.headerLength=80)
    if (is.character(file)) {
        if (0 == file.info(file)$size)
            stop("empty file")
        filename <- fullFilename(file)
        file <- file(file, "rb")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        filename <- "(connection)"
        open(file, "rb")
        on.exit(close(file))
    }
    res <- new("adp")
    seek(file, 0, "end")
    fileSize <- seek(file, 0, "start")
    oceDebug(debug, "file", filename, "has", fileSize, "bytes\n")
    buf <- readBin(file, what="raw", n=fileSize, endian="little")
    ## See if there is a header here.  (May not be, since the file may have been chopped
    ## into parts by a deck unit.)
    frequency <- NA
    ## 0x10 is what we had for years; 0x40 is a test to try to read sontek/argonaut data.

    ## Infer variety of file
    if (missing(type)) {
        type <- if (buf[1] == 0x10 && buf[2] == 0x02 && buf[3] == 0x60 && buf[4] == 0x00) {
            "adp"
        } else if (buf[1] == 0x40 && buf[2] == 0x02 && buf[3] == 0x60) {
            ## argonaut has 0x40, 0x20 as.raw(96)=0x60
            oceDebug(debug, "read.adp.sontek() recognized argonaut_adp type\n")
            "argonaut_adp"
        } else {
            stop("can only auto-recognize \"adp\" and \"adp_argonaut\" SonTek file types; for \"pcadcp\", supply the 'type' argument")
        }
    }
    typeAllowed <- c("adp", "argonaut_adp", "pcadp")
    tmp <- pmatch(type, typeAllowed)
    oceDebug(debug, "after checking within file (if 'type' not given), infer type='", type, "'\n", sep="")
    if (is.na(tmp))
        stop("type=\"", type, "\" is not permitted; it must be one of: \"", paste(typeAllowed, collapse='", "'), "\"")
    type <- typeAllowed[tmp]

    ##if ((buf[1] == 0x10 || buf[1] == 0x40) && buf[2] == 0x02 && 96 == readBin(buf[3:4], "integer", n=1, size=2, signed=FALSE, endian="little")) {
    if (type == "adp" || type == "pcadp") {
        oceDebug(debug, "adp type ... scanning the header, but ignoring it for now\n")
        ## Comments like [p83] refer to logical page number of ADPManual_v710.pd; add 14 for file page number
        bytesInConfiguration <- readBin(buf[3:4], "integer", n=1, size=2, endian="little")
        if (bytesInConfiguration != 96)
            warning("bytes 3:4 of the header suggest", bytesInConfiguration, "but this should be 96\n")
        ## skip DateTimeType, which is 8 bytes long, in bytes 5:12
        cpuSoftwareVerNum <- as.integer(buf[13]) / 10 # CPUSoftwareVerNum [p83]
        dspSoftwareVerNum <- as.integer(buf[14]) / 10 # DSPSoftwareVerNum [p83]
        boardRev <- readBin(buf[15], "character", n=1, size=1, signed=TRUE) # BoardRev [p83]
        serialNumber <- readBin(buf[16:25], "character")
        oceDebug(debug, vectorShow(serialNumber))
        frequencyIndex  <- readBin(buf[26], what="integer", n=1, size=1) # 0-3; 1=1.5; 2-750; 3-500 [p83]
        oceDebug(debug, vectorShow(frequencyIndex))
        frequency <- switch(frequencyIndex + 1, 3000, 1500, 750, 500, 250)
        oceDebug(debug, vectorShow(frequency))
        nbeams <- as.integer(buf[27])
        oceDebug(debug, vectorShow(nbeams))
        beam.geometry <- as.integer(buf[28])
        oceDebug(debug, vectorShow(beam.geometry,
                  postscript="0 (2 beams); 1 (3 beams), 2 (4 beams with 1 vertical), 3 (4 beams, Janus)"))
        slant.angle <- readBin(buf[29:30], "integer", n=1, size=2, signed=FALSE) / 10
        oceDebug(debug, vectorShow(slant.angle))
        orientation <- switch(as.integer(buf[31]) + 1, "down", "up", "side")
        oceDebug(debug, vectorShow(orientation))
        compass.installed <- switch(as.integer(buf[32]) + 1, FALSE, TRUE) # nolint (variable not used)
        recorder.installed <- switch(as.integer(buf[33]) + 1, FALSE, TRUE) # nolint (variable not used)
        temp.installed <- switch(as.integer(buf[34]) + 1, FALSE, TRUE) # nolint (variable not used)
        press.installed <- switch(as.integer(buf[35]) + 1, FALSE, TRUE) # nolint (variable not used)
        ## 36 = spare
        ## 37 int[16], so I guess 2-byte ints, signed?
    } else if (type == "argonaut_adp") {
        ## bufEXPORT<<-buf;message("exported bufEXPORT")
        ## See reference [2] print page 87, PDF page 99.
        oceDebug(debug, "about to read 'Argonaut sensor configuration structure' (96 bytes)\n", style="red")
        bytesInConfiguration <- readBin(buf[3:4], "integer", n=1, size=2, endian="little")
        if (bytesInConfiguration != 96)
            warning("bytes 3:4 of the header suggest", bytesInConfiguration, "but this should be 96\n")
        ## 5:12 DateTimeType    ConfigTime;
        ConfigTime <- sontekDateTime(buf[5:12])
        oceDebug(debug, vectorShow(ConfigTime))
        cpuSoftwareVerNum <- as.integer(buf[13]) / 10 # CPUSoftwareVerNum [p83]
        dspSoftwareVerNum <- as.integer(buf[14]) / 10 # DSPSoftwareVerNum [p83]
        boardRev <- readBin(buf[15], "character", n=1, size=1, signed=TRUE) # BoardRev [p83]
        serialNumber <- readBin(buf[16:25], "character")
        oceDebug(debug, vectorShow(serialNumber, postscript=" [expect E5131 for issue 1637]"))
        systemTypeByte <- buf[26]
        oceDebug(debug, "systemType bits: ", rawToBits(systemTypeByte), "\n")
        lowNibble <- ifelse(rawToBits(systemTypeByte)[8:5] == "01", 1, 0)
        highNibble <- ifelse(rawToBits(systemTypeByte)[4:1] == "01", 1, 0)
        oceDebug(debug, vectorShow(lowNibble))
        frequency <- if (all(lowNibble == c(0, 0, 0, 1))) 1.5 else if (all(lowNibble == c(0, 0, 0, 0))) 3 else stop("low nibble must be 0001 or 0000 but it is ", paste(lowNibble, collapse=""))
        oceDebug(debug, vectorShow(frequency, postscript=" MHz"))
        oceDebug(debug, vectorShow(highNibble))
        systemType <- if (all(highNibble == c(0, 0, 0, 0))) {
            "MD"
        } else if (all(highNibble == c(0, 0, 0, 1))) {
            "XR"
        } else if (all(highNibble == c(0, 0, 1, 0))) {
            "SL"
        } else stop("high nibble must be 0000, 0001 or 0010 but it is ", paste(highNibble, collapse=""))
        oceDebug(debug, vectorShow(systemType))

        ## FIXME: store type and freq
        nbeams <- as.integer(buf[27])
        oceDebug(debug, vectorShow(nbeams))
        beam.geometry <- as.integer(buf[28])
        oceDebug(debug, vectorShow(beam.geometry,
                  postscript="  (0 means 2 beams; 1 means 3 beams; 2 means 4 beams w/ 1 vert.; 3 means 4 beams, Janus)"))
        slant.angle <- readBin(buf[29:30], "integer", n=1, size=2, signed=FALSE) / 10
        oceDebug(debug, vectorShow(slant.angle))
        orientation <- switch(as.integer(buf[31]) + 1, "down", "up", "side")
        oceDebug(debug, vectorShow(orientation))
        compass.installed <- switch(as.integer(buf[32]) + 1, FALSE, TRUE) # nolint (variable not used)
        recorder.installed <- switch(as.integer(buf[33]) + 1, FALSE, TRUE) # nolint (variable not used)
        temp.installed <- switch(as.integer(buf[34]) + 1, FALSE, TRUE) # nolint (variable not used)
        press.installed <- switch(as.integer(buf[35]) + 1, FALSE, TRUE) # nolint (variable not used)
        oceDebug(debug, "skipping several things in 'Argonaut sensor configuration structure'\n")
        ## FIXME: there are quite a few more things defined in the table, but we skip for now.
        off <- 96
        oceDebug(debug, "about to see if we have 'Argonaut operation configuration structure' (64 bytes)\n")
        if (buf[off+1] == 0x41 && buf[off+2] == 0x02) {
            oceDebug(debug, "found 'Argonaut operation configuration structure' (64 bytes)\n", style="red")
            ##     1 char ConfigType;
            ##     2 char ConfigVer;
            ##   3:4 int Nbytes;
            Nbytes <- readBin(buf[off+3:4], "integer", size=2, endian="little")
            if (Nbytes != 64)
                stop("'Argonaut operation configuration structure' nbytes should be 64 but it is ", Nbytes, "\n")
            ##  5:12 DateTimeType ConfigTime;
            ConfigTime <- sontekDateTime(buf[off+5:12])
            oceDebug(debug, vectorShow(ConfigTime))
            ## 13:14 int NpingsPerBeam;
            NpingsPerBeam <- readBin(buf[off+13:14], "integer", size=2, endian="little")
            oceDebug(debug, vectorShow(NpingsPerBeam))
            ## 15:16 int SampInterval;
            SampInterval <- readBin(buf[off+15:16], "integer", size=2, endian="little")
            oceDebug(debug, vectorShow(SampInterval))
            ## 17:18 int Lag;
            Lag <- readBin(buf[off+17:18], "integer", size=2, endian="little")
            oceDebug(debug, vectorShow(Lag))
            ## 19:20 int PulseLength;
            ## 21:22 int RecLength;
            ## 23:24 int MinBlankLength;
            ## 25:26 int OperatingRange;
            ## 27:28 int PingDelay;
            ## 29:30 int AutoFilter;
            ## 30:33 int FilterA[2];
            ## 34:37 int FilterB[2];
            ## 38    char ModemMode;
            ## 39:40 int TempOffset;
            ## 41:42 int TempScale;
            ## (next, assume that MAX_BEAMS is 4)
            ## 43:46 unsigned char NominalNoise[MAX_BEAMS];
            ## 48    unsigned char VelRangeInd;
            ## 48    char FastMode;
            ## 49    char SampleRecordMode;
            ## 50    char UseCompassFlux;
            ## 51    unsigned char MaxLevelPressDiff
            ## 52    char LevelOffset;
            ## 53    char ProfilingMode;
            ProfilingMode <- as.integer(buf[off+53])
            oceDebug(debug, vectorShow(ProfilingMode, postscript=" (0=no, 1=yes) [expect 1 for issue 1637]"))
            ## 54    char Ncells;
            Ncells <- as.integer(buf[off+54])
            oceDebug(debug, vectorShow(Ncells, postscript=" [expect 11 for issue 1637]"))
            ## 55:56 int CellSize;
            CellSize <- readBin(buf[off+55:56], "integer", size=2, endian="little")
            oceDebug(debug, vectorShow(CellSize, postscript=" (cm) [expect 350 for issue 1637] NB appears twice"), style="blue")
            ## 57     char     SdiFormat;
            SdiFormat <- as.integer(buf[off+57])
            oceDebug(debug, vectorShow(SdiFormat, postscript=" (0=Sontek, 1=Sidekick)"))
            ## 58:63  char     Spare[6];
            ## 64     char     DebugOn;
            off <- off + 64
            oceDebug(debug, "about to check for a 'User setup parameters structure' (258 bytes)\n")
            if (buf[off+1] == 0x42 && buf[off+2] == 0x02) {
                oceDebug(debug, "found 'User setup parameters structure' (258 bytes)\n", style="red")
                ## User setup parameters structure (258 bytes)
                ## 1       unsigned char ConfigType; 0x42
                ## 2       unsigned char ConfigVer;  0x02
                ## 3:4     unsigned int Nbytes;      258
                nbytes <- readBin(buf[off+3:4], "integer", size=2, signed=TRUE, endian="little")
                if (nbytes != 258)
                    stop("'Argonaut operation configuration structure' nbytes should be 258 but it is ", nbytes, "\n")
                ## 5:12    DateTimeType ConfigTime;
                ConfigTime <- sontekDateTime(buf[off+5:12])
                time_setup<<-buf[off+5:12]
                oceDebug(debug, vectorShow(ConfigTime))
                ## 13:14   int Temp;
                Temp <- 0.01 * readBin(buf[off+13:14], "integer", size=2, signed=TRUE, endian="little")
                oceDebug(debug, vectorShow(Temp, postscript=" (degC) [expect ?? for issue 1637]"))
                ## 15:16   int Sal;
                Sal <- 0.01 * readBin(buf[off+15:16], "integer", size=2, signed=TRUE, endian="little")
                oceDebug(debug, vectorShow(Sal, postscript=" (ppt) [expect ?? for issue 1637]"))
                ## 17:18   int Cw;
                Cw <- 0.1 * readBin(buf[off+17:18], "integer", size=2, signed=TRUE, endian="little")
                oceDebug(debug, vectorShow(Cw, postscript=" (m/s) [expect ?? for issue 1637]"))
                ## 19:20   unsigned int BlankDistance;
                BlankDistance <- readBin(buf[off+19:20], "integer", size=2, signed=FALSE, endian="little")
                oceDebug(debug, vectorShow(BlankDistance, postscript=" cm [expect ?? for issue 1637]"))
                ## 21:22   unsigned int PulseLength;
                PulseLength  <- readBin(buf[off+21:22], "integer", size=2, signed=FALSE, endian="little")
                oceDebug(debug, vectorShow(PulseLength , postscript=" cm [expect ?? for issue 1637]"))
                ## 23:24   unsigned int CellSize;
                CellSize <- readBin(buf[off+23:24], "integer", size=2, signed=FALSE, endian="little")
                oceDebug(debug, vectorShow(CellSize, postscript=" cm [expect 350 for issue 1637] NB appears twice"), style="blue")
                ## 25      char TempMode;
                ## 26:29   long AvgInterval;
                ## 30:33   long SampleInterval;
                ## 34:35   unsigned int PingInterval;
                ## 36:37   unsigned int BurstMode;
                ## 38:41   long BurstInterval;
                ## 42:43   unsigned int SamplesPerBurst;
                SamplesPerBurst <- readBin(buf[off+42:43], "integer", size=2, signed=FALSE, endian="little")
                oceDebug(debug, vectorShow(SamplesPerBurst, postscript=" [expect ?? for issue 1637]"))
                ## 44      char CoordSystem;
                CoordSystem <- as.integer(buf[off+44])
                oceDebug(debug, vectorShow(CoordSystem, postscript=" (0=beam 1=xyz 2=enu)"))
                ## 45      char OutMode;
                ## 46      char OutFormat;
                ## 47      char RecorderEnabled;
                ## 48      char RecorderMode;
                ## 49      char DeploymentMode;
                ## 50:58   char DeploymentName[9]
                DeploymentName <- readBin(buf[off+50:59], "character", size=9, endian="little")
                oceDebug(debug, vectorShow(DeploymentName))
                ## 59:66   DateTimeType BeginDeploymentDateTime
                BeginDepoymentTime <- sontekDateTime(buf[off+59:66])
                oceDebug(debug, vectorShow(BeginDepoymentTime))
                ## 67:126  char CommentLine1[60];
                CommentLine1 <- readBin(buf[off+67:126], "character", size=60, endian="little")
                oceDebug(debug, vectorShow(CommentLine1))
                ## 127:186 char CommentLine2[60];
                CommentLine2 <- readBin(buf[off+127:186], "character", size=60, endian="little")
                oceDebug(debug, vectorShow(CommentLine2))
                ## 187:246 char CommentLine3[60];
                CommentLine3 <- readBin(buf[off+187:246], "character", size=60, endian="little")
                oceDebug(debug, vectorShow(CommentLine3))
                ## 247     char AutoSleep;
                ## 248     char DynBoundAdj;
                ## 249:250 int CellBegin;
                CellBegin <- 0.01 * readBin(buf[off+249:250], "integer", signed=TRUE, size=2, endian="little")
                oceDebug(debug, vectorShow(CellBegin, postscript=" m, vert. from instrument [expect ??? for issue 1637]"))
                ## 251:252 int CellEnd;
                CellEnd <- 0.01 * readBin(buf[off+251:252], "integer", signed=TRUE, size=2, endian="little")
                oceDebug(debug, vectorShow(CellEnd, postscript=" m, vert. from instrument [expect ??? for issue 1637]"))
                ## 253:254 int CohLag;
                ## 255     char DataFormat;
                DataFormat  <- as.integer(buf[off+255])
                oceDebug(debug, vectorShow(DataFormat, postscript=" (0 means long, 1 means short)"))
                ## 256     char WaveSpectra;
                ## 257:258 int WaterDepth;
                WaterDepth <- 0.01 * readBin(buf[off+251:252], "integer", size=2, endian="little")
                oceDebug(debug, vectorShow(WaterDepth, postscript=" m [expect ??? for issue 1637]"))
                off <- off + 258
                oceDebug(debug, "done with 'User setup configuration structure' (258 bytes); off=", off, "\n", style="bold")
                if (debug) {
                    ii <- seq(-5, 5)
                    oceDebug(debug, "below are some bytes (exported to BUF); off=", off, " (exported to OFF)\n", style="red")
                    BUF<<-buf
                    OFF<<-off
                    print(data.frame(i=off+ii, buf=buf[off+ii]))
                    message("NOTE: first byte after header is %x", buf[off+1])
                    message("NOTE: 160 bytes later, have %x", buf[off+1+160])
                    message("NOTE: 161 bytes later, have %x", buf[off+1+161])
                    message("NOTE: 162 bytes later, have %x", buf[off+1+162])

                }
            }
        } else {
            warning("Argonaut operation configuration structure (64 bytes) not found; expected 0x41 0x02 0x40 but got",
                    " 0x", buf[97], " 0x", buf[98], " 0x", buf[99], sep="")
        }

    } else {
        cpuSoftwareVerNum <- dspSoftwareVerNum <- boardRev <-
            type <- nbeams <- slant.angle <- orientation <-
                compass.installed <- recorder.installed <- temp.installed <- press.installed <- "?"
    }
    ##profileStart <- .Call("match2bytes", buf, parameters$profile.byte1, parameters$profile.byte2, FALSE)
    ##profileStart <- .Call("ldc_sontek_adp", buf, 0, 0, 0, 1, -1) # no ctd, no gps, no bottom-track; pcadp; all data
    type.int <- 0L
    type.int <- if (type == "adp") {
        0L 
    } else if (type == "argonaut_adp") {
        1L 
    } else if (type == "pcadp") {
        2L
    } else {
        stop("'type' must be \"adp\", \"argonaut_adp\" or \"pcadp\", but it is \"", type, "\"")
    }
    oceDebug(debug, vectorShow(type.int))
    if (type == "argonaut_adp") {
        profileStart <- do_ldc_sontek_argonaut(buf, -1L)
    } else {
        profileStart <- do_ldc_sontek_adp(buf, 0L, 0L, 0L, as.integer(type.int), -1L) # no ctd, no gps, no bottom-track; pcadp; all data
    }
    profileStart2 <- sort(c(profileStart, profileStart+1)) # use this to subset for 2-byte reads
    oceDebug(debug, "first 10 profileStart:", profileStart[1:10], "\n")
    oceDebug(debug, "first 100 bytes of first profile:", paste(buf[profileStart[1]:(99+profileStart[1])], collapse=" "), "\n")
    ## Examine the first profile to get numberOfBeams, etc.
    headerLength <- 80                 # FIXME: should this really be hard-wired??
    s <- profileStart[1]
    ## Only read (important) things that don't change profile-by-profile
    numberOfBeams <- as.integer(buf[s+26])
    oceDebug(debug, "numberOfBeams=", numberOfBeams, "\n")
    if (numberOfBeams != 3)
        stop("there should be 3 beams, but the file indicates ", numberOfBeams)
    orientation <- as.integer(buf[s+27])
    oceDebug(debug, "orientation=", orientation, "\n")
    temp.mode <- as.integer(buf[s+28])
    oceDebug(debug, "temp.mode=", temp.mode, "\n")
    originalCoordinate <- as.integer(buf[s+29])
    oceDebug(debug, "originalCoordinate=", originalCoordinate, "\n")
    numberOfCells <- readBin(buf[s+30:31], "integer", n=1, size=2, endian="little", signed=FALSE)
    oceDebug(debug, "numberOfCells=", numberOfCells, "\n")
    cellSize <- readBin(buf[s+32:33], "integer", n=1, size=2, endian="little", signed=FALSE) / 100 # metres
    oceDebug(debug, "cellSize=", cellSize, "m\n")
    blankingDistance <- readBin(buf[s+34:35], "integer", n=1, size=2, endian="little", signed=FALSE) / 100 # metres
    oceDebug(debug, "blankingDistance=", blankingDistance, "m\n")
    soundSpeed <- readBin(buf[s+60:61], "integer", n=1, size=2, endian="little", signed=FALSE) / 10
    oceDebug(debug, "soundSpeed=", soundSpeed, "m/s\n")
    profilesInFile <- length(profileStart)
    ##id <- buf[profileStart]
    bytesPerProfile <- diff(profileStart[1:2])
    oceDebug(debug, "bytesPerProfile=", bytesPerProfile, "\n")

    ## File time range and deltat
    measurementStart <- ISOdatetime(readBin(buf[profileStart[1]+18:19], "integer", n=1, size=2, signed=FALSE, endian="little"), # year
                                    as.integer(buf[profileStart[1]+21]), # month
                                    as.integer(buf[profileStart[1]+20]), # day
                                    as.integer(buf[profileStart[1]+23]), # hour
                                    as.integer(buf[profileStart[1]+22]), # min
                                    as.integer(buf[profileStart[1]+25])+0.01*as.integer(buf[profileStart[1]+24]), # sec (decimal)
                                    tz=tz)
    oceDebug(debug, "measurementStart=", format(measurementStart), "\n")
    oceDebug(debug, "length(profileStart)=", length(profileStart), " [FIXME: if to not given, use this??]\n")

    measurementEnd <- ISOdatetime(readBin(buf[profileStart[profilesInFile]+18:19], "integer", n=1, size=2, signed=FALSE, endian="little"), # year
                                   as.integer(buf[profileStart[profilesInFile]+21]), # month
                                   as.integer(buf[profileStart[profilesInFile]+20]), # day
                                   as.integer(buf[profileStart[profilesInFile]+23]), # hour
                                   as.integer(buf[profileStart[profilesInFile]+22]), # min
                                   as.integer(buf[profileStart[profilesInFile]+25])+0.01*as.integer(buf[profileStart[1]+24]), # sec (decimal)
                                   tz=tz)
    oceDebug(debug, "sampling.end=", format(measurementEnd), "\n")
    measurementDeltat <- as.numeric(ISOdatetime(readBin(buf[profileStart[2]+18:19], "integer", n=1, size=2, signed=FALSE, endian="little"), # year
                                                 as.integer(buf[profileStart[2]+21]), # month
                                                 as.integer(buf[profileStart[2]+20]), # day
                                                 as.integer(buf[profileStart[2]+23]), # hour
                                                 as.integer(buf[profileStart[2]+22]), # min
                                                 as.integer(buf[profileStart[2]+25])+0.01*as.integer(buf[profileStart[1]+24]), # sec
                                                 tz=tz)) - as.numeric(measurementStart)
    oceDebug(debug, "sampling.deltat=", format(measurementDeltat), "\n")

    ## Window data buffer, using bisection in case of a variable number of vd between sd pairs.
    if (inherits(from, "POSIXt")) {
        if (!inherits(to, "POSIXt"))
            stop("if 'from' is POSIXt, then 'to' must be, also")
        fromPair <- bisectSontekAdp(buf, from, -1, debug-1)
        from <- fromIndex <- fromPair$index
        toPair <- bisectSontekAdp(buf, to, 1, debug-1)
        to <- to.index <- toPair$index
        oceDebug(debug, "  from=", format(fromPair$t), " yields profileStart[", fromIndex, "]\n",
                  "  to  =", format(toPair$t),   " yields profileStart[", to.index, "]\n",
                  "  by=", by, "s\n",
                  "profileStart[1:10]=", profileStart[1:10], "\n",
                  "profileStart[", fromPair$index, "]=", profileStart[fromPair$index], "at time", format(fromPair$t), "\n",
                  "profileStart[",   toPair$index, "]=", profileStart[  toPair$index], "at time", format(  toPair$t), "\n")
        ## FIXME next line reads year incorrectly
        two.times <- ISOdatetime(readBin(buf[profileStart[1:2]+18:19], "integer", size=2, signed=FALSE, endian="little"),  # year
                                 as.integer(buf[profileStart[1:2]+21]), # month
                                 as.integer(buf[profileStart[1:2]+20]), # day
                                 as.integer(buf[profileStart[1:2]+23]), # hour
                                 as.integer(buf[profileStart[1:2]+22]), # min
                                 as.integer(buf[profileStart[1:2]+25])+0.01*as.integer(buf[profileStart[1]+24]), # sec
                                 tz=tz)
        dt <- as.numeric(difftime(two.times[2], two.times[1], units="secs"))
        oceDebug(debug, "dt=", dt, "s; at this stage, by=", by, "(not interpreted yet)\n")
        profileStart <- profileStart[profileStart[fromIndex] < profileStart & profileStart < profileStart[to.index]]
        if (is.character(by))
            by <- floor(0.5 + ctimeToSeconds(by) / dt)
        oceDebug(debug, "by=", by, "profiles (after change)\n")
        profileStart <- profileStart[seq(1, length(profileStart), by=by)]
        oceDebug(debug, "dt=", dt, "\n", "by=", by, "profileStart[1:10] after indexing:", profileStart[1:10], "\n")
    } else {
        fromIndex <- from
        if (missing.to)
            to <- length(profileStart)
        to.index <- to
        if (to.index < 1 + fromIndex)
            stop("need more separation between from and to")
        if (is.character(by))
            stop("cannot have string for 'by' if 'from' and 'to' are integers")
        profileStart <- profileStart[seq(from=from, to=to, by=by)]
        oceDebug(debug, "profileStart[1:10] after indexing:", profileStart[1:10], "\n")
    }
    profilesToRead <- length(profileStart)
    oceDebug(debug, "profilesInFile=", profilesInFile, "; profilesToRead=", profilesToRead, "\n")
    profileStart2 <- sort(c(profileStart, profileStart+1)) # use this to subset for 2-byte reads
    year   <- readBin(buf[profileStart2 + 18], "integer", n=profilesToRead, size=2, endian="little", signed=FALSE)
    day    <- as.integer(buf[profileStart + 20])
    month  <- as.integer(buf[profileStart + 21])
    minute <- as.integer(buf[profileStart + 22])
    hour   <- as.integer(buf[profileStart + 23])
    sec100 <- as.integer(buf[profileStart + 24])
    second <- as.integer(buf[profileStart + 25])
    time <- ISOdatetime(year, month, day, hour, minute, second+sec100/100, tz=tz)
    rm(year, day, month, minute, hour, sec100, second)
    temperature <- readBin(buf[profileStart2 + 46], "integer", n=profilesToRead, size=2, endian="little", signed=TRUE) / 100
    oceDebug(debug, "temperature[1:10]=", temperature[1:10], "\n")
    pressure <- readBin(buf[profileStart2 + 48], "integer", n=profilesToRead, size=2, endian="little", signed=FALSE) / 100
    ## FIXME: pressure (+else?) is wrong.  Need to count bytes on p84 of ADPManual to figure out where to look [UGLY]
    oceDebug(debug, "pressure[1:10]=", pressure[1:10], "\n")
    heading <- readBin(buf[profileStart2 + 40], "integer", n=profilesToRead, size=2, endian="little", signed=TRUE) / 10
    pitch <- readBin(buf[profileStart2 + 42], "integer", n=profilesToRead, size=2, endian="little", signed=TRUE) / 10
    roll <- readBin(buf[profileStart2 + 44], "integer", n=profilesToRead, size=2, endian="little", signed=TRUE) / 10

    oceDebug(debug, "time[1:10]=", format(time[1:10]), "\n")
    v <- array(numeric(), dim=c(profilesToRead, numberOfCells, numberOfBeams))
    a <- array(raw(), dim=c(profilesToRead, numberOfCells, numberOfBeams))
    q <- array(raw(), dim=c(profilesToRead, numberOfCells, numberOfBeams))
    nd <- numberOfCells * numberOfBeams
    oceDebug(debug, "preparing to read v,a,q: nd=", nd, ", numberOfCells=", numberOfCells, ", numberOfBeams=", numberOfBeams, ",  headerLength=", headerLength, "\n")
    if (type == "pcadp") {
        nbeamMax <- 4                 # Max numberOfBeams, not actual number
        headerLength <- headerLength + 2 * (8 + nbeamMax) + 2 * nbeamMax + nbeamMax
        oceDebug(debug, "pcadp: changed headerLength to", headerLength, "\n")
        ## Below is C code from SonTek, for pulse-coherent adp (2-byte little-endian
        ## integers).  FIXME: should perhaps read these things, but this is not a
        ## high priority, since in the data file for which the code was originally
        ## developed, all distances were set to 123 mm and all velocities to
        ## 9999 mm/s, suggestive of insignificant, place-holder values.
        ##
        ##typedef struct
        ##{
        ##  unsigned int  ResLag;             /* in mm     Used for single cell    */
        ##  unsigned int  ResUa;              /* in mm/s   Ambiguity resolution    */
        ##  unsigned int  ResStart;           /* in mm     Position of resolve     */
        ##  unsigned int  ResLength;          /* in mm     cell                    */
        ##  unsigned int  PrfLag;             /* in mm     Used for full profile   */
        ##  unsigned int  PrfUa;              /* in mm/s                           */
        ##  unsigned int  PrfStart;           /* in mm     Position/Length of first*/
        ##  unsigned int  PrfLength;          /* in mm     cell in profile         */
        ##  unsigned int  Range[MAX_BEAMS];   /* in mm     Range to boundary       */
        ##           int  Ures[MAX_BEAMS];    /* in mm/s   Velocities from Resolve */
        ##                                    /*           lag                     */
        ##  unsigned char Cres[MAX_BEAMS];    /* in %      Correlations from       */
        ##                                    /*           resolve lag             */
        ##} PCrecordType;
    }
    velocityScale <- 1e-3
    if (profilesToRead > 0) {
        for (i in 1:profilesToRead) {
            v_ <- velocityScale * matrix(readBin(buf[profileStart[i] + headerLength + seq(0, 2*nd-1)],
                                                 "integer", n=nd, size=2, signed=TRUE, endian="little"),
                                         ncol=numberOfBeams, byrow=FALSE)
            a_ <- matrix(buf[profileStart[i] + headerLength + 2*nd + seq(0, nd-1)], ncol=numberOfBeams, byrow=FALSE)
            q_ <- matrix(buf[profileStart[i] + headerLength + 3*nd + seq(0, nd-1)], ncol=numberOfBeams, byrow=FALSE)
            for (b in 1:numberOfBeams) {
                ## FIXME: probably could be speeded up
                v[i, , b] <- v_[, b]
                a[i, , b] <- a_[, b]
                q[i, , b] <- q_[, b]
            }
            if (monitor) {
                cat(".")
                if (!(i %% 50))
                    cat(i, "\n")
            }
        }
        rm(v_, a_, q_)
        if (monitor)
            cat("\nRead", profilesToRead,  "of the", profilesInFile, "profiles in", filename, "\n")
        if (type == "pcadp")
            v <- v / 10                # it seems pcadp is in 0.1mm/s
    } else {
        stop("please request to read *some* profiles")
    }
    ## interpolate headings (which may be less frequent than profiles ... FIXME: really???)
    nheading <- length(heading)
    nv <- dim(v)[1]
    if (nheading != nv) {
        warning("read.adp.sontek() interpolating ", nheading, " heading/pitch/roll values to the ", nv, " velocity profiles")
        oceDebug(debug, "BEFORE: length(heading)=", nheading, ", nv=", nv, "\n")
        ##xout <- seq(1, nheading, length.out=nv)
        heading <- approx(1:nheading, heading, seq(1, nheading, length.out=nv))$y
        ##print(data.frame(xout=xout, heading=heading))
        pitch <- approx(1:nheading, pitch, seq(1, nheading, length.out=nv))$y
        roll <- approx(1:nheading, roll, seq(1, nheading, length.out=nv))$y
        oceDebug(debug, "AFTER:  length(heading)=", length(heading), "\n")
    }
    res@data <- list(v=v, a=a, q=q,
                     distance=seq(blankingDistance, by=cellSize, length.out=numberOfCells),
                     time=time,
                     temperature=temperature,
                     pressure=pressure,
                     heading=heading, pitch=pitch, roll=roll)
    oceDebug(debug, "slant.angle=", slant.angle, "; type=", type, "\n")
    beamAngle <- if (slant.angle == "?") 25 else slant.angle
    res@metadata$manufacturer <- "sontek"
    res@metadata$type <- type
    res@metadata$filename <- filename
    res@metadata$serialNumber <- if (exists('serialNumber')) serialNumber else "?"
    res@metadata$longitude <- longitude
    res@metadata$latitude <- latitude
    res@metadata$numberOfSamples <- dim(v)[1]
    res@metadata$numberOfCells <- dim(v)[2]
    res@metadata$numberOfBeams <- dim(v)[3]
    res@metadata$velocityResolution <- velocityScale
    res@metadata$velocityMaximum <- velocityScale * 2^15
    res@metadata$measurementStart <- measurementStart
    res@metadata$measurementEnd <- measurementEnd
    res@metadata$measurementDeltat <- measurementDeltat
    res@metadata$frequency <- frequency
    res@metadata$cpuSoftwareVerNum <- cpuSoftwareVerNum
    res@metadata$dspSoftwareVerNum <- dspSoftwareVerNum
    res@metadata$boardRev <- boardRev
    res@metadata$originalCoordinate <- c("beam", "xyz", "enu", "other")[originalCoordinate+1]
    res@metadata$oceCoordinate <- c("beam", "xyz", "enu", "other")[originalCoordinate+1]
    res@metadata$beamAngle <- beamAngle
    res@metadata$oceBeamUnspreaded <- FALSE
    res@metadata$orientation <- if (1==orientation) "upward" else "downward"
    if (numberOfBeams == 3) {
        if (res@metadata$orientation == "upward") {
            ##S  <- 1 / (3 * sin(25 * pi / 180))             # 0.7887339
            ##CS <- 1 / cos(30*pi/180) / sin(25*pi/180) / 2  # 1.366127 (30deg from 3-beam pattern)
            ##C  <- 1 / (3 * cos(25 * pi / 180))             # 0.3677926
            S  <- 1 / (3 * sin(beamAngle * pi / 180)) # 0.7887339
            CS <- 1 / cos(30*pi/180) / sin(beamAngle*pi/180) / 2 # 1.366127 (30deg from 3-beam pattern)
            C  <- 1 / (3 * cos(beamAngle * pi / 180))             # 0.3677926
            ## FIXME: check up and down; also read it and check
            res@metadata$transformationMatrix <- matrix(c(2*S,  -S,  -S,
                                                            0, -CS,  CS,
                                                            C,   C,   C),
                                                    nrow=3, byrow=TRUE)
        } else {
            S  <- 1 / (3 * sin(beamAngle * pi / 180)) # 0.7887339
            CS <- 1 / cos(30*pi/180) / sin(beamAngle*pi/180) / 2 # 1.366127 (30deg from 3-beam pattern)
            C  <- 1 / (3 * cos(beamAngle * pi / 180))             # 0.3677926
            ## warning("*****FIXME: check up and down; also read it and check*****")
            res@metadata$transformationMatrix <- matrix(c(2*S,  -S,  -S,
                                                            0,  CS, -CS,
                                                           -C,  -C,  -C),
                                                    nrow=3, byrow=TRUE)
        }
        ## For later use, RC says that the PC-ADP uses
        ## T =  2.576  -1.288  -1.288
        ##      0.000  -2.230   2.230
        ##      0.345   0.345   0.345
        ## and these are by the same formulae, with 25 switched to 15 (different beamAngle)
    } else
        stop("can only handle 3-beam devices")
    res@metadata$units <- list(v="m/s", distance="m")
    if (missing(processingLog)) {
        pl <- processingLogItem(paste("read.adp.sontek(\"", filename, "\", from=", from, ", to=", to, ")", sep=""))
    } else {
        pl <- processingLogAppend(processingLogItem(processingLog),
                                  paste("read.adp.sontek(\"", filename, "\", from=", from, ", to=", to, ")", sep=""))
    }
    res@processingLog <- pl
    res
}

sontek.time <- function(t, tz=getOption("oceTz"))
{
    minute <- bcdToInteger(t[1])
    second <- bcdToInteger(t[2])
    day <- bcdToInteger(t[3])
    hour <- bcdToInteger(t[4])
    year <- bcdToInteger(t[5])
    year <- year + if (year >= 90) 1900 else 2000 # page 51 of System Integrator Guide
    month <- bcdToInteger(t[6])
    milliseconds <- readBin(t[7:8], "integer", n=1, size=2, endian="little", signed=FALSE)
    ISOdatetime(year, month, day, hour, minute, second+milliseconds/1000, tz=tz)
}

#' Read a serial SonTek ADP file
#'
#' Read a SonTek acoustic-Doppler profiler file, in a serial form that
#' is possibly unique to Dalhousie University.
#'
#' @param beamAngle angle between instrument axis and beams, in degrees.
#'
#' @param type a character string indicating the type of instrument.
#'
#' @param orientation optional character string specifying the orientation of the
#' sensor, provided for those cases in which it cannot be inferred from the
#' data file.  The valid choices are `"upward"`, `"downward"`, and
#' `"sideward"`.
#'
#' @template adpTemplate
#'
#' @author Dan Kelley and Clark Richards
#'
#' @family things related to adp data
read.adp.sontek.serial <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                                   longitude=NA, latitude=NA,
                                   type=c("adp", "pcadp"),
                                   beamAngle=25, orientation,
                                   monitor=FALSE, processingLog,
                                   debug=getOption("oceDebug"))
{
    if (!missing(file) && is.character(file) && 0 == file.info(file)$size)
        stop("empty file")
    ## Data format is described in
    ##   SonTek/YSI
    ##   ADPManual_v710.pdf
    ## A3. Profile Header/CTD/GPS/Bottom Track,/SonWave/Profile Data Structures
    bisectAdpSontekSerial <- function(buf, t.find, add=0, tz="UTC", debug=0) {
        oceDebug(debug, "bisectAdpSontekSerial(t.find=", format(t.find), ", add=", add, "\n")
        len <- length(p)
        lower <- 1
        upper <- len
        passes <- floor(10 + log(len, 2)) # won't need this many; only do this to catch coding errors
        for (pass in 1:passes) {
            middle <- floor( (upper + lower) / 2 )
            year   <- readBin(buf[p[middle] + 18:19], what="integer", n=1, size=2, signed=FALSE, endian="little")
            day    <- readBin(buf[p[middle] + 20], what="integer", n=1, size=1, signed=FALSE)
            month  <- readBin(buf[p[middle] + 21], what="integer", n=1, size=1, signed=FALSE)
            min    <- readBin(buf[p[middle] + 22], what="integer", n=1, size=1, signed=FALSE)
            hour   <- readBin(buf[p[middle] + 23], what="integer", n=1, size=1, signed=FALSE)
            sec100 <- readBin(buf[p[middle] + 24], what="integer", n=1, size=1, signed=FALSE)
            sec    <- readBin(buf[p[middle] + 25], what="integer", n=1, size=1, signed=FALSE)
            t <- ISOdatetime(year=year, month=month, day=day, hour=hour, min=min, sec=sec + sec100/100, tz=tz)
            oceDebug(debug, "t=", format(t), " y=", year, " m=", month, " d=", format(day, width=2),
                      " h=", format(hour, width=2),
                      " m=", format(min, width=2),
                      " s=", format(sec+sec100/100, width=4),
                      "| pass", format(pass, width=2), "/", passes, "| middle=", middle,
                      "(", format(middle/upper*100, digits=4), "%)\n", sep="")
            if (t.find < t)
                upper <- middle
            else
                lower <- middle
            if (upper - lower < 2)
                break
        }
        middle <- middle + add          # may use add to extend before and after window
        if (middle < 1)
            middle <- 1
        if (middle > len)
            middle <- len
        t <- ISOdatetime(readBin(buf[p[middle]+18:19], "integer", size=2, signed=FALSE, endian="little"),
                         as.integer(buf[p[middle]+21]), # month
                         as.integer(buf[p[middle]+20]), # day
                         as.integer(buf[p[middle]+23]), # hour
                         as.integer(buf[p[middle]+22]), # min
                         as.integer(buf[p[middle]+25])+0.01*as.integer(buf[p[middle]+24]),
                         tz=tz)
        oceDebug(debug, "result: t=", format(t), " at d[", middle, "]=", p[middle], "\n")
        return(list(index=middle, time=t))
    }
    oceDebug(debug, paste("read.adp.sontek.serial(file[1]=\"", file[1],
                           "\", from=", from,
                           if (missing(to)) "to," else sprintf(", to=%s, ", format(to)),
                           ", by=", by,
                           ", latitude=", latitude, ", longitude=", longitude,
                           ", monitor=", monitor,
                           ", processingLog=(not shown), debug=", debug, ") {\n", sep=""), unindent=1)
    nfile <- length(file)
    if (nfile > 1) {
        ## handle multiple files
        oceDebug(debug, "handling multiple files\n")
        buf <- NULL
        for (i in 1:nfile) {
            if (monitor)
                cat("\"", file[i], "\" ", sep="")
            thisFile <- file(file[i], "rb")
            seek(thisFile, 0, "end", rw="read")
            fileSize <- seek(thisFile, 0, origin="start", rw="read")
            if (monitor)
                cat(fileSize, "bytes\n")
            buf <- c(buf, readBin(thisFile, what="raw", n=fileSize, endian="little"))
            close(thisFile)
        }
        filename <- paste("(\"", file[i], "\", ...)", sep="")
    } else {
        ## handle single file (which might be a connection, etc)
        if (is.character(file)) {
            filename <- fullFilename(file)
            file <- file(file, "rb")
            on.exit(close(file))
        }
        if (!inherits(file, "connection"))
            stop("argument `file' must be a character string or connection")
        if (!isOpen(file)) {
            filename <- "(connection)"
            open(file, "rb")
            on.exit(close(file))
        }
        ## read whole file into buffer
        seek(file, 0, "end", rw="read")
        fileSize <- seek(file, 0, origin="start", rw="read")
        oceDebug(debug, "filesize=", fileSize, "\n")
        buf <- readBin(file, what="raw", n=fileSize, endian="little")
    }
    ##p <- .Call("ldc_sontek_adp", buf, 0, 0, 0, 0, -1) # no ctd, no gps, no bottom-track; all data
    p <- do_ldc_sontek_adp(buf, 0L, 0L, 0L, 0L, -1L) # no ctd, no gps, no bottom-track; all data
    ## read some unchanging things from the first profile only
    serialNumber <- paste(readBin(buf[p[1]+4:13], "character", n=10, size=1), collapse="")
    numberOfBeams <- readBin(buf[p[1]+26], "integer", n=1, size=1, signed=FALSE)
    if (missing(orientation)) {
    orientation <- readBin(buf[p[1]+27], "integer", n=1, size=1, signed=FALSE)
    if (orientation == 0)
        orientation <- "upward"
    else if (orientation == 1)
        orientation  <- "downward"
    else if (orientation == 2)
        orientation <- "sideward"
    else
        stop("orientation=", orientation, "but must be 0 (upward), 1 (downward), or 2 (sideward)")
    } else {
        if (orientation != "upward" && orientation != "downward")
            stop("orientation \"", orientation, " is not allowed; try \"upward\" or \"downward\"")
    }
    ## 28 is tempmode
    ## coord.system 0=beam 1=XYZ 2=ENU
    originalCoordinate <- readBin(buf[p[1]+29], "integer", n=1, size=1, signed=FALSE)
    if (originalCoordinate == 0)
        originalCoordinate <- "beam"
    else if (originalCoordinate == 1)
        originalCoordinate <- "xyz"
    else if (originalCoordinate == 2)
        originalCoordinate <- "enu"
    else
        stop("originalCoordinate=", originalCoordinate, "but must be 0 (beam), 1 (xyz), or 2 (enu)")
    numberOfCells <- readBin(buf[p[1]+30:31], "integer", n=1, size=2, signed=FALSE, endian="little")
    cellSize <- 0.01*readBin(buf[p[1]+32:33], what="integer", n=1, size=2, signed=FALSE, endian="little")
    blankingDistance <- 0.01*readBin(buf[p[1]+34:35], what="integer", n=1, size=2, signed=FALSE)
    distance <- blankingDistance + cellSize * seq(from=0.5, by=cellSize, length.out=numberOfCells)
    ## trim, if from and to are integers
    if (!missing(to)) {
        if (is.numeric(from) && is.numeric(to) && is.numeric(by)) {
            if (from < 1)
                stop("from=", from, " but must exceed 1")
            if (to < 1)
                stop("to=", to, " but must exceed 1")
            if (by < 1)
                stop("by=", by, " but must exceed 1")
            if (to <= from)
                stop("from=", from, " must exceed to=", to)
            p <- p[seq(from=from, to=to, by=by)]
        } else {
            if (inherits(from, "POSIXt")) {
                if (!inherits(to, "POSIXt"))
                    stop("if 'from' is POSIXt, then 'to' must be, also")
                if (!is.numeric(by)) {
                    warning("'by' must be numeric")
                    by <- 1
                }
                fromPair <- bisectAdpSontekSerial(buf, from, add=-1, tz=tz, debug=debug-1)
                from <- fromIndex <- fromPair$index
                toPair <- bisectAdpSontekSerial(buf, to, add=1, tz=tz, debug=debug-1)
                to <- to.index <- toPair$index
                oceDebug(debug, "from=", format(fromPair$t), " yields p[", fromIndex, "]\n",
                          "  to  =", format(toPair$t), "yields p[", to.index, "]\n",
                          "  by=", by, "(not yet decoded)\n",
                          vectorShow(p, "p:"),
                          "  p[", fromPair$index, "]=", p[fromPair$index], "at time", format(fromPair$t), "\n",
                          "  p[",   toPair$index, "]=", p[  toPair$index], "at time", format(  toPair$t), "\n")
                p <- p[seq(from=from, to=to, by=by)]
            }
        }
    }
    np <- length(p)
    pp <- sort(c(p, p+1)) # for 2-byte addressing ('int' in the Sontek docs)
    ##pppp <- sort(c(p, p+1, p+2, p+3)) # for 4-byte addressing ('long' in the Sontek docs)

    ## read profile-specific things profile by profile
    ##profile.number <- readBin(buf[pppp+14], "integer", n=np, size=4)
    ## FIXME: should check that profile number is monotonic ... it may
    ## help us with daily blank-outs, also!
    year <- readBin(buf[pp+18], "integer", n=np, size=2, signed=FALSE)
    day <- readBin(buf[p+20], "integer", n=np, size=1, signed=FALSE)
    month <- readBin(buf[p+21], "integer", n=np, size=1, signed=FALSE)
    min <- readBin(buf[p+22], "integer", n=np, size=1, signed=FALSE)
    hour <- readBin(buf[p+23], "integer", n=np, size=1, signed=FALSE)
    sec100 <- readBin(buf[p+24], "integer", n=np, size=1, signed=FALSE)
    sec <- readBin(buf[p+25], "integer", n=np, size=1, signed=FALSE)
    time <- ISOdatetime(year, month, day, hour, min, sec+0.01*sec100, tz=tz)
    rm(year, day, month, min, hour, sec100, sec) # possibly this space will come in handy
    heading <- 0.1 * readBin(buf[pp+40], "integer", n=np, size=2, signed=TRUE)
    pitch <- 0.1 * readBin(buf[pp+42], "integer", n=np, size=2, signed=TRUE)
    roll <- 0.1 * readBin(buf[pp+44], "integer", n=np, size=2, signed=TRUE)
    temperature <- 0.01 * readBin(buf[pp+46], "integer", n=np, size=2, signed=TRUE)
    v <- array(numeric(), dim=c(np, numberOfCells, numberOfBeams))
    a <- array(raw(), dim=c(np, numberOfCells, numberOfBeams))
    q <- array(raw(), dim=c(np, numberOfCells, numberOfBeams))
    ndata <- numberOfCells * numberOfBeams
    i1 <- seq(1, ndata)
    i2 <- seq(1, 2*ndata)
    for (ip in 1:np) {
        p0 <- p[ip] + 79
        v_ <- matrix(0.001*readBin(buf[p0 + i2], "integer", endian="little", n=ndata, size=2, signed=TRUE),
                     ncol=numberOfBeams, byrow=FALSE)
        p0 <- p0 + 2 * ndata
        ## NOTE: q is std-dev; need to multiply by 0.001 to get in m/s
        q_ <- matrix(buf[p0 + i1], ncol=numberOfBeams, byrow=FALSE)
        p0 <- p0 + ndata
        a_ <- matrix(buf[p0 + i1], ncol=numberOfBeams, byrow=FALSE)
        for (b in 1:numberOfBeams) {
            v[ip, , b] <- v_[, b]
            a[ip, , b] <- a_[, b]
            q[ip, , b] <- q_[, b]
        }
        if (monitor) {
            if (ip %% 50)
                cat(".")
            else
                cat(".", ip, "\n")
        }
    }
    if (monitor)
        cat("\nRead", np,  "of the", np, "profiles in", filename[1], "\n")
    S  <- sin(beamAngle * pi / 180)
    C  <- cos(beamAngle * pi / 180)
    ## FIXME: use the transformation.matrix, if it has been discovered in a header
    if (orientation == "upward") {
        ## OAR explains the method of determining the matrix.
        transformationMatrix <- rbind(c( 2/3/S,       -1/3/S, -1/3/S),
                                      c(     0, -1/sqrt(3)/S, 1/sqrt(3)/S),
                                      c( 1/3/C,        1/3/C,  1/3/C))
    } else {
        transformationMatrix <- rbind(c( 2/3/S,       -1/3/S, -1/3/S),
                                      c(     0,  1/sqrt(3)/S, -1/sqrt(3)/S),
                                      c(-1/3/C,       -1/3/C, -1/3/C))
    }
    res <- new("adp")
    res@metadata$manufacturer <- "sontek"
    res@metadata$instrumentType <- "adp"
    res@metadata$serialNumber <- serialNumber
    res@metadata$filename <- filename
    res@metadata$latitude <- latitude
    res@metadata$longitude <- longitude
    res@metadata$transformationMatrix <- transformationMatrix
    res@metadata$measurementStart <- 0 # FIXME: should fill in
    res@metadata$measurementEnd <- np # FIXME: should fill in
    res@metadata$measurementDeltat <- mean(diff(as.numeric(time)))
    res@metadata$subsampleStart <- 0 # FIXME: should fill in
    res@metadata$subsampleEnd <- np
    res@metadata$subsampleDeltat <- mean(diff(as.numeric(time)))
    res@metadata$frequency <- NA # FIXME
    res@metadata$numberOfSamples <- np
    res@metadata$numberOfBeams <- numberOfBeams
    res@metadata$originalCoordinate <- originalCoordinate
    res@metadata$oceCoordinate <- originalCoordinate
    res@metadata$beamAngle <- beamAngle
    res@metadata$oceBeamUnspreaded <- FALSE
    res@metadata$orientation <- orientation
    res@metadata$units <- list(v="m/s", distance="m")
    res@data <- list(v=v, a=a, q=q,
                     distance=distance,
                     time=time,
                     heading=heading, pitch=pitch, roll=roll,
                     temperature=temperature,
                     pressure=rep(0, length(temperature)),
                     distance=distance)
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    hitem <- processingLogItem(processingLog)
    res@processingLog <- hitem
    res
}
