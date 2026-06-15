#' parse a collectl output -- could be conditional on discovered call
#' @importFrom lubridate as_datetime
#' @importFrom utils browseURL read.delim
#' @param path character(1) path to (possibly gzipped) collectl output
#' @param tz character(1) POSIXct time zone code, defaults to "EST"
#' @return a data.frame
#' @note A lubridate datetime is added as a column.
#' @examples
#' lk = cl_parse(system.file("demotab/demo_1123.tab.gz", package="Rcollectl"))
#' head(lk)
#' @export
cl_parse = function(path, tz="EST") {
	full = readLines(path)
        inds = grep("^####", full)
        stopifnot(length(inds)==2)
        lastm = inds[2]
	meta = readLines(path)[seq_len(lastm)]
	dat = read.delim(path, skip=lastm, check.names=FALSE, sep=" ")
	names(dat) = gsub("\\[(...)\\]", "\\1_", names(dat))
        dat = revise_date(dat, tz=tz)
    proc_path = sub("\\.tab\\.gz$", ".prc.gz", path)
    if (file.exists(proc_path))
        dat = merge(dat, cl_parse_process(proc_path, tz=tz),
            by="sampdate", all.x=TRUE, sort=FALSE)
	attr(dat, "meta") = meta
	dat
}

cl_parse_process <- function(path, tz = "EST") {
  lines <- readLines(path)
  h <- grep("^#Date Time", lines)[1]
  nms <- strsplit(trimws(lines[h]), "\\s+")[[1]]
  rec <- lines[(h + 1L):length(lines)]
  rec <- rec[nzchar(trimws(rec)) & !grepl("^#", rec)]

  dat <- as.data.frame(do.call(rbind, lapply(strsplit(trimws(rec), "\\s+"), \(x)
    c(x[seq_len(length(nms) - 1L)],
      paste(x[length(nms):length(x)], collapse = " "))
  )), stringsAsFactors = FALSE, check.names = FALSE)

  names(dat) <- paste0("PROC_", sub("^\\[PROC\\]", "", sub("^#", "", nms)))
  names(dat)[names(dat) == "PROC_PCT"] <- "PROC_Pct"
  names(dat)[names(dat) == "PROC_VmRSS"] <- "PROC_RSS"
  dat[] <- lapply(dat, type.convert, as.is = TRUE)

  dat$sampdate <- revise_date(dat[c("PROC_Date", "PROC_Time")], tz = tz)$sampdate
  metrics <- intersect(c("PROC_Pct", "PROC_RSS", "PROC_RKB", "PROC_WKB"), names(dat))

  dat[metrics] <- lapply(dat[metrics], \(x) {
    x <- as.character(x)
    s <- ifelse(grepl("[KMGT]$", x), sub(".*([KMGT])$", "\\1", x), "")
    as.numeric(sub("[KMGT]$", "", x)) *
      c("" = 1, K = 1024, M = 1024^2, G = 1024^3, T = 1024^4)[s]
  })

  aggregate(dat[metrics], list(sampdate = dat$sampdate), sum, na.rm = TRUE)
}

revise_date = function(x, tz) {
 c2 = paste(x[,1], x[,2])
 pred = gsub("(....)(..)(..)(.*)", "\\1-\\2-\\3\\4", c2)
 x$sampdate = lubridate::as_datetime(pred, tz=tz)
 x
}
