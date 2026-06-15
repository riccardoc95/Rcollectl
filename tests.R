#!/usr/bin/env Rscript

required <- c("ggplot2", "lubridate", "processx")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly=TRUE)]
if (length(missing))
  stop("Missing packages: ", paste(missing, collapse=", "))

suppressPackageStartupMessages(library(ggplot2))

for (file in list.files("R", pattern="\\.R$", full.names=TRUE))
  source(file)

plot_dir <- "test-plots"
dir.create(plot_dir, showWarnings=FALSE)

check <- function(ok, message) {
  if (!isTRUE(ok))
    stop(message)
  cat("[OK]", message, "\n")
}

save_plot <- function(plot, name) {
  path <- file.path(plot_dir, paste0(name, ".png"))
  ggsave(path, plot=plot, width=10, height=10, dpi=150)
  check(file.exists(path), paste(name, "plot is saved to", path))
}

cat("Testing existing parsing and plotting...\n")
demo <- "inst/demotab/demo_1123.tab.gz"
legacy <- cl_parse(demo)
check(nrow(legacy) > 0, "legacy output is parsed")
check(all(c("CPU_Idle%", "MEM_Used", "NET_RxKBTot", "DSK_WriteKBTot") %in%
  names(legacy)), "legacy machine columns are present")
legacy_plot <- plot_usage(legacy)
check(inherits(legacy_plot, "ggplot"), "legacy plot is created")
save_plot(legacy_plot, "legacy")

if (!cl_exists()) {
  cat("[SKIP] collectl is not installed; live tests were not run\n")
  quit(status=0)
}

run_live_test <- function(pid=NULL) {
  label <- if (is.null(pid)) "machine" else "machine and current PID"
  cat("Testing live collection:", label, "...\n")

  target <- tempfile(pattern="Rcollectl-test-")
  proc <- cl_start(target=target, pid=pid)
  on.exit({
    if (proc$process$is_alive())
      cl_stop(proc)
  }, add=TRUE)

  x <- rnorm(2e7)
  invisible(sum(x))
  Sys.sleep(3)
  cl_stop(proc)
  Sys.sleep(2)

  path <- cl_result_path(proc)
  check(file.exists(path), paste(label, "output file exists"))

  result <- cl_parse(path)
  check(nrow(result) > 0, paste(label, "output is parsed"))
  result_plot <- plot_usage(result)
  check(inherits(result_plot, "ggplot"), paste(label, "plot is created"))
  save_plot(result_plot, if (is.null(pid)) "live-machine" else "live-current-pid")

  if (!is.null(pid)) {
    check(file.exists(sub("\\.tab\\.gz$", ".prc.gz", path)),
      "process output file exists")
    check(any(grepl("^PROC_", names(result))), "process columns are present")
    check("PROC_Pct" %in% names(result), "process CPU column is present")
    check("PROC_RSS" %in% names(result), "process RSS column is present")
    check(any(grepl("process", vizdf(result)$type)),
      "process metrics are present in plot data")
  }

  unlink(c(path, sub("\\.tab\\.gz$", ".prc.gz", path)))
}

run_live_test()
run_live_test(pid="current")

cat("All tests completed successfully.\n")
