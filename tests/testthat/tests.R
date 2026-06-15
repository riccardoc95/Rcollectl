
library(Rcollectl)

test_that("cl_parse succeeds", {
 lk = cl_parse(system.file("demotab/demo_1123.tab.gz", package="Rcollectl"))
 expect_true(all(dim(lk)==c(478,71)))
 expect_true(length(grep("CPU", names(lk)))==21)
})

test_that("plot_usage succeeds", {
 lk = cl_parse(system.file("demotab/demo_1123.tab.gz", package="Rcollectl"))
 x = plot_usage(lk)
 expect_true("ggplot" %in% class(x))
 expect_true("FacetGrid" %in% class(x$facet))
})

test_that("cl_start/stop succeed", {
 if (cl_exists()) {
  lk = cl_start()
  expect_true(lk$process$is_alive())
  cl_stop(lk)
  Sys.sleep(2)
  expect_false(lk$process$is_alive())
  }
})

test_that("it works", {
 if (cl_exists()) {
  lk = cl_start()
  x = rnorm(2e7)
  Sys.sleep(5)
  x = rnorm(2e7)
  cl_stop(lk)
  targ = cl_result_path(lk)
  expect_true(file.exists(targ))
  p = cl_parse(targ)
  expect_true(nrow(p)>=5)
  }
})

test_that("cl_parse_process parses and aggregates PID metrics", {
    path <- tempfile(fileext = ".prc.gz")

    writeLines(c(
      "#Date Time [PROC]PID [PROC]PCT [PROC]VmRSS [PROC]RKB [PROC]WKB [PROC]Command",
      "20260101 12:00:00 100 1.5 2K 3M 4K R worker",
      "20260101 12:00:00 101 2.5 1K 1M 2K child process"
    ), gzfile(path))

    result <- Rcollectl:::cl_parse_process(path, tz = "UTC")

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 1)
    expect_named(
      result,
      c("sampdate", "PROC_Pct", "PROC_RSS", "PROC_RKB", "PROC_WKB")
    )

    expect_equal(result$PROC_Pct, 4)
    expect_equal(result$PROC_RSS, 3 * 1024)
    expect_equal(result$PROC_RKB, 4 * 1024^2)
    expect_equal(result$PROC_WKB, 6 * 1024)
  })

test_that("cl_start monitors the current PID", {
    skip_if_not(cl_exists(), "collectl is not installed")

    target <- tempfile("Rcollectl-pid-")
    proc <- cl_start(target = target, pid = "current")

    on.exit({
      if (!inherits(proc$process, "try-error") && proc$process$is_alive())
        cl_stop(proc)
    })

    expect_equal(proc$pid, Sys.getpid())
    expect_true(proc$process$is_alive())

    x <- rnorm(2e7)
    Sys.sleep(3)

    cl_stop(proc)
    Sys.sleep(2)

    tab_path <- cl_result_path(proc)
    prc_path <- sub("\\.tab\\.gz$", ".prc.gz", tab_path)

    expect_true(file.exists(tab_path))
    expect_true(file.exists(prc_path))

    result <- cl_parse(tab_path)

    expect_true("PROC_Pct" %in% names(result))
    expect_true("PROC_RSS" %in% names(result))
  })
