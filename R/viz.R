
vizdf = function(x, tz="EST") {
    cpu_active = function(x)
    data.frame(tm = as.POSIXct(x$sampdate, tz=tz), xtype="CPU_MEM",
        pos="top", value = 100-x$`CPU_Idle%`, type="%CPU active")
    mem_used = function(x)
    data.frame(tm = as.POSIXct(x$sampdate, tz=tz), xtype="CPU_MEM",
        pos="bot", value = x$MEM_Used, type="MEM used")
    net_KB_cum = function(x)
    data.frame(tm = as.POSIXct(x$sampdate, tz=tz), xtype="NET_DSK",
        pos="top", value = (x$NET_RxKBTot+x$NET_TxKBTot), type="KB NET")
    dsk_KBwr_cum = function(x)
    data.frame(tm = as.POSIXct(x$sampdate, tz=tz), xtype="NET_DSK",
        pos="bot", value = cumsum(x$DSK_WriteKBTot), type="Cumul KB disk")

   ans = rbind(cpu_active(x), mem_used(x), net_KB_cum(x), dsk_KBwr_cum(x))
   if ("PROC_Pct" %in% names(x))
     ans = rbind(ans, data.frame(tm=x$sampdate, xtype="PROCESS", pos="top",
       value=x$PROC_Pct, type="%CPU process"))
   if ("PROC_RSS" %in% names(x))
     ans = rbind(ans, data.frame(tm=x$sampdate, xtype="PROCESS", pos="bot",
       value=x$PROC_RSS, type="MEM process RSS"))
   if ("PROC_RKB" %in% names(x))
     ans = rbind(ans, data.frame(tm=x$sampdate, xtype="PROCESS", pos="top",
       value=cumsum(replace(x$PROC_RKB, is.na(x$PROC_RKB), 0)),
       type="Cumul KB process read"))
   if ("PROC_WKB" %in% names(x))
     ans = rbind(ans, data.frame(tm=x$sampdate, xtype="PROCESS", pos="bot",
       value=cumsum(replace(x$PROC_WKB, is.na(x$PROC_WKB), 0)),
       type="Cumul KB process write"))
   ans
}

#' elementary display of usage data from collectl
#' @import ggplot2
#' @param x output of cl_parse
#' @return ggplot with geom_point and facet_grid
#' @examples
#' lk = cl_parse(system.file("demotab/demo_1123.tab.gz", package="Rcollectl"))
#' plot_usage(lk)
#' @export
plot_usage = function(x) {
  ggplot(vizdf(x), aes(x=tm, y=value)) + geom_point() +
          facet_grid(vars(type), scales="free")
}
