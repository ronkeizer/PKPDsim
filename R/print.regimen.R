#' Print function for PKPDsim regimen
#'
#' @param x regimen
#' @param ... arguments to pass
#' @export
print.regimen <- function(x, ...) {
  tmp <- data.frame(cbind(t = x$dose_times, type = x$type, amt = x$amt, cmt = x$cmt))
  if(!is.null(x$per_unit)) {
    if(!all(x$per_unit == 1)) {
      tmp$per_unit <- x$per_unit
    }
  }
  if(!is.null(x$t_inf)) {
    tmp$t_inf = x$t_inf
  }
  if(!is.null(x$ss_regimen)) {
    cat("Note: Steady state regimen, steady state assumed to be reached at t = 0.\n")
  }
  print(tmp)
}
