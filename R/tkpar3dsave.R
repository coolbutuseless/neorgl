#' Modal dialog for saving par3d settings
#' 
#' This function opens a TCL/TK modal dialog to allow particular views of an
#' \code{rgl} scene to be saved.
#' 
#' This opens a TCL/TK modal dialog box with \code{Record} and \code{Quit}
#' buttons.  Each time \code{Record} is clicked, a snapshot is taken of current
#' \code{\link[rgl]{par3d}} settings.  When \code{Quit} is clicked, the dialog
#' closes and the values are returned in a list.
#' 
#' If \code{times == TRUE}, then the times at which the views are recorded will
#' also be saved, so that the \code{\link[rgl]{play3d}} function will play back
#' with the same timing.
#' 
#' @aliases tkpar3dsave par3dsave
#' @param params Which parameters to save
#' @param times Should times be saved as well?
#' @param dev Which \code{rgl} device to work with
#' @param ...  Additional parameters to pass to
#' \code{\link[tcltk:TkWidgets]{tktoplevel}}
#' @return A list of the requested components.  Each one will consist of a list
#' of values that were current when the \code{Record} button was clicked.
#' These are suitable to be passed directly to the
#' \code{\link[rgl]{par3dinterp}} function.
#' @author Duncan Murdoch
#' @seealso \code{\link{par3d}}, \code{\link{par3dinterp}}
#' @examples
#' 
#' if (interactive()) {
#' 
#'   # Record a series of positions, and then play them back immediately
#'   # at evenly spaced times, in an oscillating loop
#'   example(plot3d)
#'   play3d( par3dinterp( tkpar3dsave() ) )
#' 
#'   # As above, but preserve the click timings
#' 
#'   # play3d( par3dinterp( tkpar3dsave(times=TRUE) ) )
#' }
#' 
tkpar3dsave <- function(params = c("userMatrix", "scale", "zoom", "FOV"),
                        times = FALSE,
                        dev = rgl.cur(),
                        ...) {
  if (!requireNamespace("tcltk")) {
    stop("This function requires 'tcltk'")
  }

  results <- list()
  for (n in params) results[[n]] <- list()
  if (times) {
    start <- proc.time()[3]
    results$times <- numeric(0)
  }

  RecordParms <- function() {
    values <- par3d(params)
    if (length(params) == 1) {
      values <- list(values)
      names(values) <- params
    }
    for (n in params) results[[n]] <<- c(results[[n]], list(values[[n]]))
    if (times) results$times <<- c(results$times, proc.time()[3] - start)
  }
  base <- tcltk::tktoplevel(...)
  tcltk::tkwm.title(base, "par3d")

  text <- tcltk::tklabel(base,
    text = "Click on Record to save par3d parameters.",
    justify = "left",
    wraplength = "2i"
  )
  frame <- tcltk::tkframe(base)
  save <- tcltk::tkbutton(frame, text = "Record", command = RecordParms)

  quit <- tcltk::tkbutton(frame, text = "Quit", command = function() tcltk::tkdestroy(base))

  tcltk::tkpack(save, quit, side = "left")
  tcltk::tkpack(text, frame)

  tcltk::tkwait.window(base)

  results
}
