##
## bring device to top
##
##



#' Assign focus to an RGL window
#' 
#' 'rgl.bringtotop' brings the current RGL window to the front of the window
#' stack (and gives it focus).
#' 
#' If \code{stay} is \code{TRUE}, then the window will stay on top of normal
#' windows.
#' 
#' @param stay whether to make the window stay on top.
#' @note not completely implemented for X11 graphics (stay not implemented;
#' window managers such as KDE may block this action (set "Focus stealing
#' prevention level" to None in Control Center/Window Behavior/Advanced)). Not
#' currently implemented under OS/X.
#' @author Ming Chen/Duncan Murdoch
#' @keywords dynamic
#' @examples
#' 
#' rgl.open()
#' rgl.points(rnorm(1000), rnorm(1000), rnorm(1000), color = heat.colors(1000))
#' rgl.bringtotop(stay = TRUE)    
#' 
rgl.bringtotop <- function(stay = FALSE) {
  if ((.Platform$OS.type != "windows") && stay) warning("'stay' not implemented")
  ret <- .C(rgl_dev_bringtotop, success = FALSE, as.logical(stay))

  if (!ret$success) {
    stop("'rgl.bringtotop' failed")
  }
}
