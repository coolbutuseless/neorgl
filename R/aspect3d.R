#' Set the aspect ratios of the current plot
#' 
#' This function sets the apparent ratios of the x, y, and z axes of the
#' current bounding box.
#' 
#' If the ratios are all 1, the bounding box will be displayed as a cube
#' approximately filling the display. Values may be set larger or smaller as
#' desired.  Aspect \code{"iso"} signifies that the coordinates should all be
#' displayed at the same scale, i.e. the bounding box should not be rescaled.
#' (This corresponds to the default display before \code{aspect3d} has been
#' called.) Partial matches to \code{"iso"} are allowed.
#' 
#' \code{aspect3d} works by modifying \code{par3d("scale")}.
#' 
#' @param x The ratio for the x axis, or all three ratios, or \code{"iso"}
#' @param y The ratio for the y axis
#' @param z The ratio for the z axis
#' @return The previous value of the scale is returned invisibly.
#' @author Duncan Murdoch
#' @seealso \code{\link{plot3d}}, \code{\link{par3d}}
#' @keywords dynamic
#' @examples
#' 
#'   x <- rnorm(100)
#'   y <- rnorm(100)*2
#'   z <- rnorm(100)*3
#'   
#'   open3d()
#'   plot3d(x, y, z)
#'   aspect3d(1, 1, 0.5)
#'   highlevel()  # To trigger display
#'   open3d()
#'   plot3d(x, y, z)
#'   aspect3d("iso")
#'   highlevel()
#' 
aspect3d <- function(x, y = NULL, z = NULL) {
  if (is.character(x) && pmatch(x, "iso") == 1) {
    scale <- c(1, 1, 1)
  } else {
    if (is.null(y)) {
      x <- rep(x, len = 3)
      z <- x[3]
      y <- x[2]
      x <- x[1]
    }
    for (i in 1:5) { # To handle spheres, repeat this
      bbox <- .getRanges()
      scale <- c(diff(bbox$xlim), diff(bbox$ylim), diff(bbox$zlim))
      scale <- ifelse(scale <= 0, 1, scale)

      avgscale <- sqrt(sum(scale^2) / 3)
      scale <- c(x, y, z) * avgscale / scale
      oldscale <- par3d(scale = scale)$scale
      if (isTRUE(all.equal(scale, oldscale))) {
        break
      }
    }
  }
  par3d(scale = scale)
}
