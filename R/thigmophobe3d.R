#' Find the direction away from the closest point in a 3d projection.
#' 
#' Jim Lemon's \code{\link[plotrix]{thigmophobe}} function in the
#' \code{\link[plotrix:plotrix-package]{plotrix}} package computes good
#' directions for labels in a 2D plot.  This function does the same for a
#' particular projection in a 3D plot by projecting down to 2D and calling his
#' function.
#' 
#' Since \code{thigmophobe3d} projects using fixed \code{P} and \code{M}, it
#' will not necessarily choose good directions if the user rotates the display
#' or makes any other change to the projection.
#' 
#' @param x,y,z point coordinates.  Any reasonable way of defining the
#' coordinates is acceptable.  See the function
#' \code{\link[grDevices]{xyz.coords}} for details.
#' @param P,M,windowRect The projection and modelview matrices, and the size
#' and position of the display in pixels.
#' @return A vector of values from 1 to 4 to be used as the \code{pos} argument
#' in \code{\link{text3d}}.
#' @note The example below shows how to update the directions during an
#' animation; I find that the moving labels are distracting, and prefer to live
#' with fixed ones.
#' @author Duncan Murdoch
#' @seealso \code{\link{text3d}}
#' @references c("\\href{https://CRAN.R-project.org/package=#1}{\\pkg{#1}}",
#' "plotrix")\href{https://CRAN.R-project.org/package=plotrix\pkg{plotrix}}
#' @examples
#' 
#' if (requireNamespace("plotrix")) {
#'   # Simulate some data
#'   xyz <- matrix(rnorm(30), ncol = 3)
#'   
#'   # Plot the data first, to establish the projection
#'   plot3d(xyz)
#'   
#'   # Now thigmophobe3d can choose directions
#'   textid <- text3d(xyz, texts = 1:10, pos = thigmophobe3d(xyz))
#'   
#'   # Update the label positions during an animation
#'   if (interactive() && !rgl.useNULL()) {
#'     spin <- spin3d(rpm = 5)
#'     f <- function(time) {
#'       par3d(skipRedraw = TRUE)
#'       on.exit(par3d(skipRedraw = FALSE))
#'       rgl.pop(id = textid)
#'       # Need to rotate before thigmophobe3d is called
#'       result <- spin(time)
#'       par3d(userMatrix = result$userMatrix)
#'       textid <<- text3d(xyz, texts = 1:10, pos = thigmophobe3d(xyz))
#'       result
#'     }
#'     play3d(f, duration = 5)
#'   }
#' }
#' 
thigmophobe3d <- function(x, y = NULL, z = NULL,
                          P = par3d("projMatrix"),
                          M = par3d("modelMatrix"),
                          windowRect = par3d("windowRect")) {
  if (!requireNamespace("plotrix") || packageVersion("plotrix") < "3.7-3") {
    stop("This function requires the plotrix package, version 3.7-3 or higher.")
  }

  xyz <- xyz.coords(x, y, z)

  pts3d <- rbind(xyz$x, xyz$y, xyz$z, 1)
  pts2d <- asEuclidean(t(P %*% M %*% pts3d))
  w <- diff(windowRect[c(1, 3)])
  h <- diff(windowRect[c(2, 4)])
  pts2d <- cbind(w * pts2d[, 1], h * pts2d[, 2])

  if (packageVersion("plotrix") < "3.7.5") {
    plotrix::thigmophobe(pts2d,
      plot.span = c(-w, w, -h, h),
      xlog = FALSE, ylog = FALSE
    )
  } else if (packageVersion("plotrix") < "3.7.6") {
    plotrix::thigmophobe(pts2d,
      usr = c(-w, w, -h, h),
      xlog = FALSE, ylog = FALSE
    )
  } # This doesn't seem to matter...
  else {
    plotrix::thigmophobe(pts2d,
      usr = c(-w, w, -h, h),
      xlog = FALSE, ylog = FALSE,
      pin = c(w, h) / 50
    )
  } # This doesn't seem to matter...
}
