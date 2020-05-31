#' Add a grid to a 3D plot
#' 
#' This function adds a reference grid to an RGL plot.
#' 
#' This function is similar to \code{\link{grid}} in classic graphics, except
#' that it draws a 3D grid in the plot.
#' 
#' The grid is drawn in a plane perpendicular to the coordinate axes. The first
#' letter of the \code{side} argument specifies the direction of the plane:
#' \code{"x"}, \code{"y"} or \code{"z"} (or uppercase versions) to specify the
#' coordinate which is constant on the plane.
#' 
#' If \code{at = NULL} (the default), the grid is drawn at the limit of the box
#' around the data.  If the second letter of the \code{side} argument is
#' \code{"-"} or is not present, it is the lower limit; if \code{"+"} then at
#' the upper limit.  The grid lines are drawn at values chosen by
#' \code{\link{pretty}} with \code{n} suggested locations. The default
#' locations should match those chosen by \code{\link{axis3d}} with
#' \code{nticks = n}.
#' 
#' If \code{at} is a numeric vector, the grid lines are drawn at those values.
#' 
#' If \code{at} is a list, then the \code{"x"} component is used to specify the
#' x location, the \code{"y"} component specifies the y location, and the
#' \code{"z"} component specifies the z location.  Missing components are
#' handled using the default as for \code{at = NULL}.
#' 
#' Multiple grids may be drawn by specifying multiple values for \code{side} or
#' for the component of \code{at} that specifies the grid location.
#' 
#' @param side Where to put the grid; see the Details section.
#' @param at How to draw the grid; see the Details section.
#' @param col The color of the grid lines.
#' @param lwd The line width of the grid lines. (Currently only \code{lty = 1}
#' is supported.)
#' @param lty The line type of the grid lines.
#' @param n Suggested number of grid lines; see the Details section.
#' @return A vector or matrix of object ids is returned invisibly.
#' @note If the scene is resized, the grid will not be resized; use
#' \code{\link{abclines3d}} to draw grid lines that will automatically resize.
#' @author Ben Bolker and Duncan Murdoch
#' @seealso \code{\link{axis3d}}
#' @keywords dynamic
#' @examples
#' 
#' x <- 1:10
#' y <- 1:10
#' z <- matrix(outer(x - 5, y - 5) + rnorm(100), 10, 10)
#' open3d()
#' persp3d(x, y, z, col = "red", alpha = 0.7, aspect = c(1, 1, 0.5))
#' grid3d(c("x", "y+", "z"))
#' 
grid3d <- function(side, at = NULL, col = "gray",
                   lwd = 1, lty = 1, n = 5) {
  save <- par3d(skipRedraw = TRUE, ignoreExtent = TRUE)
  on.exit(par3d(save))

  if (!missing(side) && length(side) > 1) {
    return(lowlevel(sapply(side, grid3d, at = at, col = col, lwd = lwd, lty = lty, n = n)))
  }

  ranges <- .getRanges()

  side <- c(strsplit(side, "")[[1]], "-")[1:2]
  coord <- match(toupper(side[1]), c("X", "Y", "Z"))
  spos <- match(side[2], c("-", "+"))

  sidenames <- c("x", "y", "z")

  sides <- 1:3
  sides <- sides[-coord]

  if (is.null(at)) {
    at <- list()
  } else if (is.numeric(at)) {
    at <- list(x = at, y = at, z = at)
    at[[coord]] <- NULL
  }

  result <- integer()

  for (cside in sides) {
    range <- ranges[[cside]]
    if (is.null(at1 <- at[[sidenames[cside]]])) {
      at1 <- pretty(range, n)
    }
    at1 <- at1[at1 >= range[1] & at1 <= range[2]]
    mpos1 <- matrix(rep(c(ranges$x[1], ranges$y[1], ranges$z[1]),
      each = length(at1)
    ),
    ncol = 3
    )
    mpos2 <- matrix(rep(c(ranges$x[2], ranges$y[2], ranges$z[2]),
      each = length(at1)
    ),
    ncol = 3
    )
    mpos1[, cside] <- mpos2[, cside] <- at1
    if (is.null(at[[sidenames[coord]]])) {
      mpos1[, coord] <- mpos2[, coord] <- ranges[c("x", "y", "z")][[coord]][spos]
    } else {
      # may need to duplicate
      temp1 <- temp2 <- matrix(NA, nrow = 0, ncol = 3)
      planes <- at[[sidenames[coord]]]
      planes <- planes[planes >= ranges[[coord]][1] & planes <= ranges[[coord]][2]]
      for (at2 in planes) {
        mpos1[, coord] <- mpos2[, coord] <- at2
        temp1 <- rbind(temp1, mpos1)
        temp2 <- rbind(temp2, mpos2)
      }
      mpos1 <- temp1
      mpos2 <- temp2
    }
    if (nrow(mpos1) + nrow(mpos2) > 0) {
      result[sidenames[cside]] <- segments3d(
        x = c(rbind(mpos1[, 1], mpos2[, 1])),
        y = c(rbind(mpos1[, 2], mpos2[, 2])),
        z = c(rbind(mpos1[, 3], mpos2[, 3])),
        lwd = lwd, color = col
      )
    }
  }
  lowlevel(result)
}
