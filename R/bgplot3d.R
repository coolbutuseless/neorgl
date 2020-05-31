legend3d <- function(...) {
  args <- list(...)
  idx <- which(names(args) %in% c("sphere", "fogtype", "bg.color"))
  if (length(idx)) {
    bgargs <- args[idx]
    args <- args[-idx]
  } else {
    bgargs <- NULL
  }
  do.call(bgplot3d, c(list(quote({
    par(mar = c(0, 0, 0, 0))
    plot(0, 0, type = "n", xlim = 0:1, ylim = 0:1, xaxs = "i", yaxs = "i", axes = FALSE, bty = "n")
    do.call(legend, args)
  })), bgargs))
}



#' Use base graphics for RGL background
#' 
#' Add a 2D plot or a legend in the background of an RGL window.
#' 
#' The \code{bgplot3d} function opens a \code{\link{png}} device and executes
#' \code{expression}, producing a plot there.  This plot is then used as a
#' bitmap background for the current RGL subscene.
#' 
#' The \code{legend3d} function draws a standard 2D legend to the background of
#' the current subscene by calling \code{bgplot3d} to open a device, and
#' setting up a plot region there to fill the whole display.
#' 
#' @aliases bgplot3d legend3d
#' @param expression Any plotting commands to produce a plot.
#' @param bg.color The color to use for the background.
#' @param ...  Arguments to pass to the \code{\link{bg3d}} or
#' \code{\link{legend}} function.
#' @return The \code{bgplot3d} function invisibly returns the ID of the
#' background object that was created, with attribute \code{"value"} holding
#' the value returned when the \code{expression} was evaluated.
#' 
#' The \code{legend3d} function does similarly.  The \code{"value"} attribute
#' is the result of the call to \code{\link{legend}}.  The scaling of the
#' coordinates runs from 0 to 1 in X and Y.
#' @note Because the background plots are drawn as bitmaps, they do not resize
#' very gracefully. It's best to size your window first, then draw the
#' background at that size.
#' @author Duncan Murdoch
#' @seealso \code{\link{bg3d}} for other background options.
#' @keywords graphics
#' @examples
#' 
#' x <- rnorm(100)
#' y <- rnorm(100)
#' z <- rnorm(100)
#' open3d()
#' # Needs to be a bigger window than the default
#' par3d(windowRect = c(100, 100, 612, 612))
#' parent <- currentSubscene3d()
#' mfrow3d(2, 2)
#' plot3d(x, y, z)
#' next3d(reuse = FALSE)
#' bgplot3d(plot(y, z))
#' next3d(reuse = FALSE)
#' bgplot3d(plot(x, z))
#' next3d(reuse = FALSE)
#' legend3d("center", c("2D Points", "3D Points"), pch = c(1, 16))
#' useSubscene3d(parent)
#' 
bgplot3d <- function(expression, bg.color = getr3dDefaults()$bg$color,
                     ...) {
  viewport <- par3d("viewport")
  width <- viewport["width"]
  height <- viewport["height"]
  if (width > 0 && height > 0) {
    filename <- tempfile(fileext = ".png")
    png(
      filename = filename, width = width, height = height,
      bg = bg.color
    )
    value <- try(expression)
    dev.off()
    result <- bg3d(texture = filename, col = "white", lit = FALSE, ...)
  } else {
    value <- NULL
    result <- bg3d(col = bg.color, ...)
  }
  lowlevel(structure(result, value = value))
}



#' Draw a 2D plot on a rectangle in a 3D scene.
#' 
#' This function uses a bitmap of a standard 2D graphics plot as a texture on a
#' quadrilateral.  Default arguments are set up so that it will appear on the
#' face of the bounding box of the current 3D plot, but optional arguments
#' allow it to be placed anywhere in the scene.
#' 
#' The default arguments are chosen to make it easy to place a 2D image on the
#' face of the bounding box.  If \code{x}, \code{y} and \code{z} are
#' \code{NULL} (the defaults), \code{face} will be used as a code for one of
#' the six faces of the bounding box.  The first letter should be \code{"x"},
#' \code{"y"} or \code{"z"}; this defines the axis perpendicular to the desired
#' face.  If the second letter is \code{"-"} or is missing, the face will be
#' chosen to be the face with the lower value on that axis.  Any other letter
#' will use the opposite face.
#' 
#' If any of \code{x}, \code{y} or \code{z} is given, the specified value will
#' be used to replace the value calculated above.  Usually four values should
#' be given, corresponding to the coordinates of the lower left, lower right,
#' upper right and upper left of the destination for the image before
#' \code{reverse} and \code{rotate} are used.  Fewer values can be used for one
#' or two coordinates; \code{\link{cbind}} will be used to put together all 3
#' coordinates into a 4 by 3 matrix (which will be returned as an attribute of
#' the result).
#' 
#' The bitmap plot will by default be oriented so that it is properly oriented
#' when viewed from the direction of the higher values of the perpendicular
#' coordinate, and its lower left corner is at the lower value of the two
#' remaining coordinates.  The argument \code{reverse} causes the orientation
#' to be mirrored, and \code{rotate} causes it to be rotated by multiples of 90
#' degrees.  \code{rotate} should be an integer, with \code{0} for no rotation,
#' \code{1} for a 90 degree counter-clockwise rotation, etc.
#' 
#' The \code{width} and \code{height} arguments control the shape and
#' resolution of the bitmap.  The defaults give a square bitmap, which is
#' appropriate with the usual \code{c(1,1,1)} aspect ratios (see
#' \code{aspect3d}).  Some tuning may be needed to choose the resoluttion.  The
#' plot will look best when displayed at its original size; shrinking it
#' smaller tends to make it look faded, while expanding it bigger will make it
#' look blurry.  If \code{filename} is given, the width and height will be
#' taken from the file, and \code{width} and \code{height} arguments will be
#' ignored.
#' 
#' @param expression Any plotting commands to produce a plot in standard
#' graphics. Ignored if \code{filename} is not \code{NULL}.
#' @param face A character string defining which face of the bounding box to
#' use.  See Details below.
#' @param line How far out from the bounding box should the quadrilateral be
#' placed?  Uses same convention as \code{\link{mtext3d}}: not lines of text,
#' but fraction of the bounding box size.
#' @param reverse,rotate Should the image be reversed or rotated?  See Details
#' below.
#' @param x,y,z Specific values to use to override \code{face}.
#' @param width,height Parameters to pass to \code{\link{png}} when creating
#' the bitmap.  See Details below.
#' @param filename A \file{.png} file image to use as the texture.
#' @param ignoreExtent Whether the quadrilateral should be ignored when
#' computing the bounding box of the scene.
#' @param color,specular,lit,texmipmap,texminfilter, Material properties to use
#' for the quadrilateral.
#' @param list() Material properties to use for the quadrilateral.
#' @param expand Amount by which the quadrilateral is expanded outside the
#' bounding box of the data.
#' @param texcoords Coordinates on the image.  Lower left of the bitmap is
#' \code{c(0,0)}, upper right is \code{c(1,1)}.
#' @return Invisibly returns the id value of the quadrilateral, with the
#' following attributes: \item{value}{The value returned by \code{expression}.}
#' \item{xyz}{A 4 by 3 matrix giving the coordinates of the corners as used in
#' plotting.} \item{texcoords}{A 4 by 2 matrix giving the texture coordinates
#' of the image.} \item{filename}{The filename for the temporary file holding
#' the bitmap image.}
#' @author Duncan Murdoch
#' @seealso \code{\link{bgplot3d}} uses a plot as the background for the
#' window.
#' @examples
#' 
#' example(plot3d, ask = FALSE)
#' show2d({
#'   par(mar=c(0,0,0,0))
#'   plot(x, y, col = rainbow(1000), axes=FALSE)
#'   })
#' 
show2d <- function(expression,
                   face = "z-",
                   line = 0,
                   reverse = FALSE,
                   rotate = 0,
                   x = NULL, y = NULL, z = NULL,
                   width = 480,
                   height = 480,
                   filename = NULL,
                   ignoreExtent = TRUE,
                   color = "white", specular = "black",
                   lit = FALSE,
                   texmipmap = TRUE,
                   texminfilter = "linear.mipmap.linear",
                   expand = 1.03,
                   texcoords = matrix(c(0, 1, 1, 0, 0, 0, 1, 1), ncol = 2),
                   ...) {
  save <- par3d(ignoreExtent = ignoreExtent)
  on.exit(par3d(save))

  if (is.null(filename)) {
    stopifnot(width > 0, height > 0)
    filename <- tempfile(fileext = ".png")
    png(filename = filename, width = width, height = height)
    value <- try(expression)
    dev.off()
  } else {
    value <- filename
  }
  face <- c(strsplit(face, "")[[1]], "-")[1:2]
  coord <- tolower(face[1])
  lower <- face[2] == "-"

  calc <- c(x = is.null(x), y = is.null(y), z = is.null(z))

  ranges <- .getRanges(expand = expand)
  switch(coord,
    x = {
      if (is.null(x)) {
        x <- with(ranges, if (lower) {
          x[1] - 0.075 * line * diff(x)
        } else {
          x[2] + 0.075 * line * diff(x)
        })
      }
      if (is.null(y)) {
        y <- with(ranges, c(y[1], y[2], y[2], y[1]))
      }
      if (is.null(z)) {
        z <- with(ranges, c(z[1], z[1], z[2], z[2]))
      }
    },
    y = {
      if (is.null(x)) {
        x <- with(ranges, c(x[1], x[2], x[2], x[1]))
      }
      if (is.null(y)) {
        y <- with(ranges, if (lower) {
          y[1] - 0.075 * line * diff(y)
        } else {
          y[2] + 0.075 * line * diff(y)
        })
      }
      if (is.null(z)) {
        z <- with(ranges, c(z[1], z[1], z[2], z[2]))
      }
    },
    z = {
      if (is.null(x)) {
        x <- with(ranges, c(x[1], x[2], x[2], x[1]))
      }
      if (is.null(y)) {
        y <- with(ranges, c(y[1], y[1], y[2], y[2]))
      }
      if (is.null(z)) {
        z <- with(ranges, if (lower) {
          z[1] - 0.075 * line * diff(z)
        } else {
          z[2] + 0.075 * line * diff(z)
        })
      }
    }
  )
  x <- cbind(x, y, z)
  if (nrow(x) != 4) {
    stop("Exactly 4 corners must be specified.")
  }

  if (reverse) {
    temp <- x[2, ]
    x[2, ] <- x[4, ]
    x[4, ] <- temp
  }
  if (rotate) {
    x <- x[(0:3 + rotate) %% 4 + 1, ]
  }

  result <- quads3d(x,
    texture = filename, texcoords = texcoords,
    color = color, lit = lit, texmipmap = texmipmap,
    texminfilter = texminfilter, ...
  )
  lowlevel(structure(result,
    value = value, xyz = x, texcoords = texcoords,
    filename = filename
  ))
}
