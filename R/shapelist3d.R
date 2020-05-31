#' Create and plot a list of shapes
#' 
#' These functions create and plot a list of shapes.
#' 
#' \code{shapelist3d} is a quick way to create a complex object made up of
#' simpler ones. Each of the arguments \code{shapes} through \code{override}
#' may be a vector of values (a list in the case of \code{shapes} or
#' \code{matrix}).  All values will be recycled to produce a list of shapes as
#' long as the longest of them.
#' 
#' The \code{\link{xyz.coords}} function will be used to process the \code{x},
#' \code{y} and \code{z} arguments, so a matrix may be used as \code{x} to
#' specify all three.  If a vector is used for \code{x} but \code{y} or
#' \code{z} is missing, default values of \code{0} will be used.
#' 
#' The \code{"shapelist3d"} class is simply a list of \code{"shape3d"} objects.
#' 
#' Methods for \code{\link{dot3d}}, \code{\link{wire3d}},
#' \code{\link{shade3d}}, \code{\link{translate3d}}, \code{\link{scale3d}}, and
#' \code{\link{rotate3d}} are defined for these objects.
#' 
#' @param shapes A single \code{shape3d} object, or a list of them.
#' @param x,y,z Translation(s) to apply
#' @param size Scaling(s) to apply
#' @param matrix A single matrix transformation, or a list of them.
#' @param override Whether the material properties should override the ones in
#' the shapes.
#' @param \dots Material properties to apply.
#' @param plot Whether to plot the result.
#' @return An object of class \code{c("shapelist3d", "shape3d")}.
#' @author Duncan Murdoch
#' @seealso \code{\link{mesh3d}}
#' @keywords dynamic
#' @examples
#' 
#'  
#' shapelist3d(icosahedron3d(), x = rnorm(10), y = rnorm(10), z = rnorm(10), col = 1:5, size = 0.3)
#' 
#' 
shapelist3d <- function(shapes, x = 0, y = NULL, z = NULL, size = 1, matrix = NULL, override = TRUE, ..., plot = TRUE) {
  # This function gets an element with recycling
  e <- function(x, i) x[[(i - 1) %% length(x) + 1]]

  xyz <- xyz.coords(x, y, z, recycle = TRUE)
  x <- xyz$x
  y <- xyz$y
  if (length(y) == 0) y <- 0
  z <- xyz$z
  if (length(z) == 0) z <- 0

  if (inherits(shapes, "shape3d")) shapes <- list(shapes)

  material <- list(...)

  if (!is.null(matrix)) {
    if (!is.list(matrix)) matrix <- list(matrix)
    len <- length(matrix)
  } else {
    len <- 0
  }

  len <- max(len, length(x), length(shapes), length(size), length(override))
  if (length(material)) len <- max(len, sapply(material, length))

  result <- vector("list", len)
  class(result) <- c("shapelist3d", "shape3d")

  for (i in seq_len(len)) {
    if (is.null(matrix)) {
      this <- e(shapes, i)
    } else {
      this <- rotate3d(e(shapes, i), matrix = e(matrix, i))
    }
    thissize <- e(size, i)
    this <- translate3d(scale3d(this, thissize, thissize, thissize), e(x, i), e(y, i), e(z, i))
    thismaterial <- lapply(material, function(item) e(item, i))
    if (!e(override, i)) {
      thismaterial[names(this$material)] <- this$material
    }
    this$material[names(thismaterial)] <- thismaterial
    result[[i]] <- this
  }
  if (plot) {
    shade3d(result)
    lowlevel(result)
  } else {
    invisible(result)
  }
}

dot3d.shapelist3d <- function(x, override = TRUE, ...) {
  .check3d()
  save <- par3d(skipRedraw = TRUE)
  on.exit(par3d(save))

  invisible(unlist(sapply(x, function(item) dot3d(item, override = override, ...))))
}

wire3d.shapelist3d <- function(x, override = TRUE, ...) {
  .check3d()
  save <- par3d(skipRedraw = TRUE)
  on.exit(par3d(save))

  invisible(unlist(sapply(x, function(item) wire3d(item, override = override, ...))))
}


shade3d.shapelist3d <- function(x, override = TRUE, ...) {
  .check3d()
  save <- par3d(skipRedraw = TRUE)
  on.exit(par3d(save))

  invisible(unlist(sapply(x, function(item) shade3d(item, override = override, ...))))
}

translate3d.shapelist3d <- function(obj, x, y, z, ...) {
  structure(lapply(obj, function(item) translate3d(item, x, y, z, ...)),
    class = class(obj)
  )
}

rotate3d.shapelist3d <- function(obj, angle, x, y, z, matrix, ...) {
  structure(lapply(obj, function(item) rotate3d(item, x, y, z, matrix, ...)),
    class = class(obj)
  )
}

scale3d.shapelist3d <- function(obj, x, y, z, ...) {
  structure(lapply(obj, function(item) scale3d(item, x, y, z, ...)),
    class = class(obj)
  )
}

addNormals.shapelist3d <- function(x, ...) {
  structure(lapply(x, function(item) addNormals(item, ...)),
    class = class(x)
  )
}

print.shapelist3d <- function(x, prefix = "", ...) {
  cat(prefix, " shapelist3d object with ", length(x), " items:\n", sep = "")
  for (i in seq_along(x)) {
    print(x[[i]], prefix = paste0(prefix, "[[", i, "]]"))
  }
}
