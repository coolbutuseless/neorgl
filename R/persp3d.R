#' Surface plots
#' 
#' This function draws plots of surfaces in 3-space. \code{persp3d} is a
#' generic function.
#' 
#' The default method plots a surface defined as a grid of \code{(x,y,z)}
#' locations in space.  The grid may be specified in several ways: \itemize{
#' \itemAs with \code{\link[graphics]{persp}}, \code{x} and \code{y} may be
#' given as vectors in ascending order, with \code{z} given as a matrix. There
#' should be one \code{x} value for each row of \code{z} and one \code{y} value
#' for each column.  The surface drawn will have \code{x} constant across rows
#' and \code{y} constant across columns.  This is the most convenient format
#' when \code{z} is a function of \code{x} and \code{y} which are measured on a
#' regular grid.
#' 
#' \item\code{x} and \code{y} may also be given as matrices, in which case they
#' should have the same dimensions as \code{z}.  The surface will combine
#' corresponding points in each matrix into locations \code{(x,y,z)} and draw
#' the surface through those.  This allows general surfaces to be drawn, as in
#' the example of a spherical Earth shown below.
#' 
#' \itemIf \code{x} is a \code{list}, its components \code{x$x}, \code{x$y} and
#' \code{x$z} are used for \code{x}, \code{y} and \code{z} respectively, though
#' an explicitly specified \code{z} value will have priority.}
#' 
#' One difference from \code{\link[graphics]{persp}} is that colors are
#' specified on each vertex, rather than on each facet of the surface.  To
#' emulate the \code{\link[graphics]{persp}} color handling, you need to do the
#' following.  First, convert the color vector to an \code{(nx - 1)} by
#' \code{(ny - 1)} matrix; then add an extra row before row 1, and an extra
#' column after the last column, to convert it to \code{nx} by \code{ny}.
#' (These extra colors will not be used).  For example, \code{col <- rbind(1,
#' cbind(matrix(col, nx - 1, ny - 1), 1))}.  Finally, call \code{persp3d} with
#' material property \code{smooth = FALSE}.
#' 
#' See the \dQuote{Clipping} section in \code{\link{plot3d}} for more details
#' on \code{xlim, ylim, zlim} and \code{forceClipregion}.
#' 
#' @aliases persp3d persp3d.default
#' @param x,y,z points to plot on surface.  See Details below.
#' @param xlim,ylim,zlim x-, y- and z-limits.  If present, the plot is clipped
#' to this region.
#' @param xlab,ylab,zlab titles for the axes.  N.B. These must be character
#' strings; expressions are not accepted.  Numbers will be coerced to character
#' strings.
#' @param add whether to add the points to an existing plot.
#' @param aspect either a logical indicating whether to adjust the aspect
#' ratio, or a new ratio.
#' @param forceClipregion force a clipping region to be used, whether or not
#' limits are given.
#' @param \dots additional material parameters to be passed to
#' \code{\link{surface3d}} and \code{\link{decorate3d}}.
#' @return This function is called for the side effect of drawing the plot.  A
#' vector of shape IDs is returned invisibly.
#' @author Duncan Murdoch
#' @seealso \code{\link{plot3d}}, \code{\link{persp}}. There is a
#' \code{\link{persp3d.function}} method for drawing functions, and
#' \code{\link{persp3d.deldir}} can be used to draw surfaces defined by an
#' irregular collection of points.  A formula method
#' \code{\link{persp3d.formula}} draws surfaces using this method.
#' 
#' The \code{\link{surface3d}} function is used to draw the surface without the
#' axes etc.
#' @keywords dynamic graphics
#' @examples
#' 
#' 
#' # (1) The Obligatory Mathematical surface.
#' #     Rotated sinc function.
#' 
#' x <- seq(-10, 10, length = 30)
#' y <- x
#' f <- function(x, y) { r <- sqrt(x^2 + y^2); 10 * sin(r)/r }
#' z <- outer(x, y, f)
#' z[is.na(z)] <- 1
#' open3d()
#' bg3d("white")
#' material3d(col = "black")
#' 
#' # Draw the surface twice:  the first draws the solid part, 
#' # the second draws the grid.  Offset the first so it doesn't
#' # obscure the first.
#' 
#' persp3d(x, y, z, aspect = c(1, 1, 0.5), col = "lightblue",
#'         xlab = "X", ylab = "Y", zlab = "Sinc( r )", 
#'         polygon_offset = 1)
#' persp3d(x, y, z, front = "lines", back = "lines", 
#'         lit = FALSE, add = TRUE)
#' 
#' # (2) Add to existing persp plot:
#' 
#' xE <- c(-10, 10); xy <- expand.grid(xE, xE)
#' points3d(xy[, 1], xy[, 2], 6, col = "red")
#' lines3d(x, y = 10, z = 6 + sin(x), col = "green")
#' 
#' phi <- seq(0, 2*pi, len = 201)
#' r1 <- 7.725 # radius of 2nd maximum
#' xr <- r1 * cos(phi)
#' yr <- r1 * sin(phi)
#' lines3d(xr, yr, f(xr, yr), col = "pink", lwd = 2)
#' 
#' # (3) Visualizing a simple DEM model
#' 
#' z <- 2 * volcano        # Exaggerate the relief
#' x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
#' y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)
#' 
#' open3d()
#' bg3d("slategray")
#' material3d(col = "black")
#' persp3d(x, y, z, col = "green3", aspect = "iso",
#'       axes = FALSE, box = FALSE)
#' 
#' # (4) A globe
#' 
#' lat <- matrix(seq(90, -90, len = 50)*pi/180, 50, 50, byrow = TRUE)
#' long <- matrix(seq(-180, 180, len = 50)*pi/180, 50, 50)
#' 
#' r <- 6378.1 # radius of Earth in km
#' x <- r*cos(lat)*cos(long)
#' y <- r*cos(lat)*sin(long)
#' z <- r*sin(lat)
#' 
#' open3d()
#' persp3d(x, y, z, col = "white", 
#'        texture = system.file("textures/worldsmall.png", package = "rgl"), 
#'        specular = "black", axes = FALSE, box = FALSE, xlab = "", ylab = "", zlab = "",
#'        normal_x = x, normal_y = y, normal_z = z)
#' if (!rgl.useNULL())
#'   play3d(spin3d(axis = c(0, 0, 1), rpm = 16), duration = 2.5)
#' 
#' \dontrun{
#' # This looks much better, but is slow because the texture is very big
#' persp3d(x, y, z, col = "white", 
#'        texture = system.file("textures/world.png", package = "rgl"), 
#'        specular = "black", axes = FALSE, box = FALSE, xlab = "", ylab = "", zlab = "",
#'        normal_x = x, normal_y = y, normal_z = z)
#' }
#' 
#' 
#' 
persp3d <- function(x, ...) UseMethod("persp3d")


persp3d.default <-
  function(x = seq(0, 1, len = nrow(z)), y = seq(0, 1, len = ncol(z)),
           z, xlim = NULL, ylim = NULL, zlim = NULL,
           xlab = NULL, ylab = NULL, zlab = NULL, add = FALSE, aspect = !add,
           forceClipregion = FALSE, ...) {
    if (!add) next3d()
    skip <- par3d(skipRedraw = TRUE)
    on.exit(par3d(skip))

    if (is.null(xlab)) {
      xlab <- if (!missing(x)) deparse(substitute(x)) else "X"
    }
    if (is.null(ylab)) {
      ylab <- if (!missing(y)) deparse(substitute(y)) else "Y"
    }
    if (is.null(zlab)) {
      zlab <- if (!missing(z)) deparse(substitute(z)) else "Z"
    }
    ## labcex is disregarded since we do NOT yet put  ANY labels...
    if (missing(z)) {
      if (!missing(x)) {
        if (is.list(x)) {
          z <- x$z
          y <- x$y
          x <- x$x
        }
        else {
          z <- x
          x <- seq(0, 1, len = nrow(z))
        }
      }
      else {
        stop("No 'z' matrix specified")
      }
    }
    else if (is.list(x)) {
      y <- x$y
      x <- x$x
    }
    if ((!is.matrix(x) && any(diff(x) <= 0))
    || (!is.matrix(y) && any(diff(y) <= 0))) {
      stop("Increasing 'x' and 'y' values expected")
    }

    savesubscene <- currentSubscene3d()
    result <- setClipregion(xlim, ylim, zlim, forceClipregion)
    result <- c(result, surface = surface3d(x, y, z, ...))
    useSubscene3d(savesubscene)
    if (!add) {
      result <- c(result, decorate3d(
        xlim = xlim, ylim = ylim, zlim = zlim,
        xlab = xlab, ylab = ylab, zlab = zlab, aspect = aspect, ...
      ))
      highlevel(result)
    } else {
      lowlevel(result)
    }
  }

setClipregion <- function(xlim = NULL, ylim = NULL, zlim = NULL, force = FALSE) {
  if (force || length(c(xlim, ylim, zlim))) {
    listeners <- par3d("listeners")
    result <- c(clipregion = newSubscene3d("inherit", "inherit", "inherit"))
    par3d(listeners = listeners)
    normals <- matrix(nrow = 0, ncol = 3)
    offsets <- c()
    if (length(xlim)) {
      normals <- rbind(normals, matrix(c(
        1, 0, 0,
        -1, 0, 0
      ), nrow = 2, byrow = TRUE))
      offsets <- c(offsets, -xlim[1], xlim[2])
    }
    if (length(ylim)) {
      normals <- rbind(normals, matrix(c(
        0, 1, 0,
        0, -1, 0
      ), nrow = 2, byrow = TRUE))
      offsets <- c(offsets, -ylim[1], ylim[2])
    }
    if (length(zlim)) {
      normals <- rbind(normals, matrix(c(
        0, 0, 1,
        0, 0, -1
      ), nrow = 2, byrow = TRUE))
      offsets <- c(offsets, -zlim[1], zlim[2])
    }
    keep <- is.finite(offsets)
    if (length(offsets[keep])) {
      result <- c(result, clipplanes = clipplanes3d(normals[keep, ], d = offsets[keep]))
    }
  } else {
    result <- integer()
  }
  lowlevel(result)
}



#' Plot a function of two variables
#' 
#' Plot a function \code{z(x, y)} or a parametric function \code{(x(s, t), y(s,
#' t), z(s, t))}.
#' 
#' The \code{"function"} method for \code{plot3d} simply passes all arguments
#' to \code{persp3d}.  Thus this description applies to both.
#' 
#' The first argument \code{x} is required to be a function.  It is named
#' \code{x} only because of the requirements of the S3 system; in the remainder
#' of this help page, we will assume that the assignment \code{f <- x} has been
#' made, and will refer to the function \code{f()}.
#' 
#' \code{persp3d.function} evaluates \code{f()} on a two-dimensional grid of
#' values, and displays the resulting surface.  The values on the grid will be
#' passed in as vectors in the first two arguments to the function, so
#' \code{f()} needs to be vectorized.  Other optional arguments to \code{f()}
#' can be specified in the \code{otherargs} list.
#' 
#' In the default form where \code{slim} and \code{tlim} are both \code{NULL},
#' it is assumed that \code{f(x, y)} returns heights, which will be plotted in
#' the z coordinate.  The default axis labels will be taken from the argument
#' names to \code{f()} and the expression passed as argument \code{x} to this
#' function.
#' 
#' If \code{slim} or \code{tlim} is specified, a parametric surface is plotted.
#' The function \code{f(s, t)} must return a 3-column matrix, giving x, y and z
#' coordinates of points on the surface.  The default axis labels will be the
#' column names if those are present. In this case \code{xlim}, \code{ylim} and
#' \code{zlim} are used to define a clipping region only if specified; the
#' defaults are ignored.
#' 
#' The color of the surface may be specified as the name of a color, or a
#' vector or matrix of color names. In this case the colors will be recycled
#' across the points on the grid of values.
#' 
#' Alternatively, a function may be given: it should be a function like
#' \code{\link{rainbow}} that takes an integer argument and returns a vector of
#' colors.  In this case the colors are mapped to z values.
#' 
#' The \code{normal} argument allows specification of a function to compute
#' normal vectors to the surface.  This function is passed the same arguments
#' as \code{f()} (incuding \code{otherargs} if present), and should produce a
#' 3-column matrix containing the x, y and z coordinates of the normals.
#' 
#' The \code{texcoords} argument is a function similar to \code{normal}, but it
#' produces a 2-column matrix containing texture coordinates.
#' 
#' Both \code{normal} and \code{texcoords} may also contain matrices, with 3
#' and 2 columns respectively, and rows corresponding to the points that were
#' passed to \code{f()}.
#' 
#' @aliases plot3d.function persp3d.function
#' @param x A function of two arguments.  See the details below.
#' @param xlim,ylim By default, the range of x and y values.  For a parametric
#' surface, if these are not missing, they are used as limits on the displayed
#' x and y values.
#' @param slim,tlim If not \code{NULL}, these give the range of s and t in the
#' parametric specification of the surface.  If only one is given, the other
#' defaults to \code{c(0, 1)}.
#' @param n A one or two element vector giving the number of steps in the x and
#' y (or s and t) grid.
#' @param xvals,yvals The values at which to evaluate x and y.  Ignored for a
#' parametric surface.  If used, \code{xlim} and/or \code{ylim} are ignored.
#' @param svals,tvals The values at which to evaluate s and t for a parametric
#' surface.  Only used if \code{slim} or \code{tlim} is not \code{NULL}.  As
#' with \code{xvals} and \code{yvals}, these override the corresponding
#' \code{slim} or \code{tlim} specification.
#' @param xlab,ylab,zlab The axis labels.  See the details below for the
#' defaults.
#' @param col The color to use for the plot.  See the details below.
#' @param otherargs Additional arguments to pass to the function.
#' @param normal,texcoords Functions to set surface normals or texture
#' coordinates.  See the details below.
#' @param \dots Additional arguments to pass to \code{\link{persp3d}}.
#' @return This function constructs a call to \code{\link{persp3d}} and returns
#' the value from that function.
#' @author Duncan Murdoch
#' @seealso The \code{\link{curve}} function in base graphics does something
#' similar for functions of one variable.  See the example below for space
#' curves.
#' @keywords graphics
#' @examples
#' 
#' # (1) The Obligatory Mathematical surface.
#' #     Rotated sinc function, with colors
#' 
#' f <- function(x, y) { 
#'   r <- sqrt(x^2 + y^2)
#'   ifelse(r == 0, 10, 10 * sin(r)/r)
#' }
#' open3d()
#' plot3d(f, col = colorRampPalette(c("blue", "white", "red")), 
#'        xlab = "X", ylab = "Y", zlab = "Sinc( r )", 
#'        xlim = c(-10, 10), ylim = c(-10, 10),
#'        aspect = c(1, 1, 0.5))
#'        
#' # (2) A cylindrical plot
#' 
#' f <- function(s, t) {
#'   r <- 1 + exp( -pmin( (s - t)^2, 
#'                        (s - t - 1)^2, 
#'                        (s - t + 1)^2 )/0.01 )
#'   cbind(r*cos(t*2*pi), r*sin(t*2*pi), s)
#' }
#' 
#' open3d()
#' plot3d(f, slim = c(0, 1), tlim = c(0, 1), col = "red", alpha = 0.8)
#' 
#' # Add a curve to the plot, fixing s at 0.5.
#' 
#' plot3d(f(0.5, seq.int(0, 1, length.out = 100)), type = "l", add = TRUE, 
#'        lwd = 3, depth_test = "lequal")
#' 
persp3d.function <- function(x, xlim = c(0, 1), ylim = c(0, 1),
                             slim = NULL, tlim = NULL, n = 101,
                             xvals = seq.int(min(xlim), max(xlim), length.out = n[1]),
                             yvals = seq.int(min(ylim), max(ylim), length.out = n[2]),
                             svals = seq.int(min(slim), max(slim), length.out = n[1]),
                             tvals = seq.int(min(tlim), max(tlim), length.out = n[2]),
                             xlab = NULL, ylab = NULL, zlab = NULL,
                             col = "gray",
                             otherargs = list(),
                             normal = NULL, texcoords = NULL, ...) {
  f <- x
  n <- rep(n, length.out = 2)
  parametric <- !is.null(slim) || !is.null(tlim)
  if (!parametric) {
    n1 <- length(xvals)
    n2 <- length(yvals)
    xvals <- matrix(xvals, n1, n2)
    yvals <- matrix(yvals, n1, n2, byrow = TRUE)
    args <- c(list(c(xvals), c(yvals)), otherargs)
    zvals <- do.call(f, args)
    dim(zvals) <- dim(xvals)
    argnames <- names(as.list(f))
    if (is.null(xlab)) xlab <- argnames[1]
    if (is.null(ylab)) ylab <- argnames[2]
    if (is.null(zlab)) zlab <- deparse(substitute(x))
  } else {
    if (is.null(slim)) slim <- c(0, 1)
    if (is.null(tlim)) tlim <- c(0, 1)
    n1 <- length(svals)
    n2 <- length(tvals)
    svals <- matrix(svals, n1, n2)
    tvals <- matrix(tvals, n1, n2, byrow = TRUE)

    args <- c(list(as.numeric(svals), as.numeric(tvals)), otherargs)
    allvals <- do.call(f, args)
    xvals <- matrix(allvals[, 1], n1, n2)
    yvals <- matrix(allvals[, 2], n1, n2)
    zvals <- matrix(allvals[, 3], n1, n2)
    if (!is.null(colnames <- colnames(allvals))) {
      if (is.null(xlab)) xlab <- colnames[1]
      if (is.null(ylab)) ylab <- colnames[2]
      if (is.null(zlab)) zlab <- colnames[3]
    }
  }

  if (is.function(col)) {
    zmin <- min(zvals, na.rm = TRUE)
    zscale <- 1 / (max(zvals, na.rm = TRUE) - zmin)
    colfn <- colorRamp(col(100))
    colrgba <- colfn(c((zvals - zmin) * zscale))
    colrgba[is.na(colrgba)] <- 0
    col <- rgb(colrgba, maxColorValue = 255)
    dim(col) <- dim(zvals)
  }
  if (is.function(normal)) {
    normal <- do.call(normal, args)
  }
  if (is.function(texcoords)) {
    texcoords <- do.call(texcoords, args)
  }

  args <- list(xvals, yvals, zvals,
    col = col,
    xlab = xlab, ylab = ylab, zlab = zlab, ...
  )
  if (is.matrix(normal)) {
    args <- c(args, list(
      normal_x = matrix(normal[, 1], n1, n2),
      normal_y = matrix(normal[, 2], n1, n2),
      normal_z = matrix(normal[, 3], n1, n2)
    ))
  }
  if (is.matrix(texcoords)) {
    args <- c(args, list(
      texture_s = matrix(texcoords[, 1], n1, n2),
      texture_t = matrix(texcoords[, 2], n1, n2)
    ))
  }
  if (parametric) {
    args <- c(args, list(
      xlim = if (!missing(xlim)) xlim,
      ylim = if (!missing(ylim)) ylim
    ))
  }

  do.call(persp3d, args)
}



#' Plot a Delaunay triangulation.
#' 
#' The \code{\link[deldir]{deldir}()} function in the \pkg{deldir} package
#' computes a Delaunay triangulation of a set of points.  These functions
#' display it as a surface.
#' 
#' These functions construct a \code{\link{mesh3d}} object corresponding to the
#' triangulation in \code{x}.  The \code{plot3d} and \code{persp3d} methods
#' plot it.
#' 
#' The \code{coords} parameter allows surfaces to be plotted over any
#' coordinate plane.  It should be a permutation of the column names
#' \code{c("x", "y", "z")} from the \code{"deldir"} object.  The first will be
#' used as the x coordinate, the second as the y coordinate, and the third as
#' the z coordinate.
#' 
#' The \code{...} parameters in \code{plot3d.deldir} are passed to
#' \code{persp3d.deldir}; in \code{persp3d.deldir} they are passed to both
#' \code{as.mesh3d.deldir} and \code{persp3d.mesh3d}; in
#' \code{as.mesh3d.deldir} they are used as material parameters in a
#' \code{\link{tmesh3d}} call.
#' 
#' @aliases persp3d.deldir plot3d.deldir as.mesh3d.deldir
#' @param x A \code{"deldir"} object, produced by the
#' \code{\link[deldir]{deldir}()} function.  It must contain \code{z} values.
#' @param add Whether to add surface to existing plot (\code{add = TRUE}) or
#' create a new plot (\code{add = FALSE}, the default).
#' @param col Colors to apply to each vertex in the triangulation. Will be
#' recycled as needed.
#' @param coords See Details below.
#' @param smooth Whether to average normals at vertices for a smooth
#' appearance.
#' @param normals User-specified normals at each vertex.  Requires \code{smooth
#' = FALSE}.
#' @param texcoords Texture coordinates at each vertex.
#' @param ...  See Details below.
#' @keywords graphics
#' @examples
#' 
#' x <- rnorm(200, sd = 5)
#' y <- rnorm(200, sd = 5)
#' r <- sqrt(x^2 + y^2)
#' z <- 10 * sin(r)/r
#' col <- cm.colors(20)[1 + round(19*(z - min(z))/diff(range(z)))]
#' 
#' if (requireNamespace("deldir", quietly = TRUE)) {
#'   save <- options(rgl.meshColorWarning = FALSE)
#'   dxyz <- deldir::deldir(x, y, z = z, suppressMsge = TRUE)
#'   persp3d(dxyz, col = col)
#'   open3d()
#'   # Do it without smoothing and with a different orientation.
#'   persp3d(dxyz, col = col, coords = c("z", "x", "y"), smooth = FALSE)
#'   options(save)
#' }
#' 
persp3d.deldir <- function(x, ..., add = FALSE) {
  plot3d(as.mesh3d(x, ...), add = add, ...)
}



#' Plot an interp or tripack Delaunay triangulation.
#' 
#' The \code{\link[interp]{tri.mesh}()} functions in the \pkg{interp} and
#' \pkg{tripack} packages compute a Delaunay triangulation of a set of points.
#' These functions display it as a surface.
#' 
#' These functions construct a \code{\link{mesh3d}} object corresponding to the
#' triangulation in \code{x}.  The \code{plot3d} and \code{persp3d} methods
#' plot it.
#' 
#' The \code{coords} parameter allows surfaces to be plotted over any
#' coordinate plane.  It should be a permutation of the column names
#' \code{c("x", "y", "z")}. The first will be used as the x coordinate, the
#' second as the y coordinate, and the third as the z coordinate.
#' 
#' The \code{...} parameters in \code{plot3d.triSht} and \code{plot3d.tri} are
#' passed to \code{persp3d}; in \code{persp3d.triSht} and \code{persp3d.tri}
#' they are passed to both \code{as.mesh3d} and \code{persp3d.mesh3d}; in
#' \code{as.mesh3d.triSht} and \code{as.mesh3d.tri} they are used as material
#' parameters in a \code{\link{tmesh3d}} call.
#' 
#' \code{"tri"} objects may contain constraints.  These appear internally as
#' extra nodes, representing either the inside or outside of boundaries on the
#' region being triangulated. Each of these nodes should also have a \code{z}
#' value, but triangles corresponding entirely to constraint nodes will not be
#' drawn.  In this way complex, non-convex regions can be triangulated.  See
#' the second example below.
#' 
#' @aliases persp3d.triSht plot3d.triSht as.mesh3d.triSht persp3d.tri
#' plot3d.tri as.mesh3d.tri
#' @param x A \code{"triSht"} or \code{"tri"} object, produced by the
#' \code{\link[interp]{tri.mesh}()} function in the \pkg{interp} or
#' \pkg{tripack} packages respectively.
#' @param z z coordinate values corresponding to each of the nodes in \code{x}.
#' @param add Whether to add surface to existing plot (\code{add = TRUE}) or
#' create a new plot (\code{add = FALSE}, the default).
#' @param col Colors to apply to each vertex in the triangulation. Will be
#' recycled as needed.
#' @param coords See Details below.
#' @param smooth Whether to average normals at vertices for a smooth
#' appearance.
#' @param normals User-specified normals at each vertex.  Requires \code{smooth
#' = FALSE}.
#' @param texcoords Texture coordinates at each vertex.
#' @param ...  See Details below.
#' @note If there are duplicate points, the \code{tri.mesh()} functions will
#' optionally delete some of them.  If you choose this option, the \code{z}
#' values must correspond to the nodes \emph{after} deletion, not before.
#' @keywords graphics
#' @examples
#' 
#' x <- rnorm(200, sd = 5)
#' y <- rnorm(200, sd = 5)
#' r <- sqrt(x^2 + y^2)
#' z <- 10 * sin(r)/r
#' col <- cm.colors(20)[1 + round(19*(z - min(z))/diff(range(z)))]
#' save <- NULL
#' if (requireNamespace("interp", quietly = TRUE)) {
#'   save <- options(rgl.meshColorWarning = FALSE)
#'   dxy <- interp::tri.mesh(x, y)
#'   open3d()
#'   persp3d(dxy, z, col = col, meshColor = "vertices")
#'   open3d()
#'   # Do it without smoothing and with a different orientation.
#'   persp3d(dxy, z, col = col, coords = c("z", "x", "y"), smooth = FALSE)
#' }
#' if (requireNamespace("tripack", quietly = TRUE)) {
#'   if (is.null(save))
#'     save <- options(rgl.meshColorWarning = FALSE)
#' 
#'   # Leave a circular hole around (3, 0)
#'   theta <- seq(0, 2*pi, len = 30)[-1]
#'   cx <- 2*cos(theta) + 3
#'   cy <- 2*sin(theta)
#'   keep <- (x - 3)^2 + y^2 > 4
#'   dxy2 <- tripack::tri.mesh(x[keep], y[keep])
#'   dxy2 <- tripack::add.constraint(dxy2, cx, cy)
#'   z <- dxy2$x^2 - dxy2$y^2
#'   col <- terrain.colors(20)[1 + round(19*(z - min(z))/diff(range(z)))]
#'   open3d()
#'   persp3d(dxy2, z, col = col)
#' }
#' options(save)
#' 
persp3d.triSht <-
  persp3d.tri <- function(x, z, ..., add = FALSE) {
    plot3d(as.mesh3d(x, z, ...), add = add, ...)
  }

persp3d.formula <- function(x, data = NULL, xlab = xyz$xlab, ylab = xyz$ylab, zlab = xyz$zlab, ...) {
  if (!requireNamespace("deldir", quietly = TRUE)) {
    stop("This function requires the 'deldir' package.")
  }
  if (!is.null(data)) {
    environment(x) <- list2env(data, envir = environment(x))
  }
  xyz <- xyz.coords(x)
  dxyz <- with(xyz, deldir::deldir(x, y, z = z, suppressMsge = TRUE))
  persp3d(dxyz, xlab = xlab, ylab = ylab, zlab = zlab, ...)
}
