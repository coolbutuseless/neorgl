#' 3D Scatterplot
#' 
#' Draws a 3D scatterplot.
#' 
#' \code{plot3d} is a partial 3D analogue of plot.default.
#' 
#' Missing values in the data are skipped, as in standard graphics.
#' 
#' If \code{aspect} is \code{TRUE}, aspect ratios of \code{c(1, 1, 1)} are
#' passed to \code{\link{aspect3d}}.  If \code{FALSE}, no aspect adjustment is
#' done.  In other cases, the value is passed to \code{\link{aspect3d}}.
#' 
#' With \code{type = "s"}, spheres are drawn centered at the specified
#' locations. The radius may be controlled by \code{size} (specifying the size
#' relative to the plot display, with the default \code{size = 3} giving a
#' radius about 1/20 of the plot region) or \code{radius} (specifying it on the
#' data scale if an isometric aspect ratio is chosen, or on an average scale if
#' not).
#' 
#' @aliases plot3d plot3d.default plot3d.mesh3d decorate3d
#' @param x,y,z vectors of points to be plotted. Any reasonable way of defining
#' the coordinates is acceptable.  See the function
#' \code{\link[grDevices]{xyz.coords}} for details.
#' @param xlab,ylab,zlab labels for the coordinates.
#' @param type For the default method, a single character indicating the type
#' of item to plot.  Supported types are: 'p' for points, 's' for spheres, 'l'
#' for lines, 'h' for line segments from \code{z = 0}, and 'n' for nothing.
#' For the \code{mesh3d} method, one of 'shade', 'wire', or 'dots'.  Partial
#' matching is used.
#' @param col the color to be used for plotted items.
#' @param size the size for plotted points.
#' @param lwd the line width for plotted items.
#' @param radius the radius of spheres: see Details below.
#' @param add whether to add the points to an existing plot.
#' @param aspect either a logical indicating whether to adjust the aspect
#' ratio, or a new ratio.
#' @param expand how much to expand the box around the data, if it is drawn.
#' @param xlim,ylim,zlim In \code{plot3d}, if not \code{NULL}, set clipping
#' limits for the plot.  In \code{decorate3d}, these are used for the labels.
#' @param forceClipregion Force a clipping region to be used, whether or not
#' limits are given.
#' @param \dots additional parameters which will be passed to
#' \code{\link{par3d}}, \code{\link{material3d}} or \code{decorate3d}.
#' @param box,axes whether to draw a box and axes.
#' @param main,sub main title and subtitle.
#' @param top whether to bring the window to the top when done.
#' @return \code{plot3d} is called for the side effect of drawing the plot; a
#' vector of object IDs is returned.
#' 
#' \code{decorate3d} adds the usual decorations to a plot: labels, axes, etc.
#' @section Clipping: If any of \code{xlim}, \code{ylim} or \code{zlim} are
#' specified, they should be length two vectors giving lower and upper clipping
#' limits for the corresponding coordinate.  \code{NA} limits will be ignored.
#' 
#' If any clipping limits are given, then the data will be plotted in a newly
#' created subscene within the current one; otherwise plotting will take place
#' directly in the current subscene.  This subscene is named
#' \code{"clipregion"} in the results.  This may affect the appearance of
#' transparent objects if some are drawn in the \code{plot3d} call and others
#' after, as \pkg{rgl} will not attempt to depth-sort objects if they are in
#' different subscenes.  It is best to draw all overlapping transparent objects
#' in the same subscene.  See the example in \code{\link{planes3d}}.  It will
#' also affect the use of \code{\link{clipplanes3d}}; clipping planes need to
#' be in the same subscene as the objects being clipped.
#' 
#' Use \code{forceClipregion = TRUE} to force creation of this subscene even
#' without specifying limits.
#' @author Duncan Murdoch
#' @seealso \code{\link{plot.default}}, \code{\link{open3d}},
#' \code{\link{par3d}}.  There are \code{\link{plot3d.function}} and
#' \code{\link{plot3d.deldir}} methods for plotting surfaces.
#' @keywords dynamic
#' @examples
#' 
#'   open3d()
#'   x <- sort(rnorm(1000))
#'   y <- rnorm(1000)
#'   z <- rnorm(1000) + atan2(x, y)
#'   plot3d(x, y, z, col = rainbow(1000))
#' 
plot3d <- function(x, ...) UseMethod("plot3d")


plot3d.default <- function(x, y = NULL, z = NULL,
                           xlab = NULL, ylab = NULL, zlab = NULL, type = "p",
                           col = material3d("color")[1], size = material3d("size"),
                           lwd = material3d("lwd"),
                           radius = avgscale * size / 60, add = FALSE, aspect = !add,
                           xlim = NULL, ylim = NULL, zlim = NULL,
                           forceClipregion = FALSE, ...) {
  if (!add) next3d()
  skip <- par3d(skipRedraw = TRUE)
  on.exit(par3d(skip))

  xlabel <- if (!missing(x)) deparse(substitute(x))
  ylabel <- if (!missing(y)) deparse(substitute(y))
  zlabel <- if (!missing(z)) deparse(substitute(z))

  xyz <- xyz.coords(x, y, z, xlab = xlabel, ylab = ylabel, zlab = zlabel, recycle = TRUE)
  x <- xyz$x
  y <- xyz$y
  z <- xyz$z

  if (is.null(xlab)) xlab <- xyz$xlab
  if (is.null(ylab)) ylab <- xyz$ylab
  if (is.null(zlab)) zlab <- xyz$zlab

  if (type == "s" && missing(radius)) {
    xvals <- x
    yvals <- y
    zvals <- z
    if (add && diff(bbox <- par3d("bbox"))[1] > 0) {
      xvals <- c(x, bbox[1:2])
      yvals <- c(y, bbox[3:4])
      zvals <- c(z, bbox[5:6])
    }
    if (!add) {
      if (!is.null(xlim)) {
        xvals <- xlim
      }
      if (!is.null(ylim)) {
        yvals <- ylim
      }
      if (!is.null(zlim)) {
        zvals <- zlim
      }
    }
    avgscale <- sqrt(sum(c(
      diff(range(xvals, na.rm = TRUE)),
      diff(range(yvals, na.rm = TRUE)),
      diff(range(zvals, na.rm = TRUE))
    )^2 / 3))
  }
  savesubscene <- currentSubscene3d()
  result <- setClipregion(xlim, ylim, zlim, forceClipregion)
  result <- c(result, data = switch(type,
    p = points3d(x, y, z, color = col, size = size, ...),
    s = spheres3d(x, y, z, radius = radius, color = col, ...),
    l = lines3d(x, y, z, color = col, lwd = lwd, ...),
    h = segments3d(rep(x, rep(2, length(x))),
      rep(y, rep(2, length(y))),
      rbind(rep(0, length(z)), z),
      color = rep(col, rep(2, length(col))), lwd = lwd, ...
    ),
    # this is a hack to plot invisible segments
    n = if (!add) {
      segments3d(
        rep(range(x, na.rm = TRUE), c(2, 2)),
        rep(range(y, na.rm = TRUE), c(2, 2)),
        rep(range(z, na.rm = TRUE), c(2, 2))
      )
    }
  ))
  useSubscene3d(savesubscene)
  if (!add) {
    result <- c(result, decorate3d(
      xlab = xlab, ylab = ylab, zlab = zlab, aspect = aspect,
      xlim = xlim, ylim = ylim, zlim = zlim, ...
    ))
    highlevel(result)
  } else {
    lowlevel(result)
  }
}

plot3d.mesh3d <- function(x, xlab = "x", ylab = "y", zlab = "z", type = c("shade", "wire", "dots"),
                          add = FALSE, aspect = !add, ...) {
  if (!add) next3d()
  skip <- par3d(skipRedraw = TRUE)
  on.exit(par3d(skip))

  if (missing(xlab) && !is.null(x$xlab)) xlab <- x$xlab
  if (missing(ylab) && !is.null(x$ylab)) ylab <- x$ylab
  if (missing(zlab) && !is.null(x$zlab)) zlab <- x$zlab

  result <- switch(match.arg(type),
    shade = shade3d(x, ...),
    wire = wire3d(x, ...),
    dots = dot3d(x, ...)
  )

  if (!add) {
    result <- c(result, decorate3d(
      xlab = xlab, ylab = ylab, zlab = zlab, aspect = aspect,
      ...
    ))
    highlevel(result)
  } else {
    lowlevel(result)
  }
}

decorate3d <- function(xlim = NULL, ylim = NULL, zlim = NULL,
                       xlab = "x", ylab = "y", zlab = "z",
                       box = TRUE, axes = TRUE, main = NULL, sub = NULL,
                       top = TRUE, aspect = FALSE, expand = 1.03, ...) {
  if (is.logical(aspect)) {
    autoscale <- aspect
    aspect <- c(1, 1, 1)
  } else {
    autoscale <- TRUE
  }

  result <- numeric(0)
  if (length(c(xlim, ylim, zlim))) {
    ranges <- .getRanges()
    if (is.null(xlim)) {
      xlim <- ranges$xlim
    }
    if (is.null(ylim)) {
      ylim <- ranges$ylim
    }
    if (is.null(zlim)) {
      zlim <- ranges$zlim
    }
    ind <- c(1, 1, 2, 2)
    result <- c(result, strut = segments3d(xlim[ind], ylim[ind], zlim[ind]))
  }

  if (autoscale) aspect3d(aspect)

  if (axes) result <- c(result, axes = axes3d(box = box, expand = expand))
  result <- c(result, title3d(
    xlab = xlab, ylab = ylab, zlab = zlab,
    main = main, sub = sub
  ))

  if (top) rgl.bringtotop()

  lowlevel(result)
}

plot3d.function <- function(x, ...) persp3d(x, ...)

plot3d.deldir <- function(x, ...) persp3d(x, ...)

plot3d.triSht <-
  plot3d.tri <- function(x, z, ...) persp3d(x, z, ...)



#' Methods for formulas
#' 
#' These functions provide a simple formula-based interface to
#' \code{\link{plot3d}} and \code{\link{persp3d}}.
#' 
#' Only simple formulas (the ones handled by the \code{\link{xyz.coords}}
#' function) are supported: a single variable on the left hand side (which will
#' be plotted on the Z axis), and a sum of two variables on the right hand side
#' (which will be the X and Y axis variables in the plot.)
#' 
#' @aliases plot3d.formula persp3d.formula
#' @param x A formula like \code{z ~ x + y}.
#' @param data An optional dataframe or list in which to find the components of
#' the formula.
#' @param xlab,ylab,zlab Optional axis labels to override the ones
#' automatically obtained from the formula.
#' @param \dots Additional arguments to pass to the default \code{plot3d}
#' method, or the \code{persp3d} method for \code{"deldir"} objects.
#' @return These functions are called for the side effect of drawing the plots.
#' The \code{plot3d} method draws a scatterplot.  The \code{persp3d} method
#' draws a surface plot.
#' 
#' Return values are as given by the \code{\link{plot3d.default}} method or the
#' \code{\link{persp3d.deldir}} methods.
#' @note The \code{persp3d} method requires that the suggested package
#' \pkg{deldir} is installed.
#' @author Duncan Murdoch
#' @examples
#' 
#' open3d()
#' mfrow3d(1, 2, sharedMouse = TRUE)
#' plot3d(mpg ~ wt + qsec, data = mtcars)
#' if (requireNamespace("deldir"))
#'   persp3d(mpg ~ wt + qsec, data = mtcars)
#' 
plot3d.formula <- function(x, data = NULL, xlab = xyz$xlab, ylab = xyz$ylab, zlab = xyz$zlab, ...) {
  if (!is.null(data)) {
    environment(x) <- list2env(data, envir = environment(x))
  }
  xyz <- xyz.coords(x)
  plot3d(xyz, xlab = xlab, ylab = ylab, zlab = zlab, ...)
}



#' Method for plotting simple linear fit.
#' 
#' This function provides several plots of the result of fitting a
#' two-predictor model.
#' 
#' Three plots are possible, depending on the value(s) in \code{which}:
#' \enumerate{ \item(default) Show the points and the fitted plane. \itemShow
#' the residuals and the plane at \code{z = 0}. \itemShow the predicted values
#' on the fitted plane. }
#' 
#' @param x An object inheriting from class \code{"lm"} obtained by fitting a
#' two-predictor model.
#' @param which Which plot to show?  See Details below.
#' @param plane.col,plane.alpha These parameters control the colour and
#' transparency of a plane or surface.
#' @param sharedMouse If multiple plots are requested, should they share mouse
#' controls, so that they move in sync?
#' @param use_surface3d Use the \code{\link{surface3d}} function to plot the
#' surface rather than \code{\link{planes3d}}.  This allows curved surfaces to
#' be shown.  The default is \code{FALSE} if the model looks like a simple 2
#' parameter linear fit, otherwise \code{TRUE}.
#' @param do_grid Plot a grid.
#' @param grid.col,grid.alpha,grid.steps Characteristics of the grid.
#' @param sub.steps If \code{use_surface3d} is \code{TRUE}, use an internal
#' grid of \code{grid.steps*sub.steps} to draw the surface.  \code{sub.steps >
#' 1} allows curvature within facets. Similarly, if \code{do_grid} is
#' \code{TRUE}, it allows curvature within grid lines.
#' @param vars A dataframe containing the variables to plot in the first three
#' columns, with the response assumed to be in column 1.  See the Note below.
#' @param \dots Other parameters to pass to the default \code{\link{plot3d}}
#' method, to control the appearance of aspects of the plot other than the
#' plane.
#' @return Called for the side effect of drawing one or more plots.
#' 
#' Invisibly returns a high-level vector of object ids.  Names of object ids
#' have the plot number (in drawing order) appended.
#' @note The default value for the \code{vars} argument will handle simple
#' linear models with a response and two predictors, and some models with
#' functions of those two predictors.  For models that fail (e.g. models using
#' \code{\link{poly}}), you can include the observed values as in the third
#' example below.
#' @author Duncan Murdoch
#' @examples
#' 
#' open3d()
#' ids <- plot3d(lm(mpg ~ wt + qsec, data = mtcars), which = 1:3)
#' names(ids)
#' 
#' open3d()
#' plot3d(lm(mpg ~ wt + I(wt^2) + qsec, data = mtcars))
#' 
#' open3d()
#' # Specify vars in the order:  response, pred1, pred2.
#' plot3d(lm(mpg ~ poly(wt, 3) + qsec, data = mtcars), 
#'        vars = mtcars[,c("mpg", "wt", "qsec")])
#' 
plot3d.lm <- function(x, which = 1,
                      plane.col = "gray", plane.alpha = 0.5,
                      sharedMouse = TRUE,
                      use_surface3d, do_grid = TRUE,
                      grid.col = "black", grid.alpha = 1,
                      grid.steps = 5,
                      sub.steps = 4,
                      vars = get_all_vars(terms(x), x$model),
                      ...) {
  stopifnot(which %in% 1:3)
  dots <- list(...)
  n <- length(which)
  result <- c()
  if (n > 1) {
    cols <- ceiling(sqrt(n))
    rows <- ceiling(n / cols)
    mfrow3d(rows, cols, sharedMouse = sharedMouse)
  }
  fit <- x
  missing_vars <- missing(vars)
  cols <- ncol(vars)
  if (cols < 3) {
    stop("Model has only ", cols, " variables.")
  }
  if (cols > 3) {
    warning("Model has ", cols, " variables; first 3 used.")
  }
  observed <- vars[, c(2, 3, 1)]
  names <- colnames(observed)
  if (is.null(dots$xlab)) dots$xlab <- names[1]
  if (is.null(dots$ylab)) dots$ylab <- names[2]
  if (is.null(dots$zlab)) dots$zlab <- names[3]

  if (missing(use_surface3d)) {
    use_surface3d <- !identical(class(fit), "lm") || ncol(as.matrix(model.frame(fit))) > 3
  }

  plotGrid <- function(i, x, y, x0, y0, z) {
    dots$color <- grid.col
    dots$alpha <- grid.alpha
    dots$color <- grid.col
    dots$alpha <- grid.alpha
    dots$front <- dots$back <- dots$polygon_offset <-
      dots$type <- NULL

    lenx <- length(x)
    lenx0 <- length(x0)
    leny <- length(y)
    leny0 <- length(y0)

    x <- c(
      rep(x0, each = leny + 1),
      rep(c(x, NA), leny0)
    )
    y <- c(
      rep(c(y, NA), lenx0),
      rep(y0, each = lenx + 1)
    )
    if (missing(z)) {
      newdat <- data.frame(x, y)
      names(newdat) <- names[1:2]
      z <- predict(fit, newdata = newdat)
    }
    grid <- do.call(lines3d, c(list(x, y, z), dots))
    names(grid) <- paste0("grid.", i)
    grid
  }

  plotPoints <- function(i, points, zlab) {
    dots$zlab <- zlab
    plot <- do.call(plot3d, c(list(x = points), dots))
    names(plot) <- paste0(names(plot), ".", i)
    plot
  }

  plotSurface <- function(i) {
    bbox <- par3d("bbox")
    xlim <- c(bbox[1], bbox[2])
    x0 <- pretty(xlim, grid.steps)
    ylim <- c(bbox[3], bbox[4])
    y0 <- pretty(ylim, grid.steps)
    if (sub.steps > 1) {
      x <- rep(x0, each = sub.steps) +
        seq(0, diff(x0[1:2]), length.out = sub.steps + 1)[-(sub.steps + 1)]
      y <- rep(y0, each = sub.steps) +
        seq(0, diff(y0[1:2]), length.out = sub.steps + 1)[-(sub.steps + 1)]
    } else {
      x <- x0
      y <- y0
    }
    x <- c(xlim[1], x[xlim[1] < x & x < xlim[2]], xlim[2])
    y <- c(ylim[1], y[ylim[1] < y & y < ylim[2]], ylim[2])
    newdat <- expand.grid(x = x, y = y)
    names(newdat) <- names[1:2]
    z <- try(matrix(predict(fit, newdat), length(x), length(y)))
    if (inherits(z, "try-error") && !missing_vars) {
      stop("vars should be in order: response, pred1, pred2", call. = FALSE)
    }
    dots$color <- plane.col
    dots$alpha <- plane.alpha
    if (is.null(dots$polygon_offset)) {
      dots$polygon_offset <- 1
    }
    dots$type <- NULL
    surface <- do.call("surface3d", c(list(x, y, z), dots))
    names(surface) <- paste0("surface.", i)
    grid <- if (do_grid) {
      x0 <- x0[xlim[1] <= x0 & x0 <= xlim[2]]
      y0 <- y0[ylim[1] <= y0 & y0 <= ylim[2]]
      plotGrid(i, x, y, x0, y0)
    }
    c(surface, grid)
  }
  plotPlane <- function(i, a, b, d) {
    c <- -1
    if (is.na(d)) {
      d <- 0
    }
    dots$color <- plane.col
    dots$alpha <- plane.alpha
    if (is.null(dots$polygon_offset)) {
      dots$polygon_offset <- 1
    }
    dots$type <- NULL
    plane <- do.call("planes3d", c(list(a = a, b = b, c = c, d = d), dots))
    names(plane) <- paste0("plane.", i)
    grid <- if (do_grid) {
      bbox <- par3d("bbox")
      xlim <- c(bbox[1], bbox[2])
      x0 <- pretty(xlim, grid.steps)
      ylim <- c(bbox[3], bbox[4])
      y0 <- pretty(ylim, grid.steps)
      x0 <- x0[xlim[1] <= x0 & x0 <= xlim[2]]
      y0 <- y0[ylim[1] <= y0 & y0 <= ylim[2]]
      if (isTRUE(all.equal(c(a, b, d), c(0, 0, 0)))) {
        plotGrid(i, xlim, ylim, x0, y0, z = 0)
      } else {
        plotGrid(i, xlim, ylim, x0, y0)
      }
    }
    c(plane, grid)
  }
  for (i in seq_along(which)) {
    type <- which[i]
    if (type == 1L) {
      plot <- plotPoints(i, observed, dots$zlab)
      if (use_surface3d) {
        plane <- plotSurface(i)
      } else {
        coefs <- coef(fit)
        plane <- plotPlane(i, coefs[names[1]], coefs[names[2]], coefs["(Intercept)"])
      }
    } else if (type == 2L) {
      plot <- plotPoints(i, cbind(observed[, 1:2], residuals(fit)), "Residuals")
      plane <- plotPlane(i, 0, 0, 0)
    } else if (type == 3L) {
      plot <- plotPoints(i, cbind(observed[, 1:2], predict(fit)), dots$zlab)
      if (use_surface3d) {
        plane <- plotSurface(i)
      } else {
        coefs <- coef(fit)
        plane <- plotPlane(i, coefs[names[1]], coefs[names[2]], coefs["(Intercept)"])
      }
    }
    result <- c(result, plot, plane)
  }
  highlevel(result)
}
