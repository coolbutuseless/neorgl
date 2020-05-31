#' Make an ellipsoid
#' 
#' A generic function and several methods returning an ellipsoid or other
#' outline of a confidence region for three parameters.
#' 
#' 
#' @aliases ellipse3d ellipse3d.default ellipse3d.lm ellipse3d.glm
#' ellipse3d.nls
#' @param x An object. In the default method the parameter \code{x} should be a
#' square positive definite matrix at least 3x3 in size. It will be treated as
#' the correlation or covariance of a multivariate normal distribution.
#' @param \dots Additional parameters to pass to the default method or to
#' \code{\link{qmesh3d}}.
#' @param scale If \code{x} is a correlation matrix, then the standard
#' deviations of each parameter can be given in the scale parameter.  This
#' defaults to \code{c(1, 1, 1)}, so no rescaling will be done.
#' @param centre The centre of the ellipse will be at this position.
#' @param level The confidence level of a simultaneous confidence region.  The
#' default is 0.95, for a 95\% region.  This is used to control the size of the
#' ellipsoid.
#' @param t The size of the ellipse may also be controlled by specifying the
#' value of a t-statistic on its boundary.  This defaults to the appropriate
#' value for the confidence region.
#' @param which This parameter selects which variables from the object will be
#' plotted.  The default is the first 3.
#' @param subdivide This controls the number of subdivisions (see
#' \code{\link{subdivision3d}}) used in constructing the ellipsoid.  Higher
#' numbers give a smoother shape.
#' @param smooth If \code{TRUE}, smooth interpolation of normals is used; if
#' \code{FALSE}, a faceted ellipsoid will be displayed.
#' @param dispersion The value of dispersion to use.  If specified, it is
#' treated as fixed, and chi-square limits for \code{t} are used. If missing,
#' it is taken from \code{summary(x)}.
#' @return A \code{\link{mesh3d}} object representing the ellipsoid.
#' @keywords dplot
#' @examples
#' 
#' # Plot a random sample and an ellipsoid of concentration corresponding to a 95% 
#' # probability region for a
#' # trivariate normal distribution with mean 0, unit variances and 
#' # correlation 0.8.
#' if (requireNamespace("MASS")) {
#'   Sigma <- matrix(c(10, 3, 0, 3, 2, 0, 0, 0, 1), 3, 3)
#'   Mean <- 1:3
#'   x <- MASS::mvrnorm(1000, Mean, Sigma)
#'   
#'   open3d()
#'   
#'   plot3d(x, box = FALSE)
#'   
#'   plot3d( ellipse3d(Sigma, centre = Mean), col = "green", alpha = 0.5, add = TRUE)
#' }  
#' 
#' # Plot the estimate and joint 90% confidence region for the displacement and cylinder
#' # count linear coefficients in the mtcars dataset
#' 
#' data(mtcars)
#' fit <- lm(mpg ~ disp + cyl , mtcars)
#' 
#' open3d()
#' plot3d(ellipse3d(fit, level = 0.90), col = "blue", alpha = 0.5, aspect = TRUE)
#' 
ellipse3d <- function(x, ...) {
  UseMethod("ellipse3d")
}

ellipse3d.default <-
  function(x, scale = c(1, 1, 1), centre = c(0, 0, 0), level = 0.95,
           t = sqrt(qchisq(level, 3)), which = 1:3, subdivide = 3, smooth = TRUE, ...) {
    stopifnot(is.matrix(x))

    cov <- x[which, which]
    chol <- chol(cov)

    sphere <- subdivision3d(cube3d(...), subdivide)
    norm <- sqrt(sphere$vb[1, ]^2 + sphere$vb[2, ]^2 + sphere$vb[3, ]^2)
    for (i in 1:3) sphere$vb[i, ] <- sphere$vb[i, ] / norm
    sphere$vb[4, ] <- 1
    if (smooth) {
      sphere$normals <- sphere$vb
    }

    result <- scale3d(transform3d(sphere, chol), t, t, t)

    if (!missing(scale)) {
      result <- scale3d(result, scale[1], scale[2], scale[3])
    }
    if (!missing(centre)) {
      result <- translate3d(result, centre[1], centre[2], centre[3])
    }
    return(result)
  }

ellipse3d.lm <-
  function(x, which = 1:3, level = 0.95, t = sqrt(3 * qf(
             level,
             3, x$df.residual
           )), ...) {
    s <- summary(x)
    names <- names(x$coefficients[which])
    structure(c(ellipse3d.default(s$sigma^2 * s$cov.unscaled[which, which],
      centre = x$coefficients[which], t = t, ...
    ),
    xlab = names[1], ylab = names[2], zlab = names[3]
    ), class = "mesh3d")
  }

ellipse3d.glm <- function(x, which = 1:3, level = 0.95, t, dispersion, ...) {
  s <- summary(x)
  est.disp <- missing(dispersion) & !(x$family$family %in% c("poisson", "binomial"))
  if (missing(dispersion)) dispersion <- s$dispersion
  if (missing(t)) {
    t <- ifelse(est.disp, sqrt(3 * qf(level, 3, s$df[2])),
      sqrt(qchisq(level, 3))
    )
  }

  names <- names(x$coefficients[which])
  structure(c(ellipse3d.default(dispersion * s$cov.unscaled[which, which],
    centre = x$coefficients[which], t = t, ...
  ),
  xlab = names[1], ylab = names[2], zlab = names[3]
  ), class = "mesh3d")
}

ellipse3d.nls <- function(x, which = 1:3, level = 0.95, t = sqrt(3 * qf(
                            level,
                            3, s$df[2]
                          )), ...) {
  s <- summary(x)
  names <- names(x$m$getPars()[which])
  structure(c(ellipse3d.default(s$sigma^2 * s$cov.unscaled[which, which],
    centre = x$m$getPars()[which], t = t, ...
  ),
  xlab = names[1], ylab = names[2], zlab = names[3]
  ), class = "mesh3d")
}
