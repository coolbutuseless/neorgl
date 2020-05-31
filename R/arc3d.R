#' Draw arcs.
#' 
#' Given starting and ending points on a sphere and the center of the sphere,
#' draw the great circle arc between the starting and ending points.  If the
#' starting and ending points have different radii, a segment of a logarithmic
#' spiral will join them.
#' 
#' If any of \code{from}, \code{to} or \code{center} is an n by 3 matrix with n
#' > 1, multiple arcs will be drawn by recycling each of these parameters to
#' the number of rows of the longest one.
#' 
#' If the vector lengths of \code{from - center} and \code{to - center} differ,
#' then instead of a spherical arc, the function will draw a segment of a
#' logarithmic spiral joining the two points.
#' 
#' By default, the arc is drawn along the shortest great circle path from
#' \code{from} to \code{to}, but the \code{base} parameter can be used to
#' modify this.  If \code{base = 1} is used, the longer arc will be followed.
#' Larger positive integer values will result in \code{base - 1} loops in that
#' direction completely around the sphere.  Negative values will draw the curve
#' in the same direction as the shortest arc, but with \code{abs(base)} full
#' loops. It doesn't make much sense to ask for such loops unless the radii of
#' \code{from} and \code{to} differ, because spherical arcs would overlap.
#' Normally the \code{base} parameter is left at its default value of \code{0}.
#' 
#' When \code{base} is non-zero, the curve will be constructed in multiple
#' pieces, between \code{from}, \code{to}, \code{-from} and \code{-to}, for as
#' many steps as necessary.  If \code{n} is specified, it will apply to each of
#' these pieces.
#' 
#' @param from One or more points from which to start arcs.
#' @param to One or more destination points.
#' @param center One or more center points.
#' @param radius If not missing, a vector of length \code{n} giving the radii
#' at each point between \code{from} and \code{to}. If missing, the starting
#' and ending points will be joined by a logarithmic spiral.
#' @param n If not missing, how many segments to use between the first and last
#' point.  If missing, a value will be calculated based on the angle between
#' starting and ending points as seen from the center.
#' @param circle How many segments would be used if the arc went completely
#' around a circle.
#' @param base See Details below.
#' @param plot Should the arcs be plotted, or returned as a matrix?
#' @param \dots Additional parameters to pass to \code{\link{points3d}}.
#' @return If \code{plot = TRUE}, called mainly for the side effect of drawing
#' arcs.  Invisibly returns the object ID of the collection of arcs.
#' 
#' If \code{plot = FALSE}, returns a 3 column matrix containing the points that
#' would be drawn as the arcs.
#' @author Duncan Murdoch
#' @examples
#' 
#' normalize <- function(v) v/sqrt(sum(v^2))
#' 
#' # These vectors all have the same length
#' 
#' from <- t(apply(matrix(rnorm(9), ncol = 3), 1, normalize))
#' to <- normalize(rnorm(3))
#' center <- c(0, 0, 0)
#' 
#' open3d()
#' spheres3d(center, radius = 1, col = "white", alpha = 0.2)
#' 
#' arc3d(from, to, center, col = "red")
#' arc3d(from, 2*to, center, col = "blue")
#' 
#' text3d(rbind(from, to, center, 2*to), 
#'        text = c(paste0("from", 1:3), "to", "center", "2*to"),
#'        depth_mask = FALSE, depth_test = "always")
#' 
arc3d <- function(from, to, center, radius, n, circle = 50, base = 0, plot = TRUE, ...) {
  fixarg <- function(arg) {
    if (is.matrix(arg)) {
      arg[, 1:3, drop = FALSE]
    } else {
      matrix(arg, 1, 3)
    }
  }
  normalize <- function(v) {
    v / veclen(v)
  }
  getrow <- function(arg, i) {
    arg[1 + (i - 1) %% nrow(arg), ]
  }
  from <- fixarg(from)
  to <- fixarg(to)
  center <- fixarg(center)

  m <- max(nrow(from), nrow(to), nrow(center), length(base))
  base <- rep_len(base, m)

  result <- matrix(NA_real_, nrow = 1, ncol = 3)

  for (j in seq_len(m)) {
    from1 <- getrow(from, j)
    to1 <- getrow(to, j)
    center1 <- getrow(center, j)
    base1 <- base[j]
    logr1 <- log(veclen(from1 - center1))
    logr2 <- log(veclen(to1 - center1))
    A <- normalize(from1 - center1)
    B <- normalize(to1 - center1)
    steps <- if (base1 <= 0) 4 * abs(base1) + 1 else 4 * base1 - 1
    for (k in seq_len(steps)) {
      if (k %% 2) {
        A1 <- A * (-1)^(k %/% 2)
        B1 <- B * (-1)^(k %/% 2 + (base1 > 0))
      } else {
        A1 <- B * (-1)^(k %/% 2 + (base1 <= 0))
        B1 <- A * (-1)^(k %/% 2)
      }
      theta <- acos(sum(A1 * B1))
      if (isTRUE(all.equal(theta, pi))) {
        warning("Arc ", j, " points are opposite each other!  Arc is not well defined.")
      }
      if (missing(n)) {
        n1 <- ceiling(circle * theta / (2 * pi))
      } else {
        n1 <- n
      }

      if (missing(radius)) {
        pretheta <- (k %/% 2) * pi - (k %% 2 == 0) * theta
        if (k == 1) {
          totaltheta <- (steps %/% 2) * pi - (steps %% 2 == 0) * theta + theta
        }
        p1 <- pretheta / totaltheta
        p2 <- (pretheta + theta) / totaltheta
        radius1 <- exp(seq(
          from = (1 - p1) * logr1 + p1 * logr2,
          to = (1 - p2) * logr1 + p2 * logr2,
          length.out = n1 + 1
        ))
      } else {
        radius1 <- rep_len(radius, n1)
      }
      arc <- matrix(NA_real_, nrow = n1 + 1, ncol = 3)
      p <- seq(0, 1, length.out = n1 + 1)
      arc[1, ] <- center1 + radius1[1] * A1
      arc[n1 + 1, ] <- center1 + radius1[n1 + 1] * B1
      AB <- veclen(A1 - B1)
      for (i in seq_len(n1)[-1]) {
        ptheta <- p[i] * theta
        phi <- pi / 2 + (0.5 - p[i]) * theta
        q <- (sin(ptheta) / sin(phi)) / AB
        D <- (1 - q) * A1 + q * B1
        arc[i, ] <- center1 + radius1[i] * normalize(D)
      }
      if (k == 1) {
        result <- rbind(result, arc)
      } else {
        result <- rbind(result[-nrow(result), , drop = FALSE], arc)
      }
    }
    result <- rbind(result, result[1, ])
  }
  if (plot) {
    lines3d(result[c(-1, -nrow(result)), , drop = FALSE], ...)
  } else {
    result[c(-1, -nrow(result)), , drop = FALSE]
  }
}
