#' The Gram-Schmidt algorithm
#' 
#' Generate a 3x3 orthogonal matrix using the Gram-Schmidt algorithm.
#' 
#' This function orthogonalizes the matrix \code{rbind(v1, v2, v3)} using the
#' Gram-Schmidt algorithm.  It can handle rank 2 matrices (returning a rank 3
#' matrix).  If the original is rank 1, it is likely to fail.
#' 
#' The \code{order} vector determines the precedence of the original vectors.
#' For example, if it is \code{c(i, j, k)}, then row \code{i} will be unchanged
#' (other than normalization); row \code{j} will normally be transformed within
#' the span of rows \code{i} and \code{j}.  Row \code{k} will be transformed
#' orthogonally to the span of the others.
#' 
#' @param v1,v2,v3 Three length 3 vectors (taken as row vectors).
#' @param order The precedence order for the vectors; see Details.
#' @return A 3x3 matrix whose rows are the orthogonalization of the original
#' row vectors.
#' @author Duncan Murdoch
#' @examples
#' 
#' # Proceed through the rows in order
#' print(A <- matrix(rnorm(9), 3, 3))
#' GramSchmidt(A[1, ], A[2, ], A[3, ])
#' 
#' # Keep the middle row unchanged
#' print(A <- matrix(c(rnorm(2), 0, 1, 0, 0, rnorm(3)), 3, 3, byrow = TRUE))
#' GramSchmidt(A[1, ], A[2, ], A[3, ], order = c(2, 1, 3))
#' 
GramSchmidt <- function(v1, v2, v3, order = 1:3) {
  A <- rbind(v1, v2, v3)
  A <- A[order, ]
  v1 <- A[1, ]
  v2 <- A[2, ]
  v3 <- A[3, ]
  if (isTRUE(all.equal(as.numeric(v1), c(0, 0, 0)))) v1 <- xprod(v2, v3)
  v1 <- normalize(v1)
  v2 <- v2 - sum(v2 * v1) * v1
  if (isTRUE(all.equal(as.numeric(v2), c(0, 0, 0)))) v2 <- xprod(v3, v1)
  v2 <- normalize(v2)
  v3 <- v3 - sum(v3 * v1) * v1 - sum(v3 * v2) * v2
  if (isTRUE(all.equal(as.numeric(v3), c(0, 0, 0)))) v3 <- xprod(v1, v2)
  v3 <- normalize(v3)
  rbind(v1, v2, v3)[order(order), ]
}



#' Create cylindrical or "tube" plots.
#' 
#' This function converts a description of a space curve into a
#' \code{\link[=mesh3d]{"mesh3d"}} object forming a cylindrical tube around the
#' curve.
#' 
#' 
#' The number of points in the space curve is determined by the vector lengths
#' in \code{center}, after using \code{\link{xyz.coords}} to convert it to a
#' list.  The other arguments \code{radius}, \code{twist}, \code{e1},
#' \code{e2}, and \code{e3} are extended to the same length.
#' 
#' The \code{closed} argument controls how the ends of the cylinder are
#' handled. If \code{closed > 0}, it represents the number of points of overlap
#' in the coordinates.  \code{closed == TRUE} is the same as \code{closed = 1}.
#' If \code{closed > 0} but the ends don't actually match, a warning will be
#' given and results will be somewhat unpredictable.
#' 
#' Negative values of \code{closed} indicate that caps should be put on the
#' ends of the cylinder.  If \code{closed == -1}, a cap will be put on the end
#' corresponding to \code{center[1, ]}.  If \code{closed == -2}, caps will be
#' put on both ends.
#' 
#' If \code{section} is \code{NULL} (the default), a regular \code{sides}-sided
#' polygon is used, and \code{radius} measures the distance from the center of
#' the cylinder to each vertex.  If not \code{NULL}, \code{sides} is ignored
#' (and set internally to \code{nrow(section)}), and \code{radius} is used as a
#' multiplier to the vertex coordinates.  \code{twist} specifies the rotation
#' of the polygon.  Both \code{radius} and \code{twist} may be vectors, with
#' values recycled to the number of rows in \code{center}, while \code{sides}
#' and \code{section} are the same at every point along the curve.
#' 
#' The three optional arguments \code{e1}, \code{e2}, and \code{e3} determine
#' the local coordinate system used to create the vertices at each point in
#' \code{center}.  If missing, they are computed by simple numerical
#' approximations.  \code{e1} should be the tangent coordinate, giving the
#' direction of the curve at the point.  The cross-section of the polygon will
#' be orthogonal to \code{e1}. When \code{rotationMinimizing} is \code{TRUE},
#' \code{e2} and \code{e3} are chosen to give a rotation minimizing frame (see
#' Wang et al., 2008).  When it is \code{FALSE}, \code{e2} defaults to an
#' approximation to the normal or curvature vector; it is used as the image of
#' the \code{y} axis of the polygon cross-section.  \code{e3} defaults to an
#' approximation to the binormal vector, to which the \code{x} axis of the
#' polygon maps.  The vectors are orthogonalized and normalized at each point.
#' 
#' @param center An n by 3 matrix whose columns are the x, y and z coordinates
#' of the space curve.
#' @param radius The radius of the cross-section of the tube at each point in
#' the center.
#' @param twist The amount by which the polygon forming the tube is twisted at
#' each point.
#' @param e1,e2,e3 The local coordinates to use at each point on the space
#' curve.  These default to a rotation minimizing frame or Frenet coordinates.
#' @param sides The number of sides in the polygon cross section.
#' @param section The polygon cross section as a two-column matrix, or
#' \code{NULL}.
#' @param closed Whether to treat the first and last points of the space curve
#' as identical, and close the curve, or put caps on the ends.  See the
#' Details.
#' @param rotationMinimizing Use a rotation minimizing local frame if
#' \code{TRUE}, or a Frenet or user-specified frame if \code{FALSE}.
#' @param debug If \code{TRUE}, plot the local Frenet coordinates at each
#' point.
#' @param keepVars If \code{TRUE}, return the local variables in attribute
#' \code{"vars"}.
#' @return A \code{\link[=mesh3d]{"mesh3d"}} object holding the cylinder,
#' possibly with attribute \code{"vars"} containing the local environment of
#' the function.
#' @author Duncan Murdoch
#' @references Wang, W., Jüttler, B., Zheng, D. and Liu, Y. (2008).
#' Computation of rotation minimizing frames.  ACM Transactions on Graphics,
#' Vol. 27, No. 1, Article 2.
#' @keywords dynamic
#' @examples
#' 
#' # A trefoil knot
#' open3d()
#' theta <- seq(0, 2*pi, len = 25)
#' knot <- cylinder3d(
#'       center = cbind(
#'         sin(theta) + 2*sin(2*theta), 
#'         2*sin(3*theta), 
#'         cos(theta) - 2*cos(2*theta)),
#'       e1 = cbind(
#'         cos(theta) + 4*cos(2*theta), 
#'         6*cos(3*theta), 
#'         sin(theta) + 4*sin(2*theta)),
#'       radius = 0.8, 
#'       closed = TRUE)
#'                      
#' shade3d(addNormals(subdivision3d(knot, depth = 2)), col = "green")  
#' 
cylinder3d <- function(center, radius = 1, twist = 0, e1 = NULL, e2 = NULL, e3 = NULL,
                       sides = 8, section = NULL, closed = 0,
                       rotationMinimizing = is.null(e2) && is.null(e3),
                       debug = FALSE, keepVars = FALSE) {
  force(rotationMinimizing) # Need this since e2 and e3 get changed later
  center <- as.matrix(as.data.frame(xyz.coords(center)[c("x", "y", "z")]))
  n <- nrow(center)
  inds <- seq_len(n)
  if (closed > 0) {
    ind0 <- c(n - 1 - closed, n - closed, inds)
    ind1 <- c(n - closed, inds, 1 + closed)
    ind2 <- c(inds, 1 + closed, 2 + closed)
  } else {
    ind0 <- c(1, 1, inds)
    ind1 <- c(1, inds, n)
    ind2 <- c(inds, n, n)
  }

  missings <- c(e1 = is.null(e1), e2 = is.null(e2), e3 = is.null(e3))

  fixup <- function(coord) {
    usable <- apply(coord, 1, function(v) all(is.finite(v)) & (veclen(v) > 0))
    if (!any(usable)) {
      stop(gettextf(
        "No usable coordinate values in '%s'",
        deparse(substitute(coord))
      ), domain = NA)
    }
    firstgood <- min(which(usable))
    inds <- seq_len(n)
    if (firstgood > 1) {
      coord[inds[inds < firstgood], ] <- coord[rep(firstgood, firstgood - 1), ]
      usable[seq_len(firstgood)] <- TRUE
    }
    for (i in inds[-1]) inds[i] <- ifelse(usable[i], inds[i], inds[i - 1])
    coord[inds, ]
  }

  if (!is.null(e1)) {
    e1 <- as.matrix(as.data.frame(xyz.coords(e1)[c("x", "y", "z")]))
    e1 <- e1[rep(seq_len(nrow(e1)), len = n), ]
  } else { # if (n < 5)
    e1 <- (center[ind2, ] - center[ind0, ])[inds, ]
  }
  # else { # Use high order approximation from Wang et al
  #   i <- 3:(n-2)
  #   e1 <- rbind(-25*center[1,] + 48*center[2,] - 36*center[3,]
  #   	        +16*center[4,] - 3*center[5,],
  #   	        - 3*center[1,] - 10*center[2,] + 18*center[3,]
  #   	        - 6*center[4,] + center[5,],
  #   	            center[i-2,] - 8*center[i-1,]
  #   	        + 8*center[i+1,] -   center[i+2,],
  #   	          3*center[n,] + 10*center[n-1,] - 18*center[n-2,]
  #   	        + 6*center[n-3,] - center[n-4,],
  #   	         25*center[n,] - 48*center[n-1,] + 36*center[n-2,]
  #   	        -16*center[n-3,] + 3*center[n-4,])
  # }

  # Fix up degenerate cases by repeating existing ones, or using arbitrary ones
  zeros <- rowSums(e1^2) == 0
  if (all(zeros)) {
    e1[, 1] <- 1
    zeros <- FALSE
  } else if (any(zeros)) {
    e1[1, ] <- e1[which(!zeros)[1], ]
    zeros[1] <- FALSE
    if (any(zeros)) {
      zeros <- which(zeros)
      for (i in zeros) {
        e1[i, ] <- e1[i - 1, ]
      }
    }
  }
  if (!is.null(e2)) {
    e2 <- as.matrix(as.data.frame(xyz.coords(e2)[c("x", "y", "z")]))
    e2 <- e2[rep(seq_len(nrow(e2)), len = n), ]
  } else {
    e2 <- (e1[ind2, ] - e1[ind0, ])[inds, ]
  }

  # Fix up degenerate e2's similarly, then force different than e1
  zeros <- rowSums(e2^2) == 0
  if (all(zeros)) {
    e2[, 2] <- 1
    zeros <- FALSE
  } else if (any(zeros)) {
    e2[1, ] <- e2[which(!zeros)[1], ]
    zeros[1] <- FALSE
    if (any(zeros)) {
      zeros <- which(zeros)
      for (i in zeros) {
        e2[i, ] <- e2[i - 1, ]
      }
    }
  }
  parallel <- sapply(inds, function(i) all(xprod(e1[i, ], e2[i, ]) == 0))
  if (any(parallel)) {
    # rotate in the xy plane
    e2[parallel, ] <- cbind(-e2[parallel, 2], e2[parallel, 1], e2[parallel, 3])
    parallel <- sapply(inds, function(i) all(xprod(e1[i, ], e2[i, ]) == 0))
    if (any(parallel)) {
      # if any are still parallel, they must be the z axis
      e2[parallel, 1] <- 1
      e2[parallel, 3] <- 0
    }
  }

  if (!is.null(e3)) {
    e3 <- as.matrix(as.data.frame(xyz.coords(e3)[c("x", "y", "z")]))
    e3 <- e3[rep(seq_len(nrow(e3)), len = n), ]
  } else {
    e3 <- matrix(NA_real_, n, 3)
    for (i in inds) e3[i, ] <- xprod(e1[i, ], e2[i, ])
  }

  for (i in inds) {
    A <- GramSchmidt(e1[i, ], e2[i, ], e3[i, ], order = order(missings))
    e1[i, ] <- A[1, ]
    e2[i, ] <- A[2, ]
    e3[i, ] <- A[3, ]
  }
  e1 <- fixup(e1)
  e2 <- fixup(e2)
  e3 <- fixup(e3)

  if (rotationMinimizing) {
    # Keep e1 and the first entries in e2 and e3,
    # modify the rest according to Wang et al (2008).
    t <- e1
    r <- e2
    s <- e3
    for (i in seq_len(n - 1)) {
      v1 <- center[i + 1, ] - center[i, ]
      c1 <- sum(v1^2)
      rL <- r[i, ] - (2 / c1) * sum(v1 * r[i, ]) * v1
      tL <- t[i, ] - (2 / c1) * sum(v1 * t[i, ]) * v1
      v2 <- t[i + 1, ] - tL
      c2 <- sum(v2^2)
      r[i + 1, ] <- rL - (2 / c2) * sum(v2 * rL) * v2
      s[i + 1, ] <- xprod(t[i + 1, ], r[i + 1, ])
    }
    e2 <- r
    e3 <- s
  }

  radius <- rep(radius, len = n)
  twist <- rep(twist, len = n)

  if (debug) {
    for (i in inds) {
      segments3d(rbind(
        center[i, ], center[i, ] + e3[i, ] * radius[i] * 1.5,
        center[i, ], center[i, ] + e2[i, ] * radius[i] * 1.5,
        center[i, ], center[i, ] + e1[i, ] * radius[i] * 1.5
      ),
      col = rep(c("red", "green", "blue"), each = 2)
      )
      text3d(center, texts = inds)
    }
  }

  if (closed > 0) {
    n <- n - closed + 1
    if (isTRUE(all.equal(e1[1, ], e1[n, ]))) {
      # Need to match starting and ending e2 as
      # well.
      angle <- atan2(
        sum(xprod(e2[1, ], e2[n, ]) * e1[1, ]),
        sum(e2[1, ] * e2[n, ])
      )
      twist <- twist + (twist[n] - twist[1] - angle) * (seq_len(n) - 1) / (n - 1)
    }
  }
  if (is.null(section)) {
    theta <- seq(0, 2 * pi, len = sides + 1)[-1]
    section <- cbind(cos(theta), sin(theta), 0)
  } else {
    sides <- nrow(section)
  }

  vertices <- matrix(0, 3, sides * n)
  indices <- matrix(0, 4, sides * (n - 1))

  if (ncol(section) == 2) {
    section <- cbind(section, 0)
  }

  for (i in seq_len(n - 1)) {
    transform <- rbind(e3[i, ], e2[i, ], e1[i, ])
    p <- rotate3d(section, twist[i], 0, 0, 1)
    p <- radius[i] * p %*% transform
    p[, 1] <- p[, 1] + center[i, "x"]
    p[, 2] <- p[, 2] + center[i, "y"]
    p[, 3] <- p[, 3] + center[i, "z"]
    vertices[, (i - 1) * sides + seq_len(sides)] <- t(p)
    for (j in seq_len(sides)) {
      indices[, (i - 1) * sides + j] <- (c(0, 0, 1, 1) + j) %% sides + 1 +
        c((i - 1) * sides, i * sides, i * sides, (i - 1) * sides)
    }
  }
  transform <- rbind(e3[n, ], e2[n, ], e1[n, ])
  p <- rotate3d(section, twist[n], 0, 0, 1)
  p <- radius[n] * p %*% transform
  p[, 1] <- p[, 1] + center[n, "x"]
  p[, 2] <- p[, 2] + center[n, "y"]
  p[, 3] <- p[, 3] + center[n, "z"]
  vertices[, (n - 1) * sides + seq_len(sides)] <- t(p)
  # Add end cap at start
  if (closed < 0) {
    vertices <- cbind(vertices, center[1, ])
    triangles <- rbind(ncol(vertices), seq_len(sides), c(2:sides, 1))
  }
  # Add end cap at end
  if (closed < -1) {
    vertices <- cbind(vertices, center[n, ])
    triangles <- cbind(triangles, rbind(
      ncol(vertices), c(2:sides, 1) + (n - 1) * sides,
      seq_len(sides) + (n - 1) * sides
    ))
  }

  result <- qmesh3d(vertices, indices, homogeneous = FALSE)
  if (closed > 0) { # Look for repeated vertices, and edit the links
    nv <- ncol(result$vb)
    for (i in seq_len(sides)) {
      dupe <- which(apply(
        result$vb[, (nv - sides + 1):nv, drop = FALSE], 2,
        function(x) isTRUE(all.equal(x, result$vb[, i]))
      )) + nv - sides
      for (j in dupe) {
        f <- result$ib == j
        result$ib[f] <- i
      }
    }
  } else if (closed < 0) {
    result$it <- triangles
  }

  if (keepVars) {
    attr(result, "vars") <- environment()
  }
  result
}
