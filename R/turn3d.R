#' Create a solid of rotation from a two-dimensional curve.
#' 
#' This function \dQuote{turns} the curve (as on a lathe) to form a solid of
#' rotation along the x axis.
#' 
#' 
#' @param x,y Points on the curve, in a form suitable for
#' \code{\link{xy.coords}}. The \code{y} values must be non-negative.
#' @param n How many steps in the rotation?
#' @param smooth logical; whether to add normals for a smooth appearance.
#' @param ...  Additional parameters to pass to \code{\link{tmesh3d}}.
#' @return A mesh object containing triangles and/or quadrilaterals.
#' @author Fang He and Duncan Murdoch
#' @seealso \code{\link{extrude3d}}
#' @keywords graphics
#' @examples
#' 
#' x <- 1:10
#' y <- rnorm(10)^2
#' shade3d(turn3d(x, y), col = "green")
#' 
turn3d <- function(x, y = NULL, n = 12, smooth = FALSE, ...) {
  xy <- xy.coords(x, y)
  x <- xy$x
  y <- zapsmall(xy$y)
  stopifnot(all(y >= 0))
  len <- length(x)
  inds <- seq_len(len)

  if (smooth) {
    nx <- -diff(y)
    nx <- c(nx, 0) + c(0, nx)
    ny <- diff(x)
    ny <- c(ny, 0) + c(0, ny)
    nlen <- sqrt(nx^2 + ny^2)
    nlen[nlen == 0] <- 1
    nx <- nx / nlen
    ny <- ny / nlen
    normals <- matrix(nrow = 4, ncol = 0)
  }
  zero <- y == 0
  vb <- matrix(nrow = 4, ncol = 0)
  ib <- matrix(nrow = 4, ncol = 0)
  it <- matrix(nrow = 3, ncol = 0)

  theta <- seq(0, 2 * pi, len = n + 1)[-(n + 1)]
  for (i in inds) {
    vb <- cbind(vb, rbind(x[i], sin(theta) * y[i], cos(theta) * y[i], 1))
    if (smooth) {
      normals <- cbind(normals, rbind(nx[i], sin(theta) * ny[i], cos(theta) * ny[i], 1))
    }
    if (i > 1) {
      if (zero[i] && zero[i - 1]) { # do nothing
      } else if (!zero[i] && zero[i - 1]) { # draw triangles
        prev <- ncol(vb) - n - 0:(n - 1)
        curr <- ncol(vb) - 0:(n - 1)
        curr2 <- curr + 1
        curr2[1] <- curr2[1] - n

        it <- cbind(it, rbind(prev, curr, curr2))
      } else if (zero[i] && !zero[i - 1]) { # other triangles
        prev <- ncol(vb) - n - 0:(n - 1)
        curr <- ncol(vb) - 0:(n - 1)
        prev2 <- prev + 1
        prev2[1] <- prev2[1] - n
        it <- cbind(it, rbind(prev, curr, prev2))
      } else { # quads
        prev <- ncol(vb) - n - 0:(n - 1)
        prev2 <- prev + 1
        prev2[1] <- prev2[1] - n
        curr <- ncol(vb) - 0:(n - 1)
        curr2 <- curr + 1
        curr2[1] <- curr2[1] - n
        ib <- cbind(ib, rbind(prev, curr, curr2, prev2))
      }
    }
  }

  result <- tmesh3d(vb, it, normals = if (smooth) t(normals), ...)
  if (length(ib)) result$ib <- ib
  result
}
