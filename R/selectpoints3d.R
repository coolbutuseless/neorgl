#' Select points from a scene
#' 
#' This function uses the \code{\link{select3d}} function to allow the user to
#' choose a point or region in the scene, then reports on all the vertices in
#' or near that selection.
#' 
#' The \code{multiple} argument may be a logical value or a function.  If
#' logical, it controls whether multiple selections will be performed.  If
#' \code{multiple} is \code{FALSE}, a single selection will be performed; it
#' might contain multiple points.  If \code{TRUE}, multiple selections will
#' occur and the results will be combined into a single matrix.
#' 
#' If \code{multiple} is a function, it should take a single argument. This
#' function will be called with the argument set to a matrix containing newly
#' added rows to the value, i.e.  it will contain coordinates of the newly
#' selected points (if \code{value = TRUE}), or the indices of the points (if
#' \code{value = FALSE}).  It should return a logical value, \code{TRUE} to
#' indicate that selection should continue, \code{FALSE} to indicate that it
#' should stop.
#' 
#' In either case, if multiple selections are being performed, the \code{ESC}
#' key will stop the process.
#' 
#' @param objects A vector of object id values to use for the search.
#' @param value If \code{TRUE}, return the coordinates of the points;
#' otherwise, return their indices.
#' @param closest If \code{TRUE}, return the points closest to the selection of
#' no points are exactly within it.
#' @param multiple If \code{TRUE} or a function, do multiple selections.  See
#' the Details below.
#' @param \dots Other parameters to pass to \code{\link{select3d}}.
#' @return If \code{value} is \code{TRUE}, a 3-column matrix giving the
#' coordinates of the selected points.  All rows in the matrix will be unique
#' even if multiple vertices have the same coordinates.
#' 
#' If \code{value} is \code{FALSE}, a 2-column matrix containing columns:
#' \item{id}{The object id containing the point.} \item{index}{The index of the
#' point within \code{\link{rgl.attrib}(id, "vertices")}. If multiple points
#' have the same coordinates, all indices will be returned.}
#' @author Duncan Murdoch
#' @seealso \code{\link{select3d}} to return a selection function.
#' @keywords graphics
#' @examples
#' 
#' xyz <- cbind(rnorm(20), rnorm(20), rnorm(20))
#' ids <- plot3d( xyz )
#' 
#' if (interactive()) {
#'   # Click near a point to select it and put a sphere there.
#'   # Press ESC to quit...
#' 
#'   # This version returns coordinates
#'   selectpoints3d(ids["data"], 
#'      multiple = function(x) {
#'         spheres3d(x, color = "red", alpha = 0.3, radius = 0.2)
#'         TRUE
#'      })
#' 
#'   # This one returns indices
#'   selectpoints3d(ids["data"], value = FALSE,
#'      multiple = function(ids) {
#'         spheres3d(xyz[ids[, "index"], , drop = FALSE], color = "blue", 
#'                   alpha = 0.3, radius = 0.2)
#'         TRUE
#'      })
#' }
#' 
selectpoints3d <- function(objects = rgl.ids()$id, value = TRUE, closest = TRUE,
                           multiple = FALSE, ...) {
  if (value) {
    result <- cbind(x = numeric(0), y = numeric(0), z = numeric(0))
  } else {
    result <- cbind(id = integer(0), index = integer(0))
  }
  rdist <- I

  first <- TRUE
  while (first || is.function(multiple) || multiple) {
    f <- select3d(...)
    if (is.null(f)) break

    e <- environment(f)

    dist <- Inf
    prev <- nrow(result)

    for (id in objects) {
      verts <- rgl.attrib(id, "vertices")
      hits <- f(verts)

      if (any(hits)) {
        dist <- 0
      } else if (closest && dist > 0 && nrow(verts)) {
        wincoords <- rgl.user2window(verts, projection = e$proj)
        wz <- wincoords[, 3]
        keep <- (0 <= wz) && (wz <= 1)
        wincoords <- wincoords[keep, , drop = FALSE]

        if (!nrow(wincoords)) next

        wx <- wincoords[, 1]
        xdist <- ifelse(wx < e$llx, (wx - e$llx)^2, ifelse(wx < e$urx, 0, (wx - e$urx)^2))

        wy <- wincoords[, 2]
        ydist <- ifelse(wy < e$lly, (wy - e$lly)^2, ifelse(wy < e$ury, 0, (wy - e$ury)^2))

        dists <- xdist + ydist
        hits <- (dists < dist) & (dists == min(dists))
        dist <- min(c(dist, dists))
      }

      if (!any(hits)) next

      if (prev && nrow(result) > prev && rdist > dist) {
        result <- result[seq_len(prev), , drop = FALSE]
      }

      if (value) {
        result <- rbind(result, verts[hits, ])
      } else {
        result <- rbind(result, cbind(id, which(hits)))
      }

      if (is.function(multiple) && nrow(result) > prev
      && !multiple(result[(prev + 1):nrow(result), , drop = FALSE])) {
        break
      }

      rdist <- dist

      first <- FALSE
    }

    if (value) {
      result <- unique(result)
    }
  }
  result
}
