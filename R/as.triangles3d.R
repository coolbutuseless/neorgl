#' Convert an object to triangles.
#' 
#' This generic and its methods extract or creates a matrix of coordinates of
#' triangles from an object, suitable for passing to \code{\link{triangles3d}}.
#' 
#' The method for \code{"rglId"} objects can extract several different
#' attributes, organizing them as it would organize the vertices for the
#' triangles.
#' 
#' @aliases as.triangles3d as.triangles3d.rglId
#' @param obj The object to convert.
#' @param attribute Which attribute of an rgl object to extract?
#' @param subscene Which subscene is this object in?
#' @param \dots Additional arguments used by the methods.
#' @return An \code{n x 3} matrix containing the vertices of triangles making
#' up the object.  Each successive 3 rows of the matrix corresponds to a
#' triangle.
#' 
#' If the attribute doesn't exist, \code{NULL} will be returned.
#' @author Duncan Murdoch
#' @seealso \code{\link{as.mesh3d}} to also capture material properties.
#' @examples
#' 
#' open3d()
#' x <- surface3d(x = 1:10, y = 1:10, z = rnorm(100), col = "red")
#' tri <- as.triangles3d(x)
#' open3d()
#' triangles3d(tri, col = "blue")
#' 
as.triangles3d <- function(obj, ...) {
  UseMethod("as.triangles3d")
}

as.triangles3d.mesh3d <- function(obj, attribute = c(
                                    "vertices", "normals", "texcoords",
                                    "colors"
                                  ),
                                  ...) {
  indices <- NULL
  if (!is.null(obj$it)) {
    indices <- c(obj$it)
  }
  if (!is.null(obj$ib)) {
    indices <- c(indices, c(obj$ib[1:3, ], obj$ib[c(1, 3, 4), ]))
  }
  if (!is.null(indices)) {
    switch(match.arg(attribute),
      vertices = t(obj$vb[1:3, indices]) / obj$vb[4, indices],
      normals = if (!is.null(obj$normals)) {
        if (nrow(obj$normals) == 4) {
          t(obj$normals[1:3, indices] / obj$normals[4, indices])
        } else {
          t(obj$normals[, indices])
        }
      },
      texcoords = if (!is.null(obj$texcoords)) {
        t(obj$texcoords[, indices])
      },
      colors = if (!is.null(obj$material) && !is.null(obj$material$color)) {
        col <- t(col2rgb(rep(obj$material$color, length.out = max(indices))))
        alpha <- if (is.null(obj$material$alpha)) {
          1
        } else {
          obj$material$alpha
        }
        alpha <- rep(alpha, length.out = max(indices))
        cbind(col, alpha)[indices, ]
      }
    )
  }
}

as.triangles3d.rglId <- function(obj,
                                 attribute = c(
                                   "vertices", "normals", "texcoords",
                                   "colors"
                                 ),
                                 subscene = NA,
                                 ...) {
  attribute <- match.arg(attribute)
  ids <- rgl.ids(subscene = subscene)
  ids <- ids[ids$id %in% obj, ]
  result <- NULL
  for (i in seq_len(nrow(ids))) {
    id <- ids[i, "id"]
    nvert <- rgl.attrib.count(id, "vertices")
    attrib <- rgl.attrib(id, attribute)
    if (nrow(attrib)) {
      if (nrow(attrib) < nvert) {
        attrib <- apply(attrib, 2, function(col) rep(col, len = nvert))
      }
      type <- ids[i, "type"]
      result <- rbind(result, switch(as.character(type),
        triangles = ,
        planes = attrib,
        quads = {
          nquads <- nrow(attrib) / 4
          attrib[4 * rep(seq_len(nquads) - 1, each = 6) + c(1, 2, 3, 1, 3, 4), , drop = FALSE]
        },
        surface = {
          dim <- rgl.attrib(id, "dim")
          ul <- rep(2:dim[1], dim[2] - 1) + dim[1] * rep(0:(dim[2] - 2), each = dim[1] - 1)
          if (rgl.attrib(id, "flags")["flipped", ]) {
            indices <- c(rbind(
              c(ul - 1, ul - 1 + dim[1]),
              c(ul, ul),
              c(ul - 1 + dim[1], ul + dim[1])
            ))
          } else {
            indices <- c(rbind(
              c(ul, ul),
              c(ul - 1, ul - 1 + dim[1]),
              c(ul - 1 + dim[1], ul + dim[1])
            ))
          }

          attrib[indices, , drop = FALSE]
        },
        NULL
      ))
    }
  }
  result
}
