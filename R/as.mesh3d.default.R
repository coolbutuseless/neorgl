as.mesh3d.default <- function(x, y = NULL, z = NULL,
                              triangles = length(x) %% 3 == 0,
                              smooth = FALSE,
                              tolerance = sqrt(.Machine$double.eps),
                              notEqual = NULL,
                              merge = TRUE,
                              ...) {
  if (missing(x)) {
    x <- rglId(rgl.ids()$id)
    return(as.mesh3d(x, ...))
  }
  xyz <- xyz.coords(x, y, z, recycle = TRUE)
  x <- xyz$x
  y <- xyz$y
  z <- xyz$z
  if (triangles) {
    stopifnot(length(x) %% 3 == 0)
  } else {
    stopifnot(length(x) %% 4 == 0)
  }
  nvert <- length(x)
  verts <- rbind(x, y, z)
  indices <- matrix(seq_along(x), nrow = if (triangles) 3 else 4)

  if (merge) {
    if (!is.null(notEqual)) {
      dim <- dim(notEqual)
      if (length(dim) != 2 || dim[1] != nvert || dim[2] != nvert) {
        stop("'notEqual' should be a ", nvert, " by ", nvert, " logical matrix.")
      }
      notEqual <- notEqual | t(notEqual) # Make it symmetric
    }
    o <- order(x, y, z)
    i1 <- seq_len(nvert)[o]
    for (i in seq_len(nvert)[-1]) {
      if (isTRUE(all.equal(verts[, i1[i - 1]], verts[, i1[i]], tolerance = tolerance))
      && (is.null(notEqual) || !isTRUE(notEqual[i1[i - 1], i1[i]]))) {
        indices[indices == i1[i]] <- i1[i - 1]
        notEqual[i1[i], ] <- notEqual[i1[i - 1], ] <- notEqual[i1[i], ] | notEqual[i1[i - 1], ]
        notEqual[, i1[i]] <- notEqual[, i1[i - 1]] <- notEqual[i1[i], ] | notEqual[, i1[i - 1]]
        i1[i] <- i1[i - 1]
      }
    }
    i1 <- sort(unique(i1))
    keep <- seq_along(i1)
    for (i in keep) {
      verts[, i] <- verts[, i1[i]]
      indices[indices == i1[i]] <- i
    }
  } else {
    keep <- seq_len(ncol(verts))
  }
  if (triangles) {
    mesh <- tmesh3d(verts[, keep], indices,
      homogeneous = FALSE,
      material = list(...)
    )
  } else {
    mesh <- qmesh3d(verts[, keep], indices,
      homogeneous = FALSE,
      material = list(...)
    )
  }
  if (smooth) {
    mesh <- addNormals(mesh)
  }
  mesh
}



#' Convert object in plot to mesh3d object.
#' 
#' This method attempts to read the attributes of objects in the rgl display
#' and construct a mesh3d object to approximate them.
#' 
#' This function attempts to construct a triangle mesh to approximate one or
#' more objects from the current display.  It can only handle objects of types
#' from \code{c("triangles", "quads", "planes", "surface")}.
#' 
#' Since this method only produces triangular meshes, they won't necessarily be
#' an exact match to the original object.
#' 
#' If the generic \code{\link{as.mesh3d}} is called with no \code{x} argument,
#' this method will be called with \code{x} set to the ids in the current
#' scene.
#' 
#' @param x A vector of rgl identifiers of objects in the specified subscene.
#' @param type A vector of names of types of shapes to convert.  Other shapes
#' will be ignored.
#' @param subscene Which subscene to look in; the default \code{NA} specifies
#' the current subscene.
#' @param \dots Ignored.
#' @return A triangular mesh object.
#' @author Duncan Murdoch
#' @seealso \code{\link{as.triangles3d.rglId}} for extracting the triangles,
#' \code{\link{clipMesh3d}} to apply complex clipping to a mesh object.
#' @examples
#' 
#' # volcano example taken from "persp"
#' #
#' data(volcano)
#' 
#' z <- 2 * volcano        # Exaggerate the relief
#' 
#' x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
#' y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)
#' 
#' zlim <- range(y)
#' zlen <- zlim[2] - zlim[1] + 1
#' 
#' colorlut <- terrain.colors(zlen) # height color lookup table
#' 
#' col <- colorlut[ z - zlim[1] + 1 ] # assign colors to heights for each point
#' 
#' open3d(useNULL = TRUE)
#' surface3d(x, y, z, color = col)
#' m <- as.mesh3d()
#' rgl.close()
#' 
#' open3d()
#' shade3d(m)
#' 
as.mesh3d.rglId <- function(x, type = NA, subscene = NA,
                            ...) {
  local_t <- function(x) {
    if (!is.null(x)) t(x)
  }
  ids <- rgl.ids(subscene = subscene)
  ids <- ids[ids$id %in% x, ]
  if (!is.na(type)) {
    ids <- ids[ids$type %in% type, ]
  }
  indices <- NULL
  vertices <- NULL
  normals <- NULL
  texcoords <- NULL
  material <- NULL
  for (i in seq_len(NROW(ids))) {
    id <- ids[i, "id"]
    verts <- rgl.attrib(id, "vertices")
    nvert <- NROW(verts)
    if (nvert) {
      type <- ids[i, "type"]
      inds <- switch(as.character(type),
        triangles = ,
        planes = matrix(1:nvert, nrow = 3),
        quads = {
          nquads <- nvert / 4
          matrix(4 * rep(seq_len(nquads) - 1, each = 6) + c(1, 2, 3, 1, 3, 4), nrow = 3)
        },
        surface = {
          dim <- rgl.attrib(id, "dim")
          ul <- rep(2:dim[1], dim[2] - 1) + dim[1] * rep(0:(dim[2] - 2), each = dim[1] - 1)
          if (rgl.attrib(id, "flags")["flipped", ]) {
            rbind(
              c(ul - 1, ul - 1 + dim[1]),
              c(ul, ul),
              c(ul - 1 + dim[1], ul + dim[1])
            )
          } else {
            rbind(
              c(ul, ul),
              c(ul - 1, ul - 1 + dim[1]),
              c(ul - 1 + dim[1], ul + dim[1])
            )
          }
        },
        NULL
      )
      if (NCOL(inds)) {
        indices <- cbind(indices, inds)
        vertices <- cbind(vertices, local_t(rgl.attrib(id, "vertices")))
        normals <- rbind(normals, rgl.attrib(id, "normals"))
        if (rgl.attrib.count(id, "texcoords")) {
          texcoords <- rbind(texcoords, rgl.attrib(id, "texcoords"))
        }
        mat <- rgl.getmaterial(nvert, id = id)
        if (is.null(material)) {
          material <- mat
        } else if (!isTRUE(all.equal(mat, material))) {
          warning("Only first material used")
        }
      }
    }
  }
  if (NCOL(vertices)) {
    tmesh3d(vertices, indices,
      homogeneous = FALSE, material = material,
      normals = normals, texcoords = texcoords
    )
  }
}



#' Merge duplicate vertices in mesh object
#' 
#' A mesh object can have the same vertex listed twice.  Each copy is allowed
#' to have separate normals, texture coordinates, and color. However, it is
#' more efficient to have just a single copy if those differences aren't
#' needed.  For automatic smoothing using \code{\link{addNormals}}, triangles
#' and quads need to share vertices. This function merges identical (or
#' similar) vertices to achieve this.
#' 
#' 
#' @param mesh A \code{\link{mesh3d}} object.
#' @param notEqual A logical matrix indicating that certain pairs should not be
#' merged even if they appear identical.
#' @param attribute Which attribute(s) should be considered in comparing
#' vertices?  A vector chosen from \code{c("vertices", "colors", "normals",
#' "texcoords"))}
#' @param tolerance When comparing vertices using \code{\link{all.equal}}, this
#' tolerance will be used to ignore rounding error.
#' @return A new mesh object.
#' @author Duncan Murdoch
#' @seealso \code{\link{as.mesh3d.rglId}}, which often constructs mesh objects
#' containing a lot of duplication.
#' @examples
#' 
#' (mesh1 <- cube3d())
#' id <- shade3d(mesh1, col = rainbow(6), meshColor = "face")
#' (mesh2 <- as.mesh3d(id))
#' (mesh3 <- mergeVertices(mesh2))
#' 
mergeVertices <- function(mesh, notEqual = NULL, attribute = "vertices",
                          tolerance = sqrt(.Machine$double.eps)) {
  nvert <- ncol(mesh$vb)
  if (!is.null(notEqual)) {
    dim <- dim(notEqual)
    if (length(dim) != 2 || dim[1] != nvert || dim[2] != nvert) {
      stop("'notEqual' should be a ", nvert, " by ", nvert, " logical matrix.")
    }
    notEqual <- notEqual | t(notEqual) # Make it symmetric
  }
  attribute <- match.arg(attribute,
    choices = c("vertices", "colors", "normals", "texcoords"),
    several.ok = TRUE
  )
  verts <- matrix(numeric(), ncol = 0, nrow = nvert)
  if ("vertices" %in% attribute) {
    verts <- cbind(
      mesh$vb[1, ] / mesh$vb[4, ],
      mesh$vb[2, ] / mesh$vb[4, ],
      mesh$vb[3, ] / mesh$vb[4, ]
    )
  }
  if (!is.null(normals <- mesh$normals) && "normals" %in% attribute) {
    if (nrow(normals) == 3) {
      verts <- cbind(verts, t(normals))
    } else {
      verts <- cbind(
        verts, normals[1, ] / normals[4, ],
        normals[2, ] / normals[4, ],
        normals[3, ] / normals[4, ]
      )
    }
  }
  colors <- NULL
  if (!is.null(mesh$material) && !is.null(colors <- mesh$material$color)
  && "colors" %in% attribute) {
    verts <- cbind(verts, t(col2rgb(colors)))
  }
  if (!is.null(texcoords <- mesh$texcoords) && "texcoords" %in% attribute) {
    verts <- cbind(verts, t(texcoords))
  }

  o <- do.call(order, as.data.frame(verts))
  indices <- c(mesh$it, mesh$ib)
  i1 <- seq_len(nvert)[o]
  for (i in seq_len(nvert)[-1]) {
    if (isTRUE(all.equal(verts[i1[i - 1], ], verts[i1[i], ], tolerance = tolerance))
    && (is.null(notEqual) || !isTRUE(notEqual[i1[i - 1], i1[i]]))) {
      notEqual[i1[i], ] <- notEqual[i1[i - 1], ] <- notEqual[i1[i], ] | notEqual[i1[i - 1], ]
      notEqual[, i1[i]] <- notEqual[, i1[i - 1]] <- notEqual[i1[i], ] | notEqual[, i1[i - 1]]
      i1[i] <- i1[i - 1]
    }
  }
  indices <- i1[order(o)][indices]
  keep <- sort(unique(i1))
  newvals <- numeric(nvert)
  newvals[keep] <- seq_along(keep)
  indices <- newvals[indices]
  mesh$vb <- mesh$vb[, keep]
  if (!is.null(normals)) {
    mesh$normals <- normals[, keep]
  }
  if (!is.null(texcoords)) {
    mesh$texcoords <- texcoords[, keep]
  }
  if (!is.null(colors)) {
    mesh$material$color <- colors[keep]
  }

  if (ntri <- length(mesh$it)) {
    mesh$it <- matrix(indices[seq_len(ntri)], nrow = 3)
  }
  if (length(mesh$ib)) {
    mesh$ib <- matrix(indices[seq_len(length(indices) - ntri) + ntri], nrow = 4)
  }
  mesh
}
