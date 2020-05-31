# Support for objects from alphashape3d package

persp3d.ashape3d <- function(x, ..., add = FALSE) {
  plot3d(as.mesh3d(x, ...), add = add, ...)
}

plot3d.ashape3d <- function(x, ...) persp3d(x, ...)

reOrient <- function(vertices) {
  warned <- FALSE

  # Count how many other triangles touch each edge of this one, in order 2-3, 1-3, 1-2:
  edgeCounts <- function(index) {
    triangle <- vertices[, index]
    result <- integer(3)
    for (i in 1:3) {
      result[i] <- sum(apply(vertices, 2, function(col) all(triangle[-i] %in% col)))
    }
    result - 1
  }
  polys <- ncol(vertices)
  verts <- nrow(vertices)
  cols <- col(vertices)
  fixed <- 0L
  for (i in seq_len(polys - 1)) {
    fixed <- max(i, fixed)
    # Get all polygons touching polygon i
    thistriangle <- vertices[, i]
    if (any(is.na(thistriangle))) next
    touches <- which(matrix(vertices %in% thistriangle, nrow = 3), arr.ind = TRUE)
    if (!nrow(touches)) next
    counts <- table(touches[, 2L])
    # Get col number of all polygons sharing an edge with i
    shared <- as.numeric(names(counts)[counts > 1L])
    shared <- shared[shared != i]
    if (!length(shared)) next
    otherverts <- vertices[, shared, drop = FALSE]

    # FIXME:  otherverts may include multiple triangles sharing the
    #         same edge, because ashape3d sometimes embeds
    #         tetrahedrons (or larger polyhedra?) in the surfaces
    #         it produces.  It doesn't appear to be safe to just
    #         delete these

    if (!warned && length(shared) > 1L && any(edgeCounts(i) > 1)) {
      warning("Surface may not be simple; smoothing may not be possible.")
      warned <- TRUE
    }

    shared <- shared[shared > fixed]
    if (!length(shared)) next

    otherverts <- vertices[, shared, drop = FALSE]

    for (m in seq_len(ncol(otherverts))) { # m is intersection number
      # For each vertex in i, see if it is in the shared one, and
      # if they have opposite orientations as we need
      for (j in seq_len(verts)) {
        # Where is j in the others?
        jother <- which(otherverts[, m] == vertices[j, i])
        if (!length(jother)) next
        k <- j %% verts + 1L # k follows j
        kother <- jother %% verts + 1L # kother is entry following jother
        if (vertices[k, i] == otherverts[kother, m]) {
          otherverts[, m] <- rev(otherverts[, m])
          break
        }
      }
    }
    # Now move all of shared to the front
    unshared <- (fixed + 1L):max(shared)
    unshared <- unshared[!(unshared %in% shared)]
    if (length(unshared)) {
      vertices[, (fixed + length(shared) + 1L):max(shared)] <- vertices[, unshared]
    }
    vertices[, fixed + seq_along(shared)] <- otherverts
    fixed <- fixed + length(shared)
  }
  vertices
}



#' Convert alpha-shape surface of a cloud of points to mesh3d object.
#' 
#' The \code{\link[alphashape3d:ashape3d]{alphashape3d::ashape3d}} function
#' computes the 3D \eqn{\alpha}-shape of a cloud of points. This is an
#' approximation to the visual outline of the cloud.  It may include isolated
#' points, line segments, and triangular faces: this function converts the
#' triangular faces to an \pkg{rgl} \code{\link{tmesh3d}} object.
#' 
#' Edelsbrunner and Mucke's (1994) \eqn{\alpha}-shape algorithm is intended to
#' compute a surface of a general cloud of points. Unlike the convex hull, the
#' cloud may have voids, isolated points, and other oddities.  This function is
#' designed to work in the case where the surface is made up of simple
#' polygons.
#' 
#' If \code{smooth = TRUE}, this method attempts to orient all of the triangles
#' in the surface consistently and add normals at each vertex by averaging the
#' triangle normals.  However, for some point clouds, the \eqn{\alpha}-shape
#' will contain sheets of polygons with a few solid polyhedra embedded. This
#' does not allow a consistent definition of "inside" and outside.  If this is
#' detected, a warning is issued and the resulting mesh will likely contain
#' boundaries where the assumed orientation of triangles changes, resulting in
#' ugly dark lines through the shape.  Larger values of \code{alpha} in the
#' call to \code{\link[alphashape3d:ashape3d]{alphashape3d::ashape3d}} may
#' help.
#' 
#' Methods for \code{\link{plot3d}} and \code{\link{persp3d}} are also defined:
#' they call the \code{\link{as.mesh3d}} method and then plot the result.
#' 
#' @param x An object of class \code{"ashape3d"}.
#' @param alpha Which \code{alpha} value stored in \code{x} should be
#' converted?
#' @param tri_to_keep Which triangles to keep.  Expert use only: see
#' \code{triang} entry in \bold{Value} section of \link[alphashape3d]{ashape3d}
#' for details.
#' @param col The surface colour.
#' @param smooth Whether to attempt to add normals to make the surface look
#' smooth.  See the Details below.
#' @param normals,texcoords Normals and texture coordinates at each vertex can
#' be specified.
#' @param \dots Additional arguments to pass to use as \code{\link{material3d}}
#' properties on the resulting mesh.
#' @return A \code{"mesh3d"} object, suitable for plotting.
#' @author Duncan Murdoch
#' @references Edelsbrunner, H., Mucke, E. P. (1994). Three-Dimensional Alpha
#' Shapes. ACM Transactions on Graphics, 13(1), pp.43-72.
#' 
#' Lafarge, T. and Pateiro-Lopez, B. (2017).  alphashape3d: Implementation of
#' the 3D Alpha-Shape for the Reconstruction of 3D Sets from a Point Cloud.  R
#' package version 1.3.
#' @examples
#' 
#' if (requireNamespace("alphashape3d", quietly = TRUE)) {
#'   set.seed(123)
#'   n <- 400    # 1000 gives a nicer result, but takes longer
#'   xyz <- rbind(cbind(runif(n), runif(n), runif(n)),
#'                cbind(runif(n/8, 1, 1.5), 
#'                      runif(n/8, 0.25, 0.75), 
#'                      runif(n/8, 0.25, 0.75)))
#'   ash <- suppressMessages(alphashape3d::ashape3d(xyz, alpha = 0.2))
#'   m <- as.mesh3d(ash, smooth = TRUE)
#'   open3d()
#'   mfrow3d(1, 2, sharedMouse = TRUE)
#'   plot3d(xyz, size = 1)
#'   plot3d(m, col = "red", alpha = 0.5)
#'   points3d(xyz, size = 1)
#' }
#' 
as.mesh3d.ashape3d <- function(x, alpha = x$alpha[1], tri_to_keep = 2L,
                               col = "gray", smooth = FALSE,
                               normals = NULL, texcoords = NULL,
                               ...) {
  whichAlpha <- which(alpha == x$alpha)[1]
  if (!length(whichAlpha)) {
    stop("'alpha = ", alpha, "' not found in ", deparse(substitute(x)))
  }
  triangles <- x$triang
  keep <- triangles[, 8 + whichAlpha] %in% tri_to_keep
  triangs <- t(triangles[keep, 1:3])
  points <- t(x$x)
  if (!is.null(texcoords)) {
    texcoords <- texcoords[triangs, ]
  }
  material <- .getMaterialArgs(...)
  material$color <- col
  result <- tmesh3d(points, triangs,
    homogeneous = FALSE,
    normals = normals, texcoords = texcoords,
    material = material
  )
  if (smooth) {
    if (is.null(normals)) {
      result$it <- reOrient(result$it)
      result <- addNormals(result)
    } else {
      warning("smoothing ignored when 'normals' specified")
    }
  }
  result
}
