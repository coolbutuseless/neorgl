#' Clip mesh to general region
#' 
#' Modifies a mesh3d object so that values of a function are bounded.
#' 
#' This function transforms a mesh3d object.
#' 
#' First, all quads are converted to triangles.
#' 
#' Next, each vertex is checked against the condition.  If \code{fn} is a
#' numeric vector, with one value per vertex, those values will be used in the
#' test. If it is a function, it will be passed a matrix, whose columns are the
#' specified attribute(s), with one row per vertex.  It should return a vector
#' of values, one per vertex, to check against the bound. The \code{"vertices"}
#' and \code{"normals"} values will be converted to Euclidean coordinates.
#' \code{"index"} will be an integer from 1 to the number of vertices.
#' 
#' Modifications to the triangles depend on how many of the vertices satisfy
#' the condition (\code{fn >= bound} or \code{fn <= bound}, depending on
#' \code{greater}) for inclusion. \itemize{ \item If no vertices in a triangle
#' satisfy the condition, the triangle is omitted. \item If one vertex
#' satisfies the condition, the other two vertices in that triangle are shrunk
#' towards it by assuming \code{fn} is locally linear. \item If two vertices
#' satisfy the condition, the third vertex is shrunk along each edge towards
#' each other vertex, forming a quadrilateral made of two new triangles. \item
#' If all vertices satisfy the condition, they are included with no
#' modifications. }
#' 
#' @param mesh A \code{\link{mesh3d}} object.
#' @param fn A function used to determine clipping, or a vector of values from
#' such a function, with one value per vertex in the mesh.
#' @param bound The value(s) of \code{fn} on the clipping boundary.
#' @param greater Logical; whether to keep \code{fn >= bound} or not.
#' @param attribute Which attribute(s) to pass to \code{fn}?  Possible values
#' are \code{c("vertices", "normals", "texcoords", "index")}.
#' @return A new mesh3d object in which all vertices (approximately) satisfy
#' the clipping condition.  Note that the order of vertices will likely differ
#' from the original order, and new vertices will be added near the boundary.
#' @author Duncan Murdoch
#' @references See \url{https://stackoverflow.com/q/56242470/2554330} for the
#' motivating example.
#' @examples
#' 
#' if (requireNamespace("misc3d")) {
#'   # Togliatti surface equation: f(x,y,z) = 0
#'   # Due to Stephane Laurent
#'   f <- function(x,y,z){
#'     w <- 1
#'     64*(x-w)*
#'       (x^4-4*x^3*w-10*x^2*y^2-4*x^2*w^2+16*x*w^3-20*x*y^2*w+5*y^4+16*w^4-20*y^2*w^2) -
#'       5*sqrt(5-sqrt(5))*(2*z-sqrt(5-sqrt(5))*w)*(4*(x^2+y^2-z^2)+(1+3*sqrt(5))*w^2)^2
#'   }
#'   # make grid
#'   # The original had 220 instead of 20, this is coarse to be quicker
#'   nx <- 20; ny <- 20; nz <- 20
#'   x <- seq(-5, 5, length=nx)
#'   y <- seq(-5, 5, length=ny)
#'   z <- seq(-4, 4, length=nz)
#'   g <- expand.grid(x=x, y=y, z=z)
#'   # calculate voxel
#'   voxel <- array(with(g, f(x,y,z)), dim = c(nx,ny,nz))
#'   
#'   # compute isosurface
#'   open3d(useNULL = TRUE)
#'   surf <- as.mesh3d(misc3d::contour3d(voxel, maxvol=max(voxel), level=0, x=x, y=y, z=z))
#'   rgl.close()
#'   
#'   surf$normals <- NULL
#'   surf <- mergeVertices(surf)
#'   surf <- addNormals(surf)
#'   
#'   fn <- function(x) {
#'     rowSums(x^2)
#'   }
#'   
#'   open3d()
#'   shade3d(clipMesh3d(surf, fn, bound = 4.8^2,
#'                      greater = FALSE), col="red")
#' }
#' 
clipMesh3d <- function(mesh, fn, bound = 0, greater = TRUE,
                       attribute = "vertices") {
  stopifnot(inherits(mesh, "mesh3d"))
  # First, convert quads to triangles
  if (!is.null(mesh$ib)) {
    nquads <- ncol(mesh$ib)
    mesh$it <- cbind(
      mesh$it,
      matrix(mesh$ib[rep(4 * (seq_len(nquads) - 1), each = 6) +
        rep(c(1, 2, 3, 1, 3, 4), nquads)], nrow = 3)
    )
    mesh$ib <- NULL
  }

  attribute <- match.arg(attribute, c("vertices", "normals", "texcoords", "index"), several.ok = TRUE)
  if (is.numeric(fn)) {
    values <- fn
  } else {
    arg <- NULL
    for (a in attribute) {
      arg <- cbind(
        arg,
        switch(a,
          vertices = t(mesh$vb[1:3, ]) / mesh$vb[4, ],
          normals = if (nrow(mesh$normals) == 4) {
            t(mesh$normals[1:3, ]) / mesh$normals[4, ]
          } else {
            t(mesh$normals)
          },
          texcoords = t(mesh$texcoords),
          index = seq_len(ncol(mesh$vb))
        )
      )
    }
    values <- fn(arg) - bound
  }
  if (!greater) {
    values <- -values
  }

  if (length(values) != ncol(mesh$vb)) {
    stop("'fn' should give one value per vertex")
  }

  if (anyNA(values)) {
    stop("'fn' should not include NA values")
  }

  # Now, set all w values to 1
  mesh$vb <- t(cbind(t(mesh$vb[1:3, ]) / mesh$vb[4, ], 1))
  if (!is.null(mesh$normals) && nrow(mesh$normals) == 4) {
    mesh$normals <- t(t(mesh$normals[1:3, ]) / mesh$normals[4, ])
  }

  newVertices <- integer()
  getNewVertex <- function(good, bad) {
    names <- paste0(good, "_", bad)
    new <- which(!(names %in% names(newVertices)))
    if (length(new)) {
      goodvals <- values[good[new]]
      badvals <- values[bad[new]]
      alphas <- goodvals / (goodvals - badvals)
      newverts <- t((1 - alphas) * t(mesh$vb[, good[new]]) + alphas * t(mesh$vb[, bad[new]]))
      newvertnums <- seq_len(ncol(newverts)) + ncol(mesh$vb)
      if (!is.null(mesh$normals)) {
        mesh$normals <<- cbind(mesh$normals, t((1 - alphas) * t(mesh$normals[, good[new]]) +
          alphas * t(mesh$normals[, bad[new]])))
      }
      if (!is.null(mesh$texcoords)) {
        mesh$texcoords <<- cbind(mesh$texcoords, t((1 - alphas) * t(mesh$texcoords[, good[new]]) +
          alphas * t(mesh$texcoords[, bad[new]])))
      }
      if (!is.null(mesh$material)) {
        if (!is.null(mesh$material$color) && length(mesh$material$color) == ncol(mesh$vb)) {
          rgb <- col2rgb(mesh$material$color)
          newrgb <- (1 - alphas) * rgb[, good[new]] + alphas * rgb[, bad[new]]
          mesh$material$color <<- c(
            mesh$material$color,
            rgb(newrgb["red", ], newrgb["green", ], newrgb["blue", ], maxColorValue = 255)
          )
        }
        if (!is.null(mesh$material$alpha) && length(mesh$material$alpha) == ncol(mesh$vb)) {
          mesh$material$alpha <<- c(
            mesh$material$alpha,
            (1 - alphas) * mesh$material$alpha[good[new]] +
              alphas * mesh$material$alpha[bad[new]]
          )
        }
      }
      mesh$vb <<- cbind(mesh$vb, newverts)
      newVertices[names[new]] <<- newvertnums
    }
    newVertices[names]
  }
  keep <- values >= 0
  keept <- matrix(keep[mesh$it], nrow = 3)
  counts <- colSums(keept) # Number of vertices to keep for each triangle
  singles <- which(counts == 1)
  if (length(singles)) {
    theseTriangles <- mesh$it[, singles, drop = FALSE]
    goodRow <- apply(keept[, singles, drop = FALSE], 2, function(col) which(col))
    allcols <- seq_len(ncol(theseTriangles))
    goodVertex <- theseTriangles[cbind(goodRow, allcols)]
    badVertex1 <- theseTriangles[cbind(goodRow %% 3 + 1, allcols)]
    badVertex2 <- theseTriangles[cbind((goodRow + 1) %% 3 + 1, allcols)]
    mesh$it[cbind(goodRow %% 3 + 1, singles)] <-
      getNewVertex(goodVertex, badVertex1)
    mesh$it[cbind((goodRow + 1) %% 3 + 1, singles)] <-
      getNewVertex(goodVertex, badVertex2)
  }
  doubles <- which(counts == 2)
  if (length(doubles)) {
    theseTriangles <- mesh$it[, doubles, drop = FALSE]
    badRow <- apply(keept[, doubles, drop = FALSE], 2, function(col) which(!col))
    allcols <- seq_len(ncol(theseTriangles))
    badVertex <- theseTriangles[cbind(badRow, allcols)]
    goodVertex1 <- theseTriangles[cbind(badRow %% 3 + 1, allcols)]
    goodVertex2 <- theseTriangles[cbind((badRow + 1) %% 3 + 1, allcols)]
    newVertex1 <- getNewVertex(goodVertex1, badVertex)
    newVertex2 <- getNewVertex(goodVertex2, badVertex)
    mesh$it[cbind(badRow, doubles)] <- newVertex1
    mesh$it <- cbind(mesh$it, rbind(newVertex1, goodVertex2, newVertex2))
  }
  zeros <- which(counts == 0)
  if (length(zeros)) {
    mesh$it <- mesh$it[, -zeros]
  }
  mesh
}
