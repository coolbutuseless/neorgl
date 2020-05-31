##
## R source file
## This file is part of rgl
##
##

##
## ===[ SECTION: scene management ]===========================================
##


##
## clear scene
##
##

rgl.clear <- function(type = "shapes", subscene = 0) {
  if (is.na(subscene)) {
    subscene <- currentSubscene3d()
  }

  typeid <- rgl.enum.nodetype(type)

  userviewpoint <- 4 %in% typeid
  material <- 5 %in% typeid
  modelviewpoint <- 8 %in% typeid

  drop <- typeid %in% c(4:5, 8)
  typeid <- typeid[!drop]
  type <- names(typeid)

  if (subscene == 0) {
    idata <- as.integer(c(length(typeid), typeid))
    ret <- .C(rgl_clear,
      success = FALSE,
      idata
    )$success
  } else {
    sceneids <- rgl.ids(type = type, subscene = 0)$id
    thisids <- rgl.ids(type = type, subscene = subscene)$id
    if (length(thisids)) {
      delFromSubscene3d(ids = thisids, subscene = subscene)
      gc3d(protect = setdiff(sceneids, thisids))
    }
    ret <- 1
  }

  if (userviewpoint || modelviewpoint) {
    rgl.viewpoint(type = c("userviewpoint", "modelviewpoint")[c(userviewpoint, modelviewpoint)])
  }

  if (material) {
    rgl.material()
  }

  if (!ret) {
    stop("'rgl_clear' failed")
  }

  lowlevel()
}


##
## pop node
##
##

rgl.pop <- function(type = "shapes", id = 0) {
  type <- rgl.enum.nodetype(type)
  save <- par3d(skipRedraw = TRUE)
  on.exit(par3d(save))
  for (i in id) {
    idata <- as.integer(c(type, i))

    ret <- .C(rgl_pop,
      success = FALSE,
      idata
    )

    if (!ret$success) {
      stop(gettextf("'rgl.pop' failed for id %d", i), domain = NA)
    }
  }
  lowlevel()
}

rgl.ids <- function(type = "shapes", subscene = NA) {
  type <- c(rgl.enum.nodetype(type), 0)
  if (is.na(subscene)) {
    subscene <- currentSubscene3d()
  }

  count <- .C(rgl_id_count, as.integer(type), count = integer(1), subscene = as.integer(subscene))$count

  as.data.frame(.C(rgl_ids, as.integer(type),
    id = integer(count),
    type = rep("", count), subscene = as.integer(subscene)
  )[2:3])
}

rgl.attrib.count <- function(id, attrib) {
  stopifnot(length(attrib) == 1)
  if (is.character(attrib)) {
    attrib <- rgl.enum.attribtype(attrib)
  }

  result <- integer(length(id))
  for (i in seq_along(id)) {
    result[i] <- .C(rgl_attrib_count, as.integer(id[i]), as.integer(attrib),
      count = integer(1)
    )$count
  }
  names(result) <- names(id)
  result
}

rgl.attrib.ncol.values <- c(
  vertices = 3, normals = 3, colors = 4, texcoords = 2, dim = 2,
  texts = 1, cex = 1, adj = 2, radii = 1, centers = 3, ids = 1,
  usermatrix = 4, types = 1, flags = 1, offsets = 1,
  family = 1, font = 1, pos = 1
)



#' Get information about attributes of objects
#' 
#' These functions give information about the attributes of \pkg{rgl} objects.
#' \code{rgl.attrib.info} is the more \dQuote{user-friendly} function;
#' \code{rgl.attrib.count} is a lower-level function more likely to be used in
#' programming.
#' 
#' See the first example below to get the full list of attribute names.
#' 
#' @aliases rgl.attrib.info rgl.attrib.count
#' @param id One or more \pkg{rgl} object ids.
#' @param attribs A character vector of one or more attribute names.
#' @param showAll Should attributes with zero entries be shown?
#' @param attrib A single attribute name.
#' @return A dataframe containing the following columns: \item{id}{The id of
#' the object.} \item{attrib}{The full name of the attribute.} \item{nrow,
#' ncol}{The size of matrix that would be returned by \code{\link{rgl.attrib}}
#' for this attribute.}
#' @author Duncan Murdoch
#' @seealso \code{\link{rgl.attrib}} to obtain the attribute values.
#' @examples
#' 
#' open3d()
#' id <- points3d(rnorm(100), rnorm(100), rnorm(100), col = "green")
#' rgl.attrib.info(id, showAll = TRUE)
#' rgl.attrib.count(id, "vertices")
#' 
#' merge(rgl.attrib.info(), rgl.ids("all"))
#' 
rgl.attrib.info <- function(id = rgl.ids("all", 0)$id, attribs = NULL, showAll = FALSE) {
  ncol <- rgl.attrib.ncol.values
  if (is.null(attribs)) {
    attribs <- names(ncol)
  } else if (is.numeric(attribs)) {
    attribs <- names(ncol)[attribs]
  }
  na <- length(attribs)
  ni <- length(id)
  if (!ni) {
    result <- data.frame(
      id = numeric(0), attrib = character(0),
      nrow = numeric(0), ncol = numeric(0),
      stringsAsFactors = FALSE
    )
  } else {
    result <- data.frame(
      id = rep(id, each = na),
      attrib = rep(attribs, ni),
      nrow = 0,
      ncol = rep(ncol[attribs], ni),
      stringsAsFactors = FALSE
    )
  }
  for (j in seq_len(ni)) {
    for (i in seq_len(na)) {
      result$nrow[i + na * (j - 1)] <- rgl.attrib.count(id[j], result$attrib[i])
    }
  }
  if (!showAll) {
    result <- result[result$nrow != 0, ]
  }
  rownames(result) <- NULL
  result
}



#' Get information about shapes
#' 
#' Retrieves information about the shapes in a scene.
#' 
#' If the identifier is not found or is not a shape that has the given
#' attribute, zero will be returned by \code{rgl.attrib.count}, and an empty
#' matrix will be returned by \code{rgl.attrib}.
#' 
#' The first four \code{attrib} names correspond to the usual OpenGL
#' properties; \code{"dim"} is used just for surfaces, defining the rows and
#' columns in the rectangular grid; \code{"cex"}, \code{"adj"},
#' \code{"family"}, \code{"font"} and \code{"pos"} apply only to text objects.
#' 
#' @param id A shape identifier, as returned by \code{\link{rgl.ids}}.
#' @param attrib An attribute of a shape.  Currently supported: one of \cr
#' \code{"vertices"}, \code{"normals"}, \code{"colors"}, \code{"texcoords"},
#' \code{"dim"}, \code{"texts"}, \code{"cex"}, \code{"adj"}, \code{"radii"},
#' \code{"centers"}, \code{"ids"}, \code{"usermatrix"}, \code{"types"},
#' \code{"flags"}, \code{"offsets"}, \code{"family"}, \code{"font"},
#' \code{"pos"}\cr or unique prefixes to one of those.
#' @param first,last Specify these to retrieve only those rows of the result.
#' @return \code{rgl.attrib} returns the values of the attribute.  Attributes
#' are mostly real-valued, with the following sizes: \tabular{lll}{
#' \code{"vertices"} \tab 3 values \tab x, y, z \cr \code{"normals"} \tab 3
#' values \tab x, y, z \cr \code{"centers"} \tab 3 values \tab x, y, z \cr
#' \code{"colors"} \tab 4 values \tab r, g, b, a \cr \code{"texcoords"} \tab 2
#' values \tab s, t \cr \code{"dim"} \tab 2 values \tab r, c \cr \code{"cex"}
#' \tab 1 value \tab cex \cr \code{"adj"} \tab 2 values \tab x, y \cr
#' \code{"radii"} \tab 1 value \tab r \cr \code{"ids"} \tab 1 value \tab id \cr
#' \code{"usermatrix"} \tab 4 values \tab x, y, z, w \cr \code{"texts"} \tab 1
#' value \tab text \cr \code{"types"} \tab 1 value \tab type \cr \code{"flags"}
#' \tab 1 value \tab flag \cr \code{"family"} \tab 1 value \tab family \cr
#' \code{"font"} \tab 1 value \tab font \cr \code{"pos"} \tab 1 value \tab pos
#' \cr } The \code{"texts"}, \code{"types"} and \code{"family"} attributes are
#' character-valued; the \code{"flags"} attribute is logical valued, with named
#' rows.
#' 
#' These are returned as matrices with the row count equal to the count for the
#' attribute, and the columns as listed above.
#' @author Duncan Murdoch
#' @seealso \code{\link{rgl.ids}}, \code{\link{rgl.attrib.info}}
#' @keywords graphics
#' @examples
#' 
#' p <- plot3d(rnorm(100), rnorm(100), rnorm(100), type = "s", col = "red")
#' rgl.attrib(p["data"], "vertices", last = 10)
#' 
rgl.attrib <- function(id, attrib, first = 1,
                       last = rgl.attrib.count(id, attrib)) {
  stopifnot(length(attrib) == 1 && length(id) == 1 && length(first) == 1)
  if (is.character(attrib)) {
    attrib <- rgl.enum.attribtype(attrib)
  }
  ncol <- rgl.attrib.ncol.values[attrib]
  count <- max(last - first + 1, 0)
  if (attrib %in% c(6, 13, 16)) { # texts, types and family
    if (count) {
      result <- .C(rgl_text_attrib, as.integer(id), as.integer(attrib),
        as.integer(first - 1), as.integer(count),
        result = character(count * ncol)
      )$result
    } else {
      result <- character(0)
    }
  } else {
    if (count) {
      result <- .C(rgl_attrib, as.integer(id), as.integer(attrib),
        as.integer(first - 1), as.integer(count),
        result = numeric(count * ncol)
      )$result
    } else {
      result <- numeric(0)
    }
  }
  if (attrib == 14) {
    result <- as.logical(result)
  }
  result <- matrix(result, ncol = ncol, byrow = TRUE)
  colnames(result) <- list(
    c("x", "y", "z"), # vertices
    c("x", "y", "z"), # normals
    c("r", "g", "b", "a"), # colors
    c("s", "t"), # texcoords
    c("r", "c"), # dim
    c("text"), # texts
    c("cex"), # cex
    c("x", "y"), # adj
    "r", # radii
    c("x", "y", "z"), # centers
    "id", # ids
    c("x", "y", "z", "w"), # usermatrix
    "type", # types
    "flag", # flags
    "offset", # offsets
    "family", # family
    "font", # font
    "pos" # pos
  )[[attrib]]
  if (attrib == 14 && count) {
    if (id %in% rgl.ids("lights", subscene = 0)$id) {
      rownames(result) <- c("viewpoint", "finite")[first:last]
    } else if (id %in% rgl.ids("background", subscene = 0)$id) {
      rownames(result) <- c("sphere", "linear_fog", "exp_fog", "exp2_fog")[first:last]
    } else if (id %in% rgl.ids("bboxdeco", subscene = 0)$id) {
      rownames(result) <- "draw_front"[first:last]
    } else if (id %in% (ids <- rgl.ids("shapes", subscene = 0))$id) {
      type <- ids$type[ids$id == id]
      rownames(result) <- c(
        "ignoreExtent",
        if (type == "surface") {
          "flipped"
        } else {
          "fixedSize"
        }
      )[first:last]
    }
  }
  result
}

##
## ===[ SECTION: environment ]================================================
##



##
## set viewpoint
##
##

rgl.viewpoint <- function(theta = 0.0, phi = 15.0, fov = 60.0, zoom = 1.0, scale = par3d("scale"),
                          interactive = TRUE, userMatrix, type = c("userviewpoint", "modelviewpoint")) {
  zoom <- rgl.clamp(zoom, 0, Inf)
  phi <- rgl.clamp(phi, -90, 90)
  fov <- rgl.clamp(fov, 0, 179)

  type <- match.arg(type, several.ok = TRUE)

  polar <- missing(userMatrix)
  if (polar) userMatrix <- diag(4)

  idata <- as.integer(c(interactive, polar, "userviewpoint" %in% type, "modelviewpoint" %in% type))
  ddata <- as.numeric(c(theta, phi, fov, zoom, scale, userMatrix[1:16]))

  ret <- .C(rgl_viewpoint,
    success = FALSE,
    idata,
    ddata
  )

  if (!ret$success) {
    stop("'rgl_viewpoint' failed")
  }
}

##
## set background
##
##

rgl.bg <- function(sphere = FALSE, fogtype = "none", color = c("black", "white"), back = "lines", ...) {
  rgl.material(color = color, back = back, ...)

  fogtype <- rgl.enum.fogtype(fogtype)

  idata <- as.integer(c(sphere, fogtype))

  ret <- .C(rgl_bg,
    success = as.integer(FALSE),
    idata
  )

  if (!ret$success) {
    stop("'rgl_bg' failed")
  }

  lowlevel(ret$success)
}


##
## bbox
##
##



#' Set up Bounding Box decoration
#' 
#' Set up the bounding box decoration.
#' 
#' Four different types of tick mark layouts are possible. This description
#' applies to the X axis; other axes are similar: If \code{xat} is not
#' \code{NULL}, the ticks are set up at custom positions.  If \code{xunit} is
#' numeric but not zero, it defines the tick mark base.  If it is
#' \code{"pretty"} (the default in \code{bbox3d}), ticks are set at
#' \code{\link{pretty}} locations.  If \code{xlen} is not zero, it specifies
#' the number of ticks (a suggestion if \code{xunit} is \code{"pretty"}).
#' 
#' The first color specifies the bounding box, while the second one specifies
#' the tick mark and font color.
#' 
#' \code{bbox3d} defaults to \code{\link{pretty}} locations for the axis labels
#' and a slightly larger box, whereas \code{rgl.bbox} covers the exact range.
#' 
#' \code{\link{axes3d}} offers more flexibility in the specification of the
#' axes, but they are static, unlike those drawn by \code{\link{rgl.bbox}} and
#' \code{\link{bbox3d}}.
#' 
#' @aliases rgl.bbox bbox3d
#' @param xat,yat,zat vector specifying the tickmark positions
#' @param xlab,ylab,zlab character vector specifying the tickmark labeling
#' @param xunit,yunit,zunit value specifying the tick mark base for uniform
#' tick mark layout
#' @param xlen,ylen,zlen value specifying the number of tickmarks
#' @param marklen value specifying the length of the tickmarks
#' @param marklen.rel logical, if TRUE tick mark length is calculated using
#' 1/\code{marklen} * axis length, otherwise tick mark length is \code{marklen}
#' in coordinate space
#' @param expand value specifying how much to expand the bounding box around
#' the data
#' @param draw_front draw the front faces of the bounding box
#' @param ... Material properties (or other \code{rgl.bbox} parameters in the
#' case of \code{bbox3d}). See \code{\link{rgl.material}} for details.
#' @return This function is called for the side effect of setting the bounding
#' box decoration.  A shape ID is returned to allow \code{\link{rgl.pop}} to
#' delete it.
#' @seealso \code{\link{rgl.material}}, \code{\link{axes3d}}
#' @keywords dynamic
#' @examples
#' 
#'   rgl.open()
#'   rgl.points(rnorm(100), rnorm(100), rnorm(100))
#'   rgl.bbox(color = c("#333377", "white"), emission = "#333377", 
#'            specular = "#3333FF", shininess = 5, alpha = 0.8 )
#'   
#'   open3d()
#'   points3d(rnorm(100), rnorm(100), rnorm(100))
#'   bbox3d(color = c("#333377", "black"), emission = "#333377", 
#'          specular = "#3333FF", shininess = 5, alpha = 0.8)
#' 
rgl.bbox <- function(
                     xat = NULL, xlab = NULL, xunit = 0, xlen = 5,
                     yat = NULL, ylab = NULL, yunit = 0, ylen = 5,
                     zat = NULL, zlab = NULL, zunit = 0, zlen = 5,
                     marklen = 15.0, marklen.rel = TRUE, expand = 1, draw_front = FALSE,
                     ...) {
  rgl.material(...)

  if (is.null(xat)) {
    xlab <- NULL
  } else {
    xlen <- length(xat)
    if (is.null(xlab)) {
      xlab <- format(xat)
    } else {
      xlab <- rep(xlab, length.out = xlen)
    }
  }
  if (is.null(yat)) {
    ylab <- NULL
  } else {
    ylen <- length(yat)
    if (is.null(ylab)) {
      ylab <- format(yat)
    } else {
      ylab <- rep(ylab, length.out = ylen)
    }
  }
  if (is.null(zat)) {
    zlab <- NULL
  } else {
    zlen <- length(zat)
    if (is.null(zlab)) {
      zlab <- format(zat)
    } else {
      zlab <- rep(zlab, length.out = length(zat))
    }
  }
  xticks <- length(xat)
  yticks <- length(yat)
  zticks <- length(zat)

  if (identical(xunit, "pretty")) xunit <- -1
  if (identical(yunit, "pretty")) yunit <- -1
  if (identical(zunit, "pretty")) zunit <- -1

  length(xlen) <- 1
  length(ylen) <- 1
  length(zlen) <- 1
  length(marklen.rel) <- 1
  length(draw_front) <- 1
  length(xunit) <- 1
  length(yunit) <- 1
  length(zunit) <- 1
  length(marklen) <- 1
  length(expand) <- 1

  idata <- as.integer(c(xticks, yticks, zticks, xlen, ylen, zlen, marklen.rel, draw_front))
  ddata <- as.numeric(c(xunit, yunit, zunit, marklen, expand))

  ret <- .C(rgl_bbox,
    success = as.integer(FALSE),
    idata,
    ddata,
    as.numeric(xat),
    as.character(xlab),
    as.numeric(yat),
    as.character(ylab),
    as.numeric(zat),
    as.character(zlab)
  )

  if (!ret$success) {
    stop("'rgl_bbox' failed")
  }

  lowlevel(ret$success)
}

##
## set lights
##
##

rgl.light <- function(theta = 0, phi = 0, viewpoint.rel = TRUE, ambient = "#FFFFFF", diffuse = "#FFFFFF", specular = "#FFFFFF", x = NULL, y = NULL, z = NULL) {
  ambient <- rgl.color(ambient)
  diffuse <- rgl.color(diffuse)
  specular <- rgl.color(specular)

  # if a complete set of x, y, z is given, the light source is assumed to be part of the scene, theta and phi are ignored
  # else the light source is infinitely far away and its direction is determined by theta, phi (default)
  if (!is.null(x)) {
    if (!missing(theta) || !missing(phi)) {
      warning("'theta' and 'phi' ignored when 'x' is present")
    }
    xyz <- xyz.coords(x, y, z)
    x <- xyz$x
    y <- xyz$y
    z <- xyz$z
    if (length(x) > 1) stop("A light can only be in one place at a time")
    finite.pos <- TRUE
  }
  else {
    if (!is.null(y) || !is.null(z)) {
      warning("'y' and 'z' ignored, spherical coordinates used")
    }
    finite.pos <- FALSE
    x <- 0
    y <- 0
    z <- 0
  }


  idata <- as.integer(c(viewpoint.rel, ambient, diffuse, specular, finite.pos))
  ddata <- as.numeric(c(theta, phi, x, y, z))

  ret <- .C(rgl_light,
    success = as.integer(FALSE),
    idata,
    ddata
  )

  if (!ret$success) {
    stop("Too many lights; maximum is 8 sources per scene")
  }

  lowlevel(ret$success)
}

##
## ===[ SECTION: shapes ]=====================================================
##

##
## add primitive
##
##



#' add primitive set shape
#' 
#' Adds a shape node to the current scene
#' 
#' Adds a shape node to the scene. The appearance is defined by the material
#' properties.  See \code{\link{rgl.material}} for details.
#' 
#' The names of these functions correspond to OpenGL primitives.  They all take
#' a sequence of vertices in \code{x, y, z}.  The only non-obvious ones are
#' \code{rgl.lines} which draws line segments based on pairs of vertices, and
#' \code{rgl.linestrips} which joins the vertices.
#' 
#' For triangles and quads, the normals at each vertex may be specified using
#' \code{normals}.  These may be given in any way that would be acceptable as a
#' single argument to \code{\link[grDevices]{xyz.coords}}.  These need not
#' match the actual normals to the polygon: curved surfaces can be simulated by
#' using other choices of normals.
#' 
#' Texture coordinates may also be specified.  These may be given in any way
#' that would be acceptable as a single argument to
#' \code{\link[grDevices]{xy.coords}}, and are interpreted in terms of the
#' bitmap specified as the material texture, with \code{(0, 0)} at the lower
#' left, \code{(1, 1)} at the upper right.  The texture is used to modulate the
#' color of the polygon.
#' 
#' These are the lower level functions called by \code{\link{points3d}},
#' \code{\link{segments3d}}, \code{\link{lines3d}}, etc.  The two principal
#' differences between the \code{rgl.*} functions and the \code{*3d} functions
#' are that the former set all unspecified material properties to defaults,
#' whereas the latter use current values as defaults; the former make
#' persistent changes to material properties with each call, whereas the latter
#' make temporary changes only for the duration of the call.
#' 
#' @aliases rgl.primitive rgl.points rgl.lines rgl.linestrips rgl.triangles
#' rgl.quads
#' @param x,y,z coordinates.  Any reasonable way of defining the coordinates is
#' acceptable.  See the function \code{\link[grDevices]{xyz.coords}} for
#' details.
#' @param normals Normals at each point.
#' @param texcoords Texture coordinates at each point.
#' @param ... Material properties. See \code{\link{rgl.material}} for details.
#' @return Each primitive function returns the integer object ID of the shape
#' that was added to the scene.  These can be passed to \code{\link{rgl.pop}}
#' to remove the object from the scene.
#' @seealso \code{\link{rgl.material}}, \code{\link{rgl.spheres}},
#' \code{\link{rgl.texts}}, \code{\link{rgl.surface}},
#' \code{\link{rgl.sprites}}
#' @keywords dynamic
#' @examples
#' 
#' rgl.open()
#' rgl.points(rnorm(1000), rnorm(1000), rnorm(1000), color = heat.colors(1000))
#' 
rgl.primitive <- function(type, x, y = NULL, z = NULL, normals = NULL, texcoords = NULL, ...) {
  rgl.material(...)

  type <- rgl.enum.primtype(type)

  xyz <- xyz.coords(x, y, z, recycle = TRUE)
  x <- xyz$x
  y <- xyz$y
  z <- xyz$z

  vertex <- rgl.vertex(x, y, z)
  nvertex <- rgl.nvertex(vertex)
  if (nvertex > 0) {
    perelement <- c(points = 1, lines = 2, triangles = 3, quadrangles = 4, linestrips = 1)[type]
    if (nvertex %% perelement) {
      stop("Illegal number of vertices")
    }

    idata <- as.integer(c(type, nvertex, !is.null(normals), !is.null(texcoords)))

    if (is.null(normals)) {
      normals <- 0
    } else {
      normals <- xyz.coords(normals, recycle = TRUE)
      x <- rep(normals$x, len = nvertex)
      y <- rep(normals$y, len = nvertex)
      z <- rep(normals$z, len = nvertex)
      normals <- rgl.vertex(x, y, z)
    }

    if (is.null(texcoords)) {
      texcoords <- 0
    } else {
      texcoords <- xy.coords(texcoords, recycle = TRUE)
      s <- rep(texcoords$x, len = nvertex)
      t <- rep(texcoords$y, len = nvertex)
      texcoords <- rgl.texcoords(s, t)
    }

    ret <- .C(rgl_primitive,
      success = as.integer(FALSE),
      idata,
      as.numeric(vertex),
      as.numeric(normals),
      as.numeric(texcoords),
      NAOK = TRUE
    )

    if (!ret$success) {
      stop("'rgl_primitive' failed")
    }

    lowlevel(ret$success)
  }
}

rgl.points <- function(x, y = NULL, z = NULL, ...) {
  rgl.primitive("points", x, y, z, ...)
}

rgl.lines <- function(x, y = NULL, z = NULL, ...) {
  rgl.primitive("lines", x, y, z, ...)
}

rgl.triangles <- function(x, y = NULL, z = NULL, normals = NULL, texcoords = NULL, ...) {
  rgl.primitive("triangles", x, y, z, normals, texcoords, ...)
}

rgl.quads <- function(x, y = NULL, z = NULL, normals = NULL, texcoords = NULL, ...) {
  rgl.primitive("quadrangles", x, y, z, normals, texcoords, ...)
}

rgl.linestrips <- function(x, y = NULL, z = NULL, ...) {
  rgl.primitive("linestrips", x, y, z, ...)
}

##
## add surface
##
##

# Utility function:
# calculates the parity of a permutation of integers

perm_parity <- function(p) {
  x <- seq_along(p)
  result <- 0
  for (i in x) {
    if (x[i] != p[i]) {
      x[x == p[i]] <- x[i]
      result <- result + 1
    }
  }
  return(result %% 2)
}



#' add height-field surface shape
#' 
#' Adds a surface to the current scene. The surface is defined by a matrix
#' defining the height of each grid point and two vectors defining the grid.
#' 
#' Adds a surface mesh to the current scene. The surface is defined by the
#' matrix of height values in \code{y}, with rows corresponding to the values
#' in \code{x} and columns corresponding to the values in \code{z}.
#' 
#' The \code{coords} parameter can be used to change the geometric
#' interpretation of \code{x}, \code{y}, and \code{z}.  The first entry of
#' \code{coords} indicates which coordinate (\code{1 = X}, \code{2 = Y},
#' \code{3 = Z}) corresponds to the \code{x} parameter.  Similarly the second
#' entry corresponds to the \code{y} parameter, and the third entry to the
#' \code{z} parameter.  In this way surfaces may be defined over any coordinate
#' plane.
#' 
#' If the normals are not supplied, they will be calculated automatically based
#' on neighbouring points.
#' 
#' Texture coordinates run from 0 to 1 over each dimension of the texture
#' bitmap.  If texture coordinates are not supplied, they will be calculated to
#' render the texture exactly once over the grid.  Values greater than 1 can be
#' used to repeat the texture over the surface.
#' 
#' \code{rgl.surface} always draws the surface with the `front' upwards (i.e.
#' towards higher \code{y} values).  This can be used to render the top and
#' bottom differently; see \code{\link{rgl.material}} and the example below.
#' 
#' If the \code{x} or \code{z} argument is a matrix, then it must be of the
#' same dimension as \code{y}, and the values in the matrix will be used for
#' the corresponding coordinates. This is used to plot shapes such as cylinders
#' where y is not a function of x and z.
#' 
#' \code{NA} values in the height matrix are not drawn.
#' 
#' @param x values corresponding to rows of \code{y}, or matrix of x
#' coordinates
#' @param y matrix of height values
#' @param z values corresponding to columns of \code{y}, or matrix of z
#' coordinates
#' @param coords See details
#' @param ... Material and texture properties. See \code{\link{rgl.material}}
#' for details.
#' @param normal_x,normal_y,normal_z matrices of the same dimension as \code{y}
#' giving the coordinates of normals at each grid point
#' @param texture_s,texture_t matrices of the same dimension as \code{z} giving
#' the coordinates within the current texture of each grid point
#' @return The object ID of the displayed surface is returned invisibly.
#' @seealso \code{\link{rgl.material}}, \code{\link{surface3d}},
#' \code{\link{terrain3d}}.  See \code{\link{persp3d}} for a higher level
#' interface.
#' @keywords dynamic
#' @examples
#' 
#' 
#' #
#' # volcano example taken from "persp"
#' #
#' 
#' data(volcano)
#' 
#' y <- 2 * volcano        # Exaggerate the relief
#' 
#' x <- 10 * (1:nrow(y))   # 10 meter spacing (S to N)
#' z <- 10 * (1:ncol(y))   # 10 meter spacing (E to W)
#' 
#' ylim <- range(y)
#' ylen <- ylim[2] - ylim[1] + 1
#' 
#' colorlut <- terrain.colors(ylen) # height color lookup table
#' 
#' col <- colorlut[ y - ylim[1] + 1 ] # assign colors to heights for each point
#' 
#' rgl.open()
#' rgl.surface(x, z, y, color = col, back = "lines")
#' 
#' 
rgl.surface <- function(x, z, y, coords = 1:3, ..., normal_x = NULL, normal_y = NULL, normal_z = NULL,
                        texture_s = NULL, texture_t = NULL) {
  rgl.material(...)

  flags <- rep(FALSE, 4)

  if (is.matrix(x)) {
    nx <- nrow(x)
    flags[1] <- TRUE
    if (!identical(dim(x), dim(y))) {
      stop(gettextf("Bad dimension for %s", "rows"),
        domain = NA
      )
    }
  } else {
    nx <- length(x)
  }

  if (is.matrix(z)) {
    nz <- ncol(z)
    flags[2] <- TRUE
    if (!identical(dim(z), dim(y))) {
      stop(gettextf("Bad dimension for %s", "cols"),
        domain = NA
      )
    }
  } else {
    nz <- length(z)
  }

  ny <- length(y)

  if (nx * nz != ny) {
    stop("'y' length != 'x' rows * 'z' cols")
  }

  if (nx < 2) {
    stop("rows < 2")
  }

  if (nz < 2) {
    stop("cols < 2")
  }

  if (length(coords) != 3 || !identical(all.equal(sort(coords), 1:3), TRUE)) {
    stop("'coords' must be a permutation of 1:3")
  }

  nulls <- c(is.null(normal_x), is.null(normal_y), is.null(normal_z))
  if (!all(nulls)) {
    if (any(nulls)) stop("All normals must be supplied")
    if (!identical(dim(y), dim(normal_x))
    || !identical(dim(y), dim(normal_y))
    || !identical(dim(y), dim(normal_z))) {
      stop(gettextf("Bad dimension for %s", "normals"),
        domain = NA
      )
    }
    flags[3] <- TRUE
  }

  nulls <- c(is.null(texture_s), is.null(texture_t))
  if (!all(nulls)) {
    if (any(nulls)) stop("Both texture coordinates must be supplied")
    if (!identical(dim(y), dim(texture_s))
    || !identical(dim(y), dim(texture_t))) {
      stop(gettextf("Bad dimension for %s", "textures"),
        domain = NA
      )
    }
    flags[4] <- TRUE
  }

  idata <- as.integer(c(nx, nz))

  parity <- (perm_parity(coords) + (x[2] < x[1]) + (z[2] < z[1])) %% 2

  ret <- .C(rgl_surface,
    success = as.integer(FALSE),
    idata,
    as.numeric(x),
    as.numeric(z),
    as.numeric(y),
    as.numeric(normal_x),
    as.numeric(normal_z),
    as.numeric(normal_y),
    as.numeric(texture_s),
    as.numeric(texture_t),
    as.integer(coords),
    as.integer(parity),
    as.integer(flags),
    NAOK = TRUE
  )

  if (!ret$success) {
    stop("'rgl_surface' failed")
  }

  lowlevel(ret$success)
}

##
## add spheres
##

rgl.spheres <- function(x, y = NULL, z = NULL, radius = 1.0, ...) {
  rgl.material(...)

  vertex <- rgl.vertex(x, y, z)
  nvertex <- rgl.nvertex(vertex)
  radius <- rgl.attr(radius, nvertex)
  nradius <- length(radius)
  if (!nradius) stop("No radius specified")

  idata <- as.integer(c(nvertex, nradius))

  ret <- .C(rgl_spheres,
    success = as.integer(FALSE),
    idata,
    as.numeric(vertex),
    as.numeric(radius),
    NAOK = TRUE
  )

  if (!ret$success) {
    stop("'rgl_spheres' failed")
  }

  lowlevel(ret$success)
}

##
## add planes
##

rgl.planes <- function(a, b = NULL, c = NULL, d = 0, ...) {
  rgl.material(...)

  normals <- rgl.vertex(a, b, c)
  nnormals <- rgl.nvertex(normals)
  noffsets <- length(d)

  idata <- as.integer(c(nnormals, noffsets))

  ret <- .C(rgl_planes,
    success = as.integer(FALSE),
    idata,
    as.numeric(normals),
    as.numeric(d),
    NAOK = TRUE
  )

  if (!ret$success) {
    stop("'rgl_planes' failed")
  }

  lowlevel(ret$success)
}

##
## add clip planes
##

rgl.clipplanes <- function(a, b = NULL, c = NULL, d = 0) {
  normals <- rgl.vertex(a, b, c)
  nnormals <- rgl.nvertex(normals)
  noffsets <- length(d)

  idata <- as.integer(c(nnormals, noffsets))

  ret <- .C(rgl_clipplanes,
    success = as.integer(FALSE),
    idata,
    as.numeric(normals),
    as.numeric(d),
    NAOK = TRUE
  )

  if (!ret$success) {
    stop("'rgl_clipplanes' failed")
  }

  lowlevel(ret$success)
}


##
## add abclines
##

rgl.abclines <- function(x, y = NULL, z = NULL, a, b = NULL, c = NULL, ...) {
  rgl.material(...)

  bases <- rgl.vertex(x, y, z)
  nbases <- rgl.nvertex(bases)
  directions <- rgl.vertex(a, b, c)
  ndirs <- rgl.nvertex(directions)

  idata <- as.integer(c(nbases, ndirs))

  ret <- .C(rgl_abclines,
    success = as.integer(FALSE),
    idata,
    as.numeric(bases),
    as.numeric(directions),
    NAOK = TRUE
  )

  if (!ret$success) {
    stop("'rgl_abclines' failed")
  }

  lowlevel(ret$success)
}


##
## add texts
##

rgl.texts <- function(x, y = NULL, z = NULL, text, adj = 0.5, pos = NULL, offset = 0.5,
                      family = par3d("family"), font = par3d("font"),
                      cex = par3d("cex"), useFreeType = par3d("useFreeType"), ...) {
  rgl.material(...)

  vertex <- rgl.vertex(x, y, z)
  nvertex <- rgl.nvertex(vertex)

  if (!is.null(pos)) {
    npos <- length(pos)
    stopifnot(all(pos %in% 1:4))
    stopifnot(length(offset) == 1)
    adj <- offset
  } else {
    pos <- 0
    npos <- 1
  }
  if (length(adj) == 0) adj <- c(0.5, 0.5)
  if (length(adj) == 1) adj <- c(adj, 0.5)
  if (length(adj) > 2) warning("Only the first two entries of 'adj' are used")

  if (!length(text)) {
    if (nvertex) {
      warning("No text to plot")
    }
    return(invisible(integer(0)))
  }

  text <- rep(text, length.out = nvertex)

  idata <- as.integer(nvertex)

  nfonts <- max(length(family), length(font), length(cex))
  family <- rep(family, len = nfonts)
  font <- rep(font, len = nfonts)
  cex <- rep(cex, len = nfonts)

  family[font == 5] <- "symbol"
  font <- ifelse(font < 0 | font > 4, 1, font)

  ret <- .C(rgl_texts,
    success = as.integer(FALSE),
    idata,
    as.double(adj),
    as.character(text),
    as.numeric(vertex),
    as.integer(nfonts),
    as.character(family),
    as.integer(font),
    as.numeric(cex),
    as.integer(useFreeType),
    as.integer(npos),
    as.integer(pos),
    NAOK = TRUE
  )

  if (!ret$success) {
    stop("'rgl_texts' failed")
  }

  lowlevel(ret$success)
}

##
## add sprites
##

rgl.sprites <- function(x, y = NULL, z = NULL, radius = 1.0, shapes = NULL,
                        userMatrix = diag(4), fixedSize = FALSE,
                        ...) {
  rgl.material(...)

  center <- rgl.vertex(x, y, z)
  ncenter <- rgl.nvertex(center)
  radius <- rgl.attr(radius, ncenter)
  nradius <- length(radius)
  if (!nradius) stop("No radius specified")
  if (length(shapes) && length(userMatrix) != 16) stop("Invalid 'userMatrix'")
  if (length(fixedSize) != 1) stop("Invalid 'fixedSize'")
  idata <- as.integer(c(ncenter, nradius, length(shapes), fixedSize))

  ret <- .C(rgl_sprites,
    success = as.integer(FALSE),
    idata,
    as.numeric(center),
    as.numeric(radius),
    as.integer(shapes),
    as.numeric(userMatrix),
    NAOK = TRUE
  )

  if (!ret$success) {
    stop("'rgl_sprites' failed")
  }

  lowlevel(ret$success)
}

##
## convert user coordinate to window coordinate
##



#' Convert between rgl user and window coordinates
#' 
#' This function converts from 3-dimensional user coordinates to 3-dimensional
#' window coordinates.
#' 
#' These functions convert between user coordinates and window coordinates.
#' 
#' Window coordinates run from 0 to 1 in X, Y, and Z.  X runs from 0 on the
#' left to 1 on the right; Y runs from 0 at the bottom to 1 at the top; Z runs
#' from 0 foremost to 1 in the background.  \code{rgl} does not currently
#' display vertices plotted outside of this range, but in normal circumstances
#' will automatically resize the display to show them.  In the example below
#' this has been suppressed.
#' 
#' @aliases rgl.user2window rgl.window2user rgl.projection
#' @param x,y,z Input coordinates.  Any reasonable way of defining the
#' coordinates is acceptable.  See the function
#' \code{\link[grDevices]{xyz.coords}} for details.
#' @param projection The rgl projection to use
#' @param dev,subscene The rgl device and subscene to work with
#' @return The coordinate conversion functions produce a matrix with columns
#' corresponding to the X, Y, and Z coordinates.
#' 
#' \code{rgl.projection()} returns a list containing the following components:
#' \item{model}{the modelview matrix} \item{projection}{the projection matrix}
#' \item{viewport}{the viewport vector}
#' 
#' See \code{\link{par3d}} for more details.
#' @author Ming Chen / Duncan Murdoch
#' @seealso \code{\link{select3d}}
#' @keywords dynamic
#' @examples
#' 
#' open3d()
#' points3d(rnorm(100), rnorm(100), rnorm(100))
#' if (interactive() || !.Platform$OS == "unix") {
#' # Calculate a square in the middle of the display and plot it
#' square <- rgl.window2user(c(0.25, 0.25, 0.75, 0.75, 0.25), 
#'                           c(0.25, 0.75, 0.75, 0.25, 0.25), 0.5)
#' par3d(ignoreExtent = TRUE)
#' lines3d(square)
#' par3d(ignoreExtent = FALSE)
#' }
#' 
rgl.user2window <- function(x, y = NULL, z = NULL, projection = rgl.projection()) {
  xyz <- xyz.coords(x, y, z, recycle = TRUE)
  points <- rbind(xyz$x, xyz$y, xyz$z)

  idata <- as.integer(ncol(points))

  ret <- .C(rgl_user2window,
    success = FALSE,
    idata,
    as.double(points),
    window = double(length(points)),
    model = as.double(projection$model),
    proj = as.double(projection$proj),
    view = as.integer(projection$view)
  )

  if (!ret$success) {
    stop("'rgl_user2window' failed")
  }
  return(matrix(ret$window, ncol(points), 3, byrow = TRUE))
}

##
## convert window coordinate to user coordiante
##

rgl.window2user <- function(x, y = NULL, z = 0, projection = rgl.projection()) {
  xyz <- xyz.coords(x, y, z, recycle = TRUE)
  window <- rbind(xyz$x, xyz$y, xyz$z)
  idata <- as.integer(ncol(window))

  ret <- .C(rgl_window2user,
    success = FALSE,
    idata,
    point = double(length(window)),
    window,
    model = as.double(projection$model),
    proj = as.double(projection$proj),
    view = as.integer(projection$view)
  )

  if (!ret$success) {
    stop("'rgl_window2user' failed")
  }
  return(matrix(ret$point, ncol(window), 3, byrow = TRUE))
}

# Selectstate values
msNONE <- 1
msCHANGING <- 2
msDONE <- 3
msABORT <- 4

rgl.selectstate <- function(dev = rgl.cur(), subscene = currentSubscene3d(dev)) {
  ret <- .C(rgl_selectstate,
    as.integer(dev),
    as.integer(subscene),
    success = FALSE,
    state = as.integer(0),
    mouseposition = double(4)
  )

  if (!ret$success) {
    stop("'rgl_selectstate' failed")
  }
  return(ret)
}




#' Switch to select mode, and return the mouse position selected.
#' 
#' Mostly for internal use, this function temporarily installs a handler on a
#' button of the mouse that will return the mouse coordinates of one click and
#' drag rectangle.
#' 
#' 
#' @param button Which button to use?
#' @param dev,subscene The rgl device and subscene to work with
#' @return A vector of four coordinates: the X and Y coordinates of the start
#' and end of the dragged rectangle.
#' @author Duncan Murdoch
#' @seealso \code{\link{rgl.select3d}}, a version that allows the selection
#' region to be used to select points in the scene.
rgl.select <- function(button = c("left", "middle", "right"),
                       dev = rgl.cur(), subscene = currentSubscene3d(dev)) {
  if (rgl.useNULL()) {
    return(NULL)
  }
  button <- match.arg(button)

  newhandler <- par3d("mouseMode", dev = dev, subscene = subscene)
  newhandler[button] <- "selecting"
  oldhandler <- par3d(mouseMode = newhandler, dev = dev, subscene = subscene)
  on.exit(par3d(mouseMode = oldhandler, dev = dev, subscene = subscene))

  while ((result <- rgl.selectstate(dev = dev, subscene = subscene))$state < msDONE) {
    Sys.sleep(0.1)
  }

  rgl.setselectstate("none", dev = dev, subscene = subscene)

  if (result$state == msDONE) {
    return(result$mouseposition)
  } else {
    return(NULL)
  }
}

rgl.setselectstate <- function(state = "current",
                               dev = rgl.cur(), subscene = currentSubscene3d(dev)) {
  state <- rgl.enum(state, current = 0, none = 1, middle = 2, done = 3, abort = 4)
  idata <- as.integer(c(state))

  ret <- .C(rgl_setselectstate,
    as.integer(dev),
    as.integer(subscene),
    success = FALSE,
    state = idata
  )

  if (!ret$success) {
    stop("'rgl_setselectstate' failed")
  }

  c("none", "middle", "done", "abort")[ret$state]
}

rgl.projection <- function(dev = rgl.cur(), subscene = currentSubscene3d(dev)) {
  list(
    model = par3d("modelMatrix", dev = dev, subscene = subscene),
    proj = par3d("projMatrix", dev = dev, subscene = subscene),
    view = par3d("viewport", dev = dev, subscene = subscene)
  )
}

rgl.select3d <- function(button = c("left", "middle", "right"),
                         dev = rgl.cur(), subscene = currentSubscene3d(dev)) {
  rect <- rgl.select(button = button, dev = dev, subscene = subscene)
  if (is.null(rect)) {
    return(NULL)
  }

  llx <- rect[1]
  lly <- rect[2]
  urx <- rect[3]
  ury <- rect[4]

  if (llx > urx) {
    temp <- llx
    llx <- urx
    urx <- temp
  }
  if (lly > ury) {
    temp <- lly
    lly <- ury
    ury <- temp
  }
  proj <- rgl.projection(dev = dev, subscene = subscene)
  proj$view["x"] <- proj$view["y"] <- 0
  function(x, y = NULL, z = NULL) {
    pixel <- rgl.user2window(x, y, z, projection = proj)
    x <- pixel[, 1]
    y <- pixel[, 2]
    z <- pixel[, 3]
    (llx <= x) & (x <= urx) & (lly <= y) & (y <= ury) &
      (0 <= z) & (z <= 1)
  }
}
