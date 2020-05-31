#' Saves the current scene to a variable, and displays such variables.
#' 
#' This function saves a large part of the RGL state associated with the
#' current window to a variable.
#' 
#' The components saved are: the \code{\link{par3d}} settings, the
#' \code{\link{material3d}} settings, the \code{\link{bg3d}} settings, the
#' lights and the objects in the scene.
#' 
#' In most cases, calling \code{\link{plot3d}} on that variable will duplicate
#' the scene.  (There are likely to be small differences, mostly internal, but
#' some aspects of the scene are not currently available.) If textures are
#' used, the name of the texture will be saved, rather than the contents of the
#' texture file.
#' 
#' Other than saving the code to recreate a scene, saving the result of
#' \code{scene3d} to a file will allow it to be reproduced later most
#' accurately.  In roughly decreasing order of fidelity,
#' \code{\link{writeWebGL}}, \code{\link{writePLY}}, \code{\link{writeOBJ}} and
#' \code{\link{writeSTL}} write the scene to a file in formats readable by
#' other software.
#' 
#' If \code{minimal = TRUE} (the default), then attributes of objects will not
#' be saved if they currently have no effect on the display, thereby reducing
#' the file size.  Set \code{minimal = FALSE} if the scene is intended to be
#' used in a context where the appearance could be changed.  Currently this
#' only affects the inclusion of normals; with \code{minimal = TRUE} they are
#' omitted for objects when the material is not lit.
#' 
#' @aliases scene3d rglscene-class rglobject-class plot3d.rglscene
#' plot3d.rglobject print.rglscene print.rglobject
#' @param minimal Should attributes be skipped if they currently have no
#' effect?  See Details.
#' @param x An object of class \code{"rglscene"}
#' @param add Whether to open a new window, or add to the existing one.
#' @param ... Additional parameters, currently ignored.
#' @return The \code{scene3d} function returns an object of class
#' \code{"rglscene"}.  This is a list with some or all of the components:
#' \item{material}{The results returned from a \code{\link{material3d}} call.}
#' \item{rootSubscene}{A list containing information about the main ("root")
#' subscene.  This may include: \describe{ \item{id}{The scene id.}
#' \item{type}{"subscene"} \item{par3d}{The \code{\link{par3d}} settings for
#' the subscene.} \item{embeddings}{The \code{\link{subsceneInfo}()$embeddings}
#' for the main subscene.} \item{objects}{The ids for objects in the subscene.}
#' \item{subscenes}{A recursive list of child subscenes.}}} \item{objects}{A
#' list containing the RGL lights, background and objects in the scene.}
#' 
#' The objects in the \code{objects} component are of class \code{"rglobject"}.
#' They are lists containing some or all of the components \item{id}{The RGL
#' identifier of the object in the original scene.} \item{type}{A character
#' variable identifying the type of object.} \item{material}{Components of the
#' material that differ from the scene material.} \item{vertices, normals,
#' etc.}{Any of the attributes of the object retrievable by
#' \code{\link{rgl.attrib}}.} \item{ignoreExtent}{A logical value indicating
#' whether this object contributes to the bounding box. Currently this may
#' differ from the object in the original scene.} \item{objects}{Sprites may
#' contain other objects; they will be stored here as a list of
#' \code{"rglobject"}s.}
#' 
#' Lights in the scene are stored similarly, mixed into the \code{objects}
#' list.
#' 
#' The \code{plot3d} methods invisibly return a vector of RGL object ids that
#' were plotted.  The \code{print} methods invisibly return the object that was
#' printed.
#' @author Duncan Murdoch
#' @seealso \code{\link{rglwidget}}, \code{\link{writePLY}},
#' \code{\link{writeOBJ}} and \code{\link{writeSTL}} write the scene to a file
#' in various formats.
#' @keywords graphics
#' @examples
#' 
#' open3d()
#' z <- 2 * volcano        # Exaggerate the relief
#' x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
#' y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)
#' persp3d(x, y, z, col = "green3", aspect = "iso")
#' 
#' s <- scene3d()
#' # Make it bigger
#' s$par3d$windowRect <- 1.5*s$par3d$windowRect
#' # and draw it again
#' plot3d(s)
#' 
scene3d <- function(minimal = TRUE) {
  saveSubscene <- currentSubscene3d()
  on.exit(useSubscene3d(saveSubscene))

  defaultmaterial <- material3d()

  matdiff <- function(mat) {
    for (m in names(mat)) {
      if (identical(mat[[m]], defaultmaterial[[m]])) {
        mat[[m]] <- NULL
      }
    }
    mat
  }

  getObject <- function(id, type) {
    result <- list(id = id, type = type)

    if (!(type %in% c("light", "clipplanes"))) {
      mat <- rgl.getmaterial(id = id)
      lit <- mat$lit
      result$material <- matdiff(mat)
    } else {
      lit <- FALSE
    }

    attribs <- c(
      "vertices", "colors", "texcoords", "dim",
      "texts", "cex", "adj", "radii", "ids",
      "usermatrix", "types", "offsets", "centers",
      "family", "font", "pos"
    )
    if (lit || !minimal || type %in% c("light", "clipplanes")) {
      attribs <- c(attribs, "normals")
    }
    for (a in attribs) {
      if (rgl.attrib.count(id, a)) {
        result[[a]] <- rgl.attrib(id, a)
      }
    }
    flags <- rgl.attrib(id, "flags")
    if (length(flags)) {
      if ("ignoreExtent" %in% rownames(flags)) {
        result$ignoreExtent <- flags["ignoreExtent", 1]
      }
      if ("fixedSize" %in% rownames(flags)) {
        result$fixedSize <- flags["fixedSize", 1]
      }
    }
    if (!is.null(result$ids)) {
      objlist <- vector("list", nrow(result$ids))
      for (i in seq_len(nrow(result$ids))) {
        objlist[[i]] <- getObject(result$ids[i, 1], result$types[i, 1])
      }
      result$objects <- objlist
    }
    if (type == "background") {
      flags <- rgl.attrib(id, "flags")
      result$sphere <- flags["sphere", 1]
      result$fogtype <- if (flags["linear_fog", 1]) {
        "linear"
      } else if (flags["exp_fog", 1]) {
        "exp"
      } else if (flags["exp2_fog", 1]) {
        "exp2"
      } else {
        "none"
      }
    } else if (type == "bboxdeco") {
      flags <- rgl.attrib(id, "flags")
      result$draw_front <- flags["draw_front", 1]
    } else if (type == "light") {
      flags <- rgl.attrib(id, "flags")
      result$viewpoint <- flags["viewpoint", 1]
      result$finite <- flags["finite", 1]
    }
    class(result) <- c(paste0("rgl", type), "rglobject")
    result
  }

  getSubscene <- function(id) {
    useSubscene3d(id)

    result <- list(id = id, type = "subscene", par3d = par3d())

    result$embeddings <- subsceneInfo()$embeddings

    objs <- rgl.ids(c("background", "bboxdeco", "shapes", "lights"))
    result$objects <- objs$id

    if (nrow(obj <- rgl.ids("subscene", subscene = id))) {
      subscenes <- vector("list", nrow(obj))
      ids <- obj$id
      for (i in seq_len(nrow(obj))) {
        subscenes[[i]] <- getSubscene(ids[i])
      }
      result$subscenes <- subscenes
    }
    class(result) <- c("rglsubscene", "rglobject")
    result
  }

  result <- list()
  result$material <- defaultmaterial

  result$rootSubscene <- getSubscene(rootSubscene())

  objs <- rgl.ids(c("shapes", "lights", "background", "bboxdeco"), subscene = 0)
  objlist <- vector("list", nrow(objs))
  ids <- objs$id
  types <- as.character(objs$type)
  for (i in seq_len(nrow(objs))) {
    objlist[[i]] <- getObject(ids[i], types[i])
    names(objlist)[i] <- as.character(ids[i])
  }
  result$objects <- objlist

  class(result) <- "rglscene"
  result
}

print.rglscene <- function(x, ...) {
  cat(gettext("RGL scene containing:\n"))
  if (!is.null(x$par3d)) {
    cat(gettext("  par3d:\tscene information\n"))
  }
  if (!is.null(x$material)) {
    cat(gettext("  material:\tdefault material properties\n"))
  }
  if (!is.null(x$objects)) {
    cat(gettextf("  objects:\tlist of %d object(s):\n", length(x$objects)))
    cat("          \t", sapply(x$objects, function(obj) obj$type), "\n")
  }
  if (!is.null(x$root)) {
    cat(gettext("  root:\ta root subscene\n"))
  }
  invisible(x)
}

summary.rglscene <- function(object, ...) {
  result <- list()
  nobjs <- length(object$objects)
  if (nobjs) result$objects <- data.frame(type = sapply(object$objects, function(obj) obj$type))
  if (!is.null(object$rootSubscene)) {
    result$subscenes <- summary(object$rootSubscene)
  }
  result
}

summary.rglsubscene <- function(object, ...) {
  result <- data.frame(id = object$id, parent = NA, objects = 0)
  result$objects <- list(object$objects)
  if (length(object$subscenes)) {
    children <- do.call(rbind, lapply(object$subscenes, summary))
    children[is.na(children$parent), "parent"] <- object$id
    result <- rbind(result, children)
  }
  result
}

print.rglobject <- function(x, ...) {
  cat(gettextf("RGL object of type %s containing components\n", x$type))
  cat("  ")
  cat(names(x), sep = ", ")
  cat("\n")
}

print.rglsubscene <- function(x, ...) {
  cat(gettext("RGL subscene containing components\n"))
  cat("  ")
  cat(names(x), sep = ", ")
  cat("\n")
}


plot3d.rglscene <- function(x, add = FALSE, ...) {
  root <- x$rootSubscene
  if (is.null(root)) root <- x # Should work with pre-subscene objects
  if (!add) {
    params <- getr3dDefaults()
    if (!is.null(x$material)) {
      if (is.null(params$material)) params$material <- list()
      params$material[names(x$material)] <- x$material
    }
    if (!is.null(root$bg)) {
      if (is.null(params$bg)) params$bg <- list()
      params$bg[names(params$material)] <- params$material
      params$bg[names(x$bg$material)] <- x$bg$material
      x$bg$material <- x$bg$id <- x$bg$type <- NULL
      params$bg[names(x$bg)] <- x$bg
    }
    if (!is.null(root$par3d)) {
      ind <- !(names(root$par3d) %in% .Par3d.readonly)
      params[names(root$par3d)[ind]] <- root$par3d[ind]
    }
    open3d(params = params)

    # Some older scenes might not have a light in them, so only clear if one is there
    for (i in seq_along(x$objects)) {
      obj <- x$objects[[i]]
      if (obj$type == "light") {
        clear3d("lights")
        break
      }
    }
  }
  save <- par3d(skipRedraw = TRUE)
  on.exit(par3d(save))

  if (is.null(x$rootSubscene)) {
    results <- c()
    for (i in seq_along(x$objects)) {
      obj <- x$objects[[i]]
      results <- c(results, plot3d(obj))
    }
    if (!is.null(obj <- x$bbox)) {
      plot3d(obj)
    }
  } else {
    results <- plot3d(root, x$objects, root = TRUE, ...)
  }

  highlevel(results)
}

plot3d.rglsubscene <- function(x, objects, root = TRUE, ...) {
  if (root) {
    if (!is.null(x$embeddings)) {
      info <- subsceneInfo(embeddings = x$embeddings)
      subscene <- info$id
    } else {
      subscene <- currentSubscene3d()
    }
    if (!is.null(x$par3d$viewport)) {
      par3d(viewport = x$par3d$viewport)
    }
  } else {
    subscene <- newSubscene3d(
      viewport = x$embeddings["viewport"],
      projection = x$embeddings["projection"],
      model = x$embeddings["model"],
      newviewport = x$par3d$viewport,
      copyLights = FALSE
    )
  }

  listeners <- list(x$par3d$listeners) # list contains old ids
  names(listeners) <- subscene # names are new ids

  results <- subscene
  names(results) <- paste0("subscene", as.character(x$id))

  objs <- x$objects
  for (id in as.character(objs)) {
    obj <- objects[[id]]
    if (is.null(obj$newid)) {
      results <- c(results, objects[[id]]$newid <- plot3d(obj, ...))
    } else {
      addToSubscene3d(obj$newid)
    }
  }
  for (i in seq_along(x$subscenes)) {
    useSubscene3d(subscene)
    res <- plot3d(x$subscenes[[i]], objects, root = FALSE, ...)
    results <- c(results, res$results)
    listeners <- c(listeners, res$listeners)
    objects <- res$objects
  }
  if (root) {
    # Translate all the listener values
    dotranslations <- function(id) {
      info <- subsceneInfo(id = id)
      oldlisteners <- listeners[[as.character(id)]]
      par3d(listeners = results[paste0("subscene", oldlisteners)], subscene = id)
      for (child in info$children) {
        dotranslations(child)
      }
    }
    dotranslations(subscene)
    return(results)
  } else {
    return(list(results = results, objects = objects, listeners = listeners))
  }
}

plot3d.rglobject <- function(x, ...) {
  type <- x$type
  fn <- switch(type,
    points = points3d,
    lines = segments3d,
    linestrip = lines3d,
    triangles = triangles3d,
    quads = quads3d,
    text = texts3d,
    spheres = spheres3d,
    abclines = abclines3d,
    planes = planes3d,
    surface = surface3d,


#' add sprite set shape
#' 
#' Adds a sprite set shape node to the scene.
#' 
#' Simple sprites (used when \code{shapes} is \code{NULL}) are 1 by 1 squares
#' that are directed towards the viewpoint. Their primary use is for fast (and
#' faked) atmospherical effects, e.g. particles and clouds using alpha blended
#' textures. Particles are Sprites using an alpha-blended particle texture
#' giving the illusion of clouds and gasses.  The centre of each square will be
#' at the coordinates given by \code{x, y, z}.
#' 
#' When \code{shapes} is not \code{NULL}, it should be a vector of identifers
#' of objects to plot in the scene (e.g. as returned by plotting functions or
#' by \code{\link{rgl.ids}}).  These objects will be removed from the scene and
#' duplicated as a sprite image in a constant orientation, as specified by
#' \code{userMatrix}.  The origin \code{0, 0, 0} will be plotted at the
#' coordinates given by \code{x, y, z}.
#' 
#' The \code{userMatrix} argument is ignored for \code{shapes = NULL}.  For
#' shapes, \code{sprites3d} defaults the matrix to
#' \code{r3dDefaults$userMatrix} while \code{rgl.sprites} defaults it to an
#' identity transformation.
#' 
#' If any coordinate is \code{NA}, the sprite is not plotted.
#' 
#' The id values of the shapes are retrieved using \code{rgl.attrib(id,
#' "ids")}; the user matrix is retrieved using \code{rgl.attrib(id,
#' "usermatrix")}.
#' 
#' @aliases sprites3d particles3d rgl.sprites
#' @param x,y,z point coordinates.  Any reasonable way of defining the
#' coordinates is acceptable.  See the function
#' \code{\link[grDevices]{xyz.coords}} for details.
#' @param radius vector or single value defining the sphere radius
#' @param shapes \code{NULL} for a simple square, or a vector of identifiers of
#' shapes in the scene
#' @param userMatrix if \code{shape} is not \code{NULL}, the transformation
#' matrix for the shapes
#' @param fixedSize should sprites remain at a fixed size, or resize with the
#' scene?
#' @param ... material properties when \code{shape == 0}, texture mapping is
#' supported
#' @return These functions are called for the side effect of displaying the
#' sprites.  The shape ID of the displayed object is returned.
#' @seealso \code{\link{rgl.material}}
#' @keywords dynamic
#' @examples
#' 
#' open3d()
#' particles3d( rnorm(100), rnorm(100), rnorm(100), color = rainbow(100) )
#' # is the same as
#' sprites3d( rnorm(100), rnorm(100), rnorm(100), color = rainbow(100),
#'   lit = FALSE, alpha = .2,
#'   textype = "alpha", texture = system.file("textures/particle.png", package = "rgl") )
#' sprites3d( rnorm(10) + 6, rnorm(10), rnorm(10), shape = shade3d(tetrahedron3d(), col = "red") )
#' 
    sprites = sprites3d,
    light = light3d,
    clipplanes = clipplanes3d,
    NULL
  )
  if (is.null(fn)) {
    warning(gettextf("Object type '%s' not handled.", type),
      domain = NA
    )
    return()
  }
  if (!is.null(x$ignoreExtent)) {
    save <- par3d(ignoreExtent = x$ignoreExtent)
    on.exit(par3d(save))
  }
  args <- list()
  args$x <- x$vertices
  args$normals <- x$normals
  args$texcoords <- x$texcoords
  args$texts <- x$texts
  args$cex <- x$cex
  args$adj <- x$adj
  args$radius <- x$radii
  args$d <- x$offsets

  switch(type,
    abclines = {
      odd <- seq_len(nrow(args$x)) %% 2 == 1
      ends <- args$x[odd, , drop = FALSE]
      args$a <- args$x[!odd, , drop = FALSE] - ends
      args$x <- ends
    },
    planes = ,
    clipplanes = {
      args$a <- args$normals
      args$x <- NULL
      args$normals <- NULL
    },
    surface = {
      dim <- x$dim
      args$y <- matrix(args$x[, 2], dim[1], dim[2])
      args$z <- matrix(args$x[, 3], dim[1], dim[2])
      args$x <- matrix(args$x[, 1], dim[1], dim[2])
      if (!is.null(args$normals)) {
        args$normal_x <- matrix(args$normals[, 1], dim[1], dim[2])
        args$normal_y <- matrix(args$normals[, 2], dim[1], dim[2])
        args$normal_z <- matrix(args$normals[, 3], dim[1], dim[2])
        args$normals <- NULL
      }
      if (!is.null(args$texcoords)) {
        args$texture_s <- matrix(args$texcoords[, 1], dim[1], dim[2])
        args$texture_t <- matrix(args$texcoords[, 2], dim[1], dim[2])
        args$texcoords <- NULL
      }
    },
    sprites = {
      save2 <- par3d(skipRedraw = TRUE)
      on.exit(par3d(save2), add = TRUE)

      if (!is.null(x$objects)) {
        ids <- numeric(length(x$objects))
        for (i in seq_along(ids)) {
          ids[i] <- plot3d(x$objects[[i]])
        }
        args$shapes <- ids
      }
      args$userMatrix <- x$usermatrix
    }
  )

  mat <- x$material
  if (is.null(mat)) mat <- list()
  if (!is.null(col <- x$colors)) {
    mat$color <- rgb(col[, 1], col[, 2], col[, 3])
    mat$alpha <- col[, 4]
  }

  if (type == "light") {
    if (!x$finite) {
      args$x <- NULL
      vx <- x$vertices[1]
      vy <- x$vertices[2]
      vz <- x$vertices[3]
      args$phi <- atan2(vy, sqrt(vx^2 + vz^2)) * 180 / pi
      args$theta <- atan2(vx, vz) * 180 / pi
    }
    args$viewpoint.rel <- x$viewpoint
    args$ambient <- mat$color[1]
    args$diffuse <- mat$color[2]
    args$specular <- mat$color[3]
  } else {
    args <- c(args, mat)
  }

  do.call(fn, args)
}

plot3d.rglbboxdeco <- function(x, ...) {
  args <- list()
  v <- x$vertices
  t <- x$texts
  ind <- is.na(v[, 2]) & is.na(v[, 3])
  if (any(ind)) {
    args$xat <- v[ind, 1]
    if (!is.null(t)) {
      args$xlab <- t[ind]
    } else {
      args$xlab <- signif(args$xat, 4)
    }
  }
  ind <- is.na(v[, 1]) & is.na(v[, 3])
  if (any(ind)) {
    args$yat <- v[ind, 2]
    if (!is.null(t)) {
      args$ylab <- t[ind]
    } else {
      args$ylab <- signif(args$yat, 4)
    }
  }
  ind <- is.na(v[, 1]) & is.na(v[, 2])
  if (any(ind)) {
    args$zat <- v[ind, 3]
    if (!is.null(t)) {
      args$zlab <- t[ind]
    } else {
      args$zlab <- signif(args$zat, 4)
    }
  }
  args$draw_front <- x$draw_front
  args <- c(args, x$material)

  do.call("bbox3d", args)
}

plot3d.rglbackground <- function(x, ...) {
  mat <- x$material
  if (is.null(mat)) mat <- list()
  if (!is.null(col <- x$colors)) {
    mat$color <- rgb(col[, 1], col[, 2], col[, 3])
    mat$alpha <- col[, 4]
  }
  args <- c(list(sphere = x$sphere, fogtype = x$fogtype), mat)
  do.call("bg3d", args)
}

plot3d.rglWebGL <- function(x, ...) {
  plot3d(attr(x, "origScene"), ...)
}
