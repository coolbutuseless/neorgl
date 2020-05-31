subsetControl <- function(value = 1, subsets, subscenes = NULL,
                          fullset = Reduce(union, subsets),
                          accumulate = FALSE) {
  subsets <- lapply(subsets, as.integer)
  fullset <- as.integer(fullset)
  if (length(names(subsets))) {
    labels <- names(subsets)
  } else {
    labels <- NULL
  }
  structure(list(
    type = "subsetSetter",
    value = value - 1,
    subsets = unname(subsets),
    subscenes = subscenes,
    fullset = fullset,
    accumulate = accumulate,
    labels = labels
  ),
  class = "rglControl"
  )
}



#' Controls to use with playwidget().
#' 
#' These are setter functions to produce actions in a Shiny app, or in an
#' animation.
#' 
#' \code{subsetControl} produces data for \code{\link{playwidget}} to display
#' subsets of the object in one or more subscenes.  This code will not touch
#' objects in the subscenes if they are not in \code{fullset}.  \code{fullset}
#' defaults to the union of all the object ids mentioned in \code{subsets}, so
#' by default if an id is not mentioned in one of the subsets, it will not be
#' controlled by the slider.  If \code{value} is specified in R code, it will
#' be a 1-based index into the \code{subsets} list; when specified internally
#' in Javascript, 0-based indexing into the corresponding array will be used.
#' 
#' \code{propertyControl} sets individual properties.  Here the row of
#' \code{values} is determined by the position of \code{value} in \code{param}.
#' 
#' @aliases subsetControl propertyControl
#' @param value The value to use for input (typically \code{input$value} in a
#' Shiny app.)
#' @param subsets A list of vectors of object identifiers; the value will
#' choose among them.
#' @param fullset Objects in the subscene which are not in \code{fullset} will
#' not be touched.
#' @param subscenes The subscenes to be controlled.  If \code{NULL}, the root
#' subscene.
#' @param accumulate If \code{TRUE}, the subsets will accumulate (by union) as
#' the value increases.
#' @param entries,properties,objids Which properties to set.
#' @param values Values to set.
#' @param param Parameter values corresponding to the rows of \code{value}
#' @param interp Whether to use linear interpolation between \code{param}
#' values
#' @return These functions return controller data in a list of class
#' \code{"rglControl"}.
#' @author Duncan Murdoch
#' @seealso \code{\link{subsetSetter}} for a way to embed a pure Javascript
#' control, and \code{\link{playwidget}} for a way to use these in animations
#' (including Shiny), \code{\link{rglShared}} for linking using the
#' \pkg{crosstalk} package.
propertyControl <- function(value = 0, entries, properties, objids, values = NULL,
                            param = seq_len(NROW(values)) - 1, interp = TRUE) {
  objids <- as.integer(objids)
  structure(list(
    type = "propertySetter",
    value = value,
    values = values,
    entries = entries,
    properties = properties,
    objids = objids,
    param = param,
    interp = interp
  ),
  class = "rglControl"
  )
}



#' Sets attributes of a clipping plane.
#' 
#' This is a function to produce actions in a web display.  A
#' \code{\link{playwidget}} or Shiny input control (e.g. a
#' \code{\link[shiny]{sliderInput}} control) sets a value which controls
#' attributes of one or more clipping planes.
#' 
#' 
#' @param a,b,c,d Parameter values for the clipping planes.
#' @param plane Which plane in the clipplane object?
#' @param clipplaneids The id of the clipplane object.
#' @param \dots Other parameters passed to \code{\link{propertyControl}}.
#' @return A list of class \code{"rglControl"} of cleaned up parameter values,
#' to be used in an rgl widget.
#' @author Duncan Murdoch
#' @examples
#' 
#'   open3d()
#'   saveopts <- options(rgl.useNULL = TRUE)
#'   xyz <- matrix(rnorm(300), ncol = 3)
#'   id <- plot3d(xyz, type="s", col = "blue", zlim = c(-3,3))["clipplanes"]
#'   dvals <- c(3, -3)
#'   widget <- rglwidget() %>%
#'     playwidget(clipplaneControl(d = dvals, clipplaneids = id),
#'                start = 0, stop = 1, step = 0.01,
#'                rate = 0.5)
#'   if (interactive())
#'     widget
#'   options(saveopts)
#' 
clipplaneControl <- function(a = NULL, b = NULL, c = NULL, d = NULL,
                             plane = 1, clipplaneids,
                             ...) {
  values <- cbind(a = a, b = b, c = c, d = d)
  col <- which(colnames(values) == letters[1:4]) - 1
  propertyControl(
    values = values, entries = 4 * (plane - 1) + col,
    properties = "vClipplane", objids = clipplaneids,
    ...
  )
}



#' Set attributes of vertices based on their age.
#' 
#' This is a function to produce actions in response to a
#' \code{\link{playwidget}} or Shiny input control.  The mental model is that
#' each of the vertices of some object has a certain birth time; a control sets
#' the current time, so that vertices have ages depending on the control
#' setting. Attributes of those vertices can then be changed.
#' 
#' All attributes must have the same number of entries (rows for the matrices)
#' as the ages vector. The births vector must have the same number of entries
#' as the number of vertices in the object.
#' 
#' Not all objects contain all attributes; if one is chosen that is not a
#' property of the corresponding object, a Javascript \code{alert()} will be
#' generated.  (This restriction may be removed in the future by attempting to
#' add the attribute when it makes sense.)
#' 
#' If a \code{births} entry is \code{NA}, no change will be made to that
#' vertex.
#' 
#' @param births Numeric birth times of vertices.
#' @param ages Chosen ages at which the following attributes will apply.
#' @param objids Object ids to which the changes apply.
#' @param value Initial value; typically overridden by input.
#' @param colors,alpha,radii,vertices,normals,origins,texcoords Attributes of
#' the vertices that can be changed.  There should be one entry or row for each
#' entry in \code{ages}.
#' @param x,y,z,red,green,blue These one-dimensional components of vertices and
#' colors are provided for convenience.
#' @return A list of class \code{"rglControl"} of cleaned up parameter values,
#' to be used in an rgl widget.
#' @author Duncan Murdoch
#' @examples
#' 
#'   saveopts <- options(rgl.useNULL = TRUE)
#' 
#'   theta <- seq(0, 4*pi, len=100)
#'   xyz <- cbind(sin(theta), cos(theta), sin(theta/2))
#'   lineid <- plot3d(xyz, type="l", alpha = 0, lwd = 5, col = "blue")["data"]
#' 
#'   widget <- rglwidget() %>%
#'   playwidget(ageControl(births = theta,
#'                         ages = c(-4*pi, -4*pi, 1-4*pi, 0, 0, 1),
#'                         objids = lineid,
#'                         alpha = c(0, 1, 0, 0, 1, 0)),
#'              start = 0, stop = 4*pi,
#'              step = 0.1, rate = 4)
#'   if (interactive())
#'     widget
#'   options(saveopts)
#' 
ageControl <- function(births, ages, objids, value = 0, colors = NULL, alpha = NULL,
                       radii = NULL, vertices = NULL, normals = NULL,
                       origins = NULL, texcoords = NULL,
                       x = NULL, y = NULL, z = NULL,
                       red = NULL, green = NULL, blue = NULL) {
  lengths <- c(
    colors = NROW(colors), alpha = length(alpha),
    radii = length(radii), vertices = NROW(vertices),
    normals = NROW(normals), origins = NROW(origins),
    texcoords = NROW(texcoords),
    x = length(x), y = length(y), z = length(z),
    red = length(red), green = length(green), blue = length(blue)
  )
  lengths <- lengths[lengths > 0]
  n <- unique(lengths)
  stopifnot(length(n) == 1, n == length(ages), all(diff(ages) >= 0))

  ages <- c(-Inf, ages, Inf)
  rows <- c(1, 1:n, n)

  result <- list(
    type = "ageSetter",
    objids = as.integer(objids),
    value = value,
    births = births,
    ages = ages
  )

  if (!is.null(colors)) {
    colors <- col2rgb(colors) / 255
    colors <- as.numeric(colors[, rows])
    result <- c(result, list(colors = colors))
  }

  if (!is.null(alpha)) {
    result <- c(result, list(alpha = alpha[rows]))
  }

  if (!is.null(radii)) {
    result <- c(result, list(radii = radii[rows]))
  }

  if (!is.null(vertices)) {
    stopifnot(ncol(vertices) == 3)
    result <- c(result, list(vertices = as.numeric(t(vertices[rows, ]))))
  }

  if (!is.null(normals)) {
    stopifnot(ncol(normals) == 3)
    result <- c(result, list(normals = as.numeric(t(normals[rows, ]))))
  }

  if (!is.null(origins)) {
    stopifnot(ncol(origins) == 2)
    result <- c(result, list(origins = as.numeric(t(origins[rows, ]))))
  }

  if (!is.null(texcoords)) {
    stopifnot(ncol(texcoords) == 2)
    result <- c(result, list(texcoords = as.numeric(t(texcoords[rows, ]))))
  }

  if (!is.null(x)) {
    result <- c(result, list(x = x[rows]))
  }

  if (!is.null(y)) {
    result <- c(result, list(y = y[rows]))
  }

  if (!is.null(z)) {
    result <- c(result, list(z = z[rows]))
  }

  if (!is.null(red)) {
    result <- c(result, list(red = red[rows]))
  }

  if (!is.null(green)) {
    result <- c(result, list(green = green[rows]))
  }

  if (!is.null(blue)) {
    result <- c(result, list(blue = blue[rows]))
  }

  structure(result, class = "rglControl")
}



#' Set attributes of vertices.
#' 
#' This is a function to produce actions in a web display.  A
#' \code{\link{playwidget}} or Shiny input control (e.g. a
#' \code{\link[shiny]{sliderInput}} control) sets a value which controls
#' attributes of a selection of vertices.
#' 
#' This function modifies attributes of vertices in a single object.  The
#' \code{attributes} are properties of each vertex in a scene; not all are
#' applicable to all objects. In order, they are: coordinates of the vertex
#' \code{"x", "y", "z"}, color of the vertex \code{"red", "green", "blue",
#' "alpha"}, normal at the vertex \code{"nx", "ny", "nz"}, radius of a sphere
#' at the vertex \code{"radius"}, origin within a texture \code{"ox", "oy"} and
#' perhaps \code{"oz"}, texture coordinates \code{"ts", "tt"}.
#' 
#' Planes are handled specially.  The coefficients \code{a, b, c} in the
#' \code{\link{planes3d}} or \code{\link{clipplanes3d}} specification are
#' controlled using \code{"nx", "ny", "nz"}, and \code{d} is handled as
#' \code{"offset"}.  The \code{vertices} argument is interpreted as the indices
#' of the planes when these attributes are set.
#' 
#' If only one attribute of one vertex is specified, \code{values} may be given
#' as a vector and will be treated as a one-column matrix.  Otherwise
#' \code{values} must be given as a matrix with \code{ncol(values) ==
#' max(length(vertices), length(attributes))}.  The \code{vertices} and
#' \code{attributes} vectors will be recycled to the same length, and entries
#' from column \code{j} of \code{values} will be applied to vertex
#' \code{vertices[j]}, attribute \code{attributes[j]}.
#' 
#' The \code{value} argument is translated into a row (or two rows if
#' \code{interp = TRUE}) of \code{values} by finding its location in
#' \code{param}.
#' 
#' @param value The value to use for input (typically \code{input$value} in a
#' Shiny app.)  Not needed with \code{\link{playwidget}}.
#' @param values A matrix of values, each row corresponding to an input value.
#' @param vertices Which vertices are being controlled?  Specify
#' \code{vertices} as a number from 1 to the number of vertices in the
#' \code{objid}.
#' @param attributes A vector of attributes of a vertex, from \code{c("x", "y",
#' "z", "red", "green", "blue", "alpha", "nx", "ny", "nz", "radius", "ox",
#' "oy", "oz", "ts", "tt", "offset")}.  See Details.
#' @param objid A single \pkg{rgl} object id.
#' @param param Parameter values corresponding to each row of \code{values}.
#' @param interp Whether to interpolate between rows of \code{values}.
#' @return A list of class \code{"rglControl"} of cleaned up parameter values,
#' to be used in an rgl widget.
#' @author Duncan Murdoch
#' @examples
#' 
#'   saveopts <- options(rgl.useNULL = TRUE)
#' 
#'   theta <- seq(0, 6*pi, len=100)
#'   xyz <- cbind(sin(theta), cos(theta), theta)
#'   plot3d(xyz, type="l")
#'   id <- spheres3d(xyz[1,,drop=FALSE], col="red")
#' 
#'   widget <- rglwidget(width=500, height=300) %>%
#'   playwidget(vertexControl(values=xyz,
#'                            attributes=c("x", "y", "z"),
#'                            objid = id, param=1:100),
#'              start = 1, stop = 100, rate=10)
#'   if (interactive())
#'     widget
#'   options(saveopts)
#' 
vertexControl <- function(value = 0, values = NULL, vertices = 1, attributes, objid,
                          param = seq_len(NROW(values)) - 1, interp = TRUE) {
  attributes <- match.arg(attributes,
    choices = c(
      "x", "y", "z",
      "red", "green", "blue", "alpha",
      "radii",
      "nx", "ny", "nz",
      "ox", "oy", "oz",
      "ts", "tt",
      "offset"
    ),
    several.ok = TRUE
  )
  if (!is.null(values)) {
    ncol <- max(length(vertices), length(attributes))
    if (is.matrix(values)) {
      stopifnot(ncol == ncol(values))
    } else {
      stopifnot(ncol == 1)
      values <- matrix(values, ncol = 1)
    }
    # Repeat first and last values to make search simpler.
    param <- c(-Inf, param, Inf)
    values <- rbind(values[1, ], values, values[nrow(values), ])
  }

  structure(list(
    type = "vertexSetter",
    value = value,
    values = values,
    vertices = vertices - 1, # Javascript 0-based indexing
    attributes = attributes,
    objid = as.integer(objid),
    param = param, # Javascript 0-based indexing
    interp = interp
  ),
  class = "rglControl"
  )
}



#' Control rgl widget like par3dinterp().
#' 
#' This control works with \code{\link{playwidget}} to change settings in a
#' WebGL display in the same way as \code{\link{par3dinterp}} does within R.
#' 
#' \code{par3dinterpSetter} sets parameters corresponding to values produced by
#' the result of \code{par3dinterp}.
#' 
#' @param fn A function returned from \code{\link{par3dinterp}}.
#' @param from,to,steps Values where \code{fn} should be evaluated.
#' @param subscene Which subscene's properties should be modified?
#' @param omitConstant If \code{TRUE}, do not set values that are constant
#' across the range.
#' @param ... Additional parameters which will be passed to
#' \code{\link{propertyControl}}.
#' @return Returns controller data in a list of class "rglControl".
#' @author Duncan Murdoch
#' @examples
#' 
#' example(plot3d)
#' M <- r3dDefaults$userMatrix
#' fn <- par3dinterp(times = (0:2)*0.75, userMatrix = list(M,
#'                                       rotate3d(M, pi/2, 1, 0, 0),
#'                                       rotate3d(M, pi/2, 0, 1, 0)),
#'                                       scale = c(0.5, 1, 2))
#' control <- par3dinterpControl(fn, 0, 3, steps = 15)
#' control      
#' if (interactive()) 
#'   rglwidget(width = 500, height = 250) %>%
#'   playwidget(control,
#'        step = 0.01, loop = TRUE, rate = 0.5)
#' 
par3dinterpControl <- function(fn, from, to, steps, subscene = NULL,
                               omitConstant = TRUE, ...) {
  rename <- character()
  times <- seq(from, to, length.out = steps + 1)
  fvals <- lapply(times, fn)
  f0 <- fvals[[1]]
  entries <- numeric(0)
  properties <- character(0)
  values <- NULL

  props <- c("FOV", "userMatrix", "scale", "zoom")
  for (i in seq_along(props)) {
    prop <- props[i]
    propname <- rename[prop]
    if (is.na(propname)) {
      propname <- prop
    }
    if (!is.null(value <- f0[[prop]])) {
      newvals <- sapply(fvals, function(e) as.numeric(e[[prop]]))
      if (is.matrix(newvals)) newvals <- t(newvals)
      rows <- NROW(newvals)
      cols <- NCOL(newvals)
      stopifnot(rows == length(fvals))
      entries <- c(entries, seq_len(cols) - 1)
      properties <- c(properties, rep(propname, cols))
      values <- cbind(values, newvals)
    }
  }
  if (omitConstant) {
    keep <- apply(values, 2, var) > 0
  } else {
    keep <- TRUE
  }

  if (is.null(subscene)) subscene <- f0$subscene

  propertyControl(
    values = c(t(values[, keep])), entries = entries[keep],
    properties = properties[keep],
    objids = subscene, param = times, ...
  )
}

# This is a bridge to the old system
# In the old system, the rglClass object was a global named
# <prefix>rgl, and controls install methods on it.  In the
# new system, the rglClass object is just a field of a <div>
# element.  The R code below creates an empty global for the
# controls to modify, then the Javascript code in oldBridge
# imports those into the real scene object.



#' Use widget with old-style controls.
#' 
#' The \code{\link{rglwidget}} control is designed to work in the
#' \pkg{htmlwidgets} framework.  Older \pkg{rgl} web pages that used
#' \code{\link{writeWebGL}} or \pkg{knitr} used a different method of linking
#' the controls to the scene.  This is a partial bridge between the two
#' systems.  You should adopt the new system, not use this function.
#' 
#' 
#' @param elementId An element identifier from a \code{\link{rglwidget}} call.
#' @param prefix The prefix to use in the old-style control.
#' @return This function generates Javascript code, so it should be used in an
#' \code{results = "asis"} block in a \pkg{knitr} document.
#' @author Duncan Murdoch
elementId2Prefix <- function(elementId, prefix = elementId) {
  .Deprecated("", msg = "This function is not needed if you use rglwidget().")
  cat(paste0("<script>var ", prefix, "rgl = {};</script>"))
  playwidget(elementId, structure(list(
    type = "oldBridge",
    prefix = prefix
  ),
  class = "rglControl"
  ),
  components = character(0)
  )
}

# This puts together a custom message for a more extensive change



#' Make large change to a scene from Shiny
#' 
#' These functions allow Shiny apps to make relatively large changes to a
#' scene, adding and removing objects from it.
#' 
#' \code{registerSceneChange} must be called in the UI component of a Shiny app
#' to register the \code{"sceneChange"} custom message.
#' 
#' @aliases sceneChange registerSceneChange
#' @param elementId The id of the element holding the \code{rglClass} instance.
#' @param x The new scene to use as a source for objects to add.
#' @param delete,add,replace Object ids to modify in the scene.  The
#' \code{delete} and \code{replace} ids must be present in the old scene in the
#' browser; the \code{add} and \code{replace} ids must be present in \code{x}.
#' @param material Logical to indicate whether default material should be
#' updated.
#' @param rootSubscene Logical to indicate whether root subscene should be
#' updated.
#' @param delfromSubscenes A vector of subscene ids that may have been changed
#' by deletions.  By default, all subscenes in \code{x} are used, but the
#' objects may be included in subscenes in the browser that are different.
#' @param skipRedraw If \code{TRUE}, stop the scene from redrawing until
#' \code{skipRedraw=FALSE} is sent.  If \code{NA}, don't redraw this time, but
#' don't change the state of the \code{skipRedraw} flag.
#' @param minimal See \code{\link{scene3d}}.
#' @return \code{registerSceneChange} returns the HTML code to register the
#' message.
#' 
#' \code{sceneChange} returns a list to be used as the \code{"sceneChange"}
#' message to change the scene.  Use
#' \code{\link[shiny:session]{shiny::session$sendCustomMessage}} to send it.
#' @author Duncan Murdoch
#' @seealso \code{\link{playwidget}} for a different approach to modifying
#' scenes that can be much faster, but may be less flexible.  The Shiny demo in
#' this package makes use of all of these approaches.
#' @examples
#' 
#' \dontrun{
#' shinyUI(fluidPage(
#'   registerSceneChange(),
#'   actionButton("thebutton", "Change")
#' ))
#' 
#' shinyServer(function(input, output, session) {
#'   observeEvent(input$thebutton, {
#'     session$sendCustomMessage("sceneChange",
#'       sceneChange("thewidget", delete = deletes, add = adds))
#'   })
#' })
#' }
#' 
sceneChange <- function(elementId, x = scene3d(minimal),
                        delete = NULL, add = NULL, replace = NULL,
                        material = FALSE, rootSubscene = FALSE,
                        delfromSubscenes = NULL, skipRedraw = FALSE,
                        minimal = TRUE) {
  allSubscenes <- function() {
    result <- numeric()
    for (obj in scene$objects) {
      if (obj$type == "subscene") {
        result <- c(result, obj$id)
      }
    }
    result
  }
  inSubscenes <- function(id, subs) {
    result <- numeric()
    for (sub in subs) {
      if (id %in% sub$objects) {
        result <- c(result, sub$id)
      }
    }
    result
  }
  delete <- unique(c(delete, replace))
  add <- unique(c(add, replace))

  scene <- convertScene(x)
  allsubids <- allSubscenes()
  allsubs <- scene$objects[as.character(allsubids)]
  for (id in add) {
    scene$objects[[as.character(id)]]$inSubscenes <- inSubscenes(id, allsubs)
  }

  scene$elementId <- elementId
  allIds <- names(scene$objects)
  dontSend <- setdiff(allIds, as.character(add))
  scene$objects[dontSend] <- NULL
  if (!length(scene$objects)) {
    scene$objects <- NULL
  }
  scene$sphereVerts <- NULL
  if (!material) {
    scene$material <- NULL
  }
  if (!rootSubscene) {
    scene$rootSubscene <- NULL
  }
  scene$delete <- delete
  if (is.null(delfromSubscenes)) {
    delfromSubscenes <- allsubids
  }
  scene$delfromSubscenes <- as.numeric(delfromSubscenes)
  if (is.na(skipRedraw)) {
    scene$redrawScene <- FALSE
  } else {
    scene$redrawScene <- !skipRedraw
    scene$skipRedraw <- skipRedraw
  }
  scene
}

registerSceneChange <- function() {
  tags$script('
Shiny.addCustomMessageHandler("sceneChange",
  rglwidgetClass.prototype.sceneChangeHandler);
')
}
