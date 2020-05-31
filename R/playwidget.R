#' Widget output function for use in Shiny
#'
#' @export
playwidgetOutput <- function(outputId, width = "0px", height = "0px") {
  shinyWidgetOutput(outputId, "rglPlayer", width, height, package = "rgl")
}

#' Widget render function for use in Shiny
#'
#' @export
renderPlaywidget <- function(expr, env = parent.frame(), quoted = FALSE, outputArgs = list()) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  markRenderFunction(playwidgetOutput,
    shinyRenderWidget(expr, playwidgetOutput, env, quoted = TRUE),
    outputArgs = outputArgs
  )
}



#' Add a widget to play animations.
#' 
#' This is a widget that can be put in a web page to allow animations with or
#' without Shiny.
#' 
#' The \code{components} are buttons to control the animation, a slider for
#' manual control, and a label to show the current value.  They will be
#' displayed in the order given in \code{components}.  Not all need be
#' included.
#' 
#' The buttons have the following behaviour: \describe{ \item{Reverse}{Reverse
#' the direction.} \item{Play}{Play the animation.} \item{Slower}{Decrease the
#' playing speed.} \item{Faster}{Increase the playing speed.} \item{Reset}{Stop
#' the animation and reset to the start value.} }
#' 
#' If \code{respondTo} is used, no \code{components} are shown, as it is
#' assumed Shiny (or whatever control is being referenced) will provide the UI
#' components.
#' 
#' The \code{sceneId} component can be another \code{playwidget}, a
#' \code{\link{rglwidget}} result, or a result of
#' \code{htmltools::\link[htmltools:builder]{tags}} or
#' \code{htmltools::\link[htmltools:tag]{tagList}}.  This allows you to use a
#' \pkg{magrittr}-style \dQuote{pipe} command to join an \code{rglwidget} with
#' one or more \code{\link{playwidget}}s.  If a \code{playwidget} comes first,
#' \code{sceneId} should be set to \code{NA}.  If the \code{\link{rglwidget}}
#' does not come first, previous values should be piped into its
#' \code{controllers} argument.  Other HTML code (including other widgets) can
#' be used in the chain if wrapped in
#' \code{htmltools::\link[htmltools:tag]{tagList}}.
#' 
#' @param sceneId The HTML id of the rgl scene being controlled, or an object.
#' See the Details below.
#' @param controls A single \code{"rglControl"} object, e.g.
#' \code{\link{propertyControl}}, or a list of several.
#' @param start,stop The starting and stopping values of the animation.  If
#' \code{labels} is supplied \code{stop} will default to step through the
#' labels.
#' @param interval The requested interval (in seconds) between updates.
#' Updates may occur at longer intervals.
#' @param rate The number of units of \dQuote{nominal} time per real world
#' second.
#' @param components Which components should be displayed?  See Details below.
#' @param loop When the player reaches the end of the interval, should it loop
#' back to the beginning?
#' @param step Step size in the slider.
#' @param labels Optional labels to use, corresponding to slider steps.  Set to
#' \code{NULL} for auto-generated labels.
#' @param precision If \code{labels=NULL}, the precision to use when displaying
#' timer values.
#' @param elementId The HTML id of the generated widget, containing buttons,
#' slider, etc.
#' @param respondTo The HTML ID of a Shiny input control (e.g. a
#' \code{\link[shiny]{sliderInput}} control) to respond to.
#' @param reinit A vector of ids that will need re-initialization before being
#' drawn again.
#' @param buttonLabels,pause These are the labels that will be shown on the
#' buttons if they are displayed.  \code{pause} will be shown on the
#' \code{"Play"} button while playing.
#' @param height The height of the widget in pixels.  In a pipe, this is a
#' relative height.
#' @param ... Additional arguments to pass to to
#' \code{htmlwidgets::\link{createWidget}}.
#' @return A widget suitable for use in an \pkg{Rmarkdown}-generated web page,
#' or elsewhere.
#' @section Appearance: The appearance of the controls is set by the stylesheet
#' in \code{system.file("htmlwidgets/lib/rglClass/rgl.css")}.
#' 
#' The overall widget is of class \code{rglPlayer}, with id set according to
#' \code{elementId}.
#' 
#' The buttons are of HTML class \code{rgl-button}, the slider is of class
#' \code{rgl-slider}, and the label is of class \code{rgl-label}.  Each element
#' has an id prefixed by the widget id, e.g. \code{elementId-button-Reverse},
#' \code{elementId-slider}, etc. (where \code{elementId} should be replaced by
#' the actual id).
#' 
#' The \code{reinit} parameter handles the case where an object needs
#' re-initialization after each change.  For example, plane objects may need
#' this if their intersection with the bounding box changes shape.  Note that
#' re-initialization is generally incompatible with the
#' \code{\link{vertexControl}} as it modifies values which are set during
#' initialization.
#' @author Duncan Murdoch
#' @seealso \code{\link{subsetControl}}, \code{\link{propertyControl}},
#' \code{\link{ageControl}} and \code{\link{vertexControl}} are possible
#' controls to use.
#' 
#' \code{\link{toggleWidget}} is a wrapper for \code{playwidget} and
#' \code{\link{subsetControl}} to insert a single button to toggle some
#' elements in a display.
#' @examples
#' 
#' saveopts <- options(rgl.useNULL = TRUE)
#' 
#' objid <- plot3d(1:10, 1:10, rnorm(10), col=c("red", "red"), type = "s")["data"]
#' 
#' control <- ageControl(value=0,
#'              births=1:10,
#'              ages = c(-5,0,5),
#'              colors = c("green", "yellow", "red"),
#'              objids = objid)
#' 
#' \donttest{
#' # This example uses explicit names
#' rglwidget(elementId = "theplot", controllers = "theplayer",
#'           height = 300, width = 300)
#' playwidget("theplot", control, start = -5, stop = 5,
#'            rate = 3, elementId = "theplayer",
#'            components = c("Play", "Slider"))
#' }
#' 
#' # This example uses pipes, and can skip the names
#' 
#' widget <- rglwidget(height = 300, width = 300) %>%
#' playwidget(control, start = -5, stop = 5,
#'            rate = 3, components = c("Play", "Slider"))
#' if (interactive())
#'   widget
#' 
#' options(saveopts)
#' 
playwidget <- function(sceneId, controls, start = 0, stop = Inf, interval = 0.05, rate = 1,
                       components = c("Reverse", "Play", "Slower", "Faster", "Reset", "Slider", "Label"),
                       loop = TRUE,
                       step = 1, labels = NULL,
                       precision = 3,
                       elementId = NULL, respondTo = NULL,
                       reinit = NULL,
                       buttonLabels = components,
                       pause = "Pause",
                       height = 40,
                       ...) {
  if (is.null(elementId) && !inShiny()) {
    elementId <- newElementId("rgl-play")
  }

  # sceneId = NA turns into prevRglWidget = NULL
  if (is.character(sceneId) && !is.na(sceneId)) {
    upstream <- list(prevRglWidget = sceneId)
  } else {
    upstream <- processUpstream(sceneId, playerId = elementId)
  }

  if (!is.null(respondTo)) {
    components <- buttonLabels <- NULL
  }

  if (length(stop) != 1 || !is.finite(stop)) stop <- NULL

  if (identical(controls, NA)) {
    stop(dQuote("controls"), " should not be NA.")
  }

  stopifnot(is.list(controls))

  if (inherits(controls, "rglControl")) {
    controls <- list(controls)
  }

  types <- vapply(controls, class, "")
  if (any(bad <- types != "rglControl")) {
    bad <- which(bad)[1]
    stop("Controls should be of class 'rglControl', control ", bad, " is ", types[bad])
  }
  names(controls) <- NULL

  if (length(reinit)) {
    bad <- vapply(controls, function(x) x$type == "vertexSetter" && length(intersect(reinit, x$objid)), FALSE)
    if (any(bad)) {
      warning("'vertexControl' is incompatible with re-initialization")
    }
  }

  if (!length(components)) {
    components <- character()
  } else {
    components <- match.arg(components,
      c("Reverse", "Play", "Slower", "Faster", "Reset", "Slider", "Label", "Step"),
      several.ok = TRUE
    )
  }

  buttonLabels <- as.character(buttonLabels)
  pause <- as.character(pause)
  stopifnot(
    length(buttonLabels) == length(components),
    length(pause) == 1
  )

  for (i in seq_along(controls)) {
    control <- controls[[i]]
    if (!is.null(labels)) {
      labels <- control$labels
    }
    if (!is.null(control$param)) {
      start <- min(start, control$param[is.finite(control$param)])
      stop <- max(stop, control$param[is.finite(control$param)])
    }
  }

  if (is.null(stop) && !missing(labels) && length(labels)) {
    stop <- start + (length(labels) - 1) * step
  }

  if (is.null(stop)) {
    if ("Slider" %in% components) {
      warning("Cannot have slider with non-finite limits")
      components <- setdiff(components, "Slider")
    }
    labels <- NULL
  } else {
    if (stop == start) {
      warning("'stop' and 'start' are both ", start)
    }
  }

  control <- list(
    actions = controls,
    start = start,
    stop = stop,
    value = start,
    interval = interval,
    rate = rate,
    components = components,
    buttonLabels = buttonLabels,
    pause = pause,
    loop = loop,
    step = step,
    labels = labels,
    precision = precision,
    reinit = reinit,
    sceneId = upstream$prevRglWidget,
    respondTo = respondTo
  )

  result <- createWidget(
    name = "rglPlayer",
    x = control,
    elementId = elementId,
    package = "rgl",
    height = height,
    sizingPolicy = sizingPolicy(
      defaultWidth = "auto",
      defaultHeight = "auto"
    ),
    ...
  )
  if (is.list(upstream$objects)) {
    do.call(
      combineWidgets,
      c(
        upstream$objects,
        list(combineWidgets(result, nrow = 1),
          rowsize = c(upstream$rowsizes, height),
          ncol = 1
        )
      )
    )
  } else {
    result
  }
}



#' An HTML widget to toggle display of elements of a scene.
#' 
#' This function produces a button in an HTML scene that will toggle the
#' display of items in the scene.
#' 
#' Like \code{\link{playwidget}}, this function is designed to work within the
#' \pkg{htmlwidgets} framework.  If the value is printed, the button will be
#' inserted into the output.
#' 
#' It is also designed to work with \pkg{magrittr}-style pipes: the result of
#' \code{\link{rglwidget}} or other widgets can be piped into it to add it to a
#' display.  It can also appear first in the pipeline, if \code{sceneId} is set
#' to \code{NA}.
#' 
#' @param sceneId The HTML id of the rgl scene being controlled, or an object
#' as in \code{\link{playwidget}}.
#' @param ids,hidden The rgl id numbers of the objects to toggle.  Those in
#' \code{ids} are initially shown; those in \code{hidden} are initially hidden.
#' @param subscenes The subscenes in which to toggle the objects.
#' @param label The label to put on the button.
#' @param \dots Additional arguments to pass to \code{\link{playwidget}}.
#' @return A widget suitable for use in an \pkg{Rmarkdown}-generated web page,
#' or elsewhere.
#' @author Duncan Murdoch
#' @seealso \code{\link{toggleButton}} for the older style of HTML control.
#' @examples
#' 
#' theplot <- plot3d(rnorm(100), rnorm(100), rnorm(100), col = "red")
#' widget <- rglwidget(height = 300, width = 300) %>%
#'   toggleWidget(theplot["data"], 
#'                hidden = theplot[c("xlab", "ylab", "zlab")], 
#'                label = "Points")
#' if (interactive())
#'   widget
#' 
toggleWidget <- function(sceneId, ids = integer(),
                         hidden = integer(),
                         subscenes = NULL,
                         label = deparse(substitute(ids)),
                         ...) {
  playwidget(sceneId,
    subsetControl(subsets = list(ids, hidden), subscenes = subscenes),
    start = 0, stop = 1,
    components = "Step",
    buttonLabels = label,
    interval = 1,
    ...
  )
}
