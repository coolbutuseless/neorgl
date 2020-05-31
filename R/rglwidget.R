# Shiny objects if the widget sets elementId, so we
# need to detect it.  Thanks to Joe Cheng for suggesting this code.
inShiny <- function() !is.null(getDefaultReactiveDomain())

rmarkdownOutput <- function() {
  if (requireNamespace("rmarkdown")) {
    output <- rmarkdown::metadata$output
    if (length(output)) {
      if (is.character(output)) {
        return(output[1])
      } else if (is.list(output) && length(names(output))) {
        return(names(output)[1])
      }
    }
  }
  NULL
}



#' Create shared data from an rgl object.
#' 
#' The \pkg{crosstalk} package provides a way for different parts of an
#' interactive display to communicate about datasets, using \dQuote{shared
#' data} objects.  When selection or filtering is performed in one view, the
#' result is mirrored in all other views.
#' 
#' This function allows vertices of rgl objects to be treated as shared data.
#' 
#' Some functions which normally work on dataframe-like datasets will accept
#' shared data objects in their place.
#' 
#' If a selection is in progress, the alpha value for unselected points is
#' multiplied by \code{deselectedFade}. If \code{deselectedColor} is
#' \code{NULL}, the color is left as originally specified; if not, the point is
#' changed to the color given by \code{deselectedColor}.
#' 
#' If no points have been selected, then by default points are shown in their
#' original colors.  However, if \code{selectedIgnoreNone = FALSE}, all points
#' are displayed as if unselected.
#' 
#' The \code{selectedColor} argument is similarly used to change the color (or
#' not) of selected points, and \code{filteredFade} and \code{filteredColor}
#' are used for points that have been filtered out of the display.
#' 
#' @param id An existing rgl id.
#' @param key Optional unique labels to apply to each vertex. If missing,
#' numerical keys will be used.
#' @param group Optional name of the shared group to which this data belongs.
#' If missing, a random name will be generated.
#' @param deselectedFade,deselectedColor Appearance of points that are not
#' selected. See Details.
#' @param selectedColor Appearance of points that are selected.
#' @param selectedIgnoreNone If no points are selected, should the points be
#' shown in their original colors (\code{TRUE}), or in the deselected colors
#' (\code{FALSE})?
#' @param filteredFade,filteredColor Appearance of points that have been
#' filtered out.
#' @return An object of class \code{"SharedData"} which contains the x, y and z
#' coordinates of the rgl object with the given \code{id}.
#' @author Duncan Murdoch
#' @references \url{https://rstudio.github.io/crosstalk/index.html}
#' @examples
#' 
#'   save <- options(rgl.useNULL = TRUE)
#'   open3d()
#'   x <- sort(rnorm(100))
#'   y <- rnorm(100)
#'   z <- rnorm(100) + atan2(x, y)
#'   ids <- plot3d(x, y, z, col = rainbow(100))
#'   # The data will be selected and filtered, the the axes.
#'   sharedData <- rglShared(ids["data"])
#'   
#'   # Also add some labels that are only displayed
#'   # when points are selected
#'   
#'   sharedLabel <- rglShared(text3d(x, y, z, text = 1:100,
#'                                   adj = -0.5),
#'                            group = sharedData$groupName(),
#'                            deselectedFade = 0,
#'                            selectedIgnoreNone = FALSE) 
#'   if (interactive()) 
#'     crosstalk::filter_slider("x", "x", sharedData, ~x) %>%
#'     rglwidget(shared = list(sharedData, sharedLabel), controller = .) %>% 
#'     rglMouse()
#'       
#'   options(save)
#'   
rglShared <- function(id, key = NULL, group = NULL,
                      deselectedFade = 0.1,
                      deselectedColor = NULL,
                      selectedColor = NULL,
                      selectedIgnoreNone = TRUE,
                      filteredFade = 0,
                      filteredColor = NULL) {
  data <- as.data.frame(rgl.attrib(id, "vertices"))
  attr(data, "rglId") <- as.integer(id)
  attr(data, "rglOptions") <- list(
    deselectedFade = deselectedFade,
    deselectedColor = if (!is.null(deselectedColor)) as.numeric(col2rgb(deselectedColor, alpha = TRUE) / 255),
    selectedColor = if (!is.null(selectedColor)) as.numeric(col2rgb(selectedColor, alpha = TRUE) / 255),
    selectedIgnoreNone = selectedIgnoreNone,
    filteredFade = filteredFade,
    filteredColor = if (!is.null(filteredColor)) as.numeric(col2rgb(filteredColor, alpha = TRUE) / 255)
  )
  n <- nrow(data)
  if (!n) {
    stop("No vertices in object ", id)
  }
  if (!is.null(key) && (n != length(key) || anyDuplicated(key))) {
    stop("'key' must have exactly one unique value for each vertex")
  }
  result <- if (is.null(group)) {
    SharedData$new(data, key)
  } else {
    SharedData$new(data, key, group)
  }
  structure(result, class = c("rglShared", class(result)))
}

CSStoPixels <- function(x, DPI = 100) {
  if (is.null(x)) {
    return(x)
  }
  num <- function(x) {
    as.numeric(sub("[^[:digit:].]*$", "", x))
  }
  units <- function(x) {
    sub("^[[:digit:].]+", "", x)
  }
  if (!is.numeric(x)) {
    units <- units(x)
    if (units == "auto") {
      stop("Only fixed CSS sizes allowed")
    }
    val <- num(x)
    if (units != "") {
      val <- switch(units,
        "px" = val,
        "in" = val * DPI,
        "cm" = val * DPI / 2.54,
        "mm" = val * DPI / 254,
        "pt" = val * DPI / 72,
        "pc" = val * DPI / 6,
        stop("Only fixed CSS sizes allowed")
      )
    }
  } else {
    val <- x
  }
  val
}

# These sizes are taken from htmlwidgets/R/sizing.R
DEFAULT_WIDTH <- 960
DEFAULT_HEIGHT <- 500
DEFAULT_PADDING <- 40
DEFAULT_WIDTH_VIEWER <- 450
DEFAULT_HEIGHT_VIEWER <- 350
DEFAULT_PADDING_VIEWER <- 15

# For widgets, we use the sizingPolicy to see how it would be
# displayed
resolveHeight <- function(x, inViewer = TRUE, default = 40) {
  if (inViewer) {
    refsize <- DEFAULT_HEIGHT_VIEWER
  } else {
    refsize <- DEFAULT_HEIGHT
  }
  result <- x$height
  if (is.null(result) && !is.null(policy <- x$sizingPolicy)) {
    if (inViewer) {
      viewer <- policy$viewer
      if (isTRUE(viewer$fill)) {
        result <- refsize
      } else {
        result <- viewer$defaultHeight
      }
    } else {
      result <- NULL
    }
    if (is.null(result) && isTRUE(policy$fill)) {
      result <- refsize
    }
    if (is.null(result)) {
      result <- policy$defaultHeight
    }
    if (is.null(result)) {
      result <- refsize
    }
  }
  if (is.null(result)) {
    result <- default
  }

  CSStoPixels(result, refsize)
}

getWidgetId <- function(widget) {
  if (inherits(widget, "htmlwidget")) {
    widget$elementId
  } else {
    stop("object is not an html widget")
  }
}

# Get information from previous objects being piped into
# this one, and modify a copy of them as necessary

getHeights <- function(objects, defaultHeight = 40) {
  if (inherits(objects, "combineWidgets")) {
    heights <- objects$params$rowsize
  } else {
    if (inherits(objects, c("shiny.tag", "htmlwidget")) ||
      !is.list(objects)) {
      objects <- tagList(objects)
    }
    heights <- rep(defaultHeight, length(objects))
    for (i in seq_along(objects)) {
      tag <- objects[[i]]
      if (inherits(tag, "rglWebGL") && is.null(tag$height)) {
        heights[i] <- tag$x$height
      } else if (inherits(tag, "htmlwidget")) {
        heights[i] <- resolveHeight(tag)
      } else if (is.list(tag) &&
        !is.null(tag$height) &&
        !is.na(height <- suppressWarnings(as.numeric(tag$height)))) {
        heights[i] <- height
      }
    }
  }
  heights
}

processUpstream <- function(upstream, elementId = NULL, playerId = NULL) {
  rowsizes <- getHeights(upstream)

  prevRglWidget <- NULL
  players <- character()

  if (inherits(upstream, "combineWidgets")) {
    upstream <- upstream$widgets
  }

  if (inherits(upstream, c("shiny.tag", "htmlwidget"))) {
    upstream <- tagList(upstream)
  }

  if (is.list(upstream)) {
    # Objects upstream of the current one may need to know about an rgl widget,
    # or this object may need to know about an upstream rgl widget.  Stop when
    # you find one.
    lookForRglWidget <- function(upstream) {
      prevRglWidget <- NULL
      players <- character()
      for (i in rev(seq_along(upstream))) {
        tag <- upstream[[i]]
        if (inherits(tag, "rglWebGL")) {
          prevRglWidget <- tag$elementId
          if (is.null(prevRglWidget)) {
            prevRglWidget <- tag$elementId <- upstream[[i]]$elementId <- newElementId("rgl")
          }
          if (!is.null(playerId) && !(playerId %in% tag$x$players)) {
            upstream[[i]]$x$players <- c(tag$x$players, playerId)
          }
        } else if (inherits(tag, "rglPlayer") && is.null(tag$x$sceneId)) {
          players <- c(players, tag$elementId)
          if (!is.null(elementId)) {
            upstream[[i]]$x$sceneId <- elementId
          }
        } else if (inherits(tag, "shiny.tag") && !tagHasAttribute(tag, "rglSceneId")) {
          upstream[[i]] <- tagAppendAttributes(tag, rglSceneId = elementId)
        } else if (inherits(tag, "combineWidgets")) {
          temp <- lookForRglWidget(tag$widgets)
          players <- c(players, temp$players)
          prevRglWidget <- temp$prevRglWidget
          upstream[[i]]$widgets <- temp$objects
        }
        if (!is.null(prevRglWidget)) {
          break
        }
      }
      list(
        objects = upstream,
        players = players,
        prevRglWidget = prevRglWidget
      )
    }
    result <- lookForRglWidget(upstream)
    result$rowsizes <- rowsizes
  } else {
    result <- list(
      objects = upstream,
      players = if (is.character(upstream)) upstream else character(),
      prevRglWidget = if (is.character(upstream)) upstream,
      rowsizes = rowsizes
    )
  }

  result
}



#' Convenience functions for rgl HTML layouts
#' 
#' The \code{asRow} function arranges objects in a row in the display; the
#' \code{getWidgetId} function extracts the HTML element ID from an HTML
#' widget.
#' 
#' \code{asRow} produces a \code{"combineWidgets"} object which is a single
#' column whose last element is another \code{"combineWidgets"} object which is
#' a single row.
#' 
#' If \code{n} objects are given as input and \code{last} is given a value less
#' than \code{n}, the first \code{n - last} objects will be displayed in a
#' column above the row containing the \code{last} objects.
#' 
#' @aliases asRow getWidgetId
#' @param \dots Either a single \code{"combineWidgets"} object produced by
#' \code{asRow} or a \code{%>%} pipe of \pkg{rgl} objects, or several objects
#' intended for rearrangement.
#' @param last If not \code{NA}, the number of objects from \code{...} that are
#' to be arranged in a row.  Earlier ones will remain in a column.
#' @param height An optional height for the resulting row.  This is normally
#' specified in pixels, but will be rescaled as necessary to fit the display.
#' @param colsize A vector of relative widths for the columns in the row.
#' @param widget A single HTML widget from which to extract the HTML element
#' ID.
#' @return \code{asRow} returns a single \code{"combineWidgets"} object
#' suitable for display or nesting within a more complicated display.
#' 
#' \code{getWidgetId} returns a character string containing the HTML element ID
#' of the widget.
#' @author Duncan Murdoch
#' @seealso \link{pipe} for the \code{%>%} operator.
#' @examples
#' 
#' library(crosstalk)
#' sd <- SharedData$new(mtcars)
#' ids <- plot3d(sd$origData(), col = mtcars$cyl, type = "s")
#' # Copy the key and group from existing shared data
#' rglsd <- rglShared(ids["data"], key = sd$key(), group = sd$groupName())
#' w <- rglwidget(shared = rglsd) %>%
#' asRow("Mouse mode: ", rglMouse(getWidgetId(.)), 
#'       "Subset: ", filter_checkbox("cylinderselector", 
#' 		                "Cylinders", sd, ~ cyl, inline = TRUE),
#'       last = 4, colsize = c(1,2,1,2), height = 60)
#' if (interactive())
#'   w
#' 
asRow <- function(..., last = NA, height = NULL, colsize = 1) {
  args <- list(...)
  if (length(args) == 1
  && inherits(args[[1]], "combineWidgets")) {
    orig <- args[[1]]
  } else {
    orig <- do.call(combineWidgets, c(args, list(ncol = 1, rowsize = getHeights(args))))
  }
  origlen <- length(orig$widgets)
  if (is.na(last)) {
    last <- origlen
  } else if (last > origlen) {
    stop("'last' must be no more than the number of widgets")
  }
  keep <- seq_len(origlen - last)
  inrow <- seq_len(last) + origlen - last
  origRowsizes <- rep_len(orig$params$rowsize, origlen)
  if (length(inrow)) {
    maxinrow <- max(origRowsizes[inrow])
    if (is.null(height)) {
      height <- maxinrow
    }
  } else if (is.null(height)) {
    height <- 0
  }

  orig$params$rowsize <- c(origRowsizes[keep], height)

  row <- do.call(combineWidgets, c(orig$widgets[inrow], list(
    nrow = 1,
    colsize = colsize
  )))
  orig$widgets <- c(orig$widgets[keep], list(row))
  orig
}

newElementId <- function(prefix) {
  paste0(prefix, p_sample(100000, 1))
}



#' An htmlwidget to hold an rgl scene.
#' 
#' The \pkg{htmlwidgets} package provides a framework for embedding graphical
#' displays in HTML documents of various types.  This function provides the
#' necessities to embed an \pkg{rgl} scene in one.
#' 
#' This produces a WebGL version of an \pkg{rgl} scene using the
#' \pkg{htmlwidgets} framework.  This allows display of the scene in the
#' RStudio IDE, a browser, an \pkg{rmarkdown} document or in a \pkg{shiny} app.
#' 
#' \code{options(rgl.printRglwidget = TRUE)} will cause \code{rglwidget()} to
#' be called and displayed when the result of an \pkg{rgl} call that changes
#' the scene is printed.
#' 
#' In a \pkg{shiny} app, there will often be one or more
#' \code{\link{playwidget}} objects in the app, taking input from the user.  In
#' order to be sure that the initial value of the user control is reflected in
#' the scene, you should list all players in the \code{controllers} argument.
#' See the sample application in \code{system.file("shinyDemo", package =
#' "rglwidget")} for an example.
#' 
#' In RMarkdown or in standalone code, you can use a \pkg{magrittr}-style
#' \dQuote{pipe} command to join an \code{rglwidget} with a
#' \code{\link{playwidget}} or \code{\link{toggleWidget}}.  If the control
#' widget comes first, it should be piped into the \code{controllers} argument.
#' If the \code{rglwidget} comes first, it can be piped into the first argument
#' of \code{playwidget} or \code{toggleWidget}.
#' 
#' If the \code{reuse} argument is \code{FALSE} (the default in interactive
#' use), earlier information will be cleared before drawing the new scene.  If
#' \code{TRUE}, earlier data will be re-used in the current scene, so it may be
#' smaller and faster to load.  In both cases information from the current
#' scene (added to earlier information if \code{reuse=TRUE}) will be saved for
#' possible use in a future scene. If \code{reuse=NA}, the saved information
#' will neither be used nor updated.
#' 
#' If \code{elementId} is \code{NULL} and we are not in a Shiny app,
#' \code{elementId} is set to a random value to facilitate re-use of
#' information.
#' 
#' To save the display to a file, use \code{htmlwidgets::\link{saveWidget}}.
#' This requires \command{pandoc} to be installed.
#' 
#' The \code{webGLoptions} argument is a list which will be passed when the
#' WebGL context is created.  See the WebGL 1.0 specification on
#' \url{https://www.khronos.org/registry/webgl/specs} for possible settings.
#' The default in \code{rglwidget} differs from the WebGL default by setting
#' \code{preserveDrawingBuffer = TRUE} in order to allow other tools to read
#' the image, but please note that some implementations of WebGL contain bugs
#' with this setting.  We have attempted to work around them, but may change
#' our default in the future if this proves unsatisfactory.
#' 
#' The \code{snapshot} argument can be a logical value or a character value.
#' If logical and \code{FALSE}, no snapshot will be included.  If \code{TRUE},
#' a snapshot will be taken of the current scene using \code{\link{snapshot3d}}
#' and included in the scene to use when WebGL errors occur.  If
#' \code{snapshot} is a character string starting \code{"data:"}, it will be
#' assumed to be the result of the \code{\link[knitr]{image_uri}} function.
#' Otherwise it will be assumed to be a filename, and
#' \code{\link[knitr]{image_uri}} will be used to embed it in the output page.
#' 
#' @aliases rglwidget rgl.printRglwidget
#' @param x An \pkg{rgl} scene produced by the \code{\link[rgl]{scene3d}}
#' function.
#' @param width,height The width and height of the display in pixels.
#' @param controllers Names of \code{\link{playwidget}} objects associated with
#' this scene, or objects (typically piped in).  See Details below.
#' @param snapshot Control of snapshot of scene.  See Details below.
#' @param elementId The id to use on the HTML \code{div} component that will
#' hold the scene.
#' @param reuse A logical variable indicating whether rgl objects from earlier
#' scenes should be referenced. See the Details below.
#' @param webGLoptions A list of options to pass to WebGL when the drawing
#' context is created.  See the Details below.
#' @param shared An object produced by \code{\link{rglShared}}, or a list of
#' such objects.
#' @param minimal Should attributes be skipped if they currently have no
#' effect?  See \code{\link{scene3d}}.
#' @param ... Additional arguments to pass to
#' \code{htmlwidgets::\link{createWidget}}.
#' @return An object of class \code{"htmlwidget"} (or \code{"shiny.tag.list"}
#' if pipes are used) that will intelligently print itself into HTML in a
#' variety of contexts including the R console, within R Markdown documents,
#' and within Shiny output bindings.
#' 
#' If \code{reuse = TRUE}, a record will be kept of objects in the scene and
#' they need not be included in the HTML generated for later scenes. This is
#' normally useful only in \pkg{rmarkdown} documents which can have many
#' \pkg{rgl} scenes; if the widget is displayed in RStudio, only one scene will
#' be shown.
#' 
#' If objects are passed in the \code{shared} argument, then the widget will
#' respond to selection and filtering applied to those as shared datasets.  See
#' \code{\link{rglShared}} for more details and an example.
#' @section Appearance: The appearance of the display is set by the stylesheet
#' in \code{system.file("htmlwidgets/lib/rglClass/rgl.css")}.
#' 
#' The widget is of class \code{rglWebGL}, with id set according to
#' \code{elementId}.  (As of this writing, no special settings are given for
#' class \code{rglWebGL}, but you can add your own.)
#' @author Duncan Murdoch
#' @seealso \code{\link{hook_webgl}} for an earlier approach to this problem.
#' \code{\link{rglwidgetOutput}} for Shiny details.
#' @examples
#' 
#' save <- getOption("rgl.useNULL")
#' options(rgl.useNULL=TRUE)
#' example("plot3d", "rgl")
#' widget <- rglwidget()
#' if (interactive())
#'   widget
#'   
#' \donttest{
#' # Save it to a file.  This requires pandoc
#' filename <- tempfile(fileext = ".html")
#' htmlwidgets::saveWidget(rglwidget(), filename)
#' browseURL(filename)
#' }
#' 
rglwidget <- local({
  reuseDF <- NULL

  function(x = scene3d(minimal), width = figWidth(), height = figHeight(),
           controllers = NULL, snapshot = FALSE,
           elementId = NULL,
           reuse = !interactive(),
           webGLoptions = list(preserveDrawingBuffer = TRUE),
           shared = NULL,
           minimal = TRUE, ...) {
    origScene <- x
    force(shared) # It might plot something...

    if (is.na(reuse)) {
      reuseDF <- NULL
    } # local change only
    else if (!reuse) {
      reuseDF <<- NULL
    }

    if (is.null(elementId) && (!inShiny() || !is.null(controllers))) {
      elementId <- newElementId("rgl")
    }

    if (!inherits(x, "rglscene")) {
      stop("First argument should be an rgl scene.")
    }

    if (!is.list(shared)) {
      shared <- list(shared)
    }
    if (length(shared)) {
      x$crosstalk <- list(
        key = vector("list", length(shared)),
        group = character(length(shared)),
        id = integer(length(shared)),
        options = vector("list", length(shared))
      )
      dependencies <- crosstalkLibs()
    } else {
      x$crosstalk <- list(
        key = list(),
        group = character(),
        id = integer(),
        options = list()
      )
      dependencies <- NULL
    }

    for (i in seq_along(shared)) {
      s <- shared[[i]]
      if (is.SharedData(s) && inherits(s, "rglShared")) {
        x$crosstalk$key[[i]] <- s$key()
        x$crosstalk$group[i] <- s$groupName()
        x$crosstalk$id[i] <- attr(s$origData(), "rglId")
        x$crosstalk$options[[i]] <- attr(s$origData(), "rglOptions")
      } else if (!is.null(s)) {
        stop("'shared' must be an object produced by rglShared() or a list of these")
      }
    }
    if (!is.null(width)) {
      width <- CSStoPixels(width)
    }
    if (!is.null(height)) {
      height <- CSStoPixels(height)
    }
    x <- convertScene(x, width, height,
      snapshot = snapshot,
      elementId = elementId, reuse = reuseDF
    )
    if (!is.na(reuse)) {
      reuseDF <<- attr(x, "reuse")
    }

    upstream <- processUpstream(controllers, elementId = elementId)
    x$players <- upstream$players

    x$webGLoptions <- webGLoptions

    # create widget
    attr(x, "TOJSON_ARGS") <- list(na = "string")
    result <- structure(htmlwidgets::createWidget(
      name = "rglWebGL",
      x = x,
      width = width,
      height = height,
      package = "rgl",
      elementId = elementId,
      dependencies = dependencies,
      ...
    ), rglReuse = attr(x, "reuse"), origScene = origScene)

    if (is.list(upstream$objects)) {
      do.call(combineWidgets, c(
        upstream$objects,
        list(result,
          rowsize = c(upstream$rowsizes, x$height),
          ncol = 1
        )
      ))
    } else {
      result
    }
  }
})

#' Widget output function for use in Shiny
#'
#' @export
rglwidgetOutput <- function(outputId, width = "512px", height = "512px") {
  shinyWidgetOutput(outputId, "rglWebGL", width, height, package = "rgl")
}

#' Widget render function for use in Shiny
#'
#' @export
renderRglwidget <- function(expr, env = parent.frame(), quoted = FALSE, outputArgs = list()) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  markRenderFunction(rglwidgetOutput,
    shinyRenderWidget(expr, rglwidgetOutput, env, quoted = TRUE),
    outputArgs = outputArgs
  )
}

shinySetPar3d <- function(..., session,
                          subscene = currentSubscene3d(rgl.cur())) {
  if (!requireNamespace("shiny")) {
    stop("function requires shiny")
  }
  args <- list(...)
  argnames <- names(args)
  badargs <- argnames[!(argnames %in% .Par3d) | argnames %in% .Par3d.readonly]
  if (length(badargs)) {
    stop("Invalid parameter(s): ", badargs)
  }
  for (arg in argnames) {
    session$sendCustomMessage(
      "shinySetPar3d",
      list(
        subscene = subscene,
        parameter = arg,
        value = args[[arg]]
      )
    )
  }
}



#' Communicate rgl parameters between R and Javascript in Shiny.
#' 
#' These functions allow Shiny apps to read and write the \code{par3d} settings
#' that may have been modified by user interaction in the browser.
#' 
#' Requesting information from the browser is a complicated process. The
#' \code{shinyGetPar3d} function doesn't return the requested value, it just
#' submits a request for the value to be returned later in \code{input$par3d},
#' a reactive input.  No action will result except when a reactive observer
#' depends on \code{input$par3d}. See the example code below.
#' 
#' The \code{shinySetPar3d} function sends a message to the browser asking it
#' to change a particular parameter.  The change will be made immediately,
#' without sending the full scene to the browser, so should be reasonably fast.
#' 
#' @aliases shinyGetPar3d shinySetPar3d
#' @param parameters A character vector naming the parameters to get.
#' @param session The Shiny session object.
#' @param subscene The subscene to which the parameters apply.  Defaults to the
#' currently active subscene in the R session.
#' @param tag An arbitrary string or value which will be sent as part of the
#' response.
#' @param ...  A number of \code{name = value} pairs to be modified.
#' @return These functions are called for their side effects, and don't return
#' useful values.
#' 
#' The side effect of \code{shinyGetPar3d} is to cause \code{input$par3d} to be
#' updated sometime later.  Besides the requested parameter values,
#' \code{input$par3d} will contain a copy of the \code{subscene} and \code{tag}
#' arguments.
#' 
#' The side effect of \code{shinySetPar3d} is to send a message to the browser
#' to update its copy of the \code{par3d} parameters immediately.
#' @note R and the browser don't maintain a perfect match between the way
#' parameters are stored internally.  The browser version of parameters will be
#' returned by \code{shinyGetPar3d} and should be supplied to
#' \code{shinySetPar3d}.
#' @author Duncan Murdoch
#' @references
#' \url{https://shiny.rstudio.com/articles/communicating-with-js.html}
#' describes the underlying mechanisms used by these two functions.
#' @examples
#' 
#' if (interactive()) {
#'   save <- options(rgl.useNULL = TRUE)
#' 
#'   xyz <- matrix(rnorm(300), ncol = 3)
#' 
#'   app = shiny::shinyApp(
#'     ui = shiny::bootstrapPage(
#'       shiny::actionButton("redraw", "Redraw"),
#'       rglwidgetOutput("rglPlot")
#'     ),
#'     server = function(input, output, session) {
#'       # This waits until the user to click on the "redraw" 
#'       # button, then sends a request for the current userMatrix
#'       shiny::observeEvent(input$redraw, {
#'         shinyGetPar3d("userMatrix", session)
#'       })
#'     
#'       # This draws the plot whenever input$par3d changes,
#'       # i.e. whenever a response to the request above is
#'       # received.
#'       output$rglPlot <- renderRglwidget({
#'         if (length(rgl.dev.list())) rgl.close()
#'         col <- sample(colors(), 1)
#'         plot3d(xyz, col = col, type = "s", main = col)
#'         par3d(userMatrix = input$par3d$userMatrix)
#'         rglwidget()
#'       })
#'     })
#'   shiny::runApp(app)
#'   options(save)
#' }
#' 
shinyGetPar3d <- function(parameters, session,
                          subscene = currentSubscene3d(rgl.cur()),
                          tag = "") {
  badargs <- parameters[!(parameters %in% .Par3d)]
  if (length(badargs)) {
    stop("invalid parameter(s): ", badargs)
  }
  session$sendCustomMessage(
    "shinyGetPar3d",
    list(
      tag = tag, subscene = subscene,
      parameters = parameters
    )
  )
}

convertShinyPar3d <- function(par3d, ...) {
  if (!is.null(par3d$userMatrix)) {
    par3d$userMatrix <- matrix(unlist(par3d$userMatrix), 4, 4)
  }
  par3d
}
