#' Generate HTML code to select mouse mode.
#' 
#' This generates an HTML \code{select} element to choose among the mouse modes
#' supported by \code{\link{rglwidget}}.
#' 
#' A result of an \code{\link{rglwidget}} call can be passed as the
#' \code{sceneId} argument.  This allows the widget to be \dQuote{piped} into
#' the \code{rglMouse} call. The widget will appear first, the selector next in
#' a \code{\link[htmltools:tag]{tagList}}.
#' 
#' If the \code{sceneId} is a character string, it should be the
#' \code{elementId} of a separately constructed \code{\link{rglwidget}} result.
#' 
#' Finally, the \code{sceneId} can be omitted.  In this case the
#' \code{rglMouse} result needs to be passed into an \code{\link{rglwidget}}
#' call as part of the \code{controllers} argument.  This will place the
#' selector before the widget on the resulting display.
#' 
#' If the mouse mode is changed while brushing the scene, by default the brush
#' will be removed (and so the selection will be cleared too).  If this is not
#' desired, set \code{stayActive = TRUE}.
#' 
#' @param sceneId Either an \code{\link{rglwidget}} or the \code{elementId}
#' from one of them.
#' @param choices Which mouse modes to support?
#' @param labels How to label each mouse mode.
#' @param button Which mouse button is being controlled.
#' @param dev The rgl device used for defaults.
#' @param subscene Which subscene is being modified.
#' @param default What is the default entry to show in the control.
#' @param stayActive Whether a selection brush should stay active if the mouse
#' mode is changed.
#' @param height The (relative) height of the item in the output display.
#' @param ...  Additional arguments to pass to \code{htmltools::tags$select()},
#' e.g. \code{id} or \code{class}.
#' @return A browsable value to put in a web page.
#' @author Duncan Murdoch
#' @examples
#' 
#' if (interactive()) {
#'   open3d()
#'   xyz <- matrix(rnorm(300), ncol = 3)
#'   id <- plot3d(xyz, col = "red", type = "s")["data"]
#'   par3d(mouseMode = "selecting")
#'   share <- rglShared(id)
#' 
#' # This puts the selector below the widget.
#'   rglwidget(shared = share, width = 300, height = 300) %>% rglMouse()
#'   
#' # This puts the selector above the widget.
#'   rglMouse() %>% rglwidget(shared = share, width = 300, height = 300, controllers = .) 
#' }
#' 
rglMouse <- function(sceneId,
                     choices = c(
                       "trackball", "selecting",
                       "xAxis", "yAxis", "zAxis",
                       "polar", "zoom", "fov",
                       "none"
                     ),
                     labels = choices,
                     button = 1,
                     dev = rgl.cur(),
                     subscene = currentSubscene3d(dev),
                     default = par3d("mouseMode", dev = dev, subscene = subscene)[button],
                     stayActive = FALSE,
                     height = 40,
                     ...) {
  stopifnot(length(choices) == length(labels))
  stopifnot(length(button == 1) && button %in% 1:3)
  options <- mapply(function(x, y) tags$option(x, value = y), labels, choices,
    SIMPLIFY = FALSE
  )
  for (i in seq_along(choices)) {
    options[[i]] <- tags$option(labels[i], value = choices[i])
  }
  default <- which(choices == default)
  options[[default]] <- tagAppendAttributes(options[[default]], selected = NA)
  changecode <- "document.getElementById(this.attributes.rglSceneId.value).rglinstance.
                   setMouseMode(this.value, 
                                button = parseInt(this.attributes.rglButton.value), 
                                subscene = parseInt(this.attributes.rglSubscene.value),
                                stayActive = parseInt(this.attributes.rglStayActive.value))"
  result <- tags$select(tagList(options),
    onchange = HTML(changecode),
    rglButton = button,
    rglSubscene = subscene,
    rglStayActive = as.numeric(stayActive),
    ...
  )

  if (!missing(sceneId)) {
    upstream <- processUpstream(sceneId)
    if (!is.null(upstream$prevRglWidget)) {
      result <- tagAppendAttributes(result, rglSceneId = upstream$prevRglWidget)
    }
  } else {
    upstream <- list()
  }

  if (is.list(upstream$objects)) {
    do.call(combineWidgets, c(
      upstream$objects,
      list(result,
        rowsize = c(upstream$rowsizes, height),
        ncol = 1
      )
    ))
  } else {
    browsable(result)
  }
}
