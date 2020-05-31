#' User callbacks on mouse events
#' 
#' Set and get user callbacks on mouse events.
#' 
#' The set functions set event handlers on mouse events that occur within the
#' current rgl window. The \code{begin} and \code{update} events should be
#' functions taking two arguments; these will be the mouse coordinates when the
#' event occurs.  The \code{end} event handler takes no arguments.  The
#' \code{rotate} event takes a single argument, which will be equal to \code{1}
#' if the user pushes the wheel away by one click, and \code{2} if the user
#' pulls the wheel by one click.
#' 
#' Alternatively, the handlers may be set to \code{NULL}, the default value, in
#' which case no action will occur.
#' 
#' If a subscene has multiple listeners, the user action will still only be
#' called for the subscene that received the mouse event.  It should consult
#' \code{\link{par3d}("listeners")} if it makes sense to take action on the
#' whole group of subscenes.
#' 
#' The get function retrieves the callbacks that are currently set.
#' 
#' @aliases rgl.setMouseCallbacks rgl.getMouseCallbacks rgl.setWheelCallback
#' rgl.getWheelCallback
#' @param button Which button?
#' @param begin Called when mouse down event occurs
#' @param update Called when mouse moves
#' @param end Called when mouse is released
#' @param rotate Called when mouse wheel is rotated
#' @param dev,subscene The rgl device and subscene to work with
#' @return The set functions are called for the side effect of setting the
#' mouse event handlers.
#' 
#' The \code{rgl.getMouseCallbacks} function returns a list containing the
#' callback functions or \code{NULL} if no user callback is set. The
#' \code{rgl.getWheelCallback} returns the callback function or \code{NULL}.
#' @author Duncan Murdoch
#' @seealso \code{\link{par3d}} to set built-in handlers
#' @keywords dynamic
#' @examples
#' 
#' 
#'  pan3d <- function(button, dev = rgl.cur(), subscene = currentSubscene3d(dev)) {
#'    start <- list()
#'    
#'    begin <- function(x, y) {
#'      activeSubscene <- par3d("activeSubscene", dev = dev)
#'      start$listeners <<- par3d("listeners", dev = dev, subscene = activeSubscene)
#'      for (sub in start$listeners) {
#'        init <- par3d(c("userProjection","viewport"), dev = dev, subscene = sub)
#'        init$pos <- c(x/init$viewport[3], 1 - y/init$viewport[4], 0.5)
#'        start[[as.character(sub)]] <<- init
#'      }
#'    }
#'    
#'    update <- function(x, y) {
#'      for (sub in start$listeners) {
#'        init <- start[[as.character(sub)]]
#'        xlat <- 2*(c(x/init$viewport[3], 1 - y/init$viewport[4], 0.5) - init$pos)
#'        mouseMatrix <- translationMatrix(xlat[1], xlat[2], xlat[3])
#'        par3d(userProjection = mouseMatrix %*% init$userProjection, dev = dev, subscene = sub )
#'       }
#'    }
#'    rgl.setMouseCallbacks(button, begin, update, dev = dev, subscene = subscene)
#'    cat("Callbacks set on button", button, "of rgl device", dev, "in subscene", subscene, "\n")
#'  }
#'  shade3d(icosahedron3d(), col = "yellow")
#'  pan3d(1)
#' 
rgl.setMouseCallbacks <- function(button, begin = NULL, update = NULL, end = NULL, dev = rgl.cur(), subscene = currentSubscene3d(dev)) {
  invisible(.Call(
    rgl_setMouseCallbacks, as.integer(button), begin, update, end,
    as.integer(dev), as.integer(subscene)
  ))
}

rgl.getMouseCallbacks <- function(button, dev = rgl.cur(), subscene = currentSubscene3d(dev)) {
  .Call(rgl_getMouseCallbacks, as.integer(button), as.integer(dev), as.integer(subscene))
}

rgl.setWheelCallback <- function(rotate = NULL, dev = rgl.cur(), subscene = currentSubscene3d(dev)) {
  invisible(.Call(rgl_setWheelCallback, rotate, as.integer(dev), as.integer(subscene)))
}

rgl.getWheelCallback <- function(dev = rgl.cur(), subscene = currentSubscene3d(dev)) {
  .Call(rgl_getWheelCallback, as.integer(dev), as.integer(subscene))
}
