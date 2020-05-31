#' Identify points in plot.
#' 
#' Identify points in a plot, similarly to the \code{\link{identify}} function
#' in base graphics.
#' 
#' If \code{buttons} is length 1, the user can quit by reaching \code{n}
#' selections, or by hitting the escape key, but the result will be lost if
#' escape is used.
#' 
#' @param x,y,z coordinates of points in a scatter plot.  Alternatively, any
#' object which defines coordinates (see \code{\link{xyz.coords}}) can be given
#' as \code{x}, and \code{y} and \code{z} left missing.
#' @param labels an optional character vector giving labels for the points.
#' Will be coerced using \code{\link{as.character}}, and recycled if necessary
#' to the length of \code{x}.
#' @param n the maximum number of points to be identified.
#' @param plot logical: if \code{plot} is \code{TRUE}, the labels are printed
#' near the points and if \code{FALSE} they are omitted.
#' @param adj numeric vector to use as \code{adj} parameter to
#' \code{\link{text3d}} when plotting the labels.
#' @param tolerance the maximal distance (in pixels) for the pointer to be
#' \sQuote{close enough} to a point.
#' @param buttons a length 1 or 2 character vector giving the buttons to use
#' for selection and quitting.
#' @return A vector of selected indices.
#' @author Duncan Murdoch
#' @seealso \code{\link{identify}} for base graphics, \code{\link{select3d}}
#' for selecting regions.
#' @keywords graphics
identify3d <- function(x, y = NULL, z = NULL, labels = seq_along(x),
                       n = length(x), plot = TRUE, adj = c(-0.1, 0.5),
                       tolerance = 20, buttons = c("right", "middle")) {
  cat <- function(...) {
    base::cat(...)
    flush.console()
  }

  opar <- par3d("mouseMode")
  odisp <- rgl.cur()

  on.exit({
    disp <- rgl.cur()
    if (odisp != disp) {
      try(rgl.set(odisp), silent = TRUE)
    }
    if (rgl.cur() == odisp) {
      par3d(mouseMode = opar)
    }
    try(rgl.set(disp), silent = TRUE)
  })

  xyz <- xyz.coords(x, y, z)
  x <- xyz$x
  y <- xyz$y
  z <- xyz$z
  if (length(x) == 0) {
    return(numeric())
  }

  force(labels)
  force(adj)

  buttons <- match.arg(buttons, c("left", "right", "middle"), several.ok = TRUE)

  if (length(buttons > 1)) {
    cat(gettextf(
      "Use the %s button to select, the %s button to quit\n",
      buttons[1], buttons[2]
    ))
  } else {
    cat(gettextf("Use the %s button to select\n"), buttons[1])
  }

  buttons <- c(left = 1, right = 2, middle = 3)[buttons]

  selected <- c()

  select <- function(mousex, mousey) {
    disp <- rgl.cur()
    if (disp != odisp) {
      rgl.set(odisp)
      on.exit(rgl.set(disp))
    }
    viewport <- par3d("viewport")
    winxyz <- rgl.user2window(xyz)
    winxyz[, 1] <- winxyz[, 1] * viewport[3]
    winxyz[, 2] <- (1 - winxyz[, 2]) * viewport[4]

    dist <- sqrt((mousex - winxyz[, 1])^2 + (mousey - winxyz[, 2])^2)
    dist[winxyz[, 3] < 0 | winxyz[, 3] > 1] <- Inf
    sel <- which.min(dist)
    if (dist[sel] > tolerance) {
      cat(gettext("Warning:  no point within tolerance\n"))
    } else if (sel %in% selected) {
      cat(gettext("Warning:  nearest point already identified\n"))
    } else {
      selected <<- c(selected, sel)
      if (plot) {
        text3d(x[sel], y[sel], z[sel], texts = labels[sel], adj = adj)
      }
    }
  }

  doquit <- FALSE

  quit <- function(mousex, mousey) {
    doquit <<- TRUE
  }

  rgl.setMouseCallbacks(buttons[1], begin = select)
  if (length(buttons) > 1) {
    rgl.setMouseCallbacks(buttons[2], begin = quit)
  }

  while (!doquit && length(selected) < n) Sys.sleep(0.2)
  selected
}
