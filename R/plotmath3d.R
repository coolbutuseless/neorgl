#' Generate sprites using base graphics math plotting.
#' 
#' To plot mathematical text, this function uses base graphics functions to
#' plot it to a \file{.png} file, then uses that file as a texture in a sprite.
#' 
#' 
#' @param x,y,z coordinates.  Any reasonable way of defining the coordinates is
#' acceptable.  See the function \code{\link[grDevices]{xyz.coords}} for
#' details.
#' @param text A character vector or expression.  See
#' \code{\link[grDevices]{plotmath}} for how expressions are interpreted.
#' @param cex Character size expansion.
#' @param adj one value specifying the horizontal adjustment, or two,
#' specifying horizontal and vertical adjustment respectively.
#' @param pos,offset alternate way to specify \code{adj}; see
#' \code{\link{text3d}}
#' @param fixedSize Should the resulting sprite behave like the default ones,
#' and resize with the scene, or like text, and stay at a fixed size?
#' @param startsize,initCex These parameters are unlikely to be needed by
#' users. \code{startsize} is an over-estimate of the size (in pixels) of the
#' largest expression.  Increase this if large expressions are cut off.
#' \code{initCex} is the size of text used to form the bitmap.  Increase this
#' if letters look too blurry at the desired size.
#' @param \dots Additional arguments to pass to \code{\link[graphics]{text}}
#' when drawing the text.
#' @return Called for the side effect of displaying the sprites. The shape ID
#' of the displayed object is returned.
#' @note The \code{\link{text3d}} function passes calls to this function if its
#' \code{usePlotmath} argument is \code{TRUE}. The default value is determined
#' by examining its \code{texts} argument; if it looks like an expression,
#' \code{plotmath3d} is used.
#' @author Duncan Murdoch
#' @seealso \code{\link{text3d}}
#' @examples
#' 
#' open3d()
#' plotmath3d(1:3, 1:3, 1:3, expression(x[1] == 1, x[2] == 2, x[3] == 3))
#' # This lets the text resize with the plot
#' text3d(4, 4, 4, "resizeable text", usePlotmath = TRUE, fixedSize = FALSE)
#' 
plotmath3d <- function(x, y = NULL, z = NULL,
                       text,
                       cex = par("cex"), adj = par("adj"),
                       pos = NULL, offset = 0.5,
                       fixedSize = TRUE,
                       startsize = 480, initCex = 5,
                       ...) {
  xyz <- xyz.coords(x, y, z)
  n <- length(xyz$x)
  if (is.vector(text)) {
    text <- rep(text, length.out = n)
  }
  cex <- rep(cex, length.out = n)
  if (!is.null(pos)) {
    pos <- rep_len(pos, n)
  } else {
    adj <- c(adj, 0.5, 0.5)[1:2]
  }
  save3d <- par3d(skipRedraw = TRUE)
  save <- options(device.ask.default = FALSE)
  on.exit({
    options(save)
    par3d(save3d)
  })
  result <- integer(n)
  for (i in seq_len(n)) {
    # Open the device twice.  The first one is to measure the text...
    f <- tempfile(fileext = ".png")
    png(f, bg = "transparent", width = startsize, height = startsize)
    par(
      mar = c(0, 0, 0, 0), xaxs = "i", xaxt = "n",
      yaxs = "i", yaxt = "n",
      usr = c(0, 1, 0, 1)
    )
    plot.new()
    if (is.vector(text)) {
      thistext <- text[i]
    } else {
      thistext <- text
    }
    #    w <- strwidth(thistext, cex = initCex, ...)*(2*abs(adj[1] - 0.5) + 1)
    #    h <- strheight(thistext, cex = initCex, ...)*(2*abs(adj[2] - 0.5) + 1)
    w <- strwidth(thistext, cex = initCex, ...)
    w1 <- strwidth("m", cex = initCex, ...)
    h <- strheight(thistext, cex = initCex, ...)
    dev.off()

    if (!is.null(pos)) {
      adj <- list(
        "1" = c(0.5, 1 + offset),
        "2" = c(1 + w1 * offset / w, 0.5),
        "3" = c(0.5, -offset),
        "4" = c(-w1 * offset / w, 0.5)
      )[[pos[i]]]
    }

    # Now make a smaller bitmap
    expand <- 1.5
    size <- round(expand * startsize * max(c(w, h) * (2 * abs(adj - 0.5) + 1)))
    png(f, bg = "transparent", width = size, height = size)
    par(
      mar = c(0, 0, 0, 0), xaxs = "i", xaxt = "n",
      yaxs = "i", yaxt = "n",
      usr = c(0, 1, 0, 1)
    )
    plot.new()
    text(0.5, 0.5, thistext, adj = adj, cex = initCex, ...)
    dev.off()

    result[i] <- with(xyz, sprites3d(x[i], y[i], z[i],
      texture = f, textype = "rgba",
      col = "white", lit = FALSE, radius = cex[i] * size / initCex / 20,
      fixedSize = fixedSize
    ))
  }
  lowlevel(result)
}
