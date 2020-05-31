## Original by Barry Rowlingson, R-help, 1/10/2010
## Modified by Michael Friendly: added barblen (to specify absolute barb length)
## Modified by DJM: multiple changes



#' Draw an arrow in a scene.
#' 
#' Draws various types of arrows in a scene.
#' 
#' Four types of arrows can be drawn.  The shapes of all of them are affected
#' by \code{p0}, \code{p1}, \code{barblen}, \code{s}, \code{theta}, material
#' properties in \code{...}, and \code{spriteOrigin}.  Other parameters only
#' affect some of the types, as shown. \describe{
#' \item{list("\"extrusion\"")}{(default) A 3-dimensional flat arrow, drawn
#' with \code{\link{shade3d}}.  Affected by \code{width}, \code{thickness} and
#' \code{smooth}.} \item{list("\"lines\"")}{Drawn with lines, similar to
#' \code{\link{arrows}}, drawn with \code{\link{segments3d}}.  Affected by
#' \code{n}.} \item{list("\"flat\"")}{A flat arrow, drawn with
#' \code{\link{polygon3d}}.  Affected by \code{width} and \code{smooth}.}
#' \item{list("\"rotation\"")}{A solid of rotation, drawn with
#' \code{\link{shade3d}}.  Affected by \code{n} and \code{width}.} }
#' 
#' Normally this function draws just one arrow from \code{p0} to \code{p1}, but
#' if \code{spriteOrigin} is given (in any form that
#' \code{\link{xyz.coords}(spriteOrigin)} can handle), arrows will be drawn for
#' each point specified, with \code{p0} and \code{p1} interpreted relative to
#' those origins.  The arrows will be drawn as 3D sprites which will maintain
#' their orientation as the scene is rotated, so this is a good way to indicate
#' particular locations of interest in the scene.
#' 
#' @param p0 The base of the arrow.
#' @param p1 The head of the arrow.
#' @param barblen The length of the barbs (in display coordinates). Default
#' given by \code{s}.
#' @param s The length of the barbs as a fraction of line length.  Ignored if
#' \code{barblen} is present.
#' @param theta Opening angle of barbs
#' @param type Type of arrow to draw.  Choose one from the list of defaults.
#' Can be abbreviated.  See below.
#' @param n Number of barbs.
#' @param width Width of shaft as fraction of barb width.
#' @param thickness Thickness of shaft as fraction of barb width.
#' @param spriteOrigin If arrow is to be replicated as sprites, the origins
#' relative to which the sprites are drawn.
#' @param plot If \code{TRUE} (the default), plot the object; otherwise return
#' the computed data that would be used to plot it.
#' @param \dots Material properties passed to \code{\link{polygon3d}},
#' \code{\link{shade3d}} or \code{\link{segments3d}}.
#' @return If \code{plot = TRUE} (the default), this is called mainly for the
#' side effect of drawing the arrow; invisibly returns the id(s) of the objects
#' drawn.
#' 
#' If \code{plot = FALSE}, the data that would be used in the plot (not
#' including material properties) is returned.
#' @author Design based on \code{heplots::\link[heplots]{arrow3d}}, which
#' contains modifications by Michael Friendly to a function posted by Barry
#' Rowlingson to R-help on 1/10/2010.  Additions by Duncan Murdoch.
#' @examples
#' 
#' \donttest{
#' xyz <- matrix(rnorm(300), ncol = 3)
#' plot3d(xyz)
#' arrow3d(xyz[1,], xyz[2,], type = "extrusion", col = "red")
#' arrow3d(xyz[3,], xyz[4,], type = "flat",      col = "blue")
#' arrow3d(xyz[5,], xyz[6,], type = "rotation",  col = "green")
#' arrow3d(xyz[7,], xyz[8,], type = "lines",     col = "black")
#' arrow3d(spriteOrigin = xyz[9:12,],            col = "purple")
#' }
#' 
arrow3d <- function(p0 = c(1, 1, 1), p1 = c(0, 0, 0), barblen, s = 1 / 3, theta = pi / 12,
                    type = c("extrusion", "lines", "flat", "rotation"),
                    n = 3,
                    width = 1 / 3,
                    thickness = 0.618 * width,
                    spriteOrigin = NULL,
                    plot = TRUE, ...) {
  ##      p0: start point
  ##      p1: end point
  ## barblen: length of barb
  ##       s: length of barb as fraction of line length (unless barblen is specified)
  ##   theta: opening angle of barbs
  ##    type: type of arrow to draw
  ##       n: number of barbs
  ##   width: width of shaft as fraction of barb width
  ##   thickness: thickness of shaft as fraction of barb width
  ##  spriteOrigin: origin if drawn as sprite
  ##     ...: args passed to lines3d for line styling
  ##
  ## Returns (invisibly): integer ID(s) of the shape added to the scene

  type <- match.arg(type)

  nbarbs <- if (type == "lines") n else 2

  ## Work in isometric display coordinates

  save <- par3d(FOV = 0)

  # Compute the center line in window
  # coordinates
  xyz <- rgl.user2window(rbind(p0, p1))
  p0 <- xyz[1, ]
  p1 <- xyz[2, ]

  ## rotational angles of barbs
  phi <- seq(pi / nbarbs, 2 * pi - pi / nbarbs, len = nbarbs)

  ## length of line
  lp <- sqrt(sum((p1 - p0)^2))

  if (missing(barblen)) {
    barblen <- s * lp
  } else {
    s <- barblen / lp
  }

  ## point down the line where the barb ends line up
  cpt <- p1 + s * cos(theta) * (p0 - p1)

  ## need to find a right-angle to the line.
  gs <- GramSchmidt(p1 - p0, c(0, 0, -1), c(1, 0, 0))
  r <- gs[2, ]

  ## now compute the barb end points and draw:
  pts <- list()
  for (i in 1:length(phi)) {
    ptb <- rotate3d(r, phi[i], (p1 - p0)[1], (p1 - p0)[2], (p1 - p0)[3])
    xyz <- rbind(xyz, p1, cpt + barblen * sin(theta) * ptb)
  }
  if (type != "lines") {
    xyz <- xyz[c(
      3, # 1 head
      6, # 2 end of barb 1
      6, # 3 end of barb 1 again (to be shrunk)
      1, # 4 end of line (to be pushed out)
      1, # 5 end of line
      1, # 6 end of line (to be pushed the other way)
      4, # 7 end of barb 2 (to be shrunk)
      4, # 8 end of barb 2
      3
    ), ] # 9 head
    mid <- (xyz[2, ] + xyz[8, ]) / 2
    xyz[3, ] <- mid + width * (xyz[2, ] - mid)
    xyz[7, ] <- mid + width * (xyz[8, ] - mid)
    xyz[4, ] <- xyz[4, ] + xyz[3, ] - mid
    xyz[6, ] <- xyz[6, ] + xyz[7, ] - mid
  }

  if (type %in% c("extrusion", "rotation")) {
    xyz <- xyz %*% t(gs)
    if (type == "extrusion") {
      thickness <- thickness * sqrt(sum((xyz[2, ] - xyz[8, ])^2))
      ext <- extrude3d(xyz[, c(1, 3)], thickness = thickness)
    } else {
      mid <- xyz[1, 3]
      xyz[, 3] <- abs(xyz[, 3] - mid)
      xyz <- xyz[5:9, ]
      ext <- turn3d(xyz[, c(1, 3)], n = n)
      ext$vb[2, ] <- ext$vb[2, ] + mid
      thickness <- 0
    }
    ext$vb <- ext$vb[c(1, 3, 2, 4), ]
    ext$vb[2, ] <- ext$vb[2, ] + xyz[1, 2] - thickness / 2
    ext$vb[1:3, ] <- t(gs) %*% ext$vb[1:3, ]
    ext$vb[1:3, ] <- t(rgl.window2user(t(ext$vb[1:3, ])))
  } else {
    xyz <- rgl.window2user(xyz)
  }
  par3d(save)
  if (plot) {
    if (type == "flat") {
      id <- polygon3d(xyz, ...)
    } else if (type %in% c("extrusion", "rotation")) {
      id <- shade3d(ext, ...)
    } else {
      id <- segments3d(xyz, ...)
    }

    if (is.null(spriteOrigin)) {
      lowlevel(id)
    } else {
      sprites3d(spriteOrigin, shapes = id)
    }
  } else {
    if (type %in% c("extrusion", "rotation")) {
      ext
    } else {
      xyz
    }
  }
}
