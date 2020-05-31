# This internal function returns a list with the following components:
# xlim, ylim, zlim:  the bounding box expanded so no coordinate has zero or negative extent
# strut:  a boolean indicating whether an expansion was done above
# x, y, z:  the box above expanded by a factor of expand

.getRanges <- function(expand = 1.03, ranges = par3d("bbox")) {
  ranges <- list(xlim = ranges[1:2], ylim = ranges[3:4], zlim = ranges[5:6])

  strut <- FALSE

  ranges <- lapply(ranges, function(r) {
    d <- diff(r)
    if (d > 0) {
      return(r)
    }
    strut <<- TRUE
    if (d < 0) {
      return(c(0, 1))
    } else if (r[1] == 0) {
      return(c(-1, 1))
    } else {
      return(r[1] + 0.4 * abs(r[1]) * c(-1, 1))
    }
  })

  ranges$strut <- strut

  ranges$x <- (ranges$xlim - mean(ranges$xlim)) * expand + mean(ranges$xlim)
  ranges$y <- (ranges$ylim - mean(ranges$ylim)) * expand + mean(ranges$ylim)
  ranges$z <- (ranges$zlim - mean(ranges$zlim)) * expand + mean(ranges$zlim)

  ranges
}

axis3d <- function(edge, at = NULL, labels = TRUE, tick = TRUE, line = 0,
                   pos = NULL, nticks = 5, ...) {
  save <- par3d(skipRedraw = TRUE, ignoreExtent = TRUE)
  on.exit(par3d(save))

  ranges <- .getRanges()

  edge <- c(strsplit(edge, "")[[1]], "-", "-")[1:3]
  coord <- match(toupper(edge[1]), c("X", "Y", "Z"))

  # Put the sign in the appropriate entry of edge
  if (coord == 2) {
    edge[1] <- edge[2]
  } else if (coord == 3) edge[1:2] <- edge[2:3]

  range <- ranges[[coord]]

  if (is.null(at)) {
    at <- pretty(range, nticks)
    at <- at[at >= range[1] & at <= range[2]]
  }

  if (is.logical(labels)) {
    if (labels) {
      labels <- format(at)
    } else {
      labels <- NA
    }
  }

  mpos <- matrix(NA, 3, length(at))
  if (edge[1] == "+") {
    mpos[1, ] <- ranges$x[2]
  } else {
    mpos[1, ] <- ranges$x[1]
  }
  if (edge[2] == "+") {
    mpos[2, ] <- ranges$y[2]
  } else {
    mpos[2, ] <- ranges$y[1]
  }
  if (edge[3] == "+") {
    mpos[3, ] <- ranges$z[2]
  } else {
    mpos[3, ] <- ranges$z[1]
  }

  ticksize <- 0.05 * (mpos[, 1] - c(mean(ranges$x), mean(ranges$y), mean(ranges$z)))
  ticksize[coord] <- 0

  if (!is.null(pos)) mpos <- matrix(pos, 3, length(at))
  mpos[coord, ] <- at

  x <- c(mpos[1, 1], mpos[1, length(at)])
  y <- c(mpos[2, 1], mpos[2, length(at)])
  z <- c(mpos[3, 1], mpos[3, length(at)])
  if (tick) {
    x <- c(x, as.double(rbind(mpos[1, ], mpos[1, ] + ticksize[1])))
    y <- c(y, as.double(rbind(mpos[2, ], mpos[2, ] + ticksize[2])))
    z <- c(z, as.double(rbind(mpos[3, ], mpos[3, ] + ticksize[3])))
  }
  result <- c(ticks = segments3d(x, y, z, ...))

  if (!all(is.na(labels))) {
    result <- c(result, labels = text3d(
      mpos[1, ] + 3 * ticksize[1],
      mpos[2, ] + 3 * ticksize[2],
      mpos[3, ] + 3 * ticksize[3],
      labels, ...
    ))
  }
  lowlevel(result)
}



#' Draw boxes, axes and other text outside the data
#' 
#' These functions draw axes, boxes and text outside the range of the data.
#' \code{axes3d}, \code{box3d} and \code{title3d} are the higher level
#' functions; normally the others need not be called directly by users.
#' 
#' The rectangular prism holding the 3D plot has 12 edges.  They are identified
#' using 3 character strings.  The first character (`x', `y', or `z') selects
#' the direction of the axis.  The next two characters are each `-' or `+',
#' selecting the lower or upper end of one of the other coordinates.  If only
#' one or two characters are given, the remaining characters default to `-'.
#' For example \code{edge = 'x+'} draws an x-axis at the high level of y and
#' the low level of z.
#' 
#' By default, \code{axes3d} uses the \code{\link{bbox3d}} function to draw the
#' axes.  The labels will move so that they do not obscure the data.
#' Alternatively, a vector of arguments as described above may be used, in
#' which case fixed axes are drawn using \code{axis3d}.
#' 
#' If \code{pos} is a numeric vector of length 3, \code{edge} determines the
#' direction of the axis and the tick marks, and the values of the other two
#' coordinates in \code{pos} determine the position.  See the examples.
#' 
#' @aliases axes3d axis3d mtext3d title3d box3d
#' @param edges a code to describe which edge(s) of the box to use; see Details
#' below
#' @param labels whether to label the axes, or (for \code{axis3d}) the labels
#' to use
#' @param tick whether to use tick marks
#' @param nticks suggested number of ticks
#' @param box draw the full box if \code{"bbox"} axes are used
#' @param expand how much to expand the box around the data
#' @param main the main title for the plot
#' @param sub the subtitle for the plot
#' @param xlab,ylab,zlab the axis labels for the plot
#' @param line the ``line'' of the plot margin to draw the label on
#' @param edge,pos the position at which to draw the axis or text
#' @param text the text to draw
#' @param at the value of a coordinate at which to draw the axis
#' @param \dots additional parameters which are passed to \code{\link{bbox3d}}
#' or \code{\link{material3d}}
#' @return These functions are called for their side effects.  They return the
#' object IDs of objects added to the scene.
#' @author Duncan Murdoch
#' @seealso Classic graphics functions \code{\link{axis}}, \code{\link{box}},
#' \code{\link{title}}, \code{\link{mtext}}, and \pkg{rgl} function
#' \code{\link{bbox3d}}.
#' @keywords dynamic
#' @examples
#' 
#'   open3d()
#'   points3d(rnorm(10), rnorm(10), rnorm(10))
#' 
#'   # First add standard axes
#'   axes3d()  
#' 
#'   # and one in the middle (the NA will be ignored, a number would 
#'   # do as well)
#'   axis3d('x', pos = c(NA, 0, 0))
#' 
#'   # add titles
#'   title3d('main', 'sub', 'xlab', 'ylab', 'zlab')
#' 
#'   rgl.bringtotop()
#'   
#'   open3d()
#'   points3d(rnorm(10), rnorm(10), rnorm(10))
#'   
#'   # Use fixed axes
#'   
#'   axes3d(c('x', 'y', 'z'))
#'          
#'   # Put 4 x-axes on the plot
#'   axes3d(c('x--', 'x-+', 'x+-', 'x++'))         
#'   
#'   axis3d('x', pos = c(NA, 0, 0))     
#'   title3d('main', 'sub', 'xlab', 'ylab', 'zlab')
#' 
axes3d <- function(edges = "bbox", labels = TRUE,
                   tick = TRUE, nticks = 5, box = FALSE, expand = 1.03, ...) {
  save <- par3d(skipRedraw = TRUE, ignoreExtent = TRUE)
  on.exit(par3d(save))
  if (identical(edges, "bbox")) {
    bboxargs <- names(formals(rgl.bbox))
    bboxargs <- intersect(bboxargs, names(list(...)))
    otherargs <- setdiff(names(list(...)), bboxargs)
    bboxargs <- list(...)[bboxargs]
    otherargs <- list(...)[otherargs]
    result <- do.call("bbox3d", c(
      list(draw_front = box, expand = expand), bboxargs,
      do.call(".fixMaterialArgs", c(otherargs, list(Params = list(front = "lines", back = "lines"))))
    ))
  } else {
    result <- numeric(0)
    for (e in edges) {
      result <- c(result, axis3d(e, labels = labels, tick = tick, nticks = nticks, ...))
    }
    names(result) <- e
  }
  lowlevel(result)
}

box3d <- function(...) {
  save <- par3d(ignoreExtent = TRUE)
  on.exit(par3d(save))

  result <- numeric(0)

  ranges <- .getRanges()
  if (ranges$strut) {
    par3d(ignoreExtent = FALSE)
    result <- c(result, strut = segments3d(
      rep(ranges$xlim, c(2, 2)),
      rep(ranges$ylim, c(2, 2)),
      rep(ranges$zlim, c(2, 2))
    ))
    par3d(ignoreExtent = TRUE)
  }

  x <- c(rep(ranges$x[1], 8), rep(ranges$x, 4), rep(ranges$x[2], 8))
  y <- c(
    rep(ranges$y, 2), rep(ranges$y, c(2, 2)), rep(ranges$y, c(4, 4)),
    rep(ranges$y, 2), rep(ranges$y, c(2, 2))
  )
  z <- c(
    rep(ranges$z, c(2, 2)), rep(ranges$z, 2), rep(rep(ranges$z, c(2, 2)), 2),
    rep(ranges$z, c(2, 2)), rep(ranges$z, 2)
  )
  lowlevel(c(result, lines = segments3d(x, y, z, ...)))
}

mtext3d <- function(text, edge, line = 0, at = NULL, pos = NA, ...) {
  save <- par3d(ignoreExtent = TRUE)
  on.exit(par3d(save))

  ranges <- .getRanges()

  edge <- c(strsplit(edge, "")[[1]], "-", "-")[1:3]
  coord <- match(toupper(edge[1]), c("X", "Y", "Z"))

  # Put the sign in the appropriate entry of edge
  if (coord == 2) {
    edge[1] <- edge[2]
  } else if (coord == 3) edge[1:2] <- edge[2:3]

  range <- ranges[[coord]]

  if (is.null(at)) at <- mean(range)

  newlen <- max(length(text), length(line), length(at))
  text <- rep(text, len = newlen)
  line <- rep(line, len = newlen)
  at <- rep(at, len = newlen)

  if (all(is.na(pos))) {
    pos <- matrix(NA, 3, length(at))
    if (edge[1] == "+") {
      pos[1, ] <- ranges$x[2]
    } else {
      pos[1, ] <- ranges$x[1]
    }
    if (edge[2] == "+") {
      pos[2, ] <- ranges$y[2]
    } else {
      pos[2, ] <- ranges$y[1]
    }
    if (edge[3] == "+") {
      pos[3, ] <- ranges$z[2]
    } else {
      pos[3, ] <- ranges$z[1]
    }
  }
  else {
    pos <- matrix(pos, 3, length(at))
  }
  pos[coord, ] <- at
  ticksize <- 0.05 * (pos[, 1] - c(mean(ranges$x), mean(ranges$y), mean(ranges$z)))
  ticksize[coord] <- 0

  text3d(
    pos[1, ] + 3 * ticksize[1] * line,
    pos[2, ] + 3 * ticksize[2] * line,
    pos[3, ] + 3 * ticksize[3] * line,
    text, ...
  )
}

title3d <- function(main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
                    zlab = NULL, line = NA, ...) {
  save <- par3d(skipRedraw = TRUE, ignoreExtent = TRUE)
  on.exit(par3d(save))
  result <- numeric(0)
  if (!is.null(main)) {
    aline <- ifelse(is.na(line), 2, line)
    result <- c(result, main = mtext3d(main, "x++", line = aline, ...))
  }
  if (!is.null(sub)) {
    aline <- ifelse(is.na(line), 3, line)
    result <- c(result, sub = mtext3d(sub, "x", line = aline, ...))
  }
  if (!is.null(xlab)) {
    aline <- ifelse(is.na(line), 2, line)
    result <- c(result, xlab = mtext3d(xlab, "x", line = aline, ...))
  }
  if (!is.null(ylab)) {
    aline <- ifelse(is.na(line), 2, line)
    result <- c(result, ylab = mtext3d(ylab, "y", line = aline, ...))
  }
  if (!is.null(zlab)) {
    aline <- ifelse(is.na(line), 2, line)
    result <- c(result, zlab = mtext3d(zlab, "z", line = aline, ...))
  }
  lowlevel(result)
}
