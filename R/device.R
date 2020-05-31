##
## R source file
## This file is part of rgl
##
##

##
## ===[ SECTION: device management ]==========================================
##



#' Report default use of null device.
#' 
#' This function checks the \code{"rgl.useNULL"} option if present, or the
#' \env{RGL_USE_NULL} environment variable if it is not.  If the value is
#' \code{TRUE} or a string which matches \dQuote{yes} or \dQuote{true} in a
#' case-insensitive test, \code{TRUE} is returned.
#' 
#' 
#' @return A logical value indicating the current default for use of the null
#' device.
#' @note This function is checked by the initialization code when \pkg{rgl} is
#' loaded.  Thus if you want to run \pkg{rgl} on a system where there is no
#' graphics support, you should run \code{options(rgl.useNULL = TRUE)} or set
#' the environment variable \code{RGL_USE_NULL=TRUE} *before* calling
#' \code{library(rgl)} (or other code that loads \pkg{rgl}), and it will not
#' fail in its attempt at initialization.
#' @author Duncan Murdoch
#' @seealso \code{\link{open3d}} and \code{\link{rgl.open}}.
#' @examples
#' 
#' rgl.useNULL()
#' 
rgl.useNULL <- function() {
  opt <- getOption("rgl.useNULL", Sys.getenv("RGL_USE_NULL"))
  if (is.logical(opt)) {
    return(opt)
  }
  opt <- as.character(opt)
  if (nchar(opt)) {
    opt <- pmatch(tolower(opt), c("yes", "true"), nomatch = 3)
    return(c(TRUE, TRUE, FALSE)[opt])
  }
  FALSE
}

##
## open device
##
##



#' 3D visualization device system
#' 
#' 3D real-time rendering system.
#' 
#' The \pkg{rgl} device design is oriented towards the R device metaphor. If
#' you send scene management instructions, and there's no device open, it will
#' be opened automatically.  Opened devices automatically get the current
#' device focus. The focus may be changed by using \code{rgl.set()}.
#' \code{rgl.quit()} shuts down the rgl subsystem and all open devices,
#' detaches the package including the shared library and additional system
#' libraries.
#' 
#' The \code{rgl.open()} function attempts to open a new RGL window.  If the
#' \code{"rgl.antialias"} option is set, it will be used to select the
#' requested antialiasing.  (See \code{\link{open3d}} for more description of
#' antialiasing and an alternative way to set the value.)
#' 
#' If \code{useNULL} is \code{TRUE}, \pkg{rgl} will use a \dQuote{null} device.
#' This device records objects as they are plotted, but displays nothing. It is
#' intended for use with \code{\link{rglwidget}} and similar functions.
#' 
#' If \code{rgl.open()} fails (e.g. because X windows is not running, or its
#' \code{DISPLAY} variable is not set properly), then you can retry the
#' initialization by calling \code{\link{rgl.init}()}.  Do not do this when
#' windows have already been successfully opened: they will be orphaned, with
#' no way to remove them other than closing R.  In fact, it's probably a good
#' idea not to do this at all: quitting R and restarting it is a better
#' solution.
#' 
#' This package also includes a higher level interface which is described in
#' the \link{r3d} help topic.  That interface is designed to act more like
#' classic 2D R graphics.  We recommend that you avoid mixing \code{rgl.*} and
#' \code{*3d} calls.
#' 
#' @aliases rgl.open rgl.close rgl.cur rgl.set rgl.quit rgl.antialias
#' rgl.dev.list
#' @param useNULL whether to open the \dQuote{null} device
#' @param which device ID
#' @param silent whether to suppress update of window titles
#' @return \code{rgl.open}, \code{rgl.close} and \code{rgl.set} are called for
#' their side effects and return no useful value.  Similarly \code{rgl.quit} is
#' not designed to return useful values; in fact, users shouldn't call it at
#' all!
#' 
#' \code{rgl.cur} returns the currently active devices, or \code{0} if none is
#' active; \code{rgl.dev.list} returns a vector of all open devices.  Both
#' functions name the items according to the type of device: \code{null} for a
#' hidden null device, \code{wgl} for a Windows device, and \code{glX} for an X
#' windows device.
#' @seealso \link{r3d}, \code{\link{rgl.init}}, \code{\link{rgl.clear}},
#' \code{\link{rgl.pop}}, \code{\link{rgl.viewpoint}}, \code{\link{rgl.light}},
#' \code{\link{rgl.bg}}, \code{\link{rgl.bbox}}, \code{\link{rgl.points}},
#' \code{\link{rgl.lines}}, \code{\link{rgl.triangles}},
#' \code{\link{rgl.quads}}, \code{\link{rgl.texts}}, \code{\link{rgl.surface}},
#' \code{\link{rgl.spheres}}, \code{\link{rgl.sprites}},
#' \code{\link{rgl.snapshot}}, \code{\link{rgl.useNULL}}
#' @keywords dynamic
rgl.open <- function(useNULL = rgl.useNULL()) {
  ret <- .C(rgl_dev_open, success = FALSE, useNULL = useNULL)

  if (!ret$success) {
    stop("'rgl.open' failed")
  }
}


##
## close device
##
##

rgl.close <- function() {
  if (length(hook <- getHook("on.rgl.close"))) {
    if (is.list(hook)) hook <- hook[[1]] # test is for compatibility with R < 3.0.0
    hook()
  }

  ret <- .C(rgl_dev_close, success = FALSE)

  if (!ret$success) {
    stop("No device opened")
  }
}

##
## get current device
##
##

rgl.cur <- function() {
  .Call(rgl_dev_getcurrent)
}

##
## get all devices
##
##

rgl.dev.list <- function() {
  .Call(rgl_dev_list)
}


##
## set current device
##
##

rgl.set <- function(which, silent = FALSE) {
  idata <- c(as.integer(which), as.integer(silent))

  ret <- .C(rgl_dev_setcurrent,
    success = FALSE,
    idata
  )

  if (!ret$success) {
    stop(gettextf("No device opened with id %s", which), domain = NA)
  }
}



##
## export image
##
##



#' export screenshot
#' 
#' Saves the screenshot as png file.
#' 
#' Animations can be created in a loop modifying the scene and saving each
#' screenshot to a file. Various graphics programs (e.g. ImageMagick) can put
#' these together into a single animation. (See \code{\link{movie3d}} or the
#' example below.)
#' 
#' @aliases rgl.snapshot snapshot3d
#' @param filename full path to filename.
#' @param fmt image export format, currently supported: png
#' @param top whether to call \code{\link{rgl.bringtotop}}
#' @param ... arguments to pass to \code{rgl.snapshot}
#' @param scene an optional result of \code{\link{scene3d}} or
#' \code{\link{rglwidget}} to plot
#' @note On some systems, the snapshot will include content from other windows
#' if they cover the active rgl window.  Setting \code{top = TRUE} (the
#' default) will use \code{\link{rgl.bringtotop}} before the snapshot to avoid
#' this.
#' @seealso \code{\link{movie3d}}, \code{\link{rgl.viewpoint}}
#' @keywords dynamic
#' @examples
#' 
#' 
#' \dontrun{
#' 
#' #
#' # create animation
#' #
#' 
#' shade3d(oh3d(), color = "red")
#' rgl.bringtotop()
#' rgl.viewpoint(0, 20)
#' 
#' olddir <- setwd(tempdir())
#' for (i in 1:45) {
#'   rgl.viewpoint(i, 20)
#'   filename <- paste("pic", formatC(i, digits = 1, flag = "0"), ".png", sep = "")
#'   rgl.snapshot(filename)
#' }
#' ## Now run ImageMagick in tempdir().  Use 'convert' instead of 'magick'
#' ## if you have an older version of ImageMagick:
#' ##    magick -delay 10 *.png -loop 0 pic.gif
#' setwd(olddir)
#' }
#' 
#' 
rgl.snapshot <- function(filename, fmt = "png", top = TRUE) {
  if (top) rgl.bringtotop()

  idata <- as.integer(rgl.enum.pixfmt(fmt))
  if (length(filename) != 1) {
    stop("filename is length ", length(filename))
  }
  filename <- normalizePath(filename, mustWork = FALSE)
  ret <- .C(rgl_snapshot,
    success = FALSE,
    idata,
    filename
  )

  if (!ret$success) {
    warning("'rgl.snapshot' failed")
  }
}

##
## export postscript image
##
##



#' export screenshot
#' 
#' Saves the screenshot to a file in PostScript or other vector graphics
#' format.
#' 
#' Animations can be created in a loop modifying the scene and saving a
#' screenshot to a file. (See example below)
#' 
#' This function is a wrapper for the GL2PS library by Christophe Geuzaine, and
#' has the same limitations as that library: not all OpenGL features are
#' supported, and some are only supported in some formats. See the reference
#' for full details.
#' 
#' @param filename full path to filename.
#' @param fmt export format, currently supported: ps, eps, tex, pdf, svg, pgf
#' @param drawText logical, whether to draw text
#' @author Christophe Geuzaine / Albrecht Gebhardt
#' @seealso \code{\link{rgl.viewpoint}}, \code{\link{rgl.snapshot}}
#' @references GL2PS: an OpenGL to PostScript printing library by Christophe
#' Geuzaine, \url{https://www.geuz.org/gl2ps/}, version 1.4.0.
#' @keywords dynamic
#' @examples
#' 
#' # Create new files in tempdir
#' savedir <- setwd(tempdir())
#' 
#' x <- y <- seq(-10, 10, length = 20)
#' z <- outer(x, y, function(x, y) x^2 + y^2)
#' persp3d(x, y, z, col = 'lightblue')
#' 
#' title3d("Using LaTeX text", col = 'red', line = 3)
#' rgl.postscript("persp3da.ps", "ps", drawText = FALSE)
#' rgl.postscript("persp3da.pdf", "pdf", drawText = FALSE)
#' rgl.postscript("persp3da.tex", "tex")
#' rgl.pop()
#' title3d("Using ps/pdf text", col = 'red', line = 3)
#' rgl.postscript("persp3db.ps", "ps")
#' rgl.postscript("persp3db.pdf", "pdf")
#' rgl.postscript("persp3db.tex", "tex", drawText = FALSE)
#' 
#' setwd(savedir)
#' 
#' \dontrun{
#' 
#' #
#' # create a series of frames for an animation
#' #
#' 
#' rgl.open()
#' shade3d(oh3d(), color = "red")
#' rgl.viewpoint(0, 20)
#' 
#' for (i in 1:45) {
#'   rgl.viewpoint(i, 20)
#'   filename <- paste("pic", formatC(i, digits = 1, flag = "0"), ".eps", sep = "") 
#'   rgl.postscript(filename, fmt = "eps")
#' }
#' 
#' }
#' 
#' 
rgl.postscript <- function(filename, fmt = "eps", drawText = TRUE) {
  idata <- as.integer(c(rgl.enum.gl2ps(fmt), as.logical(drawText)))
  if (length(filename) != 1) {
    stop("filename is length ", length(filename))
  }
  ret <- .C(rgl_postscript,
    success = FALSE,
    idata,
    normalizePath(filename, mustWork = FALSE, winslash = "/")
  )

  if (!ret$success) {
    warning("Postscript conversion failed")
  }
}

##
## read image
##
##



#' Extract pixel information from window
#' 
#' This function extracts single components of the pixel information from the
#' topmost window.
#' 
#' The possible components are \code{"red"}, \code{"green"}, \code{"blue"},
#' \code{"alpha"}, \code{"depth"}, and \code{"luminance"} (the sum of the three
#' colors).  All are scaled from 0 to 1.
#' 
#' Note that the luminance is kept below 1 by truncating the sum; this is the
#' definition used for the \code{GL_LUMINANCE} component in OpenGL.
#' 
#' @param component Which component(s)?
#' @param viewport Lower left corner and size of desired region.
#' @param top Whether to bring window to top before reading.
#' @return A vector, matrix or array containing the desired components.  If one
#' component is requested, a vector or matrix will be returned depending on the
#' size of block requested (length 1 dimensions are dropped); if more, an
#' array, whose last dimension is the list of components.
#' @author Duncan Murdoch
#' @seealso \code{\link{rgl.snapshot}} to write a copy to a file,
#' \code{demo("stereo")} for functions that make use of this to draw a random
#' dot stereogram and an anaglyph.
#' @keywords dynamic
#' @examples
#' 
#' example(surface3d)
#' depth <- rgl.pixels(component = "depth")
#' if (length(depth) && is.matrix(depth)) # Protect against empty or single pixel windows
#'     contour(depth)
#' 
rgl.pixels <- function(component = c("red", "green", "blue"), viewport = par3d("viewport"), top = TRUE) {
  if (top) rgl.bringtotop()

  compnum <- as.integer(sapply(component, rgl.enum.pixelcomponent))
  stopifnot(length(viewport) == 4)
  ll <- as.integer(viewport[1:2])
  stopifnot(all(!is.na(ll)), all(ll >= 0))
  size <- as.integer(viewport[3:4])
  stopifnot(all(!is.na(size), all(size >= 0)))
  result <- array(NA_real_, dim = c(size[1], size[2], length(component)))
  dimnames(result) <- list(NULL, NULL, component)
  if (length(result) > 0) {
    for (i in seq_along(compnum)) {
      ret <- .C(rgl_pixels,
        success = FALSE,
        ll, size, compnum[i],
        values = double(size[1] * size[2])
      )

      if (!ret$success) {
        warning(gettextf("Error reading component '%s'", component[i]), domain = NA)
      }
      result[, , i] <- ret$values
    }
  }
  if (length(component) > 1) {
    return(result)
  } else {
    return(result[, , 1])
  }
}
