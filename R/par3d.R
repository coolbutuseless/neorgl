.Par3d <- c(
  "antialias", "FOV", "ignoreExtent", "listeners",
  "mouseMode", "observer",
  "modelMatrix", "projMatrix", "skipRedraw", "userMatrix",
  "userProjection",
  "scale", "viewport", "zoom", "bbox", "windowRect",
  "family", "font", "cex", "useFreeType", "fontname",
  "maxClipPlanes", "glVersion", "activeSubscene"
)

.Par3d.readonly <- c(
  "antialias", "observer",
  "modelMatrix", "projMatrix",
  "bbox", "fontname",
  "maxClipPlanes", "glVersion",
  "activeSubscene"
)



#' Set or Query RGL Parameters
#' 
#' \code{par3d} can be used to set or query graphical parameters in rgl.
#' Parameters can be set by specifying them as arguments to \code{par3d} in
#' \code{tag = value} form, or by passing them as a list of tagged values.
#' 
#' Parameters are queried by giving one or more character vectors to
#' \code{par3d}.
#' 
#' \code{par3d()} (no arguments) or \code{par3d(no.readonly = TRUE)} is used to
#' get \emph{all} the graphical parameters (as a named list).
#' 
#' By default, queries and modifications apply to the current subscene on the
#' current device; specify \code{dev} and/or \code{subscene} to change this.
#' Some parameters apply to the device as a whole; these are marked in the list
#' below.
#' 
#' \code{open3d} opens a new rgl device, and sets the parameters as requested.
#' The \code{r3dDefaults} list returned by the \code{getr3dDefaults} function
#' will be used as default values for parameters.  As installed this sets the
#' point of view to 'world coordinates' (i.e. x running from left to right, y
#' from front to back, z from bottom to top), the \code{mouseMode} to
#' \code{(zAxis, zoom, fov)}, and the field of view to 30 degrees.  Users may
#' create their own variable named \code{r3dDefaults} in the global environment
#' and it will override the installed one.  If there is a \code{bg} element in
#' the list or the arguments, it should be a list of arguments to pass to the
#' \code{\link{bg3d}} function to set the background.
#' 
#' The arguments to \code{open3d} may include \code{material}, a list of
#' material properties as in \code{\link{r3dDefaults}}, but note that high
#' level functions such as \code{\link{plot3d}} normally use the
#' \code{r3dDefaults} values in preference to this setting.
#' 
#' If \code{useNULL} is \code{TRUE}, \pkg{rgl} will use a \dQuote{null} device.
#' This device records objects as they are plotted, but displays nothing. It is
#' intended for use with \code{\link{rglwidget}}.
#' 
#' @aliases par3d open3d r3dDefaults getr3dDefaults
#' @param \dots arguments in \code{tag = value} form, or a list of tagged
#' values.  The tags must come from the graphical parameters described below.
#' @param no.readonly logical; if \code{TRUE} and there are no other arguments,
#' only those parameters which can be set by a subsequent \code{par3d()} call
#' are returned.
#' @param dev integer; the rgl device.
#' @param subscene integer; the subscene.
#' @param params a list of graphical parameters
#' @param useNULL whether to use the null graphics device
#' @return When parameters are set, their former values are returned in an
#' invisible named list.  Such a list can be passed as an argument to
#' \code{par3d} to restore the parameter values.  Use \code{par3d(no.readonly =
#' TRUE)} for the full list of parameters that can be restored.
#' 
#' When just one parameter is queried, its value is returned directly.  When
#' two or more parameters are queried, the result is a list of values, with the
#' list names giving the parameters.
#' 
#' Note the inconsistency: setting one parameter returns a list, but querying
#' one parameter returns an object.
#' 
#' The \code{r3dDefaults} variable is a list containing default settings.  The
#' \code{getr3dDefaults} function searches the user's global environment for
#' \code{r3dDefaults} and returns the one in the \pkg{rgl} namespace if it was
#' not found there.  The components of the list may include any settable
#' \code{par3d} parameter, or \code{"material"}, which should include a list of
#' default \code{\link{material3d}} properties, or \code{"bg"}, which is a list
#' of defaults to pass to the \code{\link{bg3d}} function.
#' @note The \code{"xAxis"}, \code{"yAxis"} and \code{"zAxis"} mouse modes
#' rotate relative to the coordinate system of the data, regardless of the
#' current orientation of the scene.
#' 
#' When multiple parameters are set, they are set in the order given.  In some
#' cases this may lead to warnings and ignored values; for example, some font
#' families only support \code{cex = 1}, so changing both \code{cex} and
#' \code{family} needs to be done in the right order.  For example, when using
#' the \code{"bitmap"} family on Windows, \code{par3d(family = "sans", cex =
#' 2)} will work, but \code{par3d(cex = 2, family = "sans")} will leave
#' \code{cex} at 1 (with a warning that the \code{"bitmap"} family only
#' supports that size).
#' 
#' Although \code{par3d("viewport")} names the entries of the reported vector,
#' names are ignored when setting the viewport and entries must be specified in
#' the standard order.
#' 
#' In \pkg{rgl} versions 0.94.x the \code{modelMatrix} entry had a changed
#' meaning; before and after that it contains a copy of the OpenGL MODELVIEW
#' matrix.
#' 
#' As of version 0.100.32, when changing the \code{"windowRect"} parameter, the
#' \code{"viewport"} for the root (or specified) subscene is changed
#' immediately.  This fixes a bug where in earlier versions it would only be
#' changed when the window was redrawn, potentially after another command
#' making use of the value.
#' @section Parameters:
#' 
#' \emph{\bold{R.O.}} indicates \emph{\bold{read-only arguments}}: These may
#' only be used in queries, i.e., they do \emph{not} set anything.
#' 
#' \describe{ \item{list("activeSubscene")}{\emph{\bold{R.O.}} integer.  Used
#' with \code{\link{rgl.setMouseCallbacks}}: during a callback, indicates the
#' id of the subscene that was clicked.}
#' \item{list("antialias")}{\emph{\bold{R.O.}} in \code{par3d}, may be set in
#' \code{open3d}. The (requested) number of hardware antialiasing planes to use
#' (with multisample antialiasing).  The OpenGL driver may not support the
#' requested number, in which case \code{par3d("antialias")} will report what
#' was actually set. Applies to the whole device.} \item{list("cex")}{real.
#' The default size for text.} \item{list("family")}{character.  The default
#' device independent family name; see \code{\link{text3d}}.  Applies to the
#' whole device.} \item{list("font")}{integer.  The default font number (from 1
#' to 5; see \code{\link{text3d}}).  Applies to the whole device.}
#' \item{list("useFreeType")}{logical.  Should FreeType fonts be used?  Applies
#' to the whole device.} \item{list("fontname")}{\emph{\bold{R.O.}}; the
#' system-dependent name of the current font.  Applies to the whole device.}
#' \item{list("FOV")}{real.  The field of view, from 0 to 179 degrees.  This
#' controls the degree of parallax in the perspective view.  Isometric
#' perspective corresponds to \code{FOV = 0}.}
#' \item{list("ignoreExtent")}{logical.  Set to \code{TRUE} so that
#' subsequently plotted objects will be ignored in calculating the bounding box
#' of the scene.  Applies to the whole device.}
#' \item{list("maxClipPlanes")}{\emph{\bold{R.O.}}; an integer giving the
#' maximum number of clip planes that can be defined in the current system.
#' Applies to the whole device.} \item{list("modelMatrix")}{\emph{\bold{R.O.}};
#' a 4 by 4 matrix describing the position of the user data.  See the Note
#' below.} \item{list("listeners")}{integer.  A vector of subscene id values.
#' If a subscene receives a mouse event (see \code{mouseMode} just below), the
#' same action will be carried out on all subscenes in this list.  (The
#' subscene itself is normally listed as a listener.  If it is not listed, it
#' will not respond to its own mouse events.)}
#' \item{list("mouseMode")}{character.  A vector of 4 strings describing what
#' the 3 mouse buttons and the mouse wheel do.  Partial matching is used.
#' Possible values for the first 3 entries of \code{mouseMode} (corresponding
#' to the mouse buttons) are \describe{ \item{list("\"none\"")}{No action for
#' this button.} \item{list("\"trackball\"")}{Mouse acts as a virtual
#' trackball, rotating the scene.} \item{list("\"xAxis\"")}{Similar to
#' \code{"trackball"}, but restricted to X axis rotation.}
#' \item{list("\"yAxis\"")}{Y axis rotation.} \item{list("\"zAxis\"")}{Z axis
#' rotation.} \item{list("\"polar\"")}{Mouse rotates the scene by moving in
#' polar coordinates.} \item{list("\"selecting\"")}{Mouse is used for
#' selection.  This is not normally set by the user, but is used internally by
#' the \code{\link{select3d}} function.} \item{list("\"zoom\"")}{Mouse is used
#' to zoom the display.} \item{list("\"fov\"")}{Mouse changes the field of view
#' of the display.} \item{list("\"user\"")}{Used when a user handler is set by
#' \code{\link{rgl.setMouseCallbacks}}.} } Possible values for the 4th entry
#' corresponding to the mouse wheel are \describe{ \item{list("\"none\"")}{No
#' action.} \item{list("\"pull\"")}{Pulling on the mouse wheel increases
#' magnification, i.e. \dQuote{pulls the scene closer}.}
#' \item{list("\"push\"")}{Pulling on the mouse wheel decreases magnification,
#' i.e. \dQuote{pushes the scene away}.} \item{list("\"user\"")}{Used when a
#' user handler is set by \code{\link{rgl.setWheelCallback}}.} } A common
#' default on Mac OSX is to convert a two finger drag on a trackpad to a mouse
#' wheel rotation.  } \item{list("observer")}{\emph{\bold{R.O.}}; the position
#' of the observer relative to the model.  Set by \code{\link{observer3d}}.
#' See the Note below.} \item{list("projMatrix")}{\emph{\bold{R.O.}}; a 4 by 4
#' matrix describing the current projection of the scene.}
#' \item{list("scale")}{real.  A vector of 3 values indicating the amount by
#' which to rescale each axis before display.  Set by \code{\link{aspect3d}}.}
#' \item{list("skipRedraw")}{whether to update the display.  Set to \code{TRUE}
#' to suspend updating while making multiple changes to the scene.  See
#' \code{demo(hist3d)} for an example.  Applies to the whole device.}
#' \item{list("userMatrix")}{a 4 by 4 matrix describing user actions to display
#' the scene.} \item{list("userProjection")}{a 4 by 4 matrix describing changes
#' to the projection.} \item{list("viewport")}{real.  A vector giving the
#' dimensions of the window in pixels.  The entries are taken to be \code{c(x,
#' y, width, height)} where \code{c(x, y)} are the coordinates in pixels of the
#' lower left corner within the window.} \item{list("zoom")}{real. A positive
#' value indicating the current magnification of the scene.}
#' \item{list("bbox")}{\emph{\bold{R.O.}}; real. A vector of six values
#' indicating the current values of the bounding box of the scene (xmin, xmax,
#' ymin, ymax, zmin, zmax)} \item{list("windowRect")}{integer.  A vector of
#' four values indicating the left, top, right and bottom of the displayed
#' window (in pixels).  Applies to the whole device.} }
#' @seealso \code{\link{rgl.viewpoint}} to set \code{FOV} and \code{zoom}.
#' 
#' \code{\link{rgl.useNULL}} for default usage of null device.
#' @references OpenGL Architecture Review Board (1997).  OpenGL Programming
#' Guide.  Addison-Wesley.
#' @keywords dynamic
#' @examples
#' 
#'     r3dDefaults
#'     open3d()
#'     shade3d(cube3d(color = rep(rainbow(6), rep(4, 6))))
#'     save <- par3d(userMatrix = rotationMatrix(90*pi/180, 1, 0, 0))
#'     highlevel()  # To trigger display
#'     save
#'     par3d("userMatrix")    
#'     par3d(save)
#'     highlevel()
#'     par3d("userMatrix")
#' 
par3d <- function(..., no.readonly = FALSE, dev = rgl.cur(), subscene = currentSubscene3d(dev)) {
  single <- FALSE
  args <- list(...)
  if (!length(args)) {
    args <- as.list(if (no.readonly) {
      .Par3d[-match(.Par3d.readonly, .Par3d)]
    } else {
      .Par3d
    })
  } else {
    if (is.null(names(args)) && all(unlist(lapply(args, is.character)))) {
      args <- as.list(unlist(args))
    }
    if (length(args) == 1) {
      if (is.list(args[[1]]) | is.null(args[[1]])) {
        args <- args[[1]]
      } else
      if (is.null(names(args))) {
        single <- TRUE
      }
    }
  }
  if ("dev" %in% names(args)) {
    if (!missing(dev) && dev != args[["dev"]]) stop("'dev' specified inconsistently")
    dev <- args[["dev"]]
    args[["dev"]] <- NULL
  }
  if (specifiedSubscene <- ("subscene" %in% names(args))) {
    if (!missing(subscene) && subscene != args[["subscene"]]) stop("'subscene' specified inconsistently")
    subscene <- args[["subscene"]]
    args[["subscene"]] <- NULL
  }
  dev <- as.integer(dev)
  if (!dev) dev <- open3d()
  subscene <- as.integer(subscene)

  if ("userMatrix" %in% names(args)) {
    m <- args$userMatrix
    svd <- svd(m[1:3, 1:3])
    m[1:3, 1:3] <- svd$u %*% t(svd$v)
    theta <- atan2(-m[1, 3], m[1, 1])
    m <- m %*% rotationMatrix(theta, 0, 1, 0)
    svd <- svd(m[1:3, 1:3])
    m[1:3, 1:3] <- svd$u %*% t(svd$v)
    phi <- atan2(-m[2, 3], m[3, 3])
    args$.position <- c(theta, phi) * 180 / pi
  }
  if (forceViewport <- ("windowRect" %in% names(args) &&
    !("viewport" %in% names(args)))) {
    if (specifiedSubscene) {
      warning(
        "Viewport for subscene ", subscene,
        " will be adjusted; other viewports will not be."
      )
    }
    oldviewport <- .Call(rgl_par3d, dev, subscene, list("viewport", "windowRect"))
  }
  value <-
    if (single) {
      .Call(rgl_par3d, dev, subscene, args)[[1]]
    } else {
      .Call(rgl_par3d, dev, subscene, args)
    }

  # The windowRect might be modified by the window manager (if
  # too large, for example), so we need to read it after the
  # change
  if (forceViewport) {
    oldsize <- oldviewport$windowRect[3:4] - oldviewport$windowRect[1:2]
    Sys.sleep(0.1)
    windowRect <- .Call(rgl_par3d, dev, subscene, list("windowRect"))$windowRect
    newsize <- windowRect[3:4] - windowRect[1:2]
    .Call(
      rgl_par3d, dev, subscene,
      list(viewport = round(oldviewport$viewport * newsize / oldsize))
    )
  }

  if (!is.null(names(args))) invisible(value) else value
}
