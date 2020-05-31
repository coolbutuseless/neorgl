#
# R3D rendering functions - rgl implementation
#

# Node Management

getr3dDefaults <- function() {
  result <- r3dDefaults
  if (exists("r3dDefaults", envir = globalenv())) {
    user <- get("r3dDefaults", envir = .GlobalEnv)
    for (n in names(user)) {
      if (is.list(result[[n]])) {
        result[[n]][names(user[[n]])] <- user[[n]]
      } else {
        result[[n]] <- user[[n]]
      }
    }
  }
  result
}

clear3d <- function(type = c("shapes", "bboxdeco", "material"),
                    defaults = getr3dDefaults(),
                    subscene = 0) {
  .check3d()
  rgl.clear(type, subscene = subscene)

  type <- rgl.enum.nodetype(type)
  if (4 %in% type) { # userviewpoint
    do.call("par3d", defaults["FOV"])
  }
  if (8 %in% type) { # modelviewpoint
    do.call("par3d", defaults["userMatrix"])
  }
  if (5 %in% type) { # material
    if (length(defaults$material)) {
      do.call("material3d", defaults$material)
    }
  }
  if (6 %in% type) { # background
    do.call("bg3d", as.list(defaults$bg))
  }
}

pop3d <- function(...) {
  .check3d()
  rgl.pop(...)
}

# Environment

.material3d <- c(
  "color", "alpha", "lit", "ambient", "specular",
  "emission", "shininess", "smooth", "front", "back", "size",
  "lwd", "fog", "point_antialias", "line_antialias",
  "texture", "textype", "texmipmap",
  "texminfilter", "texmagfilter", "texenvmap",
  "depth_mask", "depth_test", "isTransparent",
  "polygon_offset"
)

.material3d.readOnly <- "isTransparent"

# This function expands a list of arguments by putting
# all entries from Params (i.e. the current settings by default)
# in place for any entries that are not listed.
# Unrecognized args are left in place.

.fixMaterialArgs <- function(..., Params = material3d()) {
  f <- function(...) list(...)
  formals(f) <- c(Params, formals(f))
  names <- as.list(names(Params))
  names(names) <- names
  names <- lapply(names, as.name)
  b <- as.list(body(f))
  body(f) <- as.call(c(b[1], names, b[-1]))
  f(...)
}

# This one just gets the material args
# If warn is TRUE, give a warning instead of ignoring extras.

.getMaterialArgs <- function(..., material = list(), warn = FALSE) {
  fullyNamed <- as.list(match.call(
    rgl.material,
    as.call(c(list(
      as.name("rgl.material"),
      ...
    ), material))
  ))[-1]
  good <- names(fullyNamed) %in% .material3d
  if (warn && !all(good)) {
    warning("Argument(s) ", paste(names(fullyNamed)[!good], collapse = ", "), " not matched.")
  }
  fullyNamed[good]
}

material3d <- function(...) {
  args <- list(...)
  argnames <- setdiff(names(args), .material3d.readOnly)
  if (!length(args)) {
    argnames <- .material3d
  } else {
    if (is.null(names(args)) && all(unlist(lapply(args, is.character)))) {
      argnames <- unlist(args)
      args <- NULL
    }

    if (length(args) == 1) {
      if (is.list(args[[1]]) | is.null(args[[1]])) {
        args <- args[[1]]
        argnames <- names(args)
      }
    }
  }
  value <- rgl.getmaterial()[argnames]
  if (length(args)) {
    args <- do.call(".fixMaterialArgs", args)
    do.call("rgl.material", args)
    return(invisible(value))
  } else if (length(argnames) == 1) {
    return(value[[1]])
  } else {
    return(value)
  }
}



#' Set up Background
#' 
#' Set up the background of the scene.
#' 
#' If sphere is set to \code{TRUE}, an environmental sphere enclosing the whole
#' scene is drawn.
#' 
#' If not, but the material properties include a bitmap as a texture, the
#' bitmap is drawn in the background of the scene.  (The bitmap colors modify
#' the general color setting.)
#' 
#' If neither a sphere nor a bitmap background is drawn, the background is
#' filled with a solid color.
#' 
#' @aliases rgl.bg bg3d
#' @param fogtype fog type: \describe{ \item{"none"}{no fog}
#' \item{"linear"}{linear fog function} \item{"exp"}{exponential fog function}
#' \item{"exp2"}{squared exponential fog function} }
#' @param sphere logical, if true, an environmental sphere geometry is used for
#' the background decoration.
#' @param color Primary color is used for background clearing and as fog color.
#' Secondary color is used for background sphere geometry. See
#' \code{\link{material3d}} for details.
#' @param back Specifies the fill style of the sphere geometry. See
#' \code{\link{material3d}} for details.
#' @param ... Material properties. See \code{\link{material3d}} for details.
#' @seealso \code{\link{material3d}}, \code{\link{bgplot3d}} to add a 2D plot
#' as background.
#' @keywords dynamic
#' @examples
#' 
#'   open3d()
#'   
#'   # a simple white background
#'   
#'   bg3d("white")
#' 
#'   # the holo-globe (inspired by star trek):
#' 
#'   bg3d(sphere = TRUE, color = c("black", "green"), lit = FALSE, back = "lines" )
#' 
#'   # an environmental sphere with a nice texture.
#' 
#'   bg3d(sphere = TRUE, texture = system.file("textures/sunsleep.png", package = "rgl"), 
#'          back = "filled" )
#'          
#'   # The same texture as a fixed background
#'   
#'   open3d()
#'   bg3d(texture = system.file("textures/sunsleep.png", package = "rgl"), col = "white")
#' 
bg3d <- function(...) {
  .check3d()
  save <- material3d()
  on.exit(material3d(save))
  bgid <- rgl.ids("background")$id
  if (length(bgid) && nrow(flags <- rgl.attrib(bgid[1], "flags"))) {
    sphere <- flags["sphere", 1]
    fogtype <- if (flags["linear_fog", 1]) {
      "linear"
    } else if (flags["exp_fog", 1]) {
      "exp"
    } else if (flags["exp2_fog", 1]) {
      "exp2"
    } else {
      "none"
    }
  } else {
    sphere <- FALSE
    fogtype <- "none"
  }
  new <- .fixMaterialArgs(
    sphere = sphere, fogtype = fogtype,
    color = c("black", "white"),
    back = "lines", lit = FALSE, Params = save
  )
  do.call("rgl.bg", .fixMaterialArgs(..., Params = new))
}

light3d <- function(theta = 0, phi = 15, x = NULL, ...) {
  .check3d()
  if (is.null(x)) {
    rgl.light(theta = theta, phi = phi, x = x, ...)
  } else {
    rgl.light(x = x, ...)
  }
}

view3d <- function(theta = 0, phi = 15, ...) {
  .check3d()
  rgl.viewpoint(theta = theta, phi = phi, ...)
}

bbox3d <- function(xat = NULL,
                   yat = NULL,
                   zat = NULL,
                   xunit = "pretty",
                   yunit = "pretty",
                   zunit = "pretty",
                   expand = 1.03, draw_front = FALSE, ...) {
  .check3d()
  save <- material3d()
  on.exit(material3d(save))
  ranges <- .getRanges(expand = expand)
  do.call("rgl.bbox", c(
    list(
      xat = xat, yat = yat, zat = zat,
      xunit = xunit, yunit = yunit, zunit = zunit, expand = expand,
      draw_front = draw_front
    ),
    .fixMaterialArgs(..., Params = save)
  ))
}



#' Set the observer location.
#' 
#' This function sets the location of the viewer.
#' 
#' This function sets the location of the viewer relative to the scene, after
#' the model transformations (scaling, rotation) have been done, but before
#' lighting or projection have been applied.  (See \code{\link{par3d}} for
#' details on the rendering pipeline.)
#' 
#' The coordinate system is a slightly strange one: the X coordinate moves the
#' observer location from left to right, and the Y coordinate moves up and
#' down.  The Z coordinate changes the depth from the viewer.  All are measured
#' relative to the center of the bounding box (\code{par("bbox")}) of the
#' subscene. The observer always looks in the positive Z direction after the
#' model rotation have been done.  The coordinates are in post-scaling units.
#' 
#' @param x,y,z The location as a 3 vector, using the usual \code{xyz.coords}
#' conventions for specification.  If \code{x} is missing or any coordinate is
#' \code{NA}, no change will be made to the location.
#' @param auto If \code{TRUE}, the location will be set automatically by
#' \pkg{rgl} to make the whole bounding box visible.
#' @return Invisibly returns the previous value.
#' @note This function is likely to change in future versions of \pkg{rgl}, to
#' allow more flexibility in the specification of the observer's location and
#' orientation.
#' @author Duncan Murdoch
#' @keywords graphics
#' @examples
#' 
#' example(surface3d)  # The volcano data
#' observer3d(0, 0, 440) # Viewed from very close up
#' 
observer3d <- function(x, y = NULL, z = NULL, auto = FALSE) {
  if (missing(x)) {
    location <- c(NA, NA, NA)
  } else {
    xyz <- xyz.coords(x, y, z)
    location <- c(xyz$x, xyz$y, xyz$z)
    if (length(location) != 3) stop("A single point must be specified for the observer location")
  }
  prev <- .C(rgl_getObserver, success = integer(1), ddata = numeric(3), NAOK = TRUE)$ddata
  .C(rgl_setObserver, success = as.integer(auto), ddata = as.numeric(location), NAOK = TRUE)
  lowlevel(prev)
}

# Shapes



#' add primitive set shape
#' 
#' Adds a shape node to the current scene
#' 
#' 
#' The functions \code{points3d}, \code{lines3d}, \code{segments3d},
#' \code{triangles3d} and \code{quads3d} add points, joined lines, line
#' segments, filled triangles or quadrilaterals to the plots.  They correspond
#' to the OpenGL types \code{GL_POINTS, GL_LINE_STRIP, GL_LINES, GL_TRIANGLES}
#' and \code{GL_QUADS} respectively.
#' 
#' Points are taken in pairs by \code{segments3d}, triplets as the vertices of
#' the triangles, and quadruplets for the quadrilaterals.  Colors are applied
#' vertex by vertex; if different at each end of a line segment, or each vertex
#' of a polygon, the colors are blended over the extent of the object.
#' Polygons must be non-degenerate and quadrilaterals must be entirely in one
#' plane and convex, or the results are undefined.
#' 
#' These functions call the lower level functions \code{\link{rgl.points}},
#' \code{\link{rgl.linestrips}}, and so on, and are provided for convenience.
#' 
#' The appearance of the new objects are defined by the material properties.
#' See \code{\link{rgl.material}} for details.
#' 
#' The two principal differences between the \code{rgl.*} functions and the
#' \code{*3d} functions are that the former set all unspecified material
#' properties to defaults, whereas the latter use current values as defaults;
#' the former make persistent changes to material properties with each call,
#' whereas the latter make temporary changes only for the duration of the call.
#' 
#' @aliases points3d lines3d segments3d triangles3d quads3d
#' @param x,y,z coordinates. Any reasonable way of defining the coordinates is
#' acceptable.  See the function \code{\link[grDevices]{xyz.coords}} for
#' details.
#' @param ... Material properties (see \code{\link{rgl.material}}).  For
#' normals use \code{normals} and for texture coordinates use \code{texcoords};
#' see \code{\link{rgl.primitive}} for details.
#' @return Each function returns the integer object ID of the shape that was
#' added to the scene.  These can be passed to \code{\link{rgl.pop}} to remove
#' the object from the scene.
#' @author Ming Chen and Duncan Murdoch
#' @keywords dynamic
#' @examples
#' 
#' # Show 12 random vertices in various ways. 
#' 
#' M <- matrix(rnorm(36), 3, 12, dimnames = list(c('x', 'y', 'z'), 
#'                                        rep(LETTERS[1:4], 3)))
#' 
#' # Force 4-tuples to be convex in planes so that quads3d works.
#' 
#' for (i in c(1, 5, 9)) {
#'     quad <- as.data.frame(M[, i + 0:3])
#'     coeffs <- runif(2, 0, 3)
#'     if (mean(coeffs) < 1) coeffs <- coeffs + 1 - mean(coeffs)
#'     quad$C <- with(quad, coeffs[1]*(B - A) + coeffs[2]*(D - A) + A)
#'     M[, i + 0:3] <- as.matrix(quad)
#' }
#' 
#' open3d()
#' 
#' # Rows of M are x, y, z coords; transpose to plot
#' 
#' M <- t(M)
#' shift <- matrix(c(-3, 3, 0), 12, 3, byrow = TRUE)
#' 
#' points3d(M)
#' lines3d(M + shift)
#' segments3d(M + 2*shift)
#' triangles3d(M + 3*shift, col = 'red')
#' quads3d(M + 4*shift, col = 'green')  
#' text3d(M + 5*shift, texts = 1:12)
#' 
#' # Add labels
#' 
#' shift <- outer(0:5, shift[1, ])
#' shift[, 1] <- shift[, 1] + 3
#' text3d(shift, 
#'        texts = c('points3d', 'lines3d', 'segments3d',
#'          'triangles3d', 'quads3d', 'text3d'),
#'        adj = 0)
#'  rgl.bringtotop()
#' 
points3d <- function(x, y = NULL, z = NULL, ...) {
  .check3d()
  save <- material3d()
  on.exit(material3d(save))
  do.call("rgl.points", c(list(x = x, y = y, z = z), .fixMaterialArgs(..., Params = save)))
}

lines3d <- function(x, y = NULL, z = NULL, ...) {
  .check3d()
  save <- material3d()
  on.exit(material3d(save))
  do.call("rgl.linestrips", c(list(x = x, y = y, z = z), .fixMaterialArgs(..., Params = save)))
}

segments3d <- function(x, y = NULL, z = NULL, ...) {
  .check3d()
  save <- material3d()
  on.exit(material3d(save))
  do.call("rgl.lines", c(list(x = x, y = y, z = z), .fixMaterialArgs(..., Params = save)))
}

triangles3d <- function(x, y = NULL, z = NULL, ...) {
  .check3d()
  save <- material3d()
  on.exit(material3d(save))
  do.call("rgl.triangles", c(list(x = x, y = y, z = z), .fixMaterialArgs(..., Params = save)))
}

quads3d <- function(x, y = NULL, z = NULL, ...) {
  .check3d()
  save <- material3d()
  on.exit(material3d(save))
  do.call("rgl.quads", c(list(x = x, y = y, z = z), .fixMaterialArgs(..., Params = save)))
}



#' add text
#' 
#' Adds text to the scene. The text is positioned in 3D space.  Text is always
#' oriented towards the camera.
#' 
#' The \code{adj} parameter determines the position of the text relative to the
#' specified coordinate.  Use \code{adj = c(0, 0)} to place the left bottom
#' corner at \code{(x, y, z)}, \code{adj = c(0.5, 0.5)} to center the text
#' there, and \code{adj = c(1, 1)} to put the right top corner there. The
#' optional second coordinate for vertical adjustment defaults to \code{0.5}.
#' Placement is done using the "advance" of the string and the "ascent" of the
#' font relative to the baseline, when these metrics are known.
#' 
#' \code{text3d} and \code{texts3d} draw text using the \link{r3d} conventions.
#' These are synonyms; the former is singular to be consistent with the classic
#' 2-D graphics functions, and the latter is plural to be consistent with all
#' the other graphics primitives.  Take your choice!
#' 
#' If any coordinate or text is \code{NA}, that text is not plotted.
#' 
#' If \code{usePlotmath} is \code{TRUE}, the work will be done by the
#' \code{\link{plotmath3d}} function instead of \code{rgl.texts}.  This is the
#' default if the \code{texts} parameter is \dQuote{language}, e.g. the result
#' of a call to \code{\link{expression}} or \code{\link{quote}}.
#' 
#' @aliases rgl.texts text3d texts3d rglFonts
#' @param x,y,z point coordinates.  Any reasonable way of defining the
#' coordinates is acceptable.  See the function
#' \code{\link[grDevices]{xyz.coords}} for details.
#' @param text text character vector to draw
#' @param texts text character vector to draw
#' @param adj one value specifying the horizontal adjustment, or two,
#' specifying horizontal and vertical adjustment respectively.
#' @param pos a position specifier for the text.  If specified, this overrides
#' any \code{adj} value given. Values of 1, 2, 3 and 4, respectively indicate
#' positions below, to the left of, above and to the right of the specified
#' coordinates.
#' @param offset when \code{pos} is specified, this value gives the offset of
#' the label from the specified coordinate in fractions of a character width.
#' @param family A device-independent font family name, or ""
#' @param font A numeric font number from 1 to 5
#' @param cex A numeric character expansion value
#' @param useFreeType logical.  Should FreeType be used to draw text? (See
#' details below.)
#' @param usePlotmath logical.  Should \code{\link{plotmath3d}} be used for the
#' text?
#' @param ... In \code{rgl.texts}, material properties; see
#' \code{\link{rgl.material}} for details.  In \code{rglFonts}, device
#' dependent font definitions for use with FreeType.  In the other functions,
#' additional parameters to pass to \code{rgl.texts}.
#' @return The text drawing functions return the object ID of the text object
#' (or sprites, in case of \code{usePlotmath = TRUE}) invisibly.
#' 
#' \code{rglFonts} returns the current set of font definitions.
#' @section Fonts: Fonts are specified using the \code{family}, \code{font},
#' \code{cex}, and \code{useFreeType} arguments.  Defaults for the currently
#' active device may be set using \code{\link{par3d}}, or for future devices
#' using \code{\link{r3dDefaults}}.
#' 
#' The \code{family} specification is the same as for standard graphics, i.e.
#' families \cr \code{c("serif", "sans", "mono", "symbol")} \cr are normally
#' available, but users may add additional families.  \code{font} numbers are
#' restricted to the range 1 to 4 for standard, bold, italic and bold italic
#' respectively; with font 5 recoded as family \code{"symbol"} font 1.
#' 
#' Using an unrecognized value for \code{"family"} will result in the system
#' standard font as used in rgl up to version 0.76.  That font is not resizable
#' and \code{font} values are ignored.
#' 
#' If \code{useFreeType} is \code{TRUE}, then rgl will use the FreeType
#' anti-aliased fonts for drawing.  This is generally desirable, and it is the
#' default if rgl was built to support FreeType.
#' 
#' FreeType fonts are specified using the \code{rglFonts} function.  This
#' function takes a vector of four filenames of TrueType font files which will
#' be used for the four styles regular, bold, italic and bold italic.  The
#' vector is passed with a name to be used as the family name, e.g.
#' \code{rglFonts(sans = c("/path/to/FreeSans.ttf", ...))}.  In order to limit
#' the file size, \code{rgl} ships with just 3 font files, for regular versions
#' of the \code{serif}, \code{sans} and \code{mono} families.  Additional free
#' font files are available from the Amaya project at
#' \url{http://dev.w3.org/cvsweb/Amaya/fonts/}.  See the example below for how
#' to specify a full set of fonts.
#' 
#' Full pathnames should normally be used to specify font files.  If relative
#' paths are used, they are interpreted differently by platform.  Currently
#' Windows fonts are looked for in the Windows fonts folder, while other
#' platforms use the current working directory.
#' 
#' If FreeType fonts are not used, then bitmapped fonts will be used instead.
#' On Windows these will be based on the fonts specified using the #ifdef{
#' windows \code{\link{windowsFonts}} }#ifndef{ windows \code{windowsFonts}
#' }function, and are resizable. Other platforms will use the default bitmapped
#' font which is not resizable.
#' 
#' Bitmapped fonts have a limited number of characters supported; if any
#' unsupported characters are used, an error will be thrown.
#' @seealso \code{\link{r3d}}, \code{\link{plotmath3d}}
#' @keywords dynamic
#' @examples
#' 
#' open3d()
#' famnum <- rep(1:4, 8)
#' family <- c("serif", "sans", "mono", "symbol")[famnum]
#' font <- rep(rep(1:4, each = 4), 2)
#' cex <- rep(1:2, each = 16)
#' text3d(font, cex, famnum, text = paste(family, font), adj = 0.5, 
#'        color = "blue", family = family, font = font, cex = cex)
#' \dontrun{
#' # These FreeType fonts are available from the Amaya project, and are not shipped
#' # with rgl.  You would normally install them to the rgl/fonts directory
#' # and use fully qualified pathnames, e.g. 
#' # system.file("fonts/FreeSerif.ttf", package = "rgl")
#' 
#' rglFonts(serif = c("FreeSerif.ttf", "FreeSerifBold.ttf", "FreeSerifItalic.ttf",
#'                  "FreeSerifBoldItalic.ttf"),
#'          sans  = c("FreeSans.ttf", "FreeSansBold.ttf", "FreeSansOblique.ttf",
#'                  "FreeSansBoldOblique.ttf"),
#'          mono  = c("FreeMono.ttf", "FreeMonoBold.ttf", "FreeMonoOblique.ttf",
#'                  "FreeMonoBoldOblique.ttf"),
#'          symbol= c("ESSTIX10.TTF", "ESSTIX12.TTF", "ESSTIX9_.TTF", 
#'                  "ESSTIX11.TTF"))
#' } 
#' 
text3d <- function(x, y = NULL, z = NULL,
                   texts, adj = 0.5, pos = NULL, offset = 0.5,
                   usePlotmath = is.language(texts), ...) {
  if (usePlotmath) {
    return(plotmath3d(
      x = x, y = y, z = z, text = texts, adj = adj,
      pos = pos, offset = offset, ...
    ))
  }
  .check3d()
  save <- material3d()
  on.exit(material3d(save))
  new <- .fixMaterialArgs(..., Params = save)
  do.call("rgl.texts", c(list(
    x = x, y = y, z = z, text = texts,
    adj = adj, pos = pos,
    offset = offset
  ), new))
}
texts3d <- text3d



#' add sphere set shape
#' 
#' Adds a sphere set shape node to the scene
#' 
#' If a non-isometric aspect ratio is chosen, these functions will still draw
#' objects that appear to the viewer to be spheres.  Use
#' \code{\link{ellipse3d}} to draw shapes that are spherical in the data scale.
#' 
#' When the scale is not isometric, the radius is measured in an average scale.
#' In this case the bounding box calculation is iterative, since rescaling the
#' plot changes the shape of the spheres in user-coordinate, which changes the
#' bounding box.  Versions of \code{rgl} prior to 0.92.802 did not do this
#' iterative adjustment.
#' 
#' If any coordinate or radius is \code{NA}, the sphere is not plotted.
#' 
#' If a texture is used, its bitmap is wrapped around the sphere, with the top
#' edge at the maximum y coordinate, and the left-right edges joined at the
#' maximum in the z coordinate, centred in x.
#' 
#' @aliases rgl.spheres spheres3d
#' @param x,y,z Numeric vector of point coordinates corresponding to the center
#' of each sphere.  Any reasonable way of defining the coordinates is
#' acceptable.  See the function \code{\link[grDevices]{xyz.coords}} for
#' details.
#' @param radius Vector or single value defining the sphere radius/radii
#' @param ... Material properties. See \code{\link{rgl.material}} for details.
#' @return A shape ID of the spheres object is returned.
#' @seealso \code{\link{rgl.material}}, \code{\link{aspect3d}} for setting
#' non-isometric scales
#' @keywords dynamic
#' @examples
#' 
#' open3d()
#' spheres3d(rnorm(10), rnorm(10), rnorm(10), radius = runif(10), color = rainbow(10))
#' 
spheres3d <- function(x, y = NULL, z = NULL, radius = 1, ...) {
  .check3d()
  save <- material3d()
  on.exit(material3d(save))
  do.call("rgl.spheres", c(list(x = x, y = y, z = z, radius = radius), .fixMaterialArgs(..., Params = save)))
}



#' add planes
#' 
#' \code{planes3d} and \code{rgl.planes} add mathematical planes to a scene.
#' Their intersection with the current bounding box will be drawn.
#' \code{clipplanes3d} and \code{rgl.clipplanes} add clipping planes to a
#' scene.
#' 
#' \code{planes3d} and \code{rgl.planes} draw planes using the parametrization
#' \eqn{a x + b y + c z + d = 0}.  Multiple planes may be specified by giving
#' multiple values for any of \code{a, b, c, d}; the other values will be
#' recycled as necessary.
#' 
#' \code{clipplanes3d} and \code{rgl.clipplanes} define clipping planes using
#' the same equations.  Clipping planes suppress the display of other objects
#' (or parts of them) in the subscene, based on their coordinates. Points (or
#' parts of lines or surfaces) where the coordinates \code{x, y, z} satisfy
#' \eqn{a x + b y + c z + d < 0} will be suppressed.
#' 
#' The number of clipping planes supported by the OpenGL driver is
#' implementation dependent; use \code{par3d("maxClipPlanes")} to find the
#' limit.
#' 
#' @aliases rgl.planes planes3d rgl.clipplanes clipplanes3d
#' @param a,b,c Coordinates of the normal to the plane.  Any reasonable way of
#' defining the coordinates is acceptable.  See the function
#' \code{\link[grDevices]{xyz.coords}} for details.
#' @param d Coordinates of the "offset".  See the details.
#' @param \dots Material properties. See \code{\link{rgl.material}} for
#' details.
#' @return A shape ID of the planes or clipplanes object is returned invisibly.
#' @seealso \code{\link{abclines3d}}, \code{\link{rgl.abclines}} for
#' mathematical lines.
#' 
#' \code{\link{triangles3d}}, \code{\link{rgl.triangles}} or the corresponding
#' functions for quadrilaterals may be used to draw sections of planes that do
#' not adapt to the bounding box.
#' 
#' The example in \link{subscene3d} shows how to combine clipping planes to
#' suppress complex shapes.
#' @keywords dynamic
#' @examples
#' 
#' 
#' # Show regression plane with z as dependent variable
#' 
#' open3d()
#' x <- rnorm(100)
#' y <- rnorm(100)
#' z <- 0.2*x - 0.3*y + rnorm(100, sd = 0.3)
#' fit <- lm(z ~ x + y)
#' plot3d(x, y, z, type = "s", col = "red", size = 1)
#' 
#' coefs <- coef(fit)
#' a <- coefs["x"]
#' b <- coefs["y"]
#' c <- -1
#' d <- coefs["(Intercept)"]
#' planes3d(a, b, c, d, alpha = 0.5)
#' 
#' open3d()
#' ids <- plot3d(x, y, z, type = "s", col = "red", size = 1, forceClipregion = TRUE) 
#' oldid <- useSubscene3d(ids["clipregion"])
#' clipplanes3d(a, b, c, d)
#' useSubscene3d(oldid)
#' 
planes3d <- function(a, b = NULL, c = NULL, d = 0, ...) {
  .check3d()
  save <- material3d()
  on.exit(material3d(save))
  do.call("rgl.planes", c(list(a = a, b = b, c = c, d = d), .fixMaterialArgs(..., Params = save)))
}

clipplanes3d <- function(a, b = NULL, c = NULL, d = 0) {
  .check3d()
  rgl.clipplanes(a = a, b = b, c = c, d = d)
}



#' Lines intersecting the bounding box
#' 
#' This adds mathematical lines to a scene.  Their intersection with the
#' current bounding box will be drawn.
#' 
#' These functions draw the segment of a line that intersects the current
#' bounding box of the scene using the parametrization \eqn{ (x, y, z) + (a, b,
#' c) * s } where \eqn{s} is a real number.
#' 
#' Any reasonable way of defining the coordinates \code{x, y, z} and \code{a,
#' b, c} is acceptable.  See the function \code{\link[grDevices]{xyz.coords}}
#' for details.
#' 
#' @aliases rgl.abclines abclines3d
#' @param x,y,z Coordinates of points through which each line passes.
#' @param a,b,c Coordinates of the direction vectors for the lines.
#' @param ...  Material properties.
#' @return A shape ID of the object is returned invisibly.
#' @seealso \code{\link{planes3d}}, \code{\link{rgl.planes}} for mathematical
#' planes.
#' 
#' \code{\link{segments3d}} draws sections of lines that do not adapt to the
#' bounding box.
#' @keywords dynamic
#' @examples
#' 
#' plot3d(rnorm(100), rnorm(100), rnorm(100))
#' abclines3d(0, 0, 0, a = diag(3), col = "gray")
#' 
abclines3d <- function(x, y = NULL, z = NULL, a, b = NULL, c = NULL, ...) {
  .check3d()
  save <- material3d()
  on.exit(material3d(save))
  do.call("rgl.abclines", c(list(x = x, y = y, z = z, a = a, b = b, c = c), .fixMaterialArgs(..., Params = save)))
}

sprites3d <- function(x, y = NULL, z = NULL, radius = 1, shapes = NULL, userMatrix, ...) {
  .check3d()
  save <- material3d()
  on.exit(material3d(save))
  if (missing(userMatrix)) {
    userMatrix <- getr3dDefaults()$userMatrix
    if (is.null(userMatrix)) userMatrix <- diag(4)
  }
  savepar <- par3d(skipRedraw = TRUE, ignoreExtent = TRUE)
  on.exit(par3d(savepar), add = TRUE)
  force(shapes)
  par3d(ignoreExtent = savepar$ignoreExtent)

  do.call("rgl.sprites", c(
    list(
      x = x, y = y, z = z, radius = radius, shapes = shapes,
      userMatrix = userMatrix
    ),
    .fixMaterialArgs(..., Params = save)
  ))
}

terrain3d <- function(x, y = NULL, z = NULL, ..., normal_x = NULL, normal_y = NULL, normal_z = NULL) {
  .check3d()
  save <- material3d()
  on.exit(material3d(save))
  do.call("rgl.surface", c(
    list(
      x = x, y = z, z = y, coords = c(1, 3, 2),
      normal_x = normal_x, normal_y = normal_z, normal_z = normal_y
    ),
    .fixMaterialArgs(..., Params = save)
  ))
}


#' add height-field surface shape
#' 
#' Adds a surface to the current scene. The surface is defined by a matrix
#' defining the height of each grid point and two vectors defining the grid.
#' 
#' Adds a surface mesh to the current scene. The surface is defined by the
#' matrix of height values in \code{z}, with rows corresponding to the values
#' in \code{x} and columns corresponding to the values in \code{y}.  This is
#' the same parametrization as used in \code{\link{persp}}.
#' 
#' If the \code{x} or \code{y} argument is a matrix, then it must be of the
#' same dimension as \code{z}, and the values in the matrix will be used for
#' the corresponding coordinates. This is used to plot shapes such as cylinders
#' where z is not a function of x and y.
#' 
#' If the normals are not supplied, they will be calculated automatically based
#' on neighbouring points.
#' 
#' \code{surface3d} always draws the surface with the `front' upwards (i.e.
#' towards higher \code{z} values).  This can be used to render the top and
#' bottom differently; see \code{\link{rgl.material}} and the example below.
#' 
#' For more flexibility in defining the surface, use \code{\link{rgl.surface}}.
#' 
#' \code{surface3d} and \code{terrain3d} are synonyms.
#' 
#' @aliases surface3d terrain3d
#' @param x values corresponding to rows of \code{z}, or matrix of x
#' coordinates
#' @param y values corresponding to the columns of \code{z}, or matrix of y
#' coordinates
#' @param z matrix of heights
#' @param ... Material and texture properties. See \code{\link{rgl.material}}
#' for details.
#' @param normal_x,normal_y,normal_z matrices of the same dimension as \code{z}
#' giving the coordinates of normals at each grid point
#' @seealso \code{\link{rgl.material}}, \code{\link{rgl.surface}}.  See
#' \code{\link{persp3d}} for a higher level interface.
#' @keywords dynamic
#' @examples
#' 
#' 
#' #
#' # volcano example taken from "persp"
#' #
#' 
#' data(volcano)
#' 
#' z <- 2 * volcano        # Exaggerate the relief
#' 
#' x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
#' y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)
#' 
#' zlim <- range(z)
#' zlen <- zlim[2] - zlim[1] + 1
#' 
#' colorlut <- terrain.colors(zlen) # height color lookup table
#' 
#' col <- colorlut[ z - zlim[1] + 1 ] # assign colors to heights for each point
#' 
#' open3d()
#' surface3d(x, y, z, color = col, back = "lines")
#' 
#' 
surface3d <- terrain3d

# Interaction



#' Select a rectangle in an RGL scene
#' 
#' This function allows the user to use the mouse to select a region in an RGL
#' scene.
#' 
#' 
#' This function selects 3-dimensional regions by allowing the user to use a
#' mouse to draw a rectangle showing the projection of the region onto the
#' screen.  It returns a function which tests points for inclusion in the
#' selected region.
#' 
#' If the scene is later moved or rotated, the selected region will remain the
#' same, no longer corresponding to a rectangle on the screen.
#' 
#' @aliases select3d rgl.select3d
#' @param button Which button to use for selection.
#' @param dev,subscene The rgl device and subscene to work with
#' @param ...  Button argument to pass to \code{rgl.select3d}
#' @return Returns a function \code{f(x, y, z)} which tests whether each of the
#' points \code{(x, y, z)} is in the selected region, returning a logical
#' vector.  This function accepts input in a wide variety of formats as it uses
#' \code{\link[grDevices]{xyz.coords}} to interpret its parameters.
#' @author Ming Chen / Duncan Murdoch
#' @seealso \code{\link{selectpoints3d}}, \code{\link{locator}}
#' @keywords dynamic
#' @examples
#' 
#' 
#' # Allow the user to select some points, and then redraw them
#' # in a different color
#' 
#' if (interactive()) {
#'  x <- rnorm(1000)
#'  y <- rnorm(1000)
#'  z <- rnorm(1000)
#'  open3d()
#'  points3d(x, y, z)
#'  f <- select3d()
#'  if (!is.null(f)) {
#'    keep <- f(x, y, z)
#'    rgl.pop()
#'    points3d(x[keep], y[keep], z[keep], color = 'red')
#'    points3d(x[!keep], y[!keep], z[!keep])
#'  }
#' }
#' 
select3d <- function(...) {
  .check3d()
  rgl.select3d(...)
}

# 3D Generic Object Rendering Attributes

dot3d <- function(x, ...) UseMethod("dot3d")
wire3d <- function(x, ...) UseMethod("wire3d")
shade3d <- function(x, ...) UseMethod("shade3d")

# 3D Generic transformation


translate3d <- function(obj, x, y, z, ...) UseMethod("translate3d")
scale3d <- function(obj, x, y, z, ...) UseMethod("scale3d")
rotate3d <- function(obj, angle, x, y, z, matrix, ...) UseMethod("rotate3d")
transform3d <- function(obj, matrix, ...) rotate3d(obj, matrix = matrix, ...)



#' generic subdivision surface method
#' 
#' The subdivision surface algorithm divides and refines (deforms) a given mesh
#' recursively to certain degree (depth). The mesh3d algorithm consists of two
#' stages: divide and deform. The divide step generates for each triangle or
#' quad four new triangles or quads, the deform step drags the points
#' (refinement step).
#' 
#' \code{subdivision3d} takes a mesh object and replaces each triangle or quad
#' with 4 new ones by adding vertices half-way along the edges (and one in the
#' centre of a quad).  The positions of the vertices are deformed so that the
#' resulting surface is smoother than the original.  These operations are
#' repeated \code{depth} times.
#' 
#' The other functions do the individual steps of the subdivision.
#' \code{divide.mesh3d} adds the extra vertices.  \code{deform.mesh3d} does the
#' smoothing by replacing each vertex with the average of each of its
#' neighbours. \code{normalize.mesh3d} normalizes the homogeneous coordinates,
#' by setting the 4th coordinate to 1.  (The 4th coordinate is used as a weight
#' in the deform step.)
#' 
#' @aliases subdivision3d subdivision3d.mesh3d divide.mesh3d normalize.mesh3d
#' deform.mesh3d
#' @param x 3d geometry mesh
#' @param mesh 3d geometry mesh
#' @param depth recursion depth
#' @param normalize normalize mesh3d coordinates after division if
#' \code{deform} is \code{TRUE}
#' @param deform deform mesh
#' @param it indices for triangular faces
#' @param ib indices for quad faces
#' @param vb matrix of vertices: 4xn matrix (rows x, y, z, h) or equivalent
#' vector, where h indicates scaling of each plotted quad
#' @param ... other arguments (unused)
#' @seealso \code{\link{r3d}} \code{\link{mesh3d}}
#' @keywords dynamic
#' @examples
#' 
#'   open3d()
#'   shade3d( subdivision3d( cube3d(), depth = 3 ), color = "red", alpha = 0.5 )
#' 
subdivision3d <- function(x, ...) UseMethod("subdivision3d")

# 3D Custom shapes

particles3d <- function(x, y = NULL, z = NULL, radius = 1, ...) {
  sprites3d(
    x = x, y = y, z = z, radius = radius,
    lit = FALSE, alpha = 0.2,
    textype = "alpha",
    texture = system.file("textures/particle.png", package = "rgl"),
    ...
  )
}

# r3d default settings for new windows

r3dDefaults <- list(
  userMatrix = rotationMatrix(290 * pi / 180, 1, 0, 0),
  mouseMode = c("trackball", "zoom", "fov", "pull"),
  FOV = 30,
  bg = list(color = "white"),
  family = "sans",
  material = list(color = "black", fog = FALSE)
)

open3d <- function(..., params = getr3dDefaults(),
                   useNULL = rgl.useNULL()) {
  args <- list(...)
  if (!is.null(args$antialias)
  || !is.null(args$antialias <- r3dDefaults$antialias)) {
    saveopt <- options(rgl.antialias = args$antialias)
    on.exit(options(saveopt))
    args$antialias <- NULL
  }

  rgl.open(useNULL)

  if (!is.null(args$material)) {
    params$material <- do.call(.fixMaterialArgs, c(args$material, Params = list(params$material)))
    args$material <- NULL
  }

  if (length(args) && (is.null(names(args))
  || any(nchar(names(args)) == 0))) {
    stop("open3d parameters must be named")
  }

  params[names(args)] <- args

  clear3d("material", defaults = params)
  params$material <- NULL

  if (!is.null(params$bg)) {
    do.call("bg3d", params$bg)
    params$bg <- NULL
  }

  do.call("par3d", params)
  return(rgl.cur())
}



#' Check for an open rgl window.
#' 
#' Mostly for internal use, this function returns the current device number if
#' one exists, or opens a new device and returns that.
#' 
#' 
#' @aliases .check3d check3d
#' @return The device number of an rgl device.
#' @author Duncan Murdoch
#' @seealso \code{\link{open3d}}
#' @examples
#' 
#' rgl.dev.list()
#' .check3d()
#' rgl.dev.list()
#' .check3d()
#' rgl.dev.list()
#' rgl.close()
#' 
.check3d <- function() {
  if (result <- rgl.cur()) {
    return(result)
  } else {
    return(open3d())
  }
}

snapshot3d <- function(filename, ..., scene) {
  if (missing(scene)) {
    return(rgl.snapshot(filename, ...))
  } else if (inherits(scene, "rglWebGL")) {
    snapshot <- scene$x$snapshot
    if (!is.null(snapshot)) {
      return(saveURI(snapshot, filename))
    } else {
      scene <- attr(scene, "origScene")
    }
  }
  saveopts <- options(rgl.useNULL = FALSE)
  on.exit(options(saveopts))
  plot3d(scene) # calls open3d internally
  on.exit(rgl.close(), add = TRUE)
  rgl.snapshot(filename, ...)
}
