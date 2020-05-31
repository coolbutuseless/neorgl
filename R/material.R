##
## R source file
## This file is part of rgl
##
##

##
## ===[ SECTION: generic appearance function ]================================
##



#' Generic Appearance setup
#' 
#' Set material properties for geometry appearance.
#' 
#' Values can be queried by specifying their names in a character vector, e.g.
#' \code{material3d("color")}. There is one read-only property that can be
#' queried but not set:
#' 
#' \describe{ \item{isTransparent}{Is the current colour transparent?} }
#' 
#' Only one side at a time can be culled.
#' 
#' The \code{polygon_offset} property is a two element vector giving the
#' \samp{factor} and \samp{units} values to use in a \code{glPolygonOffset()}
#' call in OpenGL.  If only one value is given, it is used for both elements.
#' The \samp{units} value is added to the depth of all pixels in a filled
#' polygon, and the \samp{factor} value is multiplied by an estimate of the
#' slope of the polygon and then added to the depth.  Positive values
#' \dQuote{push} polygons back slightly for the purpose of depth testing, to
#' allow points, lines or other polygons to be drawn on the surface without
#' being obscured due to rounding error.  Negative values pull the object
#' forward. A typical value to use is \code{1} (which is automatically expanded
#' to \code{c(1,1)}). If values are too large, objects which should be behind
#' the polygon will show through, and if values are too small, the objects on
#' the surface will be partially obscured.  Experimentation may be needed to
#' get it right.  The first example in \code{?\link{persp3d}} uses this
#' property to add grid lines to a surface.
#' 
#' \code{material3d} is an alternate interface to the material properties,
#' modelled after \code{\link{par3d}}: rather than setting defaults for
#' parameters that are not specified, they will be left unchanged.
#' \code{material3d} may also be used to query the material properties; see the
#' examples below.
#' 
#' The current implementation does not return parameters for textures.
#' 
#' If \code{point_antialias} is \code{TRUE}, points will be drawn as circles;
#' otherwise, they will be drawn as squares.  Lines tend to appear heavier with
#' \code{line_antialias == TRUE}.
#' 
#' The \code{material} member of the \code{\link{r3dDefaults}} list may be used
#' to set default values for material properties.
#' 
#' The \code{...} parameter to \code{rgl.material} is ignored.
#' 
#' @aliases rgl.material material3d
#' @param color vector of R color characters. Represents the diffuse component
#' in case of lighting calculation (lit = TRUE), otherwise it describes the
#' solid color characteristics.
#' @param lit logical, specifying if lighting calculation should take place on
#' geometry
#' @param ambient,specular,emission,shininess properties for lighting
#' calculation. ambient, specular, emission are R color character string
#' values; shininess represents a numerical.
#' @param alpha vector of alpha values between 0.0 (fully transparent) .. 1.0
#' (opaque).
#' @param smooth logical, specifying whether Gouraud shading (smooth) or flat
#' shading should be used.
#' @param texture path to a texture image file. Supported formats: png.
#' @param textype specifies what is defined with the pixmap \describe{
#' \item{"alpha"}{alpha values} \item{"luminance"}{luminance}
#' \item{"luminance.alpha"}{luminance and alpha} \item{"rgb"}{color}
#' \item{"rgba"}{color and alpha texture} }
#' @param texmipmap Logical, specifies if the texture should be mipmapped.
#' @param texmagfilter specifies the magnification filtering type (sorted by
#' ascending quality): \describe{ \item{"nearest"}{texel nearest to the center
#' of the pixel} \item{"linear"}{weighted linear average of a 2x2 array of
#' texels} }
#' @param texminfilter specifies the minification filtering type (sorted by
#' ascending quality): \describe{ \item{"nearest"}{texel nearest to the center
#' of the pixel} \item{"linear"}{weighted linear average of a 2x2 array of
#' texels} \item{"nearest.mipmap.nearest"}{low quality mipmapping}
#' \item{"nearest.mipmap.linear"}{medium quality mipmapping}
#' \item{"linear.mipmap.nearest"}{medium quality mipmapping}
#' \item{"linear.mipmap.linear"}{high quality mipmapping} }
#' @param texenvmap logical, specifies if auto-generated texture coordinates
#' for environment-mapping should be performed on geometry.
#' @param front,back Determines the polygon mode for the specified side:
#' \describe{ \item{"filled"}{filled polygon} \item{"lines"}{wireframed
#' polygon} \item{"points"}{point polygon} \item{"culled"}{culled (hidden)
#' polygon} }
#' @param size numeric, specifying the size of points in pixels
#' @param lwd numeric, specifying the line width in pixels
#' @param fog logical, specifying if fog effect should be applied on the
#' corresponding shape
#' @param point_antialias,line_antialias logical, specifying if points and
#' lines should be antialiased
#' @param depth_mask logical, specifying whether the object's depth should be
#' stored.
#' @param depth_test Determines which depth test is used to see if this object
#' is visible, depending on its apparent depth in the scene compared to the
#' stored depth. Possible values are \code{"never"}, \code{"less"} (the
#' default), \code{"equal"}, \code{"lequal"} (less than or equal),
#' \code{"greater"}, \code{"notequal"}, \code{"gequal"} (greater than or
#' equal), \code{"always"}.
#' @param polygon_offset If non-zero, offsets are added to the recorded depth
#' of filled polygons.  See Details below.
#' @param ... Any of the arguments above can be passed to \code{material3d};
#' see Details below.  \code{rgl.material} will ignore others.
#' @return \code{rgl.material()} is called for the side effect of setting the
#' material properties. It returns a value invisibly which is not intended for
#' use by the user.
#' 
#' Users should use \code{material3d()} to query material properties.  It
#' returns values similarly to \code{\link{par3d}} as follows: When setting
#' properties, it returns the previous values in a named list.  A named list is
#' also returned when more than one value is queried.  When a single value is
#' queried it is returned directly.
#' @seealso \code{\link{rgl.primitive}}, \code{\link{rgl.bbox}},
#' \code{\link{rgl.bg}}, \code{\link{rgl.light}}
#' @keywords dynamic
#' @examples
#' 
#' save <- material3d("color")
#' material3d(color = "red")
#' material3d("color")
#' material3d(color = save)
#' 
#' # this illustrates the effect of depth_test
#' x <- c(1:3); xmid <- mean(x)
#' y <- c(2, 1, 3); ymid <- mean(y)
#' z <- 1
#' open3d()
#' tests <- c("never", "less", "equal", "lequal", "greater", 
#'                   "notequal", "gequal", "always")
#' for (i in 1:8) {
#'   triangles3d(x, y, z + i, col = heat.colors(8)[i])
#'   texts3d(xmid, ymid, z + i, paste(i, tests[i], sep = ". "), depth_test = tests[i]) 
#' }
#' highlevel()  # To trigger display
#' 
rgl.material <- function(
                         color = c("white"),
                         alpha = c(1.0),
                         lit = TRUE,
                         ambient = "black",
                         specular = "white",
                         emission = "black",
                         shininess = 50.0,
                         smooth = TRUE,
                         texture = NULL,
                         textype = "rgb",
                         texmipmap = FALSE,
                         texminfilter = "linear",
                         texmagfilter = "linear",
                         texenvmap = FALSE,
                         front = "fill",
                         back = "fill",
                         size = 3.0,
                         lwd = 1.0,
                         fog = TRUE,
                         point_antialias = FALSE,
                         line_antialias = FALSE,
                         depth_mask = TRUE,
                         depth_test = "less",
                         polygon_offset = c(0.0, 0.0),
                         ...) {
  # solid or diffuse component

  color <- rgl.mcolor(color)
  if (length(color) < 1) {
    stop("There must be at least one color")
  }

  # light properties

  ambient <- rgl.color(ambient)
  specular <- rgl.color(specular)
  emission <- rgl.color(emission)

  # others

  rgl.bool(lit)
  rgl.bool(fog)
  rgl.bool(smooth)
  rgl.bool(point_antialias)
  rgl.bool(line_antialias)
  rgl.bool(depth_mask)
  rgl.clamp(shininess, 0, 128)
  rgl.numeric(size)
  rgl.numeric(lwd)
  depth_test <- rgl.enum.depthtest(depth_test)

  # side-dependant rendering

  front <- rgl.enum.polymode(front)
  back <- rgl.enum.polymode(back)

  # texture mapping

  rgl.bool(texmipmap)

  if (length(texture) > 1) {
    stop("'texture' should be a single character string or NULL")
  }

  if (is.null(texture)) {
    texture <- ""
  } else {
    texture <- normalizePath(texture)
  }

  textype <- rgl.enum.textype(textype)
  texminfilter <- rgl.enum.texminfilter(texminfilter)
  texmagfilter <- rgl.enum.texmagfilter(texmagfilter)
  rgl.bool(texenvmap)

  # polygon offset

  stopifnot(
    is.numeric(polygon_offset),
    length(polygon_offset) <= 2,
    length(polygon_offset) >= 1
  )
  if (length(polygon_offset) == 1) {
    polygon_offset <- c(polygon_offset, polygon_offset)
  }

  # vector length

  ncolor <- dim(color)[2]
  nalpha <- length(alpha)

  # pack data

  idata <- as.integer(c(
    ncolor, lit, smooth, front, back, fog,
    textype, texmipmap, texminfilter, texmagfilter,
    nalpha, ambient, specular, emission, texenvmap,
    point_antialias, line_antialias,
    depth_mask, depth_test, color
  ))
  cdata <- as.character(c(texture))
  ddata <- as.numeric(c(shininess, size, lwd, polygon_offset, alpha))

  ret <- .C(rgl_material,
    success = FALSE,
    idata,
    cdata,
    ddata
  )
}

rgl.getcolorcount <- function() .C(rgl_getcolorcount, count = integer(1))$count

rgl.getmaterial <- function(ncolors, id = NULL) {
  if (!length(id)) id <- 0L
  if (missing(ncolors)) {
    ncolors <- if (id) rgl.attrib.count(id, "colors") else rgl.getcolorcount()
  }

  idata <- rep(0, 26 + 3 * ncolors)
  idata[1] <- ncolors
  idata[11] <- ncolors

  cdata <- paste(rep(" ", 512), collapse = "")
  ddata <- rep(0, 5 + ncolors)

  ret <- .C(rgl_getmaterial,
    success = FALSE,
    id = as.integer(id),
    idata = as.integer(idata),
    cdata = cdata,
    ddata = as.numeric(ddata)
  )

  if (!ret$success) stop("rgl.getmaterial")

  polymodes <- c("filled", "lines", "points", "culled")
  textypes <- c("alpha", "luminance", "luminance.alpha", "rgb", "rgba")
  minfilters <- c(
    "nearest", "linear", "nearest.mipmap.nearest", "nearest.mipmap.linear",
    "linear.mipmap.nearest", "linear.mipmap.linear"
  )
  magfilters <- c("nearest", "linear")
  depthtests <- c(
    "never", "less", "equal", "lequal", "greater",
    "notequal", "gequal", "always"
  )
  idata <- ret$idata
  ddata <- ret$ddata
  cdata <- ret$cdata

  list(
    color = rgb(idata[24 + 3 * (seq_len(idata[1]))],
      idata[25 + 3 * (seq_len(idata[1]))],
      idata[26 + 3 * (seq_len(idata[1]))],
      maxColorValue = 255
    ),
    alpha = if (idata[11]) ddata[seq(from = 6, length = idata[11])] else 1,
    lit = idata[2] > 0,
    ambient = rgb(idata[12], idata[13], idata[14], maxColorValue = 255),
    specular = rgb(idata[15], idata[16], idata[17], maxColorValue = 255),
    emission = rgb(idata[18], idata[19], idata[20], maxColorValue = 255),
    shininess = ddata[1],
    smooth = idata[3] > 0,
    texture = if (cdata == "") NULL else cdata,
    textype = textypes[idata[7]],
    texmipmap = idata[8] == 1,
    texminfilter = minfilters[idata[9] + 1],
    texmagfilter = magfilters[idata[10] + 1],
    texenvmap = idata[21] == 1,
    front = polymodes[idata[4]],
    back = polymodes[idata[5]],
    size = ddata[2],
    lwd = ddata[3],
    fog = idata[6] > 0,
    point_antialias = idata[22] == 1,
    line_antialias = idata[23] == 1,
    depth_mask = idata[24] == 1,
    depth_test = depthtests[idata[25] + 1],
    isTransparent = idata[26] == 1,
    polygon_offset = ddata[4:5]
  )
}
