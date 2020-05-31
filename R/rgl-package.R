

#' Imported from magrittr
#' 
#' This object is imported from \pkg{magrittr}. Follow the link to its
#' documentation. \describe{ \item{magrittr}{\code{\link[magrittr:pipe]{%>%}}}
#' }
#' 
#' Pipes can be used to string together \code{\link{rglwidget}} calls and
#' \code{\link{playwidget}} calls.  See \code{\link{ageControl}} for an
#' example.
#' 
#' 
#' @aliases %>% pipe
#' @docType import
NULL





#' Work with homogeneous coordinates
#' 
#' These functions construct 4x4 matrices for transformations in the
#' homogeneous coordinate system used by OpenGL, and translate vectors between
#' homogeneous and Euclidean coordinates.
#' 
#' OpenGL uses homogeneous coordinates to handle perspective and affine
#' transformations.  The homogeneous point \code{(x, y, z, w)} corresponds to
#' the Euclidean point \code{(x/w, y/w, z/w)}.  The matrices produced by the
#' functions \code{scaleMatrix}, \code{translationMatrix}, and
#' \code{rotationMatrix} are to be left-multiplied by a row vector of
#' homogeneous coordinates; alternatively, the transpose of the result can be
#' right-multiplied by a column vector.  The generic functions \code{scale3d},
#' \code{translate3d} and \code{rotate3d} apply these transformations to the
#' \code{obj} argument.  The \code{transform3d} function is a synonym for
#' \code{rotate3d(obj, matrix = matrix)}.
#' 
#' By default, it is assumed that \code{obj} is a row vector (or a matrix of
#' row vectors) which will be multiplied on the right by the corresponding
#' matrix, but users may write methods for these generics which operate
#' differently.  Methods are supplied for \code{\link{mesh3d}} objects.
#' 
#' To compose transformations, use matrix multiplication.  The effect is to
#' apply the matrix on the left first, followed by the one on the right.
#' 
#' \code{identityMatrix} returns an identity matrix.
#' 
#' \code{scaleMatrix} scales each coordinate by the given factor.  In Euclidean
#' coordinates, \code{(u, v, w)} is transformed to \code{(x*u, y*v, z*w)}.
#' 
#' \code{translationMatrix} translates each coordinate by the given
#' translation, i.e. \code{(u, v, w)} is transformed to \code{(u + x, v + y, w
#' + z)}.
#' 
#' \code{rotationMatrix} can be called in three ways.  With arguments
#' \code{angle, x, y, z} it represents a rotation of \code{angle} radians about
#' the axis \code{x, y, z}.  If \code{matrix} is a 3x3 rotation matrix, it will
#' be converted into the corresponding matrix in 4x4 homogeneous coordinates.
#' Finally, if a 4x4 matrix is given, it will be returned unchanged. (The
#' latter behaviour is used to allow \code{transform3d} to act like a generic
#' function, even though it is not.)
#' 
#' Use \code{asHomogeneous(x)} to convert the Euclidean vector \code{x} to
#' homogeneous coordinates, and \code{asEuclidean(x)} for the reverse
#' transformation.
#' 
#' @aliases matrices identityMatrix scaleMatrix translationMatrix
#' rotationMatrix scale3d translate3d rotate3d transform3d asHomogeneous
#' asEuclidean
#' @param x,y,z,angle,matrix See details
#' @param obj An object to be transformed
#' @param ... Additional parameters to be passed to methods
#' @return \code{identityMatrix}, \code{scaleMatrix}, \code{translationMatrix},
#' and \code{rotationMatrix} produce a 4x4 matrix representing the requested
#' transformation in homogeneous coordinates.
#' 
#' \code{scale3d}, \code{translate3d} and \code{rotate3d} transform the object
#' and produce a new object of the same class.
#' @author Duncan Murdoch
#' @seealso \code{\link{par3d}} for a description of how rgl uses matrices in
#' rendering.
#' @keywords dynamic
#' @examples
#' 
#' # A 90 degree rotation about the x axis:
#' 
#' rotationMatrix(pi/2, 1, 0, 0)
#' 
#' # Find what happens when you rotate (2, 0, 0) by 45 degrees about the y axis:
#' 
#' x <- asHomogeneous(c(2, 0, 0))
#' y <- x %*% rotationMatrix(pi/4, 0, 1, 0)
#' asEuclidean(y)
#' 
#' # or more simply...
#' 
#' rotate3d(c(2, 0, 0), pi/4, 0, 1, 0)
#' 
#' 
NULL





#' 3D Mesh objects
#' 
#' 3D triangle and quadrangle mesh object creation and a collection of sample
#' objects.
#' 
#' These functions create and work with \code{mesh3d} objects, which consist of
#' a matrix of vertex coordinates together with a matrix of indices indicating
#' which vertex is part of which face.  Such objects may have triangular faces,
#' planar quadrilateral faces, or both.
#' 
#' The sample objects optionally take a matrix transformation \code{trans} as
#' an argument.  This transformation is applied to all vertices of the default
#' shape.  The default is an identity transformation.
#' 
#' The \code{"shape3d"} class is a general class for shapes that can be plotted
#' by \code{dot3d}, \code{wire3d} or \code{shade3d}.
#' 
#' The \code{"mesh3d"} class is a class of objects that form meshes: the
#' vertices are in member \code{vb}, as a 3 or 4 by \code{n} matrix.  Meshes
#' with triangular faces will contain \code{it}, a \code{3 * n} matrix giving
#' the indices of the vertices in each face.  Quad meshes will have vertex
#' indices in \code{ib}, a \code{4 * n} matrix.  Individual meshes may have
#' both types of faces.
#' 
#' The \code{meshColor} argument controls how material colours are interpreted.
#' This parameter was added in \pkg{rgl} version 0.100.1 (0.100.27 for
#' \code{dot3d}, \code{tmesh3d} and \code{qmesh3d}).  Possible values are:
#' \describe{ \item{list("\"vertices\"")}{Colours are applied by vertex, in the
#' order they appear in the \code{vb} matrix.} \item{list("\"edges\"")}{Colours
#' are applied to each edge: first to the 3 edges of each triangle in the
#' \code{it} matrix, then the 4 edges of each quad in the \code{ib} matrix.}
#' \item{list("\"faces\"")}{Colours are applied to each face: first to the
#' triangles in the \code{it} matrix, then to the quads in the \code{ib}
#' matrix.} \item{list("\"legacy\"")}{Colours are applied in the same way as in
#' \pkg{rgl} versions earlier than 0.100.1.} } Unique partial matches of these
#' values will be recognized.
#' 
#' If colours are specified but \code{meshColor} is not and
#' \code{options(rgl.meshColorWarning = TRUE)}, a warning will be given that
#' their interpretation may have changed.  In versions 0.100.1 to 0.100.26 of
#' \pkg{rgl}, the default was to give the warning; now the default is for no
#' warning.
#' 
#' Note that the \code{shade3d} function doesn't support \code{meshColor =
#' "edges"}, and \code{dot3d} and \code{wire3d} function may draw items more
#' than once (\code{dot3d} for other than \code{meshColor = "vertices"},
#' \code{wire3d} for \code{meshColor = "faces"}).  Which copy is visible
#' depends on the order of drawing and the
#' \code{\link{material3d}("depth_test")} setting.
#' 
#' @aliases shape3d mesh3d qmesh3d tmesh3d dot3d dot3d.qmesh3d dot3d.mesh3d
#' wire3d wire3d.qmesh3d wire3d.mesh3d shade3d shade3d.qmesh3d shade3d.mesh3d
#' cube3d oh3d tetrahedron3d octahedron3d icosahedron3d dodecahedron3d
#' cuboctahedron3d
#' @param x a \code{mesh3d} object (class \code{qmesh3d} or \code{tmesh3d}).
#' @param vertices 3- or 4-component vector of coordinates
#' @param indices 4-component vector of vertex indices
#' @param homogeneous logical indicating if homogeneous (four component)
#' coordinates are used.
#' @param material material properties for later rendering
#' @param normals normals at each vertex
#' @param texcoords texture coordinates at each vertex
#' @param trans transformation to apply to objects; see below for defaults
#' @param ... additional rendering parameters
#' @param override should the parameters specified here override those stored
#' in the object?
#' @param meshColor how should colours be interpreted?  See details below
#' @return \code{qmesh3d}, \code{cube3d}, \code{oh3d}, \code{tmesh3d},
#' \code{tetrahedron3d}, \code{octahedron3d}, \code{icosahedron3d} and
#' \code{dodecahedron3d} return objects of class \code{c("mesh3d", "shape3d")}.
#' The first three of these are quad meshes, the rest are triangle meshes.
#' 
#' \code{dot3d}, \code{wire3d}, and \code{shade3d} are called for their side
#' effect of drawing an object into the scene; they return an object ID (or
#' vector of IDs, for some classes) invisibly.
#' 
#' See \code{\link{rgl.primitive}} for a discussion of texture coordinates.
#' @seealso \code{\link{r3d}}, \code{\link{par3d}}, \code{\link{shapelist3d}}
#' for multiple shapes
#' @keywords dynamic
#' @examples
#' 
#' 
#'   # generate a quad mesh object
#' 
#'   vertices <- c( 
#'      -1.0, -1.0, 0, 1.0,
#'       1.0, -1.0, 0, 1.0,
#'       1.0,  1.0, 0, 1.0,
#'      -1.0,  1.0, 0, 1.0
#'   )
#'   indices <- c( 1, 2, 3, 4 )
#'   
#'   open3d()  
#'   wire3d( qmesh3d(vertices, indices) )
#'   
#'   # render 4 meshes vertically in the current view
#' 
#'   open3d()  
#'   bg3d("gray")
#'   l0 <- oh3d(tran = par3d("userMatrix"), color = "green" )
#'   shade3d( translate3d( l0, -6, 0, 0 ))
#'   l1 <- subdivision3d( l0 )
#'   shade3d( translate3d( l1 , -2, 0, 0 ), color = "red", override = FALSE )
#'   l2 <- subdivision3d( l1 )
#'   shade3d( translate3d( l2 , 2, 0, 0 ), color = "red", override = TRUE )
#'   l3 <- subdivision3d( l2 )
#'   shade3d( translate3d( l3 , 6, 0, 0 ), color = "red" )
#'   
#'   # render all of the Platonic solids
#'   open3d()
#'   shade3d( translate3d( tetrahedron3d(col = "red"), 0, 0, 0) )
#'   shade3d( translate3d( cube3d(col = "green"), 3, 0, 0) )
#'   shade3d( translate3d( octahedron3d(col = "blue"), 6, 0, 0) )
#'   shade3d( translate3d( dodecahedron3d(col = "cyan"), 9, 0, 0) )
#'   shade3d( translate3d( icosahedron3d(col = "magenta"), 12, 0, 0) )
#' 
NULL





#' Generic 3D interface
#' 
#' Generic 3D interface for 3D rendering and computational geometry.
#' 
#' R3d is a design for an interface for 3d rendering and computation without
#' dependency on a specific rendering implementation. R3d includes a collection
#' of 3D objects and geometry algorithms.  All r3d interface functions are
#' named \code{*3d}.  They represent generic functions that delegate to
#' implementation functions.
#' 
#' The interface can be grouped into 8 categories: Scene Management, Primitive
#' Shapes, High-level Shapes, Geometry Objects, Visualization, Interaction,
#' Transformation, Subdivision.
#' 
#' The rendering interface gives an abstraction to the underlying rendering
#' model. It can be grouped into four categories: \describe{ \item{Scene
#' Management:}{A 3D scene consists of shapes, lights and background
#' environment.} \item{Primitive Shapes:}{Generic primitive 3D graphics shapes
#' such as points, lines, triangles, quadrangles and texts.} \item{High-level
#' Shapes:}{Generic high-level 3D graphics shapes such as spheres, sprites and
#' terrain.} \item{Interaction:}{Generic interface to select points in 3D space
#' using the pointer device.} }
#' 
#' In this package we include an implementation of r3d using the underlying
#' \code{rgl.*} functions.
#' 
#' 3D computation is supported through the use of object structures that live
#' entirely in R.  \describe{ \item{Geometry Objects:}{Geometry and mesh
#' objects allow to define high-level geometry for computational purpose such
#' as triangle or quadrangle meshes (see \code{\link{mesh3d}}).}
#' \item{Transformation:}{Generic interface to transform 3d objects.}
#' \item{Visualization:}{Generic rendering of 3d objects such as dotted, wired
#' or shaded.} \item{Computation:}{Generic subdivision of 3d objects.} }
#' 
#' At present, the main practical differences between the r3d functions and the
#' \code{rgl.*} functions are as follows.
#' 
#' The r3d functions call \code{\link{open3d}} if there is no device open, and
#' the \code{rgl.*} functions call \code{\link{rgl.open}}. By default
#' \code{\link{open3d}} sets the initial orientation of the coordinate system
#' in 'world coordinates', i.e. a right-handed coordinate system in which the
#' x-axis increases from left to right, the y-axis increases with depth into
#' the scene, and the z-axis increases from bottom to top of the screen.
#' \code{rgl.*} functions, on the other hand, use a right-handed coordinate
#' system similar to that used in OpenGL.  The x-axis matches that of r3d, but
#' the y-axis increases from bottom to top, and the z-axis decreases with depth
#' into the scene.  Since the user can manipulate the scene, either system can
#' be rotated into the other one.
#' 
#' The r3d functions also preserve the \code{rgl.material} setting across calls
#' (except for texture elements, in the current implementation), whereas the
#' \code{rgl.*} functions leave it as set by the last call.
#' 
#' The example code below illustrates the two coordinate systems.
#' 
#' @seealso \code{\link{points3d}}, \code{\link{lines3d}},
#' \code{\link{segments3d}}, \code{\link{triangles3d}}, \code{\link{quads3d}},
#' \code{\link{text3d}}, \code{\link{spheres3d}}, \code{\link{sprites3d}},
#' \code{\link{terrain3d}}, \code{\link{select3d}}, \code{\link{dot3d}},
#' \code{\link{wire3d}}, \code{\link{shade3d}}, \code{\link{transform3d}},
#' \code{\link{rotate3d}}, \code{\link{subdivision3d}}, \code{\link{mesh3d}},
#' \code{\link{cube3d}}, \code{\link{rgl}}
#' @keywords dynamic
#' @examples
#' 
#'     
#'      x <- c(0, 1, 0, 0)
#'      y <- c(0, 0, 1, 0)
#'      z <- c(0, 0, 0, 1)
#'      labels <- c("Origin", "X", "Y", "Z")
#'      i <- c(1, 2, 1, 3, 1, 4)
#' 
#'      # rgl.* interface
#'      
#'      rgl.open()
#'      rgl.texts(x, y, z, labels)
#'      rgl.texts(1, 1, 1, "rgl.* coordinates")
#'      rgl.lines(x[i], y[i], z[i])
#' 
#'      # *3d interface
#'      
#'      open3d()
#'      text3d(x, y, z, labels)
#'      text3d(1, 1, 1, "*3d coordinates")
#'      segments3d(x[i], y[i], z[i])
#' 
NULL





#' Internal rgl functions and data
#' 
#' internal rgl functions
#' 
#' These are not to be called by the user.
#' 
#' @aliases rgl.bool rgl.numeric rgl.range rgl.vertex rgl.nvertex rgl.color
#' rgl.mcolor rgl.clamp rgl.attr rgl.enum rgl.enum.gl2ps rgl.enum.nodetype
#' rgl.enum.pixfmt rgl.enum.polymode rgl.enum.textype rgl.enum.fogtype
#' rgl.enum.primtype rgl.enum.halign rgl.enum.texmagfilter
#' rgl.enum.texminfilter rgl.selectstate rgl.setselectstate edgemap edgeindex
#' cube3d.ib cube3d.vb oh3d.ib oh3d.vb dev3d
#' @keywords internal
NULL





#' 3D visualization device system
#' 
#' 3D real-time rendering system.
#' 
#' RGL is a 3D real-time rendering system for R.  Multiple windows are managed
#' at a time. Windows may be divided into \dQuote{subscenes}, where one has the
#' current focus that receives instructions from the R command-line.  The
#' device design is oriented towards the R device metaphor. If you send scene
#' management instructions, and there's no device open, it will be opened
#' automatically.  Opened devices automatically get the current device focus.
#' The focus may be changed by using \code{\link{rgl.set}()} or
#' \code{\link{useSubscene3d}()}.
#' 
#' \pkg{rgl} provides medium to high level functions for 3D interactive
#' graphics, including functions modelled on base graphics
#' (\code{\link{plot3d}()}, etc.) as well as functions for constructing
#' geometric objects (\code{\link{cube3d}()}, etc.).  Output may be on screen
#' using OpenGL, or to various standard 3D file formats including WebGL, PLY,
#' OBJ, STL as well as 2D image formats, including PNG, Postscript, SVG, PGF.
#' 
#' The \code{\link{open3d}()} function attempts to open a new RGL window, using
#' default settings specified by the user.
#' 
#' \pkg{rgl} also includes a lower level interface which is described in the
#' \link{rgl.open} help topic.  We recommend that you avoid mixing \code{rgl.*}
#' and \code{*3d} calls.
#' 
#' See the first example below to display the ChangeLog.
#' 
#' @aliases rgl-package rgl
#' @seealso \link{r3d} for a description of the \code{*3d} interface;
#' \code{\link{par3d}} for a description of scene properties and the rendering
#' pipeline; \code{\link{rgl.useNULL}} for a description of how to use
#' \pkg{rgl} on a system with no graphics support.
#' @keywords dynamic
#' @examples
#' 
#' file.show(system.file("NEWS", package = "rgl"))
#' example(surface3d)
#' example(plot3d)
#' 
NULL





#' rgl id values
#' 
#' All objects in an \pkg{rgl} scene have a numerical id.  These ids are
#' normally stored in vectors of class \code{c("rglIds", "numeric")}, which
#' will also have class \code{"rglHighlevel"} or \code{"rglLowlevel"} depending
#' on whether a high level function like \code{\link{plot3d}} or
#' \code{\link{persp3d}}, or a low level function created the objects.
#' 
#' These functions and classes are intended to allow \pkg{rgl} scenes to be
#' automatically displayed in R Markdown documents.  However, this is not fully
#' in place yet, so explicit \code{rglwidget()} calls are still recommended.
#' 
#' Note that \emph{all} objects in the current scene will be printed by
#' default, not just the ids in \code{x}.  (The reason for this is that lights
#' are also objects; printing objects without lights would rarely make sense.)
#' 
#' @aliases lowlevel highlevel rglId rglLowlevel rglHighlevel print.rglId
#' @param ids A vector of object ids.
#' @param x An \code{"rglId"} object to print.
#' @param rglwidget Whether to create and print an rgl widget. If false,
#' nothing is printed.
#' @param ...  Other arguments which will be passed to \code{\link{rglwidget}}
#' if it is used.
#' @return Objects of class \code{"rglId"}, \code{c("rglHighlevel", "rglId",
#' "numeric")} or \code{c("rglLowlevel", "rglId", "numeric")} for \code{rglId},
#' \code{lowlevel} or \code{highlevel} respectively.
#' @author Duncan Murdoch
#' @examples
#' 
#' x <- matrix(rnorm(30), ncol = 3, dimnames = list(NULL, c("x", "y", "z")))
#' p <- plot3d(x, type = "s")
#' str(p)
#' if (interactive())
#'   print(p, rglwidget = TRUE)
#' 
NULL





#' Functions for integration of \code{\link{rglwidget}} into Shiny app.
#' 
#' These functions allow an \pkg{rgl} scene to be embedded in a Shiny app.
#' 
#' Use \code{rglwidgetOutput} or \code{playwidgetOutput} as an output object in
#' a Shiny user interface section; use \code{renderRglwidget} or
#' \code{renderPlaywidget} as the render function in the server section.
#' 
#' In a dynamic R Markdown document with \code{runtime: shiny}, you only call
#' the render function, and may optionally pass \code{width} and \code{height}
#' to the output function by putting them in a list in \code{outputArgs}.  See
#' the example below.
#' 
#' @aliases rglwidgetOutput renderRglwidget playwidgetOutput renderPlaywidget
#' @param outputId The name for the control.
#' @param width,height Width and height to display the control.
#' @param expr An R expression returning a \code{\link{rglwidget}} (for
#' \code{renderRglwidget}) or a \code{\link{playwidget}} (for
#' \code{renderPlaywidget}) as output.
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is the expression already quoted?
#' @param outputArgs A list containing arguments; see details below.
#' @return Used internally by Shiny.
#' @author Duncan Murdoch
#' @examples
#' 
#' \dontrun{
#' # This could be used in a dynamic R Markdown document.  See
#' # demo("shinyDemo") and demo("simpleShinyRgl") for Shiny apps.
#' 
#' inputPanel(
#'   sliderInput("n", label = "n", min = 10, max = 100, value = 10, step = 10)
#' )
#' 
#' renderRglwidget({
#'     n <- input$n
#'     try(rgl.close())
#'     plot3d(rnorm(n), rnorm(n), rnorm(n))
#'     rglwidget()
#'   }, outputArgs = list(width = "auto", height = "300px"))
#' 
#' }
#' 
NULL





#' Create, select or modify a subscene.
#' 
#' This creates a new subscene, or selects one by \code{id} value, or adds
#' objects to one.
#' 
#' The rgl package allows multiple windows to be open; each one corresponds to
#' a \dQuote{scene}. Within each scene there are one or more
#' \dQuote{subscenes}.  Each subscene corresponds to a rectangular region in
#' the window, and may have its own projection, transformation and behaviour in
#' response to the mouse.
#' 
#' There is always a current subscene: most graphic operations make changes
#' there, e.g. by adding an object to it.
#' 
#' The scene \dQuote{owns} objects; \code{addToSubscene3d} and
#' \code{delFromSubscene3d} put their ids into or remove them from the list
#' being displayed within a particular subscene. The \code{gc3d} function
#' deletes objects from the scene if they are not visible in any subscene,
#' unless they are protected by having their id included in \code{protect}.
#' 
#' The \code{viewport}, \code{projection} and \code{model} parameters each have
#' three possible settings: \code{c("inherit", "modify", "replace")}.
#' \code{"inherit"} means that the corresponding value from the parent subscene
#' will be used.  \code{"replace"} means that the new subscene will have its
#' own value of the value, independent of its parent.  \code{"modify"} means
#' that the child value will be applied first, and then the parent value will
#' be applied.  For viewport, this means that if the parent viewport is
#' changed, the child will maintain its relative position.  For the two
#' matrices, \code{"modify"} is unlikely to give satisfactory results, but it
#' is available for possible use.
#' 
#' The \code{mouseMode} parameter can only be one of \code{c("inherit",
#' "replace")}.  If it is \code{"inherit"}, the subscene will use the mouse
#' controls of the parent, and any change to them will affect the parent and
#' all children that inherit from it. This is the behaviour that was present
#' before \pkg{rgl} version 0.100.13.  If it is \code{"replace"}, then it will
#' receive a copy of the parent mouse controls, but modifications to them will
#' affect only this subscene, not the parent.  Note that this is orthogonal to
#' the \code{\link{par3d}("listeners")} setting: if another subscene is listed
#' as a listener, it will respond to mouse actions using the same mode as the
#' one receiving them.
#' 
#' The \code{viewport} parameter controls the rectangular region in which the
#' subscene is displayed. It is specified using \code{newviewport} (in pixels
#' relative to the whole window), or set to match the parent viewport.
#' 
#' The \code{projection} parameter controls settings corresponding to the
#' observer.  These include the field of view and the zoom; they also include
#' the position of the observer relative to the model.  The
#' \code{par3d("projMatrix")} matrix is determined by the projection.
#' 
#' The \code{model} parameter controls settings corresponding to the model.
#' Mouse rotations affect the model, as does scaling.  The
#' \code{par3d("modelMatrix")} matrix is determined by these as well as by the
#' position of the observer (since OpenGL assumes that the observer is at (0,
#' 0, 0) after the MODELVIEW transformation).  Only those parts concerning the
#' model are inherited when \code{model} specifies inheritance, the observer
#' setting is controlled by \code{projection}.
#' 
#' If \code{copyBackground} is \code{TRUE}, the background of the newly created
#' child will overwrite anything displayed in the parent subscene, regardless
#' of depth.
#' 
#' @aliases subscene3d newSubscene3d currentSubscene3d useSubscene3d
#' addToSubscene3d delFromSubscene3d gc3d
#' @param viewport,projection,model,mouseMode How should the new subscene be
#' embedded?  Possible values are \code{c("inherit", "modify", "replace")}.
#' See Details below.
#' @param parent The parent subscene (defaults to the current subscene).
#' @param copyLights,copyShapes,copyBBoxDeco,copyBackground Whether lights,
#' shapes, bounding box decorations and background should be copied to the new
#' subscene.
#' @param newviewport Optionally specify the new subscene's viewport (in
#' pixels).
#' @param ignoreExtent Whether to ignore the subscene's bounding box when
#' calculating the parent bounding box.  Defaults to \code{TRUE} if
#' \code{model} is not \code{"inherit"}.
#' @param dev Which rgl device to query for the current subscene.
#' @param subscene Which subscene to use or modify.
#' @param ids A vector of integer object ids to add to the subscene.
#' @param protect Object ids to protect from this garbage collection.
#' @return If successful, each function returns the object id of the subscene,
#' with the exception of \code{gc3d}, which returns the count of objects which
#' have been deleted, and \code{useSubscene3d}, which returns the previously
#' active subscene id.
#' @author Duncan Murdoch and Fang He.
#' @seealso \code{\link{subsceneInfo}} for information about a subscene,
#' \code{\link{mfrow3d}} and \code{\link{layout3d}} to set up multiple panes of
#' subscenes.
#' @keywords graphics
#' @examples
#' 
#' 
#' # Show the Earth with a cutout by using clipplanes in subscenes
#' 
#' lat <- matrix(seq(90, -90, len = 50)*pi/180, 50, 50, byrow = TRUE)
#' long <- matrix(seq(-180, 180, len = 50)*pi/180, 50, 50)
#' 
#' r <- 6378.1 # radius of Earth in km
#' x <- r*cos(lat)*cos(long)
#' y <- r*cos(lat)*sin(long)
#' z <- r*sin(lat)
#' 
#' open3d()
#' obj <- surface3d(x, y, z, col = "white", 
#'        texture = system.file("textures/worldsmall.png", package = "rgl"), 
#'        specular = "black", axes = FALSE, box = FALSE, xlab = "", ylab = "", zlab = "",
#'        normal_x = x, normal_y = y, normal_z = z)
#'        
#' cols <- c(rep("chocolate4", 4), rep("burlywood1", 4), "darkgoldenrod1")
#' rs <- c(6350, 5639, 4928.5, 4207, 3486, 
#'                          (3486 + 2351)/2, 2351, (2351 + 1216)/2, 1216)
#' for (i in seq_along(rs)) 
#'   obj <- c(obj, spheres3d(0, 0, col = cols[i], radius = rs[i]))
#'   
#' root <- currentSubscene3d()
#' 
#' newSubscene3d("inherit", "inherit", "inherit", copyShapes = TRUE, parent = root)
#' clipplanes3d(1, 0, 0, 0)
#' 
#' newSubscene3d("inherit", "inherit", "inherit", copyShapes = TRUE, parent = root)
#' clipplanes3d(0, 1, 0, 0)
#' 
#' newSubscene3d("inherit", "inherit", "inherit", copyShapes = TRUE, parent = root)
#' clipplanes3d(0, 0, 1, 0)
#' 
#' # Now delete the objects from the root subscene, to reveal the clipping planes
#' useSubscene3d(root)
#' delFromSubscene3d(obj)
#' 
NULL





#' tkrgl functions
#' 
#' Functions from the former \pkg{tkrgl} package.
#' 
#' The \pkg{tkrgl} package contained functions to use TCL/TK to control an
#' \pkg{rgl} scene on screen.  These functions have now been merged into
#' \pkg{rgl}, with a goal of dropping the \pkg{tkrgl} package.
#' 
#' To avoid conflicts with \pkg{rgl} names and to indicate the TCL/TK nature of
#' these functions, they have all been prefixed with \code{tk}:
#' 
#' \describe{ \item{list(list("tkpar3dsave"))}{Formerly
#' \code{tkrgl::par3dsave}, allows interactive saving of scene parameters.}
#' \item{list(list("tkspin3d"), ", ", list("tkspinControl"))}{Formerly
#' \code{tkrgl::spin3d} and \code{tkrgl::spinControl}, create buttons to spin
#' the scene.} }
#' 
#' History: \tabular{ll}{ 0.2-2 \tab First public release \cr 0.3 \tab Added
#' possibility to control multiple windows \cr 0.4 \tab Compatibility with
#' 2.0.0 tcltk package \cr 0.5 \tab Added continuous rotation \cr 0.6 \tab
#' Added par3dsave \cr 0.7 \tab Added parameters to
#' \code{\link{tkspinControl}}, fixed startup \cr 0.8 \tab Minor fixes to pass
#' checks \cr 0.9 \tab Merge functions into \pkg{rgl} \cr }
#' 
NULL





#' Obsolete functions to write HTML/Javascript code to control a WebGL display.
#' 
#' These functions write out HTML code to control WebGL displays on the same
#' page.  They are deprecated; most documentation has now been removed.
#' 
#' 
#' @aliases subsetSlider subsetSetter clipplaneSlider toggleButton
#' @param subsets A list of vectors of object identifiers; the slider or setter
#' will choose among them.
#' @param labels Labels to display corresponding to each subset.  If
#' \code{NULL}, numeric labels will be shown.
#' @param fullset Objects in the subscene which are not in \code{fullset} will
#' not be touched.
#' @param subscenes The subscenes to be controlled.
#' @param prefixes The prefixes of the WebGL scenes to be controlled.
#' @param accumulate If \code{TRUE}, the subsets will accumulate (by union) as
#' the value increases.
#' @param id The \code{id} of the input control that will be generated.
#' @param name The name of the input control that will be generated.
#' @param ... Arguments to pass to \code{\link{propertySlider}}.
#' @param a,b,c,d The parameter values to change.  Leave as \code{NULL} to hold
#' the parameter constant.
#' @param plane,clipplaneids The identifier of the particular clipplane to
#' modify.
#' @param subset The subset that the button should toggle.
#' @param label The button label.
#' @return \code{subsetSetter} returns a length-one character vector of class
#' \code{"propertySetter"}.
#' 
#' The other functions use \code{\link{cat}} to write their output and
#' invisibly return the \code{id} of the control that was generated.
#' @author Duncan Murdoch
#' @seealso \code{\link{playwidget}} and \code{\link{toggleWidget}} for a
#' newer, preferred method of inserting controls into a scene.
NULL



