#' Convert rgl userMatrix to lattice or base angles
#' 
#' These functions take a user orientation matrix from an \pkg{rgl} scene and
#' approximate the parameters to either \pkg{lattice} or base graphics
#' functions.
#' 
#' The \pkg{lattice} package can use Euler angles in the ZYX scheme to describe
#' the rotation of a scene in its \code{\link[lattice:cloud]{wireframe}} or
#' \code{\link[lattice]{cloud}} functions.  The \code{rglToLattice} function
#' computes these angles based on \code{rotm}, which defaults to the current
#' user matrix.  This allows \pkg{rgl} to be used to interactively find a
#' decent viewpoint and then reproduce it in \pkg{lattice}.
#' 
#' The base graphics \code{\link{persp}} function does not use full Euler
#' angles; it uses a viewpoint angle, and assume the z axis remains vertical.
#' The \code{rglToBase} function computes the viewpoint angle accurately if the
#' \pkg{rgl} scene is displayed with a vertical z axis, and does an
#' approximation otherwise.
#' 
#' @aliases rglToLattice rglToBase
#' @param rotm A matrix in homogeneous coordinates to convert.
#' @return \code{rglToLattice} returns a list suitable to be used as the
#' \code{screen} argument to \code{\link[lattice:cloud]{wireframe}}.
#' 
#' \code{rglToBase} returns a list containing \code{theta} and \code{phi}
#' components which can be used as corresponding arguments in
#' \code{\link{persp}}.
#' @author Duncan Murdoch
#' @examples
#' 
#' if (requireNamespace("orientlib")) {  
#'   persp3d(volcano, col = "green")
#'   if (requireNamespace("lattice")) 
#'     lattice::wireframe(volcano, screen = rglToLattice())
#'   angles <- rglToBase()
#'   persp(volcano, col = "green", border = NA, shade = 0.5,
#'         theta = angles$theta, phi = angles$phi)
#' }
#' 
rglToLattice <- function(rotm = par3d("userMatrix")) {
  if (!requireNamespace("orientlib", quietly = TRUE)) {
    stop("The orientlib package is needed for this function")
  }
  e <- -orientlib::eulerzyx(orientlib::rotmatrix(rotm[1:3, 1:3]))@x * 180 / pi
  list(z = e[1], y = e[2], x = e[3])
}

rglToBase <- function(rotm = par3d("userMatrix")) {
  if (!requireNamespace("orientlib", quietly = TRUE)) {
    stop("The orientlib package is needed for this function")
  }
  e <- (orientlib::eulerzyx(orientlib::rotmatrix((rotm[1:3, 1:3]))))@x * 180 / pi
  list(theta = e[1], phi = 90 - e[3])
}
