##
## R source file
## This file is part of rgl
##
##

##
## ===[ SECTION: package entry/exit point ]===================================
##

##
## entry-point
##
##

.onLoad <- function(lib, pkg) {
  # OS-specific
  initValue <- 0

  dynlib <- "rgl"

  onlyNULL <- rgl.useNULL()
  unixos <- "none"

  if (.Platform$OS.type == "unix") {
    unixos <- system("uname", intern = TRUE)
    if (!length(unixos)) {
      unixos <- "unknown"
    }
    if (unixos == "Darwin") {

      # For MacOS X we have to remove /usr/X11R6/lib from the DYLD_LIBRARY_PATH
      # because it would override Apple's OpenGL framework
      Sys.setenv("DYLD_LIBRARY_PATH" = gsub("/usr/X11R6/lib", "", Sys.getenv("DYLD_LIBRARY_PATH")))
      X11 <- nchar(Sys.getenv("DISPLAY", "")) > 0 || nchar(Sys.which("Xorg")) > 0
      if (!X11) {
        stop("X11 not found; XQuartz (from www.xquartz.org) is required to run rgl.",
          call. = FALSE
        )
      }
    }
  }
  dll <- try(library.dynam(dynlib, pkg, lib))
  if (inherits(dll, "try-error")) {
    stop(paste(
      "\tLoading rgl's DLL failed.",
      if (unixos == "Darwin") {
        "\n\tOn MacOS, rgl depends on XQuartz, which you can download from xquartz.org."
      }
    ),
    call. = FALSE
    )
  }

  routines <- getDLLRegisteredRoutines(dynlib, addNames = FALSE)
  ns <- asNamespace(pkg)
  for (i in 1:4) {
    lapply(
      routines[[i]],
      function(sym) assign(sym$name, sym, envir = ns)
    )
  }

  if (.Platform$OS.type == "windows" && !onlyNULL) {
    frame <- getWindowsHandle("Frame")
    ## getWindowsHandle was numeric pre-2.6.0
    if (!is.null(frame)) initValue <- getWindowsHandle("Console")
  }

  if (onlyNULL) {
    rglFonts(serif = rep("serif", 4), sans = rep("sans", 4), mono = rep("mono", 4), symbol = rep("symbol", 4))
  } else {
    rglFonts(
      serif = rep(system.file("fonts/FreeSerif.ttf", package = "rgl"), 4),
      sans = rep(system.file("fonts/FreeSans.ttf", package = "rgl"), 4),
      mono = rep(system.file("fonts/FreeMono.ttf", package = "rgl"), 4),
      symbol = rep(system.file("fonts/FreeSerif.ttf", package = "rgl"), 4)
    )
  }

  .rglEnv$subsceneList <- NULL

  ret <- rgl.init(initValue, onlyNULL)

  if (!ret) {
    warning("'rgl.init' failed, running with 'rgl.useNULL = TRUE'.", call. = FALSE)
    options(rgl.useNULL = TRUE)
    rgl.init(initValue, TRUE)
  }

  registerInputHandler("shinyPar3d", convertShinyPar3d)
}



#' Initializing rgl
#' 
#' Initializing the \pkg{rgl} system.
#' 
#' If \code{useNULL} is \code{TRUE}, \pkg{rgl} will use a \dQuote{null} device.
#' This device records objects as they are plotted, but displays nothing. It is
#' intended for use with \code{\link{rglwidget}} and similar functions.
#' 
#' Currently \code{debug} only controls messages printed by the OpenGL library
#' during initialization.  In future \code{debug = TRUE} may become more
#' verbose.
#' 
#' \pkg{rgl} requires the OpenGL system to be installed and available in order
#' to display images on screen.  If there is a problem initializing it, you may
#' see the message \verb{'rgl.init' failed, running with 'rgl.useNULL = TRUE'.}
#' There are several causes and remedies: \itemize{ \itemOn any system, the
#' OpenGL libraries need to be present for \pkg{rgl} to be able to start.
#' \itemize{ \itemOn MacOS, you need to install Xquartz.  It is available from
#' \url{https://www.xquartz.org}. \itemOn Linux, you need to install Mesa 3D.
#' One of these commands may work, depending on your system: \verb{ zypper
#' source-install --build-deps-only Mesa # openSUSE/SLED/SLES yum-builddep mesa
#' # yum Fedora, OpenSuse(?)  dnf builddep mesa # dnf Fedora apt-get build-dep
#' mesa # Debian, Ubuntu and related }
#' 
#' \itemWindows should have OpenGL installed by default. } \itemOn Unix-alike
#' systems (MacOS and Linux, for example), \pkg{rgl} uses the GLX system for
#' creating displays.  If the graphic is created on a remote machine, it may
#' need to use \dQuote{Indirect GLX} (IGLX).  Due to security concerns, this is
#' often disabled by default.  See
#' \url{https://www.x.org/wiki/Development/Security/Advisory-2014-12-09/} for a
#' discussion of the security issues, and
#' \url{https://www.visitusers.org/index.php?title=Re-enabling_INdirect_glx_on_your_X_server}
#' for ways to re-enable IGLX. \itemThe \url{https://www.virtualgl.org} project
#' is intended to be a way to avoid IGLX, by rendering remotely and sending
#' bitmaps to the local machine.  It's not a simple install... \itemIf you
#' don't need to see \pkg{rgl} displays on screen, you can use the \dQuote{NULL
#' device}.  See \code{\link{rgl.useNULL}}. }
#' 
#' @param initValue value for internal use only
#' @param onlyNULL only initialize the null (no display) device
#' @param debug enable some debugging messages
#' @return Normally the user doesn't call \code{rgl.init} at all: it is called
#' when the package is loaded.  It returns no useful value.
rgl.init <- function(initValue = 0, onlyNULL = FALSE, debug = getOption("rgl.debug", FALSE)) {
  .Call(
    rgl_init,
    initValue, onlyNULL, environment(rgl.init), debug
  )
}

##
## exit-point
##
##

.onUnload <- function(libpath) {
  # shutdown

  ret <- .C(rgl_quit, success = FALSE)
}
