# These quaternion functions are adapted from the orientlib package

# Convert an array of rotation matrices to a matrix of unit quaternions
toQuaternions <- function(x) {
  nicesqrt <- function(x) sqrt(pmax(x, 0))
  q4 <- nicesqrt((1 + x[1, 1, ] + x[2, 2, ] + x[3, 3, ]) / 4) # may go negative by rounding
  zeros <- zapsmall(q4) == 0
  q1 <- ifelse(!zeros, (x[2, 3, ] - x[3, 2, ]) / 4 / q4, nicesqrt(-(x[2, 2, ] + x[3, 3, ]) / 2))
  q2 <- ifelse(!zeros, (x[3, 1, ] - x[1, 3, ]) / 4 / q4,
    ifelse(zapsmall(q1) != 0, x[1, 2, ] / 2 / q1, nicesqrt((1 - x[3, 3, ]) / 2))
  )
  q3 <- ifelse(!zeros, (x[1, 2, ] - x[2, 1, ]) / 4 / q4,
    ifelse(zapsmall(q1) != 0, x[1, 3, ] / 2 / q1,
      ifelse(zapsmall(q2) != 0, x[2, 3, ] / 2 / q2, 1)
    )
  )
  cbind(q1, q2, q3, q4)
}

# Convert a single quaternion to a rotation matrix

toRotmatrix <- function(x) {
  x <- x / sqrt(sum(x^2))
  matrix(c(
    1 - 2 * x[2]^2 - 2 * x[3]^2,
    2 * x[1] * x[2] - 2 * x[3] * x[4],
    2 * x[1] * x[3] + 2 * x[2] * x[4],
    2 * x[1] * x[2] + 2 * x[3] * x[4],
    1 - 2 * x[1]^2 - 2 * x[3]^2,
    2 * x[2] * x[3] - 2 * x[4] * x[1],
    2 * x[1] * x[3] - 2 * x[2] * x[4],
    2 * x[2] * x[3] + 2 * x[1] * x[4],
    1 - 2 * x[1]^2 - 2 * x[2]^2
  ), 3, 3)
}



#' Interpolator for par3d parameters
#' 
#' Returns a function which interpolates \code{par3d} parameter values,
#' suitable for use in animations.
#' 
#' This function is intended to be used in constructing animations.  It
#' produces a function that returns a list suitable to pass to
#' \code{\link{par3d}}, to set the viewpoint at a given point in time.
#' 
#' All of the parameters are optional.  Only those \code{par3d} parameters that
#' are specified will be returned.
#' 
#' The input values other than \code{times} may each be specified as lists,
#' giving the parameter value settings at a fixed time, or as matrices or
#' arrays.  If not lists, the following formats should be used:
#' \code{userMatrix} can be a \code{4 x 4 x n} array, or a \code{4 x 4n}
#' matrix; \code{scale} should be an \code{n x 3} matrix; \code{zoom} and
#' \code{FOV} should be length \code{n} vectors.
#' 
#' An alternative form of input is to put all of the above arguments into a
#' list (i.e. a list of lists, or a list of arrays/matrices/vectors), and pass
#' it as the first argument.  This is the most convenient way to use this
#' function with the function \code{\link{tkpar3dsave}}.
#' 
#' Interpolation is by cubic spline or linear interpolation in an appropriate
#' coordinate-wise fashion.  Extrapolation may oscillate (repeat the sequence
#' forward, backward, forward, etc.), cycle (repeat it forward), be constant
#' (no repetition outside the specified time range), or be natural (linear on
#' an appropriate scale).  In the case of cycling, the first and last specified
#' values should be equal, or the last one will be dropped.  Natural
#' extrapolation is only supported with spline interpolation.
#' 
#' @param times Times at which values are recorded or a list; see below
#' @param userMatrix Values of \code{par3d("userMatrix")}
#' @param scale Values of \code{par3d("scale")}
#' @param zoom Values of \code{par3d("zoom")}
#' @param FOV Values of \code{par3d("FOV")}
#' @param method Method of interpolation
#' @param extrapolate How to extrapolate outside the time range
#' @param dev Which rgl device to use
#' @param subscene Which subscene to use
#' @return A function is returned.  The function takes one argument, and
#' returns a list of \code{par3d} settings interpolated to that time.
#' @note Prior to rgl version 0.95.1476, the \code{subscene} argument defaulted
#' to the current subscene, and any additional entries would be ignored by
#' \code{\link{play3d}}.  The current default value of \code{par3d("listeners",
#' dev = dev)} means that all subscenes that share mouse responses will also
#' share modifications by this function.
#' @author Duncan Murdoch
#' @seealso \code{\link{play3d}} to play the animation.
#' @keywords dplot
#' @examples
#' 
#' f <- par3dinterp( zoom = c(1, 2, 3, 1) )
#' f(0)
#' f(1)
#' f(0.5)
#' \dontrun{
#' play3d(f)
#' }
#' 
par3dinterp <- function(times = NULL, userMatrix, scale, zoom, FOV, method = c("spline", "linear"),
                        extrapolate = c("oscillate", "cycle", "constant", "natural"),
                        dev = rgl.cur(), subscene = par3d("listeners", dev = dev)) {
  force(dev)
  force(subscene)

  if (is.list(times)) {
    for (n in setdiff(names(times), "times")) assign(n, times[[n]])
    if ("times" %in% names(times)) {
      times <- times[["times"]]
    } else {
      times <- NULL
    }
  }

  if (!missing(userMatrix) && is.list(userMatrix)) userMatrix <- do.call(cbind, userMatrix)
  if (!missing(scale) && is.list(scale)) scale <- do.call(rbind, scale)
  if (!missing(zoom) && is.list(zoom)) zoom <- unlist(zoom)
  if (!missing(FOV) && is.list(FOV)) FOV <- unlist(FOV)

  if (is.null(times)) {
    times <- if (!missing(userMatrix)) {
      0:(length(userMatrix) / 16 - 1)
    } else if (!missing(scale)) {
      0:(dim(scale)[1] - 1)
    } else if (!missing(zoom)) {
      0:(length(zoom) - 1)
    } else if (!missing(FOV)) 0:(length(FOV) - 1)
  }
  data <- matrix(0, length(times), 0)
  if (!missing(userMatrix)) {
    stopifnot(length(userMatrix) == 16 * length(times))
    userMatrix <- array(userMatrix, c(4, 4, length(times)))
    xlat <- ncol(data) + 1:4
    data <- cbind(data, t(userMatrix[, 4, , drop = TRUE]))
    persp <- ncol(data) + 1:3
    data <- cbind(data, t(userMatrix[4, 1:3, , drop = TRUE]))
    rot <- ncol(data) + 1:4
    quat <- toQuaternions(userMatrix[1:3, 1:3, , drop = FALSE])
    # Since q and -q are the same rotation, we want to interpolate
    # to the nearer one.
    for (i in seq_len(nrow(quat))[-1]) {
      if (sum((quat[i - 1, ] - quat[i, ])^2) > sum((quat[i - 1, ] + quat[i, ])^2)) {
        quat[i, ] <- -quat[i, ]
      }
    }
    data <- cbind(data, quat)
  } else {
    xlat <- NULL
  }
  if (!missing(scale)) {
    stopifnot(dim(scale)[1] == length(times))
    sc <- ncol(data) + 1:3
    data <- cbind(data, log(scale))
  } else {
    sc <- NULL
  }
  if (!missing(zoom)) {
    stopifnot(length(zoom) == length(times))
    zm <- ncol(data) + 1
    data <- cbind(data, log(zoom))
  } else {
    zm <- NULL
  }
  if (!missing(FOV)) {
    stopifnot(length(FOV) == length(times))
    fov <- ncol(data) + 1
    data <- cbind(data, FOV)
  } else {
    fov <- NULL
  }

  method <- match.arg(method)
  extrapolate <- match.arg(extrapolate)
  if (extrapolate == "oscillate") {
    n <- length(times)
    times <- c(times[-n], -rev(times) + 2 * times[length(times)])
    data <- rbind(data[-n, , drop = FALSE], data[n:1, , drop = FALSE])
    n <- 2 * n - 1
    extrapolate <- "cycle"
  } else if (extrapolate == "natural" && method != "spline") {
    stop("'natural' extrapolation only supported for spline method")
  }

  if (method == "spline") {
    fns <- apply(data, 2, function(col) {
      splinefun(times, col,
        method = ifelse(extrapolate == "cycle", "periodic", "natural")
      )
    })
  } else {
    fns <- apply(data, 2, function(col) approxfun(times, col, rule = 2))
  }

  mintime <- min(times)
  maxtime <- max(times)

  function(time) {
    if (time < mintime || time > maxtime) {
      if (extrapolate == "constant" || mintime == maxtime) {
        time <- ifelse(time < mintime, mintime, maxtime)
      } else if (extrapolate == "cycle") {
        time <- (time - mintime) %% (maxtime - mintime) + mintime
      }
    }
    data <- sapply(fns, function(f) f(time))
    result <- list(dev = dev, subscene = subscene)
    if (!is.null(xlat)) {
      userMatrix <- matrix(0, 4, 4)
      userMatrix[, 4] <- data[xlat]
      userMatrix[4, 1:3] <- data[persp]
      userMatrix[1:3, 1:3] <- toRotmatrix(data[rot])
      result$userMatrix <- userMatrix
    }
    if (!is.null(sc)) {
      result$scale <- exp(data[sc])
    }
    if (!is.null(zm)) {
      result$zoom <- exp(data[zm])
    }
    if (!is.null(fov)) {
      result$FOV <- data[fov]
    }
    result
  }
}



#' Create a function to spin a scene at a fixed rate
#' 
#' This creates a function to use with \code{\link{play3d}} to spin an rgl
#' scene at a fixed rate.
#' 
#' 
#' @param axis The desired axis of rotation
#' @param rpm The rotation speed in rotations per minute
#' @param dev Which rgl device to use
#' @param subscene Which subscene to use
#' @return A function with header \code{function(time, base = M)}, where
#' \code{M} is the result of \code{par3d("userMatrix")} at the time the
#' function is created.  This function calculates and returns a list containing
#' \code{userMatrix} updated by spinning the base matrix for \code{time}
#' seconds at \code{rpm} revolutions per minute about the specified
#' \code{axis}.
#' @note Prior to rgl version 0.95.1476, the \code{subscene} argument defaulted
#' to the current subscene, and any additional entries would be ignored by
#' \code{\link{play3d}}.  The current default value of \code{par3d("listeners",
#' dev = dev)} means that all subscenes that share mouse responses will also
#' share modifications by this function.
#' @author Duncan Murdoch
#' @seealso \code{\link{play3d}} to play the animation
#' @keywords dplot
#' @examples
#' 
#' # Spin one object
#' open3d()
#' plot3d(oh3d(col = "lightblue", alpha = 0.5))
#' if (!rgl.useNULL())
#'   play3d(spin3d(axis = c(1, 0, 0), rpm = 30), duration = 2)
#' 
#' # Show spinning sprites, and rotate the whole view
#' open3d()
#' spriteid <- NULL
#' 
#' spin1 <- spin3d(rpm = 4.5 ) # the scene spinner
#' spin2 <- spin3d(rpm = 9 ) # the sprite spinner
#' 
#' f <- function(time) {
#'     par3d(skipRedraw = TRUE) # stops intermediate redraws
#'     on.exit(par3d(skipRedraw = FALSE)) # redraw at the end
#' 
#'     rgl.pop(id = spriteid) # delete the old sprite
#'     cubeid <- shade3d(cube3d(), col = "red")
#'     spriteid <<- sprites3d(0:1, 0:1, 0:1, shape = cubeid,
#'                    userMatrix = spin2(time, 
#'                      base = spin1(time)$userMatrix)$userMatrix)
#'     spin1(time)
#' }
#' if (!rgl.useNULL())
#'   play3d(f, duration = 2)
#' 
spin3d <- function(axis = c(0, 0, 1), rpm = 5, dev = rgl.cur(), subscene = par3d("listeners", dev = dev)) {
  force(axis)
  force(rpm)
  force(dev)
  force(subscene)
  M <- par3d("userMatrix", dev = dev, subscene = subscene)
  function(time, base = M) {
    list(userMatrix = rotate3d(base, time * rpm * pi / 30, axis[1], axis[2], axis[3]), dev = dev, subscene = subscene)
  }
}



#' Play animation of rgl scene
#' 
#' \code{play3d} calls a function repeatedly, passing it the elapsed time in
#' seconds, and using the result of the function to reset the viewpoint.
#' \code{movie3d} does the same, but records each frame to a file to make a
#' movie.
#' 
#' The function \code{f} will be called in a loop with the first argument being
#' the \code{startTime} plus the time in seconds since the start (where the
#' start is measured after all arguments have been evaluated).
#' 
#' \code{play3d} is likely to place a high load on the CPU; if this is a
#' problem, calls to \code{\link{Sys.sleep}} should be made within the function
#' to release time to other processes.
#' 
#' \code{play3d} will run for the specified \code{duration} (in seconds), but
#' can be interrupted by pressing \code{ESC} while the rgl window has the
#' focus.
#' 
#' \code{movie3d} saves each frame to disk in a filename of the form
#' \file{framesXXX.png}, where XXX is the frame number, starting from 0.  If
#' \code{convert} is \code{NULL} (the default) and the
#' \pkg{\link[magick]{magick}} package is installed, it will be used to convert
#' the frames to a GIF movie (or other format if supported).  If
#' \pkg{\link[magick]{magick}} is not installed or \code{convert} is
#' \code{TRUE}, \code{movie3d} will attempt to use the external
#' \command{ImageMagick} program to convert the frames to a movie.  The newer
#' \command{magick} executable is tried first, then \command{convert} if that
#' fails. The \code{type} argument will be passed to \command{ImageMagick} to
#' use as a file extension to choose the file type.
#' 
#' Finally, \code{convert} can be a template for a command to execute in the
#' standard shell (wildcards are allowed). The template is converted to a
#' command using \cr \code{\link{sprintf}(convert, fps, frames, movie, type,
#' duration, dir)} \cr For example, \code{convert = TRUE} uses the template
#' \code{"magick -delay 1x%d %s*.png %s.%s"}. All work is done in the directory
#' \code{dir}, so paths should not be needed in the command.  (Note that
#' \code{\link{sprintf}} does not require all arguments to be used, and
#' supports formats that use them in an arbitrary order.)
#' 
#' The \code{top = TRUE} default is designed to work around an OpenGL
#' limitation: in some implementations, \code{\link{rgl.snapshot}} will fail if
#' the window is not topmost.
#' 
#' As of rgl version 0.94, the \code{dev} argument is not needed: the function
#' \code{f} can specify its device, as \code{\link{spin3d}} does, for example.
#' However, if \code{dev} is specified, it will be selected as the current
#' device as each update is played.
#' 
#' As of rgl version 0.95.1476, \code{f} can include multiple values in a
#' \code{"subscene"} component, and \code{par3d()} will be called for each of
#' them.
#' 
#' @aliases play3d movie3d
#' @param f A function returning a list that may be passed to
#' \code{\link{par3d}}
#' @param duration The duration of the animation
#' @param dev Which rgl device to select
#' @param \dots Additional parameters to pass to \code{f}.
#' @param startTime Initial time at which to start the animation
#' @param fps Number of frames per second
#' @param movie The base of the output filename, not including .gif
#' @param frames The base of the name for each frame
#' @param dir A directory in which to create temporary files for each frame of
#' the movie
#' @param convert How to convert to a GIF movie; see Details
#' @param clean If \code{convert} is \code{NULL} or \code{TRUE}, whether to
#' delete the individual frames
#' @param verbose Whether to report the \code{convert} command and the output
#' filename
#' @param top Whether to call \code{\link{rgl.bringtotop}} before each frame
#' @param type What type of movie to create.  See Details.
#' @return \code{play3d} is called for the side effect of its repeated calls to
#' \code{f}. It returns \code{NULL} invisibly.
#' 
#' \code{movie3d} is also normally called for the side effect of producing the
#' output movie.  It invisibly returns
#' @author Duncan Murdoch, based on code by Michael Friendly
#' @seealso \code{\link{spin3d}} and \code{\link{par3dinterp}} return functions
#' suitable to use as \code{f}. See \code{demo(flag)} for an example that
#' modifies the scene in \code{f}.
#' @keywords dplot
#' @examples
#' 
#' open3d()
#' plot3d( cube3d(col = "green") )
#' M <- par3d("userMatrix")
#' if (!rgl.useNULL())
#'   play3d( par3dinterp(time = (0:2)*0.75, userMatrix = list(M,
#'                                      rotate3d(M, pi/2, 1, 0, 0),
#'                                      rotate3d(M, pi/2, 0, 1, 0) ) ), 
#'         duration = 3 )
#' \dontrun{
#' movie3d( spin3d(), duration = 5 )
#' }
#' 
#' 
play3d <- function(f, duration = Inf, dev = rgl.cur(), ..., startTime = 0) {
  # Don't want to start timing until args are known: they may be obtained
  # interactively
  force(f)
  force(duration)
  force(dev)
  start <- proc.time()[3] - startTime
  rgl.setselectstate("none")
  repeat {
    if (!missing(dev) && rgl.cur() != dev) rgl.set(dev)
    time <- proc.time()[3] - start
    if (time > duration || rgl.selectstate()$state == msABORT) {
      return(invisible(NULL))
    }
    stopifnot(rgl.cur() != 0)
    args <- f(time, ...)
    subs <- args$subscene
    if (is.null(subs)) {
      subs <- currentSubscene3d(dev)
    } else {
      args$subscene <- NULL
    }
    for (s in subs) {
      par3d(args, subscene = s)
    }
  }
}

movie3d <- function(f, duration, dev = rgl.cur(), ..., fps = 10,
                    movie = "movie", frames = movie, dir = tempdir(),
                    convert = NULL, clean = TRUE, verbose = TRUE,
                    top = TRUE, type = "gif", startTime = 0) {
  olddir <- setwd(dir)
  on.exit(setwd(olddir))

  for (i in round(startTime * fps):(duration * fps)) {
    time <- i / fps
    if (rgl.cur() != dev) rgl.set(dev)
    stopifnot(rgl.cur() != 0)
    args <- f(time, ...)
    subs <- args$subscene
    if (is.null(subs)) {
      subs <- currentSubscene3d(dev)
    } else {
      args$subscene <- NULL
    }
    for (s in subs) {
      par3d(args, subscene = s)
    }
    filename <- sprintf("%s%03d.png", frames, i)
    if (verbose) {
      cat(gettextf("Writing '%s'\r", filename))
      flush.console()
    }
    rgl.snapshot(filename = filename, fmt = "png", top = top)
  }
  cat("\n")
  if (.Platform$OS.type == "windows") system <- shell
  if (is.null(convert) && requireNamespace("magick")) {
    m <- NULL
    for (i in round(startTime * fps):(duration * fps)) {
      filename <- sprintf("%s%03d.png", frames, i)
      frame <- magick::image_read(filename)
      if (is.null(m)) {
        m <- frame
      } else {
        m <- c(m, frame)
      }
      if (clean) {
        unlink(filename)
      }
    }
    m <- magick::image_animate(m, fps = fps, loop = 1, dispose = "previous")
    magick::image_write(m, paste0(movie, ".", type))
    return(invisible(m))
  } else if (is.null(convert)) {
    warning("R package 'magick' is not installed; trying external package.")
    convert <- TRUE
  }
  if (is.logical(convert) && convert) {
    # Check for ImageMagick
    progname <- "magick"
    version <- try(system2(progname, "--version",
      stdout = TRUE,
      stderr = TRUE
    ), silent = TRUE)
    if (inherits(version, "try-error") || !length(grep("ImageMagick", version))) {
      progname <- "convert"
      version <- try(system2(progname, "--version",
        stdout = TRUE,
        stderr = TRUE
      ), silent = TRUE)
    }
    if (inherits(version, "try-error") || !length(grep("ImageMagick", version))) {
      stop("'ImageMagick' not found")
    }
    filename <- paste0(movie, ".", type)
    if (verbose) cat(gettextf("Will create: %s\n", file.path(dir, filename)))
    convert <- paste(progname, "-delay 1x%d %s*.png %s.%s")
  }
  if (is.character(convert)) {
    convert <- sprintf(convert, fps, frames, movie, type, duration, dir)
    if (verbose) {
      cat(gettextf("Executing: '%s'\n", convert))
      flush.console()
    }
    system(convert)
    if (clean) {
      if (verbose) {
        cat(gettext("Deleting frames\n"))
      }
      for (i in 0:(duration * fps)) {
        filename <- sprintf("%s%03d.png", frames, i)
        unlink(filename)
      }
    }
  }
  invisible(convert)
}
