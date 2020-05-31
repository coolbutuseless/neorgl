#' Create a spin control in a TCL/TK window
#' 
#' This function may be used to embed a spin control in a TCL/TK window.
#' 
#' 
#' @param base The TCL/TK frame in which to insert this control.
#' @param dev A vector of one or more rgl device numbers to control.
#' @param continue Initial setting for continuous rotation checkbox.
#' @param speed Initial setting for speed slider.
#' @param scale Initial setting for scale slider.
#' @param ... Additional parameters to pass to
#' \code{\link[tcltk:TkWidgets]{tkframe}}
#' @author Ming Chen and Duncan Murdoch
#' @seealso \code{\link{spin3d}}
#' @examples
#' 
#' if (interactive()) { 
#'     library(tcltk)
#'     open3d()
#'     win1 <- rgl.cur()
#'     plot3d(rexp(100), rexp(100), rexp(100), size=3, col='green')
#'     
#'     open3d()
#'     win2 <- rgl.cur()
#'     plot3d(rt(100,2), rt(100,2), rt(100, 2), size=3, col='yellow')
#'     
#'     open3d()
#'     win3 <- rgl.cur()
#'     plot3d(rexp(100), rexp(100), rexp(100), size=3, col='red')
#'         
#'     open3d()
#'     win4 <- rgl.cur()
#'     plot3d(rbinom(100,10,0.5), rbinom(100,10,0.5), rbinom(100,10,0.5), size=3, col='cyan')
#'     
#'     base <- tktoplevel()
#'     tkwm.title(base, "Spinners")
#'     con1 <- spinControl(base, dev=c(win1,win2))
#'     con2 <- spinControl(base, dev=c(win3,win4))
#'     tkpack(con1, con2)
#' }
#' 
tkspinControl <- function(base, dev = rgl.cur(), continue = FALSE,
                          speed = 30, scale = 100, ...) {
  if (!requireNamespace("tcltk")) {
    stop("This function requires 'tcltk'.")
  }

  slider <- tcltk::tclVar(speed)
  getZooms <- function() {
    old <- rgl.cur()
    on.exit(rgl.set(old))
    result <- numeric(max(dev))
    for (device in dev) {
      rgl.set(device)
      result[device] <- par3d("zoom")
    }
    result
  }
  zooms <- getZooms()

  scale <- tcltk::tclVar(scale)
  continuous <- tcltk::tclVar(as.numeric(continue))

  buttonPress <- NULL
  direction <- NULL

  lastTime <- proc.time()[3]
  timeDiff <- 0

  rotateUp <- function() {
    angle <- timeDiff * as.numeric(tcltk::tclObj(slider)) * pi / 180
    par3d(userMatrix = rotationMatrix(-angle, 1, 0, 0) %*% par3d("userMatrix"))
  }

  rotateLeft <- function() {
    angle <- timeDiff * as.numeric(tcltk::tclObj(slider)) * pi / 180
    par3d(userMatrix = rotationMatrix(-angle, 0, 1, 0) %*% par3d("userMatrix"))
  }

  rotateRight <- function() {
    angle <- timeDiff * as.numeric(tcltk::tclObj(slider)) * pi / 180
    par3d(userMatrix = rotationMatrix(angle, 0, 1, 0) %*% par3d("userMatrix"))
  }

  rotateSpin <- function() {
    angle <- timeDiff * as.numeric(tcltk::tclObj(slider)) * pi / 180
    par3d(userMatrix = rotationMatrix(-angle, 0, 0, 1) %*% par3d("userMatrix"))
  }

  rotateDown <- function() {
    angle <- timeDiff * as.numeric(tcltk::tclObj(slider)) * pi / 180
    par3d(userMatrix = rotationMatrix(angle, 1, 0, 0) %*% par3d("userMatrix"))
  }

  rotate <- function() {
    old <- rgl.cur()
    on.exit(rgl.set(old))
    if (buttonPress) {
      if ((currentTime <- proc.time()[3]) > lastTime) {
        timeDiff <<- currentTime - lastTime
        lastTime <<- currentTime
        for (device in dev) {
          rgl.set(device)
          if (direction == "up") {
            rotateUp()
          }
          if (direction == "left") {
            rotateLeft()
          }
          if (direction == "spin") {
            rotateSpin()
          }
          if (direction == "right") {
            rotateRight()
          }
          if (direction == "down") {
            rotateDown()
          }
        }
      }
      tcltk::tcl("after", 5, rotate)
    }
  }

  # rotation button callback functions
  # note that "..." argument is necessary
  upButtonPress <- function(...) {
    buttonPress <<- TRUE
    lastTime <<- proc.time()[3]
    direction <<- "up"
    rotate()
  }

  leftButtonPress <- function(...) {
    buttonPress <<- TRUE
    lastTime <<- proc.time()[3]
    direction <<- "left"
    rotate()
  }

  spinButtonPress <- function(...) {
    buttonPress <<- TRUE
    lastTime <<- proc.time()[3]
    direction <<- "spin"
    rotate()
  }

  rightButtonPress <- function(...) {
    buttonPress <<- TRUE
    lastTime <<- proc.time()[3]
    direction <<- "right"
    rotate()
  }

  downButtonPress <- function(...) {
    buttonPress <<- TRUE
    lastTime <<- proc.time()[3]
    direction <<- "down"
    rotate()
  }

  onIdle <- function(...) {
    buttonPress <<- TRUE
    rotate()
    buttonPress <<- FALSE
    if (as.numeric(tcltk::tclObj(continuous))) {
      tcltk::tcl("after", "idle", onIdle)
    }
  }

  buttonRelease <- function(...) {
    buttonPress <<- FALSE
    if (as.numeric(tcltk::tclObj(continuous))) {
      tcltk::tcl("after", "idle", onIdle)
    }
  }


  resetAxis <- function(...) {
    old <- rgl.cur()
    on.exit(rgl.set(old))
    for (device in dev) {
      rgl.set(device)
      par3d(userMatrix = diag(4))
    }
  }

  setScale <- function(...) {
    old <- rgl.cur()
    on.exit(rgl.set(old))
    scale <- as.numeric(tcltk::tclObj(scale))
    for (device in dev) {
      rgl.set(device)
      par3d(zoom = 10^((scale - 100) / 50) * zooms[device])
    }
  }

  spec.frm <- tcltk::tkframe(base, ...)
  first.frm <- tcltk::tkframe(spec.frm)
  second.frm <- tcltk::tkframe(spec.frm)
  third.frm <- tcltk::tkframe(spec.frm)
  fourth.frm <- tcltk::tkframe(spec.frm)

  # rotations buttons
  upButton <- tcltk::tkbutton(first.frm, text = "  ^  ")
  leftButton <- tcltk::tkbutton(first.frm, text = "  <  ")
  spinButton <- tcltk::tkbutton(first.frm, text = "  O  ")
  rightButton <- tcltk::tkbutton(first.frm, text = "  >  ")
  downButton <- tcltk::tkbutton(first.frm, text = "  v  ")


  tcltk::tkgrid(tcltk::tklabel(first.frm, text = " "), upButton, tcltk::tklabel(first.frm, text = " "))
  tcltk::tkgrid(leftButton, spinButton, rightButton)
  tcltk::tkgrid(tcltk::tklabel(first.frm, text = " "), downButton, tcltk::tklabel(first.frm, text = " "))

  tcltk::tkbind(upButton, "<Button-1>", upButtonPress)
  tcltk::tkbind(leftButton, "<Button-1>", leftButtonPress)
  tcltk::tkbind(spinButton, "<Button-1>", spinButtonPress)
  tcltk::tkbind(rightButton, "<Button-1>", rightButtonPress)
  tcltk::tkbind(downButton, "<Button-1>", downButtonPress)

  tcltk::tkbind(upButton, "<ButtonRelease-1>", buttonRelease)
  tcltk::tkbind(leftButton, "<ButtonRelease-1>", buttonRelease)
  tcltk::tkbind(spinButton, "<ButtonRelease-1>", buttonRelease)
  tcltk::tkbind(rightButton, "<ButtonRelease-1>", buttonRelease)
  tcltk::tkbind(downButton, "<ButtonRelease-1>", buttonRelease)

  # control buttons
  frameAxis <- tcltk::tkframe(second.frm, borderwidth = 2)
  tcltk::tkpack(frameAxis)

  buttonAxis <- tcltk::tkbutton(frameAxis, text = "     Reset Axis     ", command = resetAxis)
  contBox <- tcltk::tkcheckbutton(frameAxis, text = "Continue rotating", variable = continuous)

  tcltk::tkpack(contBox, buttonAxis, fill = "x")

  # control scale frame
  frameControl <- tcltk::tkframe(third.frm, borderwidth = 4)
  tcltk::tkpack(frameControl)
  frameControlSpeed <- tcltk::tkframe(frameControl)
  sliderSpeed <- tcltk::tkscale(frameControlSpeed,
    showvalue = FALSE,
    orient = "horiz", from = 0, to = 400, resolution = 1, variable = slider
  )
  tcltk::tkpack(tcltk::tklabel(frameControlSpeed, text = "Speed:"), sliderSpeed, side = "left")

  frameControlScale <- tcltk::tkframe(frameControl)
  sliderScale <- tcltk::tkscale(frameControlScale,
    showvalue = FALSE, orient = "horiz",
    from = 0, to = 200, resolution = 1, variable = scale, command = setScale
  )
  tcltk::tkpack(tcltk::tklabel(frameControlScale, text = "Scale:"), sliderScale, side = "left")
  tcltk::tkpack(frameControlSpeed, frameControlScale)


  tcltk::tkpack(first.frm, second.frm, third.frm, fourth.frm)
  tcltk::tkpack(spec.frm, expand = TRUE)

  spec.frm
}




#' Create TCL/TK controller for rgl window
#' 
#' This function creates a TCL/TK window containing buttons to spin and resize
#' one or more rgl windows.
#' 
#' 
#' @param dev A vector of one or more rgl device numbers to control
#' @param ... Named parameters in that match named formal arguments to
#' \code{\link{tkspinControl}} are passed there, while others are passed to
#' \code{\link[tcltk:TkWidgets]{tktoplevel}}
#' @author Ming Chen and Duncan Murdoch
#' @seealso \code{\link{tkspinControl}}
#' @examples
#' 
#' if (interactive()) {
#'     open3d()
#'     points3d(rnorm(100), rnorm(100), rnorm(100), size=3)
#'     axes3d()
#'     box3d()
#'     tkspin3d()
#' }
#' 
tkspin3d <- function(dev = rgl.cur(), ...) {
  if (!requireNamespace("tcltk")) {
    stop("This function requires 'tcltk'.")
  }

  args <- list(...)
  controlargs <- list()
  if (length(args)) {
    controlargindices <- which(names(args) %in% names(formals(tkspinControl)))
    if (length(controlargindices)) {
      args <- args[-controlargindices]
      controlargs <- args[controlargindices]
    }
  }

  base <- do.call(tcltk::tktoplevel, args)
  tcltk::tkwm.title(base, "Spin")

  spin <- do.call(tkspinControl, c(list(base = base, dev = dev), controlargs))

  quit <- tcltk::tkbutton(base, text = "Quit", command = function() tcltk::tkdestroy(base))

  tcltk::tkpack(spin, quit)
}
