subst <- function(strings, ..., digits = 7) {
  substitutions <- list(...)
  names <- names(substitutions)
  if (is.null(names)) names <- rep("", length(substitutions))
  for (i in seq_along(names)) {
    if ((n <- names[i]) == "") {
      n <- as.character(sys.call()[[i + 2]])
    }
    value <- substitutions[[i]]
    if (is.numeric(value)) {
      value <- formatC(unclass(value), digits = digits, width = 1)
    }
    strings <- gsub(paste("%", n, "%", sep = ""), value, strings)
  }
  strings
}


convertBBox <- function(id,
                        verts = rgl.attrib(id, "vertices"),
                        text = rgl.attrib(id, "text"),
                        mat = rgl.getmaterial(id = id)) {
  if (!length(text)) {
    text <- rep("", NROW(verts))
  }
  if (length(mat$color) > 1) {
    mat$color <- mat$color[2]
  } # We ignore the "box" colour

  if (any(missing <- text == "")) {
    text[missing] <- apply(verts[missing, ], 1, function(row) format(row[!is.na(row)]))
  }

  res <- integer(0)
  if (any(inds <- is.na(verts[, 2]) & is.na(verts[, 3]))) {
    res <- c(res, do.call(axis3d, c(list(edge = "x", at = verts[inds, 1], labels = text[inds]), mat)))
  }
  if (any(inds <- is.na(verts[, 1]) & is.na(verts[, 3]))) {
    res <- c(res, do.call(axis3d, c(list(edge = "y", at = verts[inds, 2], labels = text[inds]), mat)))
  }
  if (any(inds <- is.na(verts[, 1]) & is.na(verts[, 2]))) {
    res <- c(res, do.call(axis3d, c(list(edge = "z", at = verts[inds, 3], labels = text[inds]), mat)))
  }
  res <- c(res, do.call(box3d, mat))
  res
}

rootSubscene <- function() {
  id <- currentSubscene3d()
  repeat {
    info <- subsceneInfo(id)
    if (is.null(info$parent)) {
      return(id)
    } else {
      id <- info$parent
    }
  }
}



#' Write scene to HTML.
#' 
#' Obsolete function to write the current scene to a collection of files that
#' contain WebGL code to reproduce it in a browser.  Please use
#' \code{\link{rglwidget}} instead.
#' 
#' This obsolete function writes out a web page containing Javascript that
#' reconstructs the scene in WebGL.  It will be formally deprecated in an
#' upcoming release; you should use \code{\link{rglwidget}} instead in any new
#' code, and start migrating old code there.
#' 
#' The remaining documentation has been removed to discourage use of this
#' function.
#' 
#' @param dir Where to write the files.
#' @param filename The filename to use for the main file.
#' @param template The template web page to which to write the Javascript for
#' the scene.  See Details below.
#' @param prefix An optional prefix to use on global identifiers in the scene;
#' use different prefixes for different scenes displayed on the same web page.
#' If not blank, it should be a legal identifier in Javascript and HTML.
#' @param snapshot Whether to include a snapshot of the scene, to be displayed
#' in browsers that don't support WebGL, or a specification of the snapshot to
#' use.  See details below.
#' @param commonParts Whether to include parts that would be common to several
#' figures on the same page.  Currently this includes a reference to and copy
#' of the \file{CanvasMatrix.js} file in the output.
#' @param reuse When writing several figures on the same page, set this to a
#' dataframe containing values to reuse.  See the Value section below.
#' @param font The font to use for text.
#' @param width,height The (optional) width and height in pixels of the image
#' to display.  If omitted, the \code{par3d("windowRect")} dimensions will be
#' used.
#' @return The \code{filename} is returned.
#' @note If \code{commonParts} is \code{TRUE}, the output includes a binary
#' copy of the CanvasMatrix Javascript library.  Its source (including the
#' copyright notice and license for free use) is included in the file named by
#' \code{system.file("htmlwidgets/lib/CanvasMatrix.src.js", package = "rgl")}.
#' @author Duncan Murdoch.
#' @seealso \code{\link{rglwidget}} should be used instead of
#' \code{writeWebGL}.  Other functions which are related: \code{\link{scene3d}}
#' saves a copy of a scene to an R variable; \code{\link{writeASY}},
#' \code{\link{writePLY}}, \code{\link{writeOBJ}} and \code{\link{writeSTL}}
#' write the scene to a file in various other formats.
writeWebGL <- function(dir = "webGL", filename = file.path(dir, "index.html"),
                       template = system.file(file.path("WebGL", "template.html"), package = "rgl"),
                       prefix = "",
                       snapshot = TRUE, commonParts = TRUE, reuse = NULL,
                       font = "Arial",
                       width = NULL, height = NULL) {

  # Lots of utility functions and constants defined first; execution starts way down there...

  header <- function() {
    if (commonParts) {
      c(
        as.character(includeScript(system.file("htmlwidgets/lib/CanvasMatrix/CanvasMatrix.src.js", package = "rgl"))),
        as.character(includeScript(system.file("htmlwidgets/lib/rglClass/rglClass.src.js", package = "rgl")))
      )
    }
  }

  scriptheader <- function() {
    subst(
      '
<div id="%elementId%" class="rglWebGL"></div>
<script type="text/javascript">
	var %prefix%div = document.getElementById("%elementId%"),
      %prefix%rgl = new rglwidgetClass();
  %prefix%div.width = %width%;
  %prefix%div.height = %height%;
  %prefix%rgl.initialize(%prefix%div,
                         %json%);
  %prefix%rgl.prefix = "%prefix%";
</script>', prefix, elementId, json, width, height
    )
  }

  footer <- function() {
    subst(
      '
	<p id="%prefix%debug">
	You must enable Javascript to view this page properly.</p>',
      prefix
    )
  }

  #  Execution starts here!

  # Do a few checks first

  elementId <- paste0(prefix, "div")

  if (!file.exists(dir)) {
    dir.create(dir)
  }
  if (!file.info(dir)$isdir) {
    stop(gettextf("'%s' is not a directory", dir), domain = NA)
  }

  if (!is.null(template)) {
    templatelines <- readLines(template)
    templatelines <- subst(templatelines, rglVersion = packageVersion("rgl"), prefix = prefix)

    target <- paste("%", prefix, "WebGL%", sep = "")
    replace <- grep(target, templatelines, fixed = TRUE)
    if (length(replace) != 1) {
      stop(gettextf("template '%s' does not contain '%s'", template, target),
        domain = NA
      )
    }

    result <- c(templatelines[seq_len(replace - 1)], header())
  } else {
    result <- header()
  }

  scene <- convertScene(
    width = width, height = height,
    elementId = elementId, reuse = reuse,
    snapshot = snapshot
  )
  scene$crosstalk <- list(
    key = list(),
    group = character(),
    id = integer(),
    options = list()
  )
  if (is.null(width)) width <- scene$width
  if (is.null(height)) height <- scene$height

  reuse <- attr(scene, "reuse")
  json <- toJSON(I(scene),
    dataframe = "columns", null = "null", na = "string",
    auto_unbox = TRUE, digits = getOption(
      "shiny.json.digits",
      7
    ),
    use_signif = TRUE, force = TRUE, POSIXt = "ISO8601",
    UTC = TRUE, rownames = FALSE, keep_vec_names = TRUE
  )

  result <- c(
    result,
    scriptheader(),
    footer(),
    if (!is.null(template)) {
      templatelines[replace + seq_len(length(templatelines) - replace)]
    } else {
      subst("<script>%prefix%rgl.start();</script>", prefix = prefix)
    }
  )

  cat(result, file = filename, sep = "\n")
  #   if (!is.null(reuse)) {
  #     prefixes <- prefixes[!duplicated(prefixes$id),]
  #     attr(filename, "reuse") <- prefixes
  #   }
  invisible(structure(filename, reuse = reuse))
}
