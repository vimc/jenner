##' Helper for producing plots+data+metadata artefacts for orderly.
##' Only works with ggplot figures, because we use both getting the
##' data out of the figure (using \code{$data}) and use
##' \code{print(object)} to produce the plot twice.
##' @title Make figure with metadata
##'
##' @param object A ggplot2 object (must inherit from \code{ggplot})
##'
##' @param filename Destination filename to save the plot in.  The
##'   extension must match the device name (e.g.., lowercase)
##'
##' @param meta An object of metadata.  Will be saved by running
##'   through \code{yaml::as.yaml}.  Any sort of data should work, but
##'   lists are probably going to be best.
##'
##' @param ... Additional arguments passed through to the device being
##'   opened. Things like width, height, etc.
##'
##' @param data The data used to create the plot.  If not given (which
##'   is the default) the data is pulled from the ggplot object.  This
##'   is probably the right thing to do.
##'
##' @param do_plot Logical, indicating if the plot should also be
##'   printed to the null device (the screen, or whatever knitr has
##'   running).
##'
##' @export
report_figure_with_metadata <- function(object, filename, meta, ...,
                                        data = NULL, do_plot = TRUE) {
  if (!inherits(object, "ggplot")) {
    stop("Expected a ggplot object")
  }
  ext <- tools::file_ext(filename)
  base <- tools::file_path_sans_ext(filename)

  filename_data <- paste0(base, ".csv")
  filename_meta <- paste0(base, ".yml")

  device <- getExportedValue("grDevices", ext)

  device(filename, ...)
  on.exit(dev.off())
  print(object)

  write.csv(data %||% object$data, filename_data, row.names = FALSE)
  writeLines(yaml::as.yaml(meta), filename_meta)

  if (do_plot) {
    print(object)
  }

  invisible(object)
}

##' Helper function for automagically creating a named list the way
##' that cbind and data.frame do.
##' @title Create a named list
##' @param ... Things to put in the list
##' @export
report_variable_list <- function(...) {
  x <- as.list(substitute(list(...)))[-1L]
  data <- list(...)
  nms <- names(data) %||% character(length(data))
  i <- !nzchar(nms)
  if (any(i)) {
    nms[i] <- vapply(x[i], function(x) if (is.name(x)) deparse(x) else "", "")
  }
  names(data) <- nms
  data
}
