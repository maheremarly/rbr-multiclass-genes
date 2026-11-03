# Command line arguments for the three entry points in scripts/.
#
# A deliberately small parser: --flag value pairs and bare switches, with the
# defaults declared by the caller. Keeping it here means the scripts themselves
# stay short enough to read in one screen.


#' Parse --flag value pairs from the command line.
#'
#' @param defaults Named list of every recognised flag and its default. A
#'   logical default makes the flag a bare switch.
#' @param args Character vector of arguments; defaults to the real ones.
#' @return `defaults` with the supplied values substituted in.
parse_args <- function(defaults, args = commandArgs(trailingOnly = TRUE)) {
  values <- defaults
  i <- 1

  while (i <= length(args)) {
    flag <- sub("^--", "", args[i])

    if (!flag %in% names(defaults)) {
      stop(
        "Unknown argument '", args[i], "'. Recognised: ",
        paste0("--", names(defaults), collapse = " "),
        call. = FALSE
      )
    }

    if (is.logical(defaults[[flag]])) {
      values[[flag]] <- TRUE
      i <- i + 1
    } else {
      if (i == length(args)) {
        stop("Argument '", args[i], "' needs a value.", call. = FALSE)
      }
      raw <- args[i + 1]
      values[[flag]] <- if (is.numeric(defaults[[flag]])) as.numeric(raw) else raw
      i <- i + 2
    }
  }

  values
}


#' Stop unless every named argument was given a value.
require_args <- function(values, required) {
  missing <- required[vapply(values[required], is.null, logical(1))]
  if (length(missing) > 0) {
    stop(
      "Missing required argument(s): ",
      paste0("--", missing, collapse = " "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
