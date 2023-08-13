#' Generate the body of a topiclongtable
#'
#' @inheritParams topic_long_table
#' @param row_border The LaTeX command between rows
#' @param latex_clean Should cleaning occur?  Cleaning is: If a data.frame or
#'   matrix, the contents are run through
#'   \code{Hmisc::latexTranslate(format(x))} prior to generating the table body.
#' @inheritParams topic_long_table_alignment
#' @return A character string for use in a topic_long_table body.
#' @export
#' @importFrom Hmisc latexTranslate
topic_long_table_body <- function(x, topic_cols, row_border="\\TopicLine", latex_clean=TRUE, verbatim=NULL) {
  if (!is.null(verbatim)) {
    if (!is.character(verbatim) || length(verbatim) != 1) {
      stop("`verbatim` must be a character scalar.")
    }
    return(verbatim)
  }
  if (is.matrix(x)) {
    # Since this is solely for the body, the easiest way to make matrices work
    # is to convert them to data.frames
    x <- as.data.frame(x)
  }
  if (!is.data.frame(x)) {
    stop("x must be a data.frame")
  }
  if (latex_clean) {
    if (is.data.frame(x)) {
      for (current_col in seq_len(ncol(x))) {
        x[[current_col]] <- Hmisc::latexTranslate(format(x[[current_col]]))
      }
    } else {
      stop( # nocov
        "Automatic LaTeX translation does not occur for class (please report a bug): ", # nocov
        paste(class(x), collapse=", ") # nocov
      ) # nocov
    }
  }
  prep_x <-
    unname(
      append(
        lapply(
          seq_len(topic_cols),
          function(i) sprintf("\\Topic[%s]", format(x[[i]]))
        ),
        lapply(
          seq_len(ncol(x) - topic_cols) + topic_cols,
          function(i) format(x[[i]])
        )
      )
    )
  prep_x$sep <- " & "
  row_text <- do.call(paste, prep_x)

  row_break <- rep("\\\\", nrow(x))
  val_next <- x[[1]][-1]
  val_prev <- x[[1]][-nrow(x)]
  mask_change1 <-
    c(
      # it changes to or from being NA
      (is.na(val_next) != is.na(val_prev)) |
        # or it is a normal value and it changes
        (val_next != val_prev),
      # The last row does not change by definition
      FALSE
    )
  # What is the level of topic change per row?
  break_level <- rep(Inf, nrow(x) - 1)
  for (col_num in seq_len(topic_cols)) {
    mask_match <-
      # It changes now
      (x[[col_num]][-1] != x[[col_num]][-nrow(x)]) &
      # It has not already been set
      (break_level > col_num)
    break_level[mask_match] <- col_num
  }

  # Define how to break (or not) between rows with the correct LaTeX command
  break_choices <-
    sprintf(
      "\\%s[%g]",
      c(rep("pagebreak", 4), rep("nopagebreak", 4)),
      c(3:0, 1:4)
    )
  row_start <- break_choices[pmin(break_level, length(break_choices))]
  # Add a blank for the last row
  row_start <- c(row_start, "")
  ret <-
    paste(
      row_border,
      row_text,
      paste0("\\\\", row_start),
      collapse="\n"
    )
  ret
}
