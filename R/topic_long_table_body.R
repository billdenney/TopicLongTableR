#' Generate the body of a topiclongtable
#'
#' @inheritParams topic_long_table
#' @param row_border The LaTeX command between rows
#' @param latex_clean Should cleaning occur?  Cleaning is: If a data.frame or
#'   matrix, the contents are run through
#'   \code{Hmisc::latexTranslate(format(x))} prior to generating the table body.
#' @return A character string for use in a topic_long_table body.
#' @export
#' @importFrom Hmisc latexTranslate
topic_long_table_body <- function(x, topic_cols, row_border="\\TopicLine", latex_clean=TRUE) {
  if (latex_clean) {
    if (is.data.frame(x)) {
      for (current_col in seq_len(ncol(x))) {
        x[[current_col]] <- Hmisc::latexTranslate(format(x[[current_col]]))
      }
    } else if (is.matrix(x)) {
      x <- Hmisc::latexTranslate(format(x))
    } else {
      warning(
        "Automatic LaTeX translation does not occur for class: ",
        paste(class(x), collapse=", ")
      )
    }
  }
  prep_x <-
    unname(
      append(
        lapply(
          1:topic_cols,
          function(i) sprintf("\\Topic[%s]", format(x[[i]]))
        ),
        lapply(
          seq_len(ncol(x) - topic_cols) + topic_cols,
          function(i) format(x[[i]])
        )
      )
    )
  prep_x$sep = " & "
  paste(
    row_border,
    do.call(paste, prep_x),
    "\\\\",
    collapse="\n"
  )
}
