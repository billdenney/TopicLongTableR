#' Generate the body of a topiclongtable
#' 
#' @inheritParams topic_long_table
#' @param row_border The LaTeX command between rows
#' @return A character string for use in a topic_long_table body.
#' @export
topic_long_table_body <- function(x, topic_cols, row_border="\\TopicLine") {
  prep_x <-
    unname(
      append(
        lapply(
          1:topic_cols,
          function(i) sprintf("\\Topic[%s]", format(x[[i]]))
        ),
        lapply(
          seq_len(ncol(x) - topic_cols) + 1,
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
