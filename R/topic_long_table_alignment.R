topic_long_table_align_choices <-
  list(l="l",
       left="l",
       c="c",
       center="c",
       r="r",
       right="r",
       .="S",
       decimal="S",
       S="S")

#' Set alignment for a topiclongtable
#'
#' @inheritParams topic_long_table
#' @param align The alignment to use.  If \code{NULL}, defaults are used.
#' @param left_border,inner_border,right_border The left, inner, and right
#'   borders of the table.
#' @param ... Passed to other \code{topic_long_table_alignment} methods.
#' @return A character scalar defining the alignment either for the current
#'   column (for individual classes) or the entire table (for table classes like
#'   data.frame and matrix).
#' @export
topic_long_table_alignment <- function(x, topic_cols, align=NULL, left_border="|", inner_border="|", right_border="|") {
  UseMethod("topic_long_table_alignment")
}
#' @describeIn topic_long_table_alignment For numbers, align on decimal.
#' @export
topic_long_table_alignment.numeric <- function(x, ...) "r"
#' @describeIn topic_long_table_alignment For anything not specified, left justify.
#' @export
topic_long_table_alignment.default <- function(x, ...) "l"
#' @describeIn topic_long_table_alignment For data.frames, alignment occurs by column class.
#' @export
topic_long_table_alignment.data.frame <- function(x, topic_cols, align=NULL, left_border="|", inner_border="|", right_border="|") {
  if (is.null(align)) {
    align <- sapply(as.list(x), topic_long_table_alignment)
  }
  align <- clean_align(x, align)
  topic_cols_ret <- c("F", rep("T", ncol(x) - 1))
  paste0(
    left_border,
    paste(topic_cols_ret, align, sep="", collapse=inner_border),
    right_border
  )
}
#' @describeIn topic_long_table_alignment For matrices, alignment occurs by class.
#' @export
topic_long_table_alignment.matrix <- function(x, topic_cols, align=NULL, ...) {
  if (is.null(align)) {
    align <- rep(topic_long_table_alignment(as.vector(x)), ncol(x))
  }
  align <- clean_align(x, align)
  topic_long_table_alignment.data.frame(x, topic_cols=topic_cols, align=align, ...)
}

#' Ensure that alignment is valid
#'
#' @param x A data.frame or similar object being turned into a topic_long_table
#' @param align The alignment specification
#' @return A vector of valid alignment information
#' @noRd
clean_align <- function(x, align) {
  if (length(missing_align <- setdiff(align, names(topic_long_table_align_choices)))) {
    warning(
      sprintf(
        "Unknown values for `align` (%s).  Unknown values may results in LaTeX compilation errors.  Values are expected to be one of the following: %s",
        paste(unique(missing_align), collapse=", "),
        paste(names(topic_long_table_align_choices), collapse=", ")
      )
    )
  }
  if (length(align) == 1) {
    align <- rep(align, ncol(x))
  } else if (length(align) != ncol(x)) {
    stop("align must be a scalar or vector where length(align) == ncol(x).")
  }
  align
}
