topic_long_table_vpos_choices <-
  list(b="b",
       bottom="b",
       c="c",
       center="c",
       t="t",
       top="t")

#' Set values that control marking of continued cells and positioning within
#' continued cells.
#'
#' @param continuation_code LaTeX text to add after a continued cell value.
#' @param vpos Vertical position to use within topic-combined cells (valid
#'   values are given by providing an invalid value like "")
#' @param reset Provide code to reset to defaults
#' @return A character scalar or vector of settings
#' @export
topic_long_table_set <- function(continuation_code=NULL, vpos=NULL, reset=FALSE) {
  if (reset) {
    return(topic_long_table_set(continuation_code="", vpos="t"))
  }
  if (all(c(is.null(continuation_code), is.null(vpos)))) {
    stop("At least one of continuation_code or vpos must be set, or reset must be TRUE.")
  }
  ret <- c()
  if (!is.null(vpos)) {
    vpos_value <- topic_long_table_vpos_choices[[vpos]]
    if (is.null(vpos_value)) {
      stop("Invalid selection for `vpos`.  `vpos` must be one of : ",
           paste(names(topic_long_table_vpos_choices), collapse=", "))
    }
    ret <- c(ret, sprintf("\\TopicSetVPos(%s)", vpos_value))
  }
  if (!is.null(continuation_code)) {
    if (length(continuation_code) != 1) {
      stop("`continuation_code` must be a scalar.")
    }
    if (!is.character(continuation_code)) {
      continuation_code <- as.character(continuation_code)
      warning("`continuation_code` was converted to a character.")
    }
    ret <- c(ret, sprintf("\\TopicSetContinuationCode{%s}", continuation_code))
  }
  paste(ret, collapse="\n")
}
