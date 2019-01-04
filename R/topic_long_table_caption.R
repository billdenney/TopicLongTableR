#' Generate a LaTeX table caption
#'
#' @inheritParams topic_long_table
#' @param text Text to place in the caption.  If \code{NULL}, the caption
#'   attribute for \code{x} will be used, if it exists.  If neither is provided,
#'   the caption will be an empty string.
#' @param label The label for the caption
#' @return The caption for inclusion in a table or an empty string if no caption
#'   is provided.
#' @export
#' @importFrom Hmisc latexTranslate
topic_long_table_caption <- function(x, text=NULL, label=NULL) {
  #\caption{My caption for this table\label{foo}}\\
  if (is.null(text)) {
    text <- attr(x, "caption", exact=TRUE)
  }
  if (is.null(text)) {
    ""
  } else {
    label_text <-
      if (is.null(label)) {
        ""
      } else {
        sprintf("\\label{%s}", Hmisc::latexTranslate(label))
      }
    sprintf("\\caption{%s%s} \\\\", Hmisc::latexTranslate(text), label_text)
  }
}
