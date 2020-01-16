#' Generate a LaTeX table caption
#'
#' @inheritParams topic_long_table
#' @inheritParams topic_long_table_body
#' @param text Text to place in the caption.  See Details.
#' @param label The label for the caption
#' @param combine_short_long Should the short and long captions be combined for
#'   the long caption?  See Details.
#' @inheritParams topic_long_table_alignment
#' @return The caption for inclusion in a table or an empty string if no caption
#'   is provided.
#'
#' @details For \code{text}, if \code{NULL}, the caption attribute for \code{x}
#'   will be used, if it exists.  If neither is provided, the caption will be an
#'   empty string (i.e. no caption).  If the length of the character vector for
#'   the caption text (as defined earlier in this paragraph) is 1, the caption
#'   will be used as-is; if the length is 2 and \code{combine_short_long ==
#'   TRUE}, then the first element will be used as the short caption and the
#'   first and second will be pasted together for the long caption.
#'
#' @export
#' @importFrom Hmisc latexTranslate
topic_long_table_caption <- function(x, text=NULL, label=NULL, combine_short_long=TRUE, latex_clean=TRUE, verbatim=NULL) {
  if (!is.null(verbatim)) {
    if (!is.character(verbatim) || length(verbatim) != 1) {
      stop("`verbatim` must be a character scalar.")
    }
    return(verbatim)
  }
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
    text_clean <-
      if (latex_clean) {
        Hmisc::latexTranslate(text)
      } else {
        text
      }
    if (length(text) == 1) {
      sprintf("\\caption{%s%s} \\\\", text_clean, label_text)
    } else if ((length(text) == 2) & combine_short_long) {
      sprintf(
        "\\caption[%s]{%s%s} \\\\",
        text_clean[1],
        paste(text_clean, collapse=" "),
        label_text
      )
    } else if ((length(text) == 2)) {
      sprintf(
        "\\caption[%s]{%s%s} \\\\", text_clean[1], text_clean[2], label_text
      )
    } else {
      stop("Cannot handle caption text with length: ", length(text))
    }
  }
}
