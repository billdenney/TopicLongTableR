#' Assist the user to know which packages to load.
#' @export
topic_long_table_document_headers <- function() {
  pkg_list <- c("sciunitx", "topiclongtable")
  cat(
    paste0("header-includes:\n",
           paste0(sprintf(" - \\usepackage{%s}\n", pkg_list),
                  collapse="")
    )
  )
}

#' The eponymous wrapper for generating a topic long table.
#'
#' @param x The object to make into a topiclongtable (typically a data.frame or
#'   matrix)
#' @param topic_cols Number of columns which are topics (where rowspans may be
#'   used).
#' @param settings,settings_after Text to add before/after the table, typically
#'   the output of \code{\link{topic_long_table_set}} and
#'   \code{topic_long_table_set(reset=TRUE)}
#' @param align_args,caption_args,header_args,footer_args,body_args A list of
#'   arguments to pass to \code{\link{topic_long_table_alignment}},
#'   \code{\link{topic_long_table_caption}},
#'   \code{\link{topic_long_table_header}},
#'   \code{\link{topic_long_table_footer}}, \code{\link{topic_long_table_body}}
#' @return A character scalar with class "topic_long_table" defining the LaTeX
#'   table (to be put into a LaTeX document)
#' @export
topic_long_table <- function(x, topic_cols,
                             settings=c(),
                             settings_after=c(),
                             align_args=list(),
                             caption_args=list(),
                             header_args=list(),
                             footer_args=list(),
                             body_args=list()) {
  align <- do.call(what=topic_long_table_alignment,
                   args=append(list(x=x, topic_cols=topic_cols), align_args))
  caption <- do.call(what=topic_long_table_caption, args=append(list(x=x), caption_args))
  header <- do.call(what=topic_long_table_header, args=append(list(x=x), header_args))
  footer <- do.call(what=topic_long_table_footer, args=append(list(x=x), footer_args))
  body <- do.call(what=topic_long_table_body,
                  args=append(list(x=x, topic_cols=topic_cols), body_args))
  structure(
    paste(c(
      settings,
      sprintf("\\begin{topiclongtable}{%s}\n", align),
      caption,
      header,
      footer,
      body,
      "\\end{topiclongtable}",
      settings_after),
      collapse="\n"
    ),
    class="topic_long_table"
  )
}

# Output for knitr
#' @importFrom knitr knit_print
#' @export
knit_print.topic_long_table <- function(x, ...) {
  if (!requireNamespace("knitr")) {
    stop("knitr is required to knit_print")
  }
  knitr::asis_output(x, cacheable=TRUE)
}
