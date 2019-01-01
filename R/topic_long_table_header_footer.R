#' Generate the table header or footer
#'
#' @param x The object to generate the header for.
#' @param col_names The column names to use for the table header (defaults to
#'   \code{Hmisc::latexTranslate(colnames(x))} if \code{NULL}).  This must have
#'   the same length as \code{ncol(x)}.
#' @param above_col_names,below_col_names LaTeX text to place above/below the
#'   col_names text (typically this will be a horizontal line with "\\hline").
#' @param subsequent_page_notification LaTeX text to place above the subsequent
#'   page headers
#' @param latex_header A fully-defined LaTeX header to use in place of the
#'   auto-generated header.  (If a character scalar, it is returned as-is; if
#'   character a vector, it is joined with " \\\\\\n" separating the elements.)
#' @return A character string of table header for a topic_long_table (a single
#'   string, not a vector).
#' @export
topic_long_table_header <- function(x,
                                    col_names=NULL,
                                    above_col_names="\\hline", below_col_names="\\hline",
                                    subsequent_page_notification="\\ldots continued",
                                    latex_header=NULL) {
  if (!is.null(latex_header)) {
    ignored_args <-
      c(
        "col_names"[!is.null(col_names)],
        "above_col_names"[!is.null(above_col_names)],
        "below_col_names"[!is.null(below_col_names)],
        "subsequent_page_notification"[!is.null(subsequent_page_notification)]
      )
    if (length(ignored_args)) {
      warning(
        paste0("`", ignored_args, "`", collapse=", "),
        ngettext(length(ignored_args), " was", " were"),
        " provided but will be ignored in favor of `latex_header`."
      )
    }
    if (length(latex_header) == 1) {
      latex_header
    } else {
      stop("latex_header must have length == 1 (for no header, use an empty string, '').")
    }
  } else {
    if (is.null(col_names)) {
      col_names <- Hmisc::latexTranslate(colnames(x))
    } else if (length(col_names) != ncol(x)) {
      stop("`col_names` must have the same length as `ncol(x)`.")
    }
    col_names_part <-
      if (is.null(col_names)) {
        # This will typically happen for matrices without column names.
        if (above_col_names == below_col_names) {
          above_col_names
        } else {
          paste(c(above_col_names, below_col_names), collapse=" \\\\\n")
        }
      } else {
        paste0(
          paste(above_col_names, paste(col_names, collapse=" & "), "\\\\\n"),
          below_col_names
        )
      }
    all_headers <-
      if (is.null(subsequent_page_notification)) {
        c(col_names_part, "\\endhead")
      } else {
        c(
          col_names_part, "\\endfirsthead",
          # show something like "... continued" at the top of subsequent pages
          paste(paste0("\\multicolumn{", ncol(x), "}{@{}l}{", subsequent_page_notification, "}"), "\\\\"),
          col_names_part, "\\endhead"
        )
      }
    paste(all_headers, collapse="\n")
  }
}

#' @describeIn topic_long_table_header Generate the footer.
#'
#' @param bottom_border The border below the tabular text on all pages.
#' @param bottom_all_pages,bottom_last_page LaTeX text below the bottom border
#'   on all (including the last) or only the last page, respectively.
#' @export
topic_long_table_footer <- function(x,
                                    bottom_border="\\hline",
                                    bottom_all_pages=NULL,
                                    bottom_last_page=NULL,
                                    subsequent_page_notification="continued \\ldots") {
  # "continued" footer method from https://tex.stackexchange.com/questions/11380/
  paste(
    c(
      bottom_border,
      bottom_all_pages,
      paste0("\\multicolumn{", ncol(x), "}{r@{}}{", subsequent_page_notification, "} \\\\")[!is.null(subsequent_page_notification)],
      "\\endfoot",
      bottom_border,
      bottom_all_pages,
      bottom_last_page,
      "\\endlastfoot"
    ),
    collapse="\n"
  )
}
