# TODO
# - [ ] Refactor multi-line feedback into single function
# - Improve object inspection performance
# - Add `out()` and `in()` to complete streams functionality
# - Move ? into stdin

# FIXME
#

#' Access the standard error stream
#'
#' This function provides an alternative to the verb-based API, by mapping the
#' set of prefixes to the corresponding verbs.
#'
#' @param ... Character vectors supporting **glue** strings and **cli** inline
#' styles. This should start with a supported prefix, to map the action to one
#' of the {rui} verbs. Otherwise, `rui::tell()` is used. Supported prefixes are
#' # (entitle), i (inform), v (approve), x (disapprove), ~ (begin/proceed), =
#' (give), * (suggest), ? (ask), ! (alert), w (warn), e (stop), . (display), and
#' $ (expose). Status bars can be resolved by providing single characters "c",
#' "v", or "x" for clear, succeed and fail respectively.
#' @param object Object to print the {rui} way through `rui::inspect()`.
#' @export
err <- function(..., .envir = parent.frame()) {
  txt <- paste(...)
  if (length(txt) == 0) {
    tell("")
    return(invisible())
  }
  type <- substr(txt, 1, 1)
  types <- c("i", "v", "x", "~", "*", "?", "=", "!", "#", ".", "c", "$", "e", "w")
  space <- substr(txt, 2, 2)
  if (!(type %in% types & (space %in% c(" ", "")))) {
    tell(txt)
    return(invisible())
  }
  if (space == "" & type %in% c("v", "x", "c")) {
    cli::cli_progress_cleanup()
    if (type == "v") succeed(.envir = .envir)
    if (type == "x") fail(.envir = .envir)
    return(invisible())
  }
  txt <- substr(txt, 3, nchar(txt))
  switch(
    type,
    `#` = tell(cli::col_yellow("#"), txt, .envir = .envir), # title
    `i` = tell(  cli::col_cyan("i"), txt, .envir = .envir), # information
    `v` = tell( cli::col_green("v"), txt, .envir = .envir), # approve
    `x` = tell(cli::col_br_red("x"), txt, .envir = .envir), # disapprove
    `*` = tell(cli::col_br_red("*"), txt, .envir = .envir), # suggest item
    `!` = tell(cli::col_br_red("!"), txt, .envir = .envir), # alert
    `~` = proceed(txt, .envir = .envir),                    # ongoing task
    # `?` = ask(txt, .envir = .envir),                        # yes/no question
    `=` = give(txt, .envir = .envir),                       # display + copy code
    `$` = tell(  cli::col_cyan("$"), txt, .envir = .envir),
    `.` = tell(  cli::col_cyan("."), txt, .envir = .envir),
    `e` = error(txt, .envir = .envir),
    `w` = warn(txt, .envir = .envir)
  )
}
tell <- function(..., .envir = parent.frame(), capture = FALSE) {
  if (capture) {
    return(capture.output(tell(..., .envir = .envir), type = "message")[1])
  }
  cli::cli_div(theme = theme())
  cli::cli_text(paste(...), .envir = .envir)
}
msg_done <- NULL
msg_failed <- NULL
begin <- function(..., .envir = parent.frame()) {
  if (interactive()) {
    cli::cli_div(theme = theme())
    cli::cli_progress_message(paste0("{cli::col_yellow('~')} ",
                                     paste(...), " ..."),
                              .envir = .envir)
  }
  assignInNamespace(
    "msg_done",
    paste0("{cli::col_green('v')} ", paste(...), " ... done"),
    "std"
  )
  assignInNamespace(
    "msg_failed",
    paste0("{cli::col_br_red('x')} ",
           paste(...), " ... failed"),
    "std"
  )
}
proceed <- function(..., .envir = parent.frame()) {
  clear(.envir)
  begin(..., .envir = .envir)
}
clear <- function(.envir = parent.frame()) {
  cli::cli_progress_done(.envir = .envir, result = "clear")
}
succeed <- function(.envir = parent.frame()) {
  clear(parent.frame())
  tell(msg_done, .envir = parent.frame())
  assignInNamespace("msg_done", NULL, "std")
  assignInNamespace("msg_failed", NULL, "std")
}
fail <- function(.envir = parent.frame()) {
  clear(parent.frame())
  tell(msg_failed, .envir = parent.frame())
  assignInNamespace("msg_done", NULL, "std")
  assignInNamespace("msg_failed", NULL, "std")
}
give <- function(..., language = "R", copy = TRUE) {
  cli::cli_div(theme = theme())
  cli::cli_code(lines = NULL, ..., language = language)
  if (copy) writeClipboard(paste(..., sep = "\n", collapse = "\n"))
}
warn <- function(..., .demo = FALSE) {
  msg <- tell(..., capture = TRUE)
  if (.demo) {
    message(paste0("Warning: ", msg))
    return(invisible())
  }
  warning(msg, call. = FALSE, immediate. = TRUE)
}
error <- function(..., .demo = FALSE) {
  msg <- tell(..., capture = TRUE)
  if (.demo) {
    message(paste0("Error: ", msg))
    return(invisible())
  }
  stop(msg, call. = FALSE)
}
