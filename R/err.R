# TODO
# - Add `out()` and `in()` to complete streams functionality
# - Move ? into stdin

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
#' @export
err <- function(...) {
  .envir <-  parent.frame()
  list2env(styles, .envir)
  lines <- glue::glue(..., .sep = " ", .envir = .envir)
  prefix <- substr(lines[1], 1, 2)
  if (prefix == "~ ") {
    lprev <- if (is.null(std:::status)) nchar(lines[1]) else nchar(std:::status)
    assignInNamespace(
      "status",
      lines,
      "std"
    )
    if (!is.null(std:::status)) cat2("\r", rep(" ", lprev), "\r")
    cat2(gsub("^~ ", .yellow("~ "), lines) |> paste0(" ..."))
    return(invisible())
  }
  if (nchar(lines[1]) == 1) {
    if (lines[1] == "x") {
      cat2("\r", rep(" ", nchar(std:::status)), "\r")
      cat2(gsub("^~ ", .red("x "), std:::status) |> paste0(" ... failed"), sep = "\n")
      assignInNamespace("status", NULL, "std")
      return(invisible())
    }
    if (lines[1] == "v") {
      lprev <- nchar(std:::status)
      cat2("\r", rep(" ", nchar(std:::status)), "\r")
      cat2(gsub("^~ ", .green("v "), std:::status) |> paste0(" ... done"), sep = "\n")
      assignInNamespace("status", NULL, "std")
      return(invisible())
    }
    if (lines[1] == "c") {
      lprev <- nchar(std:::status)
      cat2("\r", rep(" ", nchar(std:::status)), "\r")
      assignInNamespace("status", NULL, "std")
      return(invisible())
    }
  }
  if (prefix == "e ") {
    stop(gsub("^e ", "", lines) |>
           cli::ansi_strwrap(exdent = 2,
                             width = 0.9 * cli::console_width()) |>
           paste0(collapse = "\n"), call. = FALSE)
    return(invisible())
  }
  if (prefix == "w ") {
    warning(gsub("^w ", "", lines) |>
              cli::ansi_strwrap(exdent = 2,
                                width = 0.9 * cli::console_width()) |>
              paste0(collapse = "\n"), call. = FALSE, immediate. = TRUE)
    return(invisible())
  }
  if (prefix == "= ") {
    cat2(gsub("\n", "\n  ", gsub("^= ", .yellow("= "), lines)), .yellow("  Copied to clipboard."), sep = "\n")
    writeClipboard(gsub("= ", "", lines))
    return(invisible())
  }
  lines <- cli::ansi_strwrap(lines, exdent = 2,
                             width = 0.9 * cli::console_width())
  colour <- switch(
    prefix,
    "i " = \(x) gsub("^i ", .cyan("i "), x),
    "# " = \(x) gsub("^# ", .yellow("# "), x),
    "v " = \(x) gsub("^v ", .green("v "), x),
    "x " = \(x) gsub("^x ", .red("x "), x),
    "* " = \(x) gsub("^* ", .red("* "), x),
    "! " = \(x) gsub("^! ", .red("! "), x),
    "$ " = \(x) gsub("^$ ", .cyan("$ "), x),
    ". " = \(x) gsub("^. ", .cyan(". "), x),
    identity
  )
  lines[1] <- lines[1] |> colour()
  cat2(lines, sep = "\n")
}
cat2 <- function(...) cat(..., file = stderr())
# #' @export
# style <- function() {
#   if ("std:::styles" %in% search()) return(invisible())
#   attach(std:::styles)
# }
styles <- list(
  .blue = \(x) cli::col_blue(glue::glue(x)),
  .red = \(x) cli::col_red(glue::glue(x)),
  .yellow = \(x) cli::col_yellow(glue::glue(x)),
  .black = \(x) cli::col_black(glue::glue(x)),
  .cyan = \(x) cli::col_cyan(glue::glue(x)),
  .magenta = \(x) cli::col_magenta(glue::glue(x)),
  .green = \(x) cli::col_green(glue::glue(x)),
  .white = \(x) cli::col_white(glue::glue(x)),
  .bold = \(x) cli::style_bold(glue::glue(x)),
  .italic = \(x) cli::style_italic(glue::glue(x)),
  .link = \(x, y = x, z = NULL) cli::style_hyperlink(glue::glue(x), glue::glue(y), z) # text, url
  # .link(text, url, params)
  #
  # url can be
  # file://C:/... or fs::path_abs("rel.path")
  # https://...
  # ide:help:pkg::fun or x-r-help:pkg::fun
  # ide:vignette:pkg::vgn or x-r-vignette:box::faq
  # ide:run:rlang::last_error() or x-r-run:rlang::last_error()
  # see https://github.com/rstudio/rstudio/issues/12084
  #
  # params can be (for file)
  # list(line = 5, col = 5) or
  # c(line = 5, col = 5)
)
list2env(styles, environment())
status <- NULL
