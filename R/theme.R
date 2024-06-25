theme <- function() {
  make_link <- function(x, type) {
    if (interactive()) return(cli:::make_link(x, type = type))
    if (type == "href") return(cli:::re_match(x, "^\\[(?<text>.*)\\]\\((?<url>.*)\\)$")$text)
    x
  }
  list(
    # span.dt = list(postfix = ": "),
    # span.dd = list(),
    # .code = list(fmt = cli:::format_code(dark)),
    # .code.R = list(fmt = cli:::format_r_code(dark)),
    span.emph = list(`font-style` = "italic"),
    span.strong = list(`font-weight` = "bold"),
    # span.code = cli:::theme_code_tick(dark),
    # span.q = list(fmt = cli:::quote_weird_name2),
    span.pkg = list(before = "{", after = "}", transform = cli::col_yellow,
                    color = NULL),
    # span.fn = cli:::theme_function(dark),
    span.fun = list(
      before = "`",
      after = "`",
      transform = \(x) make_link(x, type = "fun") |> paste0("()") |> cli::col_blue()
    ),
    # span.arg = cli:::theme_code_tick(dark),
    # span.kbd = list(before = "[", after = "]", color = "blue"),
    # span.key = list(before = "[", after = "]", color = "red"),
    # span.file = cli:::theme_file(),
    # span.path = cli:::theme_file(),
    span.email = list(
      color = NULL,
      transform = function(x) make_link(x, type = "email") |> cli::col_cyan(),
      fmt = cli:::quote_weird_name
    ),
    span.url = list(
      before = "<",
      after = ">",
      color = NULL,
      `font-style` = NULL,
      transform = function(x) make_link(x, type = "url") |> cli::col_cyan()
    ),
    span.href = list(
      transform = function(x) make_link(x, type = "href") |> cli::col_cyan()
    ),
    # span.help = list(transform = function(x) cli:::make_link(x, type = "help")),
    # span.topic = list(transform = function(x) cli:::make_link(x, type = "topic")),
    # span.vignette = list(transform = function(x) cli:::make_link(x, type = "vignette")),
    # span.run = list(transform = function(x) cli:::make_link(x, type = "run")),
    # span.var = cli:::theme_code_tick(dark),
    # span.col = cli:::theme_code_tick(dark), span.str = list(fmt = cli:::encode_string),
    # span.envvar = cli:::theme_code_tick(dark),
    # span.val = list(transform = function(x, ...) cli_format(x, ...), color = "blue"),
    # span.field = list(color = "green"),
    # span.cls = list(collapse = "/", color = "blue", before = "<", after = ">"),
    # `span.progress-bar` = list(transform = cli:::theme_progress_bar, color = "green"),
    # span.obj_type_friendly = list(transform = function(x) cli::format_inline(cli:::typename(x))),
    # span.type = list(transform = function(x) cli::format_inline(cli:::typename(x))),
    # span.or = list(`vec-sep2` = " or ", `vec-last` = ", or "),
    # span.timestamp = list(before = "[", after = "]", color = "grey")),
    NULL
  )
}
