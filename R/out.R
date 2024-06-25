# TODO
# - Improve object inspection performance
# - Extract object inspection out of err()
# - Let out() do inspect() when interactive, otherwise use standard print
#   method, or something else if useful for stdout (eg format_csv() for df?)

# FIXME
#

# err <- function(..., object = NULL, levels = 1, .envir = parent.frame()) {
#   if (!is.null(object)) {inspect(object, levels); return(invisible())}
#   txt <- paste(...)
#   type <- substr(txt, 1, 1)
#   types <- c("i", "v", "x", "~", "*", "?", "=", "!", "#", ".", "c", "$", "e", "w")
#   space <- substr(txt, 2, 2)
#   if (!(type %in% types & (space %in% c(" ", "")))) {
#     tell(txt)
#     return(invisible())
#   }
#   if (space == "") {
#     if (type == "v") {cli::cli_progress_cleanup(); succeed(.envir = .envir); return(invisible())}
#     if (type == "x") {cli::cli_progress_cleanup(); fail(.envir = .envir); return(invisible())}
#     if (type == "c") {cli::cli_progress_cleanup(); return(invisible())}
#   }
#   txt <- substr(txt, 3, nchar(txt))
#   switch(
#     type,
#
#     `#` = tell(cli::col_yellow("#"), txt, .envir = .envir),
#     `i` = tell(  cli::col_cyan("i"), txt, .envir = .envir),
#     `v` = tell( cli::col_green("v"), txt, .envir = .envir),
#     `x` = tell(cli::col_br_red("x"), txt, .envir = .envir),
#     `*` = tell(cli::col_br_red("*"), txt, .envir = .envir),
#     `!` = tell(cli::col_br_red("!"), txt, .envir = .envir),
#     `~` = proceed(txt, .envir = .envir),
#     `?` = ask(txt, .envir = .envir),
#     `=` = give(txt, .envir = .envir),
#     `$` = expose(txt, .envir = .envir),
#     `.` = display(txt, .envir = .envir),
#     `e` = error(txt, .envir = .envir),
#     `w` = warn(txt, .envir = .envir)
#   )
# }

# expose <- function(..., level = 1) {
#   prefix <- paste0(
#     rep(paste0(c(cli::col_cyan("$"), cli::col_yellow("$")), "."),
#         ceiling(level / 2))[1:level],
#     collapse = ""
#   )
#   prefix <- prefix |> substr(1, nchar(prefix) - 1)
#   tell(prefix, ...)
#   invisible()
# }
# display <- function(...) {
#   tell(cli::col_cyan("."),
#        ...)
# }
# inspect <- function(object, levels = 1) {
#   txt <- capture.output(str(object, give.attr = FALSE,
#                             vec.len = 2,
#                             max.level = levels + 1))
#   # TODO condense this, in a separate function, by applying gsub to a nx2
#   # character matrix?
#   txt <- gsub(": num ", ": {.emph <dbl>} ", txt, fixed = TRUE)
#   txt <- gsub("^ num ", "{.emph <dbl>} ", txt)
#   txt <- gsub(": Named num ", ": {.emph <n.dbl>} ", txt, fixed = TRUE)
#   txt <- gsub("^ Named num ", "{.emph <n.dbl>} ", txt)
#
#   txt <- gsub("^ int ", "{.emph <int>} ", txt)
#   txt <- gsub(": int ", ": {.emph <int>} ", txt, fixed = TRUE)
#   txt <- gsub("^ Named int ", "{.emph <n.int>} ", txt)
#   txt <- gsub(": Named int ", ": {.emph <n.int>} ", txt, fixed = TRUE)
#
#   txt <- gsub("^ chr ", "{.emph <chr>} ", txt)
#   txt <- gsub(": chr ", ": {.emph <chr>} ", txt, fixed = TRUE)
#   txt <- gsub("^ Named chr ", "{.emph <n.chr>} ", txt)
#   txt <- gsub(": Named chr ", ": {.emph <n.chr>} ", txt, fixed = TRUE)
#
#   txt <- gsub(": logi ", ": {.emph <lgl>} ", txt, fixed = TRUE)
#   txt <- gsub("^ logi ", "{.emph <lgl>} ", txt)
#   txt <- gsub(": Named logi ", ": {.emph <n.lgl>} ", txt, fixed = TRUE)
#   txt <- gsub("^ Named logi ", "{.emph <n.lgl>} ", txt)
#
#   txt <- gsub(": raw ", ": {.emph <raw>} ", txt, fixed = TRUE)
#   txt <- gsub("^ raw ", "{.emph <raw>} ", txt)
#   txt <- gsub(": Named raw ", ": {.emph <n.raw>} ", txt, fixed = TRUE)
#   txt <- gsub("^ Named raw ", "{.emph <n.raw>} ", txt)
#
#   txt <- gsub("'difftime'", "{.emph <drtn>}", txt, fixed = TRUE)
#
#   txt <- gsub(" '", " {.val '", txt)
#   txt <- gsub("' ", "'} ", txt)
#   txt <- gsub("':", "'} ", txt)
#   txt <- gsub(":'", ": {.val '", txt)
#   txt <- gsub("^'", "{.val '", txt)
#
#   txt <- gsub(" ‘", " {.val '", txt)
#   txt <- gsub("’ ", "'} ", txt)
#   txt <- gsub("’:", "'} ", txt)
#   txt <- gsub(":‘", ": {.val '", txt)
#   txt <- gsub("^‘", "{.val '", txt)
#
#   txt <- gsub(":Classes ", ": ", txt, fixed = TRUE)
#   txt <- gsub(" and ", ", ", txt, fixed = TRUE)
#   txt <- gsub(" obs. of  ", "x", txt, fixed = TRUE)
#   txt <- gsub(" variables:", "", txt)
#   txt <- gsub("^ Date\\[", "Date[", txt)
#   txt <- gsub("Date[", "{.emph <date>} [", txt, fixed = TRUE)
#   txt <- gsub("^ POSIX", "POSIX", txt)
#   txt <- gsub("POSIXct[", "{.emph <dttm>} [", txt, fixed = TRUE)
#   txt <- gsub("POSIXlt[", "{.emph <dttm>} [", txt, fixed = TRUE)
#   txt <- gsub("Factor w/", "{.emph <fct>}", txt)
#   txt <- gsub("Time-Series", "{.emph <ts>}", txt)
#   txt <- ifelse(grepl("List of ", txt), paste0(txt, "]"), txt)
#   txt <- gsub("List of ", " {.val 'list'} [1:", txt)
#   txt <- ifelse(grepl("'data.frame'", txt),
#                 paste0(gsub("frame'} \t", "frame'} [", txt), "]"),
#                 txt)
#   txt <- ifelse(grepl(" tibble ", txt, fixed = TRUE),
#                 gsub(" tibble ", " {.val 'tbl_df'} ", txt, fixed = TRUE),
#                 txt)
#   txt <- gsub(" x ", "x", txt, fixed = TRUE)
#   txt <- gsub(">}  ", ">} ", txt, fixed = TRUE)
#   txt <- gsub(" (S3: tbl_df/tbl/data.frame)", "", txt, fixed = TRUE)
#
#   if (is.null(dim(object))) {
#     dimensions <- paste0("[1:", length(object), "]")
#   } else {
#     dimensions <- paste0("[", paste0(dim(object), collapse = "x"), "]")
#   }
#   first_line <- txt[1]
#   txt <- txt[-1]
#   if (is.vector(object) & is.atomic(object)) {
#     entitle("vector of type {.val {typeof(object)[1]}} {dimensions}")
#   } else {
#     entitle("object of class {.val {class(object)[1]}}",
#             "type {.val {typeof(object)[1]}} {dimensions}")
#   }
#   if (is.atomic(object)) {
#     display(first_line)
#     return(invisible())
#   }
#   txt_levels <- inspect_expose_level(txt)
#   txt <- inspect_remove_level_prefix(txt)
#   for (i in 1:length(txt)) {
#     if (txt_levels[i] <= levels)
#       expose(txt[i], level = txt_levels[i])
#   }
#   invisible()
# }
# inspect_expose_level <- function(txt) {
#   dots <- substring(txt, 1, regexpr("$", txt, fixed = TRUE) - 1)
#   dots <- gsub(" ", "", dots)
#   nchar(dots) / 2 + 1
# }
# inspect_remove_level_prefix <- function(txt) {
#   substring(txt,
#             regexpr("$", txt, fixed = TRUE) + 2,
#             nchar(txt))
# }
