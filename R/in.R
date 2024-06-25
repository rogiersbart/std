# TODO
# - [ ] Refactor multi-line feedback into single function
# - Improve object inspection performance
# - Add `out()` and `in()` to complete streams functionality
#

# FIXME
#

# ask <- function(..., .demo = FALSE) {
#   # NOTE this is based on usethis$ui_yeah, but uses text symbols only for
#   # consistent behaviour on all consoles/operating systems
#   tell(cli::col_yellow('?'), ...)
#   if (!interactive() & !.demo) {
#     error("User input required, but session is not interactive.")
#   }
#   if (.demo) {
#     tell("")
#     tell("1: Yes please.")
#     tell("2: No thanks.")
#     tell("")
#     return(invisible())
#   }
#   selection <- menu(c("Yes please.", "No thanks."))
#   if (selection == 0) error("A choice is required.")
#   as.logical(2 - selection)
# }
