# TODO
# - put yellow ? as prefix here always?
# - then use actual user-provided prefix for type?
#   i = integer
#   n/r/d = double
#   c = character
# - std::in("i How many exercises do you want to do?")
# - maybe also consider prefix for n entries? ix5?
#   or for inf entries ixn? for which the user needs to abort input?
#   dont remember now how?
# - this would then be consistent to std::err()

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
