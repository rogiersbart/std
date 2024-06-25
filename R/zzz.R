.onAttach <- function(libname, pkgname) {
  err("! Package {.pkg std} is still in its experimental lifecycle stage.")
  err("! Use at your own risk, and submit issues here:")
  err("! {.url https://github.com/rogiersbart/std/issues}")
  err()
  err("i You are attaching the {.pkg std} namespace.")
  err("i We advise using {.fun std::function} instead!")
  invisible()
}
