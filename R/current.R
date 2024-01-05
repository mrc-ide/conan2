##' Test if a conan installation is current. This just checks if the
##' instructions to provision have changed since we last ran, not if
##' the installation *would* do anything different.
##'
##' @title Test if a conan installation is current
##'
##' @param path_lib Path to the library to compare
##'
##' @param config A configuration object
##'
##' @return A boolean, `TRUE` if we are up to date and `FALSE` otherwise
##'
##' @export
conan_is_current <- function(path_lib, config) {
  contents <- dir(file.path(path_lib, ".conan"))
  if (length(contents) == 0) {
    return(FALSE)
  }
  prev <- readRDS(file.path(path_lib, ".conan", max(contents)))
  identical(config$hash, prev$hash)
}
