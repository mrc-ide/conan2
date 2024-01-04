##' Describe a library.  This creates a summary of version information
##' from a library.  This may be slightly slow on network filesystems
##' as it uses [installed.packages].
##'
##' @title Describe a library
##'
##' @param path_lib Path to the library
##'
##' @return A list with class `conan_describe`. We'll write some
##'   tooling to work with these soon!
##'
##' @export
conan_describe <- function(path_lib) {
  ## This set is determined fairly empirically, but should be enough
  ## to work out where a package comes from fairly comprehensively
  ## while saving the least information.
  fields <- c("Package", "Version", "Repository", "biocViews",
              "Packaged", "Built", "Date/Publication",
              "RemoteType", "RemoteUsername", "RemoteRepo", "RemoteRef",
              "RemoteSubdir", "RemoteUrl", "RemoteSha",
              "GithubSHA1", "GithubUsername", "GithubRepo")
  pkgs <- utils::installed.packages(path_lib, noCache = TRUE, fields = fields)
  ret <- list(packages = pkgs[, fields, drop = FALSE],
              time = Sys.time())
  class(ret) <- "conan_describe"
  ret
}
