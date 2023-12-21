##' Run a conan installation, in another process, blocking from this
##' process.
##'
##' @title Run a conan installation
##'
##' @inheritParams conan_write
##'
##' @return Nothing
##' @export
conan_run <- function(config) {
  ## TODO: this *must* be called from the same directory passed
  ## through to conan_configure, which is weird.
  path <- tempfile(pattern = "conan")
  ## If the integration tests fail, it will be useful to set this path
  ## to something like: /home/rich/tmp/conan-testing (or some other
  ## absolute path) so that the logs are not swallowed by the check.
  dir_create(path)
  path_script <- file.path(path, "conan.R")
  path_log <- file.path(path, "log")
  conan_write(config, path_script)
  if (config$method == "renv") {
    ## This all plays quite poorly with callr because it does a great
    ## job of setting libPaths for us, after renv has finished setting
    ## up the libPaths, so we never end up with a loaded version!
    ##
    ## I've confirmed that we can disable autoload by setting one of
    ## these:
    ##
    ## RENV_AUTOLOADER_ENABLED
    ## RENV_ACTIVATE_PROJECT
    ## RENV_CONFIG_AUTOLOADER_ENABLED
    ##
    ## To anything other than true/t/1 (case insensitive).
    withr::local_envvar(RENV_AUTOLOADER_ENABLED = "FALSE")
  }
  callr::rscript(path_script, stdout = path_log, stderr = path_log,
                 show = config$show_log)
  invisible()
}


##' Write a conan installation script
##'
##' @title Write conan installation script
##'
##' @param path The path to write to
##'
##' @param config Conan config, from [conan_configure()]
##'
##' @return Nothing
##' @export
conan_write <- function(config, path) {
  assert_is(config, "conan_config")
  name <- if (config$method == "auto") "pkgdepends" else config$method
  template <- read_string(conan_file(sprintf("template/install_%s.R", name)))
  str <- glue_whisker(template, template_data(config))
  dir_create(dirname(path))
  writeLines(str, path)
}


template_data <- function(config) {
  ret <- config
  default_repo <- "https://cloud.r-project.org"
  if (config$method == "script") {
    ret$repos <- vector_to_str(default_repo)
  } else if (config$method %in% c("pkgdepends", "auto")) {
    ret$repos <- vector_to_str(c(config$pkgdepends$repos, default_repo))
    ret$refs <- vector_to_str(config$pkgdepends$refs)
  }
  ret
}