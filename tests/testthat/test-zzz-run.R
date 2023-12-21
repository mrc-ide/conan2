test_that("can run a script-based installation", {
  path <- withr::local_tempdir()
  writeLines('install.packages("R6")', file.path(path, "provision.R"))
  path_lib <- "lib"
  path_bootstrap <- bootstrap_library(NULL)
  cfg <- conan_configure(NULL, path = path, path_lib = path_lib,
                         path_bootstrap = path_bootstrap, show_log = FALSE)
  withr::with_dir(path, conan_run(cfg))
  expect_true(file.exists(file.path(path, "lib", "R6")))
})


test_that("can run a pkgdepends-based installation", {
  path <- withr::local_tempdir()
  writeLines("R6", file.path(path, "pkgdepends.txt"))
  path_lib <- "lib"
  path_bootstrap <- bootstrap_library("pkgdepends")
  cfg <- conan_configure(NULL, path = path, path_lib = path_lib,
                         path_bootstrap = path_bootstrap, show_log = FALSE)
  withr::with_dir(path, conan_run(cfg))
  expect_true(file.exists(file.path(path, "lib", "R6")))
})


test_that("can run an automatic installation", {
  path <- withr::local_tempdir()
  environment <- list(packages = "R6")
  path_lib <- "lib"
  path_bootstrap <- bootstrap_library("pkgdepends")
  cfg <- conan_configure(NULL, path = path, path_lib = path_lib,
                         path_bootstrap = path_bootstrap,
                         environment = environment,
                         show_log = FALSE)
  withr::with_dir(path, conan_run(cfg))
  expect_true(file.exists(file.path(path, "lib", "R6")))
})


test_that("can run an renv installation", {
  path <- withr::local_tempdir()
  writeLines("library(R6)", file.path(path, "code.R"))
  res <- withr::with_dir(path, callr::r(function() {
    renv::init()
    install.packages("R6")
    renv::snapshot()
    list(pkgs = .packages(TRUE), libs = .libPaths())
  }))

  path_lib <- "lib"
  path_bootstrap <- bootstrap_library("renv")
  cfg <- conan_configure("renv", path = path, path_lib = path_lib,
                         path_bootstrap = path_bootstrap,
                         show_log = FALSE)
  withr::with_dir(path, conan_run(cfg))

  expect_true(file.exists(file.path(path, "lib", "R6")))
  expect_true(file.exists(file.path(path, "lib", "renv")))
})
