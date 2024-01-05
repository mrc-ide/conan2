test_that("can run a script-based installation", {
  path <- withr::local_tempdir()
  writeLines('install.packages("R6")', file.path(path, "provision.R"))
  path_lib <- "lib"
  path_bootstrap <- bootstrap_library(NULL)
  cfg <- conan_configure(NULL, path = path, path_lib = path_lib,
                         path_bootstrap = path_bootstrap)
  withr::with_dir(path, conan_run(cfg, show_log = FALSE))
  expect_true(file.exists(file.path(path, "lib", "R6")))

  expect_true(file.exists(file.path(path, "lib", ".conan")))
  expect_length(dir(file.path(path, "lib", ".conan")), 1)
  d <- readRDS(dir(file.path(path, "lib", ".conan"), full.names = TRUE))

  expect_equal(d$method, "script")
  expect_equal(d$hash, rlang::hash_file(file.path(path, "provision.R")))
  expect_equal(d$hash, cfg$hash)
  expect_equal(d$args$script, "provision.R")
  expect_false(d$args$delete_first)
  expect_s3_class(d$description, "conan_describe")
  expect_true("R6" %in% names(d$description$packages))

  expect_equal(conan_list(file.path(path, path_lib))$hash, cfg$hash)
})


test_that("can run a pkgdepends-based installation", {
  path <- withr::local_tempdir()
  writeLines("R6", file.path(path, "pkgdepends.txt"))
  path_lib <- "lib"
  path_bootstrap <- bootstrap_library("pkgdepends")
  cfg <- conan_configure(NULL, path = path, path_lib = path_lib,
                         path_bootstrap = path_bootstrap)
  withr::with_dir(path, conan_run(cfg, show_log = FALSE))
  expect_true(file.exists(file.path(path, "lib", "R6")))

  expect_true(file.exists(file.path(path, "lib", ".conan")))
  expect_length(dir(file.path(path, "lib", ".conan")), 1)
  d <- readRDS(dir(file.path(path, "lib", ".conan"), full.names = TRUE))

  expect_equal(d$method, "pkgdepends")
  expect_equal(d$hash, cfg$hash)
  expect_mapequal(d$args$pkgdepends, list(refs = "R6", repos = NULL))
  expect_s3_class(d$description, "conan_describe")
  expect_true("R6" %in% names(d$description$packages))
})


test_that("can run an automatic installation", {
  path <- withr::local_tempdir()
  environment <- list(packages = "R6")
  path_lib <- "lib"
  path_bootstrap <- bootstrap_library("pkgdepends")
  cfg <- conan_configure(NULL, path = path, path_lib = path_lib,
                         delete_first = TRUE,
                         path_bootstrap = path_bootstrap,
                         environment = environment)
  withr::with_dir(path, conan_run(cfg, show_log = FALSE))
  expect_true(file.exists(file.path(path, "lib", "R6")))

  expect_true(file.exists(file.path(path, "lib", ".conan")))
  expect_length(dir(file.path(path, "lib", ".conan")), 1)
  d <- readRDS(dir(file.path(path, "lib", ".conan"), full.names = TRUE))

  expect_equal(d$method, "auto")
  expect_equal(d$hash, cfg$hash)
  expect_equal(d$args$pkgdepends$refs, "R6") # repos may differ
  expect_true(d$args$delete_first)
  expect_s3_class(d$description, "conan_describe")
  expect_true("R6" %in% names(d$description$packages))
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
                         path_bootstrap = path_bootstrap)
  expect_equal(cfg$hash, rlang::hash_file(file.path(path, "renv.lock")))
  withr::with_dir(path, conan_run(cfg, show_log = FALSE))

  expect_true(file.exists(file.path(path, "lib", "R6")))
  expect_true(file.exists(file.path(path, "lib", "renv")))

  expect_true(file.exists(file.path(path, "lib", ".conan")))
  expect_length(dir(file.path(path, "lib", ".conan")), 1)
  d <- readRDS(dir(file.path(path, "lib", ".conan"), full.names = TRUE))

  expect_equal(d$method, "renv")
  expect_equal(d$hash, cfg$hash)
  expect_s3_class(d$description, "conan_describe")
  expect_true(all(c("R6", "renv") %in% names(d$description$packages)))
})
