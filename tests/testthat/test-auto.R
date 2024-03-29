test_that("can build a very straightforward case", {
  path <- withr::local_tempfile()
  dir.create(path)
  expect_equal(
    build_pkgdepends_auto(list(packages = c("apple", "banana")), path),
    list(refs = c("apple", "banana"),
         repos = character()))
  expect_equal(
    build_pkgdepends_auto(list(packages = "apple"), path),
    list(refs = "apple",
         repos = character()))
  expect_error(
    build_pkgdepends_auto(list(), path),
    "I could not work out anything to install automatically")
})


test_that("can find dependencies in source files", {
  path <- withr::local_tempfile()
  dir.create(path)
  writeLines(
    c("banana::split()",
      "library(cucumber)",
      "if (smelly) {",
      '  require("durian")',
      "}"),
    file.path(path, "src.R"))
  res <- build_pkgdepends_auto(list(packages = "apple", sources = "src.R"),
                               path)
  expect_setequal(res$refs, c("apple", "banana", "cucumber", "durian"))
  expect_equal(res$repos, character())
})


test_that("can fall back on package name if ref construction fails", {
  desc <- function(...) {
    structure(list(...), class = "packageDescription")
  }
  mock_pkg_desc <- mockery::mock(
    desc(RemoteRef = "HEAD", RemoteRepo = "repo-c"))
  mockery::stub(packages_to_pkgdepends, "utils::packageDescription",
                mock_pkg_desc)
  warn <- expect_warning(
    res <- packages_to_pkgdepends("x"),
    "Failed to work out pkgdepends ref for 'x'")
  expect_mapequal(res, list(repos = character(), refs = "x"))
  mockery::expect_called(mock_pkg_desc, 1)
  expect_equal(mockery::mock_args(mock_pkg_desc),
               list(list("x")))
})


test_that("can detect complex refs", {
  desc <- function(...) {
    structure(list(...), class = "packageDescription")
  }
  mock_pkg_desc <- mockery::mock(
    desc(Repository = "CRAN"),
    desc(Repository = "https://mrc-ide.r-universe.dev"),
    desc(RemoteRef = "HEAD", RemoteUsername = "user-c", RemoteRepo = "repo-c"),
    desc(RemoteRef = "branch", RemoteUsername = "user-d", RemoteRepo = "repo-d",
         RemoteSubdir = "path/to/src"))
  mockery::stub(packages_to_pkgdepends, "utils::packageDescription",
                mock_pkg_desc)
  res <- packages_to_pkgdepends(c("a", "b", "c", "d"))
  expect_mapequal(
    res,
    list(repos = "https://mrc-ide.r-universe.dev",
         refs = c("a", "b", "user-c/repo-c@HEAD",
                  "user-d/repo-d@branch/path/to/src")))
  mockery::expect_called(mock_pkg_desc, 4)
  expect_equal(mockery::mock_args(mock_pkg_desc),
               list(list("a"), list("b"), list("c"), list("d")))
})


test_that("can detect remote standard installs", {
  desc <- function(...) {
    structure(list(...), class = "packageDescription")
  }
  mock_pkg_desc <- mockery::mock(
    desc(RemoteRepository = "https://cran.example.com",
         RemoteRef = "a", RemoteType = "standard"))
  mockery::stub(packages_to_pkgdepends, "utils::packageDescription",
                mock_pkg_desc)
  res <- packages_to_pkgdepends("a")
  expect_mapequal(
    res,
    list(repos = "https://cran.example.com",
         refs = "a"))
  mockery::expect_called(mock_pkg_desc, 1)
  expect_equal(mockery::mock_args(mock_pkg_desc)[[1]],
               list("a"))
})
