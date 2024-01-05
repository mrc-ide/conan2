test_that("can list conan installations", {
  path <- withr::local_tempdir()
  nms <- example_installations(path)

  res <- evaluate_promise(withVisible(conan_list_installations(path)))
  expect_equal(res$result, list(value = nms, visible = FALSE))

  msg <- res$messages
  expect_length(msg, 6)
  expect_match(msg[[1]], "5 conan installations recorded")
  expect_match(msg[[2]], sprintf("1: %s \\(.+ago\\) \\[-4\\]", nms[[1]]))
  expect_match(msg[[6]], sprintf("5: %s \\(.+ago\\) \\[0\\]", nms[[5]]))
})


test_that("can list conan installations when only one present", {
  path <- withr::local_tempdir()
  nms <- example_installations(path, 1)

  res <- evaluate_promise(withVisible(conan_list_installations(path)))
  expect_equal(res$result, list(value = nms, visible = FALSE))

  msg <- res$messages
  expect_length(msg, 2)
  expect_match(msg[[1]], "1 conan installation recorded")
  expect_match(msg[[2]], sprintf("1: %s \\(.+ago\\) \\[0\\]", nms[[1]]))
})


test_that("can list conan installations when none are present", {
  path <- withr::local_tempdir()
  nms <- example_installations(path, 0)

  res <- evaluate_promise(withVisible(conan_list_installations(path)))
  expect_equal(res$result, list(value = character(), visible = FALSE))

  msg <- res$messages
  expect_length(msg, 1)
  expect_match(msg[[1]], "No conan installations recorded")
})


test_that("can select sensible comparisons by index", {
  nms <- c("a", "b", "c", "d", "e")

  ## Simple use with negative index:
  expect_equal(compare_select("lib", nms, 0, -1), list(curr = 5, prev = 4))
  expect_equal(compare_select("lib", nms, 0, -4), list(curr = 5, prev = 1))
  expect_equal(compare_select("lib", nms, 0, -5), list(curr = 5, prev = NULL))
  expect_equal(compare_select("lib", nms, 0, -Inf), list(curr = 5, prev = NULL))
  expect_equal(compare_select("lib", nms, 0, NULL), list(curr = 5, prev = NULL))
  expect_equal(compare_select("lib", nms, -3, -4), list(curr = 2, prev = 1))

  ## Simple use with positive index:
  expect_equal(compare_select("lib", nms, 2, 1), list(curr = 2, prev = 1))
  expect_equal(compare_select("lib", nms, 0, 1), list(curr = 5, prev = 1))
  expect_equal(compare_select("lib", nms, -1, 2), list(curr = 4, prev = 2))
  err <- expect_error(compare_select("lib", nms, 6, 1),
                      "Invalid entry '6' for 'curr'")
  expect_equal(err$body, c(i = "Maximum allowed value is 5"))
})


test_that("can select sensible comparisons by name", {
  expect_equal(compare_select("lib", letters[1:5], "b", "a"),
               list(curr = 2, prev = 1))
  err <- expect_error(
    compare_select("lib", letters[1:5], "foo", "a"),
    "Invalid entry 'foo' for 'curr'")
  expect_equal(
    err$body,
    c(i = "Valid entries are: 'a', 'b', 'c', 'd', and 'e'"))
  err <- expect_error(
    compare_select("lib", letters[1:3], "foo", "a"),
    "Invalid entry 'foo' for 'curr'")
  expect_equal(
    err$body,
    c(i = "Valid entries are: 'a', 'b', and 'c'"))
  err <- expect_error(
    compare_select("lib", letters, "foo", "a"),
    "Invalid entry 'foo' for 'curr'")
  expect_equal(
    err$body,
    c(i = "Valid entries include: 'v', 'w', 'x', 'y', and 'z'"))
})


test_that("comparisons must follow the arrow of time concept", {
  expect_error(compare_select("lib", letters[1:5], 3, 5),
               "'curr' (installation 3) is earlier than 'prev' (5)",
               fixed = TRUE)
  expect_error(compare_select("lib", letters[1:5], 3, 3),
               "'curr' (installation 3) is the same as 'prev'",
               fixed = TRUE)
})

test_that("prevent problematic comparisons", {
  expect_error(compare_select("lib", character(), 0, -1),
               "No conan installations found at 'lib'")
  expect_error(compare_select("lib", letters[1:5], -Inf, -Inf),
               "'curr' must be a real installation")
})


test_that("compare empty and initial", {
  path <- withr::local_tempdir()
  nms <- example_installations(path)
  res <- conan_compare(path, 1, NULL)
  expect_s3_class(res, "conan_compare")

  cmp <- readRDS(file.path(path, ".conan", nms[[1]]))
  expect_equal(res$curr$name, nms[[1]])
  expect_equal(res$curr$index, 1)
  expect_equal(res$curr$age, -4)
  expect_equal(res$curr$packages, as.data.frame(cmp$description))

  expect_null(res$prev)
  expect_equal(res$status, c(R6 = "added"))

  expect_equal(res$changes$added, "{.strong R6} ({.new 2.5.1}) CRAN")
  expect_equal(res$changes$unchanged, character())
  expect_equal(res$changes$removed, character())
  expect_equal(res$changes$updated, character())
})


test_that("compare two with additions only", {
  path <- withr::local_tempdir()
  nms <- example_installations(path)
  res <- conan_compare(path, 2, 1)
  expect_s3_class(res, "conan_compare")

  cmp1 <- readRDS(file.path(path, ".conan", nms[[1]]))
  cmp2 <- readRDS(file.path(path, ".conan", nms[[2]]))

  expect_equal(res$curr$name, nms[[2]])
  expect_equal(res$curr$index, 2)
  expect_equal(res$curr$age, -3)
  expect_equal(res$curr$packages, as.data.frame(cmp2$description))

  expect_equal(res$prev$name, nms[[1]])
  expect_equal(res$prev$index, 1)
  expect_equal(res$prev$age, -4)
  expect_equal(res$prev$packages, as.data.frame(cmp1$description))

  expect_equal(res$status,
               c(R6 = "unchanged",
                 askpass = "added",
                 ids = "added",
                 openssl = "added",
                 sys = "added",
                 uuid = "added"))

  expect_equal(res$changes$unchanged, "{.strong R6} (2.5.1) CRAN")
  expect_equal(res$changes$added,
               c("{.strong askpass} ({.new 1.2.0}) CRAN",
                 "{.strong ids} ({.new 1.0.1}) CRAN",
                 "{.strong openssl} ({.new 2.1.1}) CRAN",
                 "{.strong sys} ({.new 3.4.2}) CRAN",
                 "{.strong uuid} ({.new 1.1.1}) CRAN"))
  expect_equal(res$changes$removed, character())
  expect_equal(res$changes$updated, character())
})


test_that("compare two with deletions", {
  path <- withr::local_tempdir()
  nms <- example_installations(path)
  res <- conan_compare(path)
  expect_s3_class(res, "conan_compare")
  expect_equal(res$status,
               c(R6 = "unchanged",
                 askpass = "removed",
                 ids = "unchanged",
                 openssl = "removed",
                 sys = "unchanged",
                 uuid = "unchanged"))
  expect_equal(res$changes$unchanged,
               c("{.strong R6} (2.5.1) CRAN",
                 "{.strong ids} (1.2.2) github: richfitz/ids (07e7325)",
                 "{.strong sys} (3.4.2) CRAN",
                 "{.strong uuid} (1.1.1) CRAN"))
  expect_equal(res$changes$removed,
               c("{.strong askpass} ({.old 1.2.0}) CRAN",
                 "{.strong openssl} ({.old 2.1.1}) CRAN"))
  expect_equal(res$changes$added, character())
  expect_equal(res$changes$updated, character())
})


test_that("compare two with updates", {
  path <- withr::local_tempdir()
  nms <- example_installations(path)
  res <- conan_compare(path, -1, -2)
  expect_s3_class(res, "conan_compare")
  expect_equal(res$status,
               c(R6 = "unchanged",
                 askpass = "unchanged",
                 ids = "updated",
                 openssl = "unchanged",
                 sys = "unchanged",
                 uuid = "unchanged"))
  expect_length(res$changes$unchanged, 5)
  expect_length(res$changes$added, 0)
  expect_length(res$changes$removed, 0)
  expect_equal(
    res$changes$updated,
    paste("{.strong ids} ({.old 1.1.3} -> {.new 1.2.2})",
          "github: richfitz/ids{{{.old @tidy (651433d)} ->",
          "{.new  (07e7325)}}}"))
})


test_that("can describe changes in detail", {
  expect_equal(
    details_changes("user/repo (x)", "user/repo@foo (x)"),
    "user/repo{{{.old  (x)} -> {.new @foo (x)}}}")
  expect_equal(
    details_changes("user/repo@foo (x)", "user/repo@bar (y)"),
    "user/repo@{{{.old foo (x)} -> {.new bar (y)}}}")
  expect_equal(
    details_changes("user/repo@foo (x)", "other/repo@bar (y)"),
    "{.old user/repo@foo (x)} -> {.new other/repo@bar (y)}")
})
