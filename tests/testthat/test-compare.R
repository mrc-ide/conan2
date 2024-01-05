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
