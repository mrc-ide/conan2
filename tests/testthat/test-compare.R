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
