test_that("can't be using renv if not installed", {
  mock_require_ns <- mockery::mock(FALSE)
  mockery::stub(using_renv, "requireNamespace", mock_require_ns)
  expect_false(using_renv())
  mockery::expect_called(mock_require_ns, 1)
  expect_equal(mockery::mock_args(mock_require_ns)[[1]],
               list("renv", quietly = TRUE))
})


test_that("can't be using renv if project not defined", {
  mock_project <- mockery::mock(NULL)
  mockery::stub(using_renv, "renv::project", mock_project)
  expect_false(using_renv())
  mockery::expect_called(mock_project, 1)
  expect_equal(mockery::mock_args(mock_project)[[1]],
               list(default = NULL))
})


test_that("can't be using renv if project elsewhere to current path", {
  path <- withr::local_tempfile()
  mock_project <- mockery::mock(path)
  mockery::stub(using_renv, "renv::project", mock_project)
  expect_false(using_renv())
  mockery::expect_called(mock_project, 1)
  expect_equal(mockery::mock_args(mock_project)[[1]],
               list(default = NULL))
})


test_that("accept project if it's in current or parent directory", {
  path <- withr::local_tempfile()
  mock_project <- mockery::mock(path, dirname(path))
  mockery::stub(using_renv, "renv::project", mock_project)
  expect_true(using_renv(path))
  expect_true(using_renv(path))
})
