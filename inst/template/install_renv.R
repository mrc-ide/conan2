local({
  message("Bootstrapping from: {{path_bootstrap}}")
  message("Installing into library: {{path_lib}}")
  message(sprintf("Running in path: %s", getwd()))

  ## We can rely here on RENV_ACTIVATE_PROJECT or RENV_AUTOLOADER_ENABLED
  if (Sys.getenv("RENV_AUTOLOADER_ENABLED", "") != "FALSE") {
    stop(
      "Expected environment variable 'RENV_AUTOLOADER_ENABLED' to be 'FALSE'")
  }

  if (!requireNamespace("renv", "{{path_bootstrap}}")) {
    stop("Failed to load 'renv' from the bootstrap library")
  }

  if ({{delete_first}}) {
    message()
    message("Deleting previous library; this will fail if packages are in use")
    message("Then your running jobs may also fail.")
    unlink("{{path_lib}}", recursive = TRUE)
  }

  dir.create("{{path_lib}}", showWarnings = FALSE, recursive = TRUE)
  .libPaths(file.path(getwd(), "{{path_lib}}"))

  ## We might want to pass through packages/exclude here later,
  ## possibly also clean?
  renv::restore(library = "{{path_lib}}",
                prompt = FALSE)
})
