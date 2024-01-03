options(repos = {{repos}})

local({
  proposal <- pkgdepends::new_pkg_installation_proposal(
    refs = {{refs}},
    policy = "{{policy}}")
  proposal$solve()
  proposal$stop_for_solution_error()
  proposal$show_solution()
  proposal$download()
  proposal$stop_for_download_error()
  proposal$install()
})
