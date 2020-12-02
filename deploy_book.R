git <- function (..., echo_cmd = TRUE, echo = TRUE, error_on_status = TRUE) {
  callr::run("git", c(...), echo_cmd = echo_cmd, echo = echo,
             error_on_status = error_on_status)
}

github_worktree_add <- function(dir, remote, branch) {
  git("worktree", "add", "--track", "-B",
      branch, dir, paste0(remote, "/", branch))
}

github_worktree_remove <- function(dir) {
  git("worktree", "remove", dir)
}

build_book <- function(branch = "gh-pages", remote = "origin") {
  dest_dir <- fs::dir_create(fs::file_temp())
  on.exit(fs::dir_delete(dest_dir))

  github_worktree_add(dest_dir, remote, branch)
  on.exit(github_worktree_remove(dest_dir), add = TRUE)

  bookdown::render_book("index.Rmd", quiet = TRUE, output_dir = dest_dir)

  github_push(dest_dir, "deploying book", remote, branch)
  invisible()
}

build_book()
