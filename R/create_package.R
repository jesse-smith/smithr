#' Create a new R Package
#'
#' `create_package()` mimics `usethis::create_package()`, but uses defaults that
#' I find more helpful. Specific actions can be found in the Details.
#'
#' @inheritParams usethis::create_package
#'
#' @param license `[string]` The license to use; corresponds to one of the
#'   `usethis::use_*_license()` functions
#'
#' @param renv `[flag]` Should `{renv}` be initialized for this package? This
#'   ensures that package dependencies are portable and reproducible and is
#'   recommended.
#'
#' @param github `[flag]` Should a Github repository be initialized for this
#'   package? The default is `TRUE` but will only work if you have set up your
#'   environment to work with `usethis::use_github()`.
#'
#' @param pkgdown `[flag]` Should a `{pkgdown}` site be created for this
#'   package? Defaults to the value of `github`, so that publicly available
#'   packages get a site by default.
#'
#' @param coc `[flag]` Should a Code of Conduct be added to the package?
#'   Defaults to the value of `github`, so that publicly available packages get
#'   a Code of Conduct by default.
#'
#' @param fledge `[flag]` Should `{fledge}` be used for package NEWS and
#'   versions? The default is `FALSE`, since I only use NEWS and version tags
#'   for packages that I plan to distribute.
#'
#' @param cran_badge `[flag]` Should a badge indicating CRAN status be added to
#'   the README? Defaults to the value of `fledge`, since both are mainly used
#'   for packages that I plan to distribute.
#' @param open `[flag]` Should the package open in a new RStudio window once
#'   setup is complete?
#' @param creator `[string]` The name of the package creator (aka you). This
#'   is used to fill in the Authors field in the package Description, as well
#'   as the contact for the Code of Conduct (if used) and the copyright holder
#'   of the license (if applicable).
#'
#' @param ... Additional arguments to pass to the `license` function
#'
#' @return invisible `NULL`
#'
#' @export
create_package <- function(
    path,
    license = "mit",
    renv = TRUE,
    github = TRUE,
    pkgdown = github,
    coc = github,
    fledge = FALSE,
    cran_badge = fledge,
    open = interactive(),
    creator = NULL,
    ...
) {
  assert_installed(c("usethis", "gert", "rlang"))
  # Creator
  usethis::ui_info("{usethis::ui_code('creator')} name is not specified")
  creator <- person_name(creator)
  # Create
  fields <- list(ByteCompile = "true")
  if (!is.null(creator)) {
    desc_default <- usethis::use_description_defaults()[["Authors@R"]]
    desc_default <- tryCatch(eval(str2expression(desc_default)), error = function(e) NULL)
    if (!(is.null(desc_default) || startsWith(desc_default, "First Last"))) {
      fields <- c(
        "Authors@R" = paste0("person('", desc_default, "', , , 'first.last@example.com', role = c('aut', 'cre'))"),
        fields
      )
    }
  }
  path <- usethis::create_package(
    path,
    fields = fields,
    rstudio = TRUE,
    open = FALSE
  )
  # Set project to new package
  usethis::local_project(path)
  # License
  if (!exists("copyright_holder") && !is.null(creator)) {
    copyright_holder <- creator
  }
  tryCatch(use_license(license, ...), error = function(e) warning(e$message))
  # Package doc
  usethis::use_package_doc(open = FALSE)
  # Development tools
  usethis::use_testthat()
  usethis::use_package("usethis", "Suggests")
  usethis::use_package("devtools", "Suggests")
  if (fledge) usethis::use_package("fledge", "Suggests")
  # renv
  if (renv) usethis::use_package("renv", "Suggests")
  if (renv && rlang::is_installed("renv")) {
    init_renv <- function() {
      wd <- getwd()
      on.exit(setwd(wd), add = TRUE)
      renv::init(project = path, bare = TRUE, restart = FALSE)
      renv::install(project = path, prompt = FALSE)
      renv::clean(
        project = path,
        actions = c("package.locks", "library.tempdirs", "unused.packages"),
        prompt = FALSE
      )
      renv::snapshot(project = path, prompt = FALSE)
    }
    init_renv()
  }
  # README
  usethis::use_readme_rmd(open = FALSE)
  # CRAN badge
  if (cran_badge) usethis::use_cran_badge()
  # Build README
  if (rlang::is_installed("devtools")) devtools::build_readme(path)
  # Tidy Description file
  usethis::use_tidy_description()
  # Git
  usethis::ui_done("Initializing Git repo")
  gert::git_init(path)
  usethis::ui_done("Committing files with message 'Initial commit'")
  gert::git_add(gert::git_status(staged = FALSE, repo = path)$file, repo = path)
  gert::git_commit("Initial commit", repo = path)
  default_branch <- gert::git_branch_list(local = TRUE, repo = path)$name
  # Github
  if (github) {
    usethis::ui_done("Initializing github repo")
    usethis::use_github()
    usethis::use_directory(".github", ignore = TRUE)
    usethis::use_git_ignore("*.html", directory = ".github")
    # Commit changes
    usethis::use_tidy_description()
    gert::git_add(gert::git_status(staged = FALSE, repo = path)$file, repo = path)
    gert::git_commit("Setup github", repo = path)
  }
  # Switch to setup branch to finish
  gert::git_branch_create("setup", checkout = TRUE, repo = path)
  # pkgdown
  if (pkgdown) {
    usethis::ui_done("Initializing pkgdown site")
    if (github) {
      use_pkgdown_github_pages(nopkgdown = TRUE)
    } else {
      usethis::use_pkgdown()
    }
    # Commit changes
    usethis::use_tidy_description()
    gert::git_add(gert::git_status(staged = FALSE, repo = path)$file, repo = path)
    gert::git_commit("Setup pkgdown", repo = path)
  }
  # Github actions
  if (github) {
    usethis::ui_done("Initializing Github Actions")
    usethis::use_coverage("codecov")
    use_github_action_test_coverage(notest = TRUE, skip_force = fledge)
    use_github_action_check_standard(notest = TRUE, skip_force = fledge)
    # Rebuild README
    if (rlang::is_installed("devtools")) devtools::build_readme(path)
    # Commit changes
    usethis::use_tidy_description()
    gert::git_add(gert::git_status(staged = FALSE, repo = path)$file, repo = path)
    gert::git_commit("Setup github actions", repo = path)
  }
  # Build documentation
  if (rlang::is_installed("devtools")) {
    devtools::document(path)
    usethis::use_tidy_description()
    files <- gert::git_status(staged = FALSE, repo = path)$file
    if (length(files) > 0L) gert::git_add(files)
    files <- gert::git_status(repo = path)$file
    if (length(files) > 0L) gert::git_commit("Build documentation")
  }
  # Code of Conduct
  if (coc) {
    coc_created <- use_coc(creator)
    if (coc_created) {
      coc_lines <- c("", readClipboard(13L))
      readme <- readLines(fs::path(path, "README.Rmd"))
      readme <- append(readme, coc_lines)
      writeLines(readme, fs::path(path, "README.Rmd"))
      if (rlang::is_installed("devtools")) devtools::build_readme(path)
    }
    # Commit changes
    usethis::use_tidy_description()
    files <- gert::git_status(staged = FALSE, repo = path)$file
    if (length(files) > 0L) gert::git_add(files)
    files <- gert::git_status(repo = path)$file
    if (length(files) > 0L) gert::git_commit("Add Code of Conduct")
  }
  # Merge setup branch
  gert::git_branch_checkout(default_branch, repo = path)
  if (github) {
    gert::git_merge_stage_only("setup", repo = path)
    gert::git_commit("Merge branch `setup` into `main`\n\n/notest /nopkgdown", repo = path)
    gert::git_pull(repo = path)
    gert::git_push(repo = path)
  } else {
    gert::git_merge("setup", repo = path)
  }
  code_block <- character()
  if (renv && !rlang::is_installed("renv")) {
    usethis::ui_todo("Initialize renv in your new package with")
    code_block <- paste0(
      " # Initialize renv\n",
      "renv::init(bare = TRUE)\n",
      "renv::install(prompt = FALSE)\n",
      "renv::clean(c('package.locks', 'library.tempdirs', 'unused.packages'), prompt = FALSE)\n",
      "renv::snapshot(prompt = FALSE)\n",
      "# Commit and push\n",
      "f <- gert::git_status(staged = FALSE)$file\n",
      "if (length(f) > 0L) gert::git_add(f)\n",
      "f <- gert::git_status()$file\n",
      "if (length(f) > 0L) {\n",
      "  gert::git_commit('Initialize renv\n\n/notest/nopkgdown')\n",
      "  gert::git_pull()\n",
      "  gert::git_push()\n",
      "}\n",
      "rm(f)\n"
    )
  }
  if (fledge) {
    usethis::ui_todo("Initialize fledge in your new package")
    code_block <- paste0(
      code_block,
      "# Bump\n",
      "fledge::bump_version()\n",
      "# Nothing to add yet - just close document\n",
      "rstudioapi::documentClose()\n",
      "# Finalize and push\n",
      "fledge::finalize_version(push = TRUE)"
    )
  }
  if (length(code_block) > 0L) usethis::ui_code_block(code_block)

  if (open) usethis::proj_activate(path)
}


use_license <- function(
    license,
    copyright_holder = Sys.getenv("smithr_name"),
    version = "latest",
    include_future = TRUE
) {
  assert_installed("usethis")
  licenses <- grep(
    "^use_[a-zA-Z0-9]+_license",
    getNamespaceExports("usethis"),
    value = TRUE
  )
  licenses <- sub("^use_", "", licenses)
  licenses <- sub("_license$", "", licenses)

  license <- match.arg(trimws(tolower(license)), choices = tolower(licenses))

  if (version == "latest") {
    version <- switch(
      license,
      "agpl" = formals(usethis::use_agpl_license)$version,
      "apache" = formals(usethis::use_apache_license)$version,
      "gpl" = formals(usethis::use_gpl_license)$version,
      "lgpl" = formals(usethis::use_lgpl_license)$version,
      version
    )
  }

  switch(
    license,
    "agpl" = usethis::use_agpl_license(version, include_future),
    "agpl3" = usethis::use_agpl3_license(),
    "apache" = usethis::use_apache_license(version, include_future),
    "apl2" = usethis::use_apl2_license(),
    "cc0" = usethis::use_cc0_license(),
    "ccby" = usethis::use_ccby_license(),
    "gpl" = usethis::use_gpl_license(version, include_future),
    "gpl3" = usethis::use_gpl3_license(),
    "lgpl" = usethis::use_lgpl_license(version, include_future),
    "mit" = usethis::use_mit_license(copyright_holder),
    "proprietary" = usethis::use_proprietary_license(copyright_holder),
    {
      license <- grep(license, licenses, ignore.case = TRUE, value = TRUE)
      fn <- paste0("use_", license, "_license")
      fn <- getFunction(fn, where = getNamespace("usethis"))
      args <- intersect(names(formals()), names(formals(fn)))
      args <- mget(args, ifnotfound = formals(fn)[args])
      do.call(license_fn, args)
    }
  )
}


use_coc <- function(contact = NULL, path = NULL) {
  assert_installed("usethis")
  usethis::ui_done("Adding Code of Conduct")
  if (is.null(contact)) {
    usethis::ui_info("Code of Conduct needs a contact name")
    contact <- person_name(contact)
  }
  if (is.null(contact)) {
    usethis::ui_todo(paste(
      "Code of Conduct not created.",
      "Run {usethis::ui_code('usethis::use_code_of_conduct(\'Contact Name\')')}",
      "and commit the changes if you still wish to create one."
    ))
    return(invisible(FALSE))
  }
  checkmate::assert_string(contact)
  usethis::use_code_of_conduct(contact, path = NULL)
  return(invisible(TRUE))
}


person_name <- function(name = NULL) {
  assert_installed("usethis")
  author <- NULL
  enter_msg <- "No name detected. Would you like to enter one?"

  if (is.null(name)) {
    author <- usethis::use_description_defaults()[["Authors@R"]]
    author <- tryCatch(
      checkmate::assert_string(eval(str2expression(author))),
      error = function(e) NULL
    )
    author <- tryCatch(
      checkmate::assert_string(trimws(sub("[<\\[(].*", "", author))),
      error = function(e) NULL
    )
    if ("First Last" %in% author) author <- NULL
  }

  if (!is.null(author)) {
    use_author <- usethis::ui_yeah(paste(
      "{usethis::ui_value(name)} was detected from description defaults.",
      "Would you like to use this name?"
    ))
    if (use_author) {
      name <- author
    } else {
      enter_msg <- "Would you like to enter a name instead?"
    }
  }

  if (is.null(name)) {
    enter_name <- usethis::ui_yeah(enter_msg)
    if (enter_name) name <- readline("Name: ")
  }

  if (!is.null(name)) checkmate::assert_string(name)
  name
}


use_github_action_check_standard <- function(
    save_as = "R-CMD-check.yaml",
    notest = TRUE,
    skip_force = FALSE,
    ref = NULL,
    ignore = TRUE
) {
  assert_installed(c("usethis", "fs"))
  checkmate::assert_flag(notest)
  result <- usethis::use_github_action_check_standard(
    save_as = save_as,
    ref = ref,
    ignore = ignore,
    open = FALSE
  )
  if (notest) {
    append_github_action_skip_flag(
      fs::path(".github", "workflows", save_as),
      flag = "/notest",
      after = "^[ \t]*R-CMD-check[ \t]*:[ \t]*$",
      skip_force = skip_force
    )
  }
  invisible(result)
}


use_github_action_test_coverage <- function(
    save_as = "test-coverage.yaml",
    notest = TRUE,
    skip_force = FALSE,
    ref = NULL,
    ignore = TRUE
) {
  assert_installed(c("usethis", "fs"))
  checkmate::assert_flag(notest)
  result <- usethis::use_github_action(
    "test-coverage",
    save_as = save_as,
    ref = ref,
    ignore = ignore,
    open = FALSE
  )
  if (notest) {
    append_github_action_skip_flag(
      fs::path(".github", "workflows", save_as),
      flag = "/notest",
      after = "^[ \t]*test-coverage[ \t]*:[ \t]*$",
      skip_force = skip_force
    )
  }
  invisible(result)
}


use_pkgdown_github_pages <- function(nopkgdown = TRUE) {
  assert_installed(c("usethis", "rstudioapi", "fs"))
  checkmate::assert_flag(nopkgdown)
  result <- usethis::use_pkgdown_github_pages()
  if (rstudioapi::isAvailable() && rstudioapi::hasFun("documentPath") && rstudioapi::hasFun("documentClose")) {
    doc_file <- fs::path_file(rstudioapi::documentPath())
    if (grepl("pkgdown", doc_file)) rstudioapi::documentClose()
  }
  if (nopkgdown) {
    append_github_action_skip_flag(
      fs::path(".github", "workflows", "pkgdown.yaml"),
      flag = "/nopkgdown",
      after = "^[ \t]*pkgdown[ \t]*:[ \t]*$",
      skip_force = FALSE
    )
  }
  invisible(result)
}


append_github_action_skip_flag <- function(file, flag, after, skip_force = FALSE) {
  checkmate::assert_file_exists(file)
  checkmate::assert_string(flag, min.chars = 2L)
  checkmate::assert_string(after)
  if (!startsWith(flag, "/")) stop("Assertion on ", checkmate::vname(flag), "failed: Must start with '/'.")
  checkmate::assert_string(after)
  checkmate::assert_flag(skip_force)
  condition <- paste0(
    "if: ${{ !contains(github.event.head_commit.message, '", flag, "')",
    if (skip_force) " && !github.event.forced",
    " }}"
  )
  yaml <- readLines(file)
  where <- grep(after, yaml, value = TRUE)
  if (length(where) == 0L) return(invisible(FALSE))
  condition <- paste0("  ", sub("[^ \t]+.*", "", where), condition)
  for (i in seq_along(where)) {
    yaml <- append(yaml, condition[[i]], after = grep(after, yaml)[[i]])
  }
  yaml <- paste0(yaml, collapse = "\n")
  writeLines(yaml, file)
  invisible(TRUE)
}
