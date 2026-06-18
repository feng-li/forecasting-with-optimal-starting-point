args <- commandArgs(trailingOnly = TRUE)

has_arg <- function(name) {
  paste0("--", name) %in% args
}

arg_value <- function(name, default = NULL) {
  pattern <- paste0("^--", name, "=")
  value <- args[grepl(pattern, args)]
  if (length(value)) {
    return(sub(pattern, "", value[[length(value)]]))
  }
  default
}

`%||%` <- function(lhs, rhs) {
  if (is.null(lhs) || length(lhs) == 0L || !nzchar(lhs[[1L]])) rhs else lhs
}

default_cran_repo <- function() {
  repos <- getOption("repos")
  if (!is.null(repos) && "CRAN" %in% names(repos) && nzchar(repos[["CRAN"]]) && !identical(repos[["CRAN"]], "@CRAN@")) {
    return(repos[["CRAN"]])
  }
  "https://cloud.r-project.org"
}

source(file.path("R", "dependencies.R"))

manifest <- read_dependency_manifest(arg_value("manifest", dependency_manifest_path()))
role_arg <- arg_value("role", "all")
roles <- if (identical(role_arg, "all")) NULL else role_arg
packages <- arg_value("package", NULL)
selected <- filter_dependency_manifest(manifest, roles = roles, packages = packages)

if (!nrow(selected)) {
  stop("No dependencies matched the requested filters.", call. = FALSE)
}

status <- dependency_status(selected)
missing <- missing_dependencies(status)
print(status[c("package", "role", "installed", "version", "install_source")], row.names = FALSE)

if (!nrow(missing)) {
  message("All selected dependencies are already installed.")
  quit(save = "no", status = 0L)
}

message("Missing selected dependencies: ", format_missing_dependencies(missing))

if (has_arg("dry-run")) {
  manual <- missing[!missing$install_source %in% c("CRAN", "CRAN-or-external"), , drop = FALSE]
  if (nrow(manual)) {
    message("Manual or external dependencies will not be installed by this script: ", paste(manual$package, collapse = ", "))
  }
  message("Dry run only; no packages were installed.")
  quit(save = "no", status = 0L)
}

if (!has_arg("yes")) {
  stop("Refusing to install without --yes. Re-run with --dry-run to inspect or --yes to install.", call. = FALSE)
}

repos <- arg_value("repos", default_cran_repo())
options(repos = c(CRAN = repos))

installable <- missing[missing$install_source %in% c("CRAN", "CRAN-or-external"), , drop = FALSE]
manual <- missing[!missing$install_source %in% c("CRAN", "CRAN-or-external"), , drop = FALSE]

if (nrow(manual)) {
  message("Manual or external dependencies are still required: ", paste(manual$package, collapse = ", "))
}
if (!nrow(installable)) {
  stop("No missing selected dependencies are marked as CRAN-installable.", call. = FALSE)
}

utils::install.packages(installable$package, repos = repos)

post_status <- dependency_status(selected)
post_missing <- missing_dependencies(post_status)
if (nrow(post_missing)) {
  stop("Packages still missing after install attempt: ", paste(post_missing$package, collapse = ", "), call. = FALSE)
}

message("Installed selected dependencies.")
