# Dependency manifest helpers for the cleaned workflow.

dependency_manifest_path <- function(path = file.path("config", "dependencies.csv")) {
  path
}

read_dependency_manifest <- function(path = dependency_manifest_path()) {
  if (!file.exists(path)) {
    stop("Dependency manifest does not exist: ", path, call. = FALSE)
  }

  manifest <- utils::read.csv(path, stringsAsFactors = FALSE, strip.white = TRUE)
  required_columns <- c("package", "role", "required_for", "install_source", "notes")
  missing_columns <- setdiff(required_columns, names(manifest))
  if (length(missing_columns)) {
    stop("Dependency manifest is missing columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)
  }

  manifest <- manifest[required_columns]
  for (column in required_columns) {
    manifest[[column]] <- trimws(manifest[[column]])
  }
  if (any(!nzchar(manifest$package))) {
    stop("Dependency manifest contains an empty package name.", call. = FALSE)
  }
  if (any(!nzchar(manifest$role))) {
    stop("Dependency manifest contains an empty role.", call. = FALSE)
  }
  duplicated_packages <- unique(manifest$package[duplicated(manifest$package)])
  if (length(duplicated_packages)) {
    stop("Dependency manifest contains duplicate packages: ", paste(duplicated_packages, collapse = ", "), call. = FALSE)
  }

  manifest
}

split_dependency_values <- function(values) {
  values <- unlist(strsplit(paste(values, collapse = ","), ",", fixed = TRUE), use.names = FALSE)
  values <- trimws(values)
  values[nzchar(values)]
}

filter_dependency_manifest <- function(manifest, roles = NULL, packages = NULL) {
  if (!is.null(roles)) {
    roles <- split_dependency_values(roles)
    manifest <- manifest[manifest$role %in% roles, , drop = FALSE]
  }
  if (!is.null(packages)) {
    packages <- split_dependency_values(packages)
    manifest <- manifest[manifest$package %in% packages, , drop = FALSE]
  }
  manifest
}

dependency_status <- function(manifest = read_dependency_manifest()) {
  installed <- vapply(manifest$package, requireNamespace, logical(1), quietly = TRUE)
  version <- vapply(manifest$package, function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      return(NA_character_)
    }
    as.character(utils::packageVersion(package))
  }, character(1))

  data.frame(
    manifest,
    installed = unname(installed),
    version = unname(version),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

missing_dependencies <- function(status, roles = NULL, packages = NULL) {
  status <- filter_dependency_manifest(status, roles = roles, packages = packages)
  status[!status$installed, , drop = FALSE]
}

format_missing_dependencies <- function(status) {
  if (!nrow(status)) {
    return("none")
  }
  rows <- split(status$package, status$role)
  paste(
    sprintf("%s: %s", names(rows), vapply(rows, paste, character(1), collapse = ", ")),
    collapse = "; "
  )
}

write_dependency_status <- function(status, root = "results") {
  path <- if (exists("result_table_path", mode = "function")) {
    result_table_path("dependency_status", root = root)
  } else {
    file.path(root, "tables", "dependency_status.csv")
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(status, path, row.names = FALSE)
  path
}
