root <- Sys.getenv("PROJECT_ROOT")
if (!nzchar(root)) {
  candidates <- c(
    normalizePath(getwd(), mustWork = FALSE),
    normalizePath(file.path(getwd(), "..", ".."), mustWork = FALSE)
  )
  root <- candidates[file.exists(file.path(candidates, "R", "io.R"))][[1L]]
}
options(osp.project_root = root)

source(file.path(root, "R", "io.R"))
source(file.path(root, "R", "config.R"))
source(file.path(root, "R", "paths.R"))
source(file.path(root, "R", "dependencies.R"))
source(file.path(root, "R", "artifacts.R"))
source(file.path(root, "R", "intervals.R"))
source(file.path(root, "R", "labels.R"))
source(file.path(root, "R", "evaluation.R"))
source(file.path(root, "R", "forecast_candidates.R"))
source(file.path(root, "R", "features.R"))
source(file.path(root, "R", "osp_models.R"))
source(file.path(root, "R", "m4_workflow.R"))
source(file.path(root, "R", "empirical_workflow.R"))
source(file.path(root, "R", "paper.R"))
source(file.path(root, "R", "pipeline.R"))
