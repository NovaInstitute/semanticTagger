#' BERTopic environment readiness check
#'
#' @description
#' Utility to validate that the Python dependencies required by BERTopic are
#' visible from `reticulate` before running [fit_bertopic_topics()].
#'
#' @param verbose Logical. If `TRUE`, prints setup guidance when dependencies
#'   are missing.
#'
#' @return Named logical vector describing module availability.
#' @export
check_bertopic_environment <- function(verbose = interactive()) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required for BERTopic integration.", call. = FALSE)
  }

  status <- c(
    bertopic = reticulate::py_module_available("bertopic"),
    sentence_transformers = reticulate::py_module_available("sentence_transformers"),
    umap = reticulate::py_module_available("umap"),
    hdbscan = reticulate::py_module_available("hdbscan"),
    sklearn = reticulate::py_module_available("sklearn")
  )

  missing <- names(status)[!status]
  if (isTRUE(verbose) && length(missing) > 0) {
    message(
      "Missing Python modules: ", paste(missing, collapse = ", "), "\n",
      "Run install_bertopic_environment() once, then re-run check_bertopic_environment()."
    )
  }

  status
}

#' Ensure BERTopic Python packages are available in current reticulate Python
#'
#' @description
#' Declares the BERTopic dependency set via [reticulate::py_require()] so
#' `reticulate` can resolve modules in the active Python environment/session.
#' Also applies conservative thread/runtime defaults to reduce OpenMP-related
#' instability in long R + Python mixed sessions.
#'
#' @return Invisibly returns `NULL`.
#' @export
ensure_bertopic_python <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required for BERTopic integration.", call. = FALSE)
  }

  stabilize_bertopic_runtime()

  reticulate::py_require(c(
    "bertopic",
    "sentence-transformers",
    "umap-learn",
    "hdbscan",
    "scikit-learn"
  ))

  invisible(NULL)
}

#' Apply conservative runtime defaults for BERTopic/UMAP/HDBSCAN execution
#'
#' @description
#' BERTopic dimensionality reduction and clustering run in native Python
#' libraries (NumPy/BLAS/numba/OpenMP). In some mixed R+Python environments,
#' aggressive threading or runtime mismatches can terminate the host R session.
#' This helper sets safe defaults when the variables are not already defined.
#'
#' @return Invisibly returns the effective values applied or already present.
#' @export
stabilize_bertopic_runtime <- function() {
  defaults <- c(
    OMP_NUM_THREADS = "1",
    MKL_NUM_THREADS = "1",
    OPENBLAS_NUM_THREADS = "1",
    NUMEXPR_NUM_THREADS = "1",
    VECLIB_MAXIMUM_THREADS = "1",
    BLIS_NUM_THREADS = "1",
    NUMBA_NUM_THREADS = "1",
    NUMBA_THREADING_LAYER = "workqueue",
    TOKENIZERS_PARALLELISM = "false"
  )

  current <- Sys.getenv(names(defaults), unset = "")
  missing <- !nzchar(current)

  if (any(missing)) {
    do.call(Sys.setenv, as.list(defaults[missing]))
    current[missing] <- defaults[missing]
  }

  invisible(as.list(current))
}

#' Install Python dependencies required for BERTopic mode
#'
#' @description
#' Creates/updates a dedicated Python environment used by `reticulate`, then
#' installs the BERTopic dependency set so [fit_bertopic_topics()] can run.
#'
#' @param envname Name of the Python environment to create/use.
#' @param method Python environment backend. One of `"auto"`, `"virtualenv"`,
#'   or `"conda"`.
#' @param python_version Optional Python version passed through to
#'   [reticulate::install_miniconda()].
#' @param pip Logical. Forwarded to [reticulate::py_install()].
#'
#' @return Invisibly returns [check_bertopic_environment()] output after install.
#' @export
install_bertopic_environment <- function(
    envname = "r-novatagger-bertopic",
    method = c("auto", "virtualenv", "conda"),
    python_version = NULL,
    pip = TRUE
) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required for BERTopic integration.", call. = FALSE)
  }

  method <- match.arg(method)
  py_pkgs <- c(
    "bertopic",
    "sentence-transformers",
    "umap-learn",
    "hdbscan",
    "scikit-learn"
  )

  if (method %in% c("auto", "conda")) {
    if (!dir.exists(reticulate::miniconda_path())) {
      reticulate::install_miniconda(python_version = python_version)
    }
  }

  install_method <- if (method == "auto") "auto" else method
  reticulate::py_install(
    packages = py_pkgs,
    envname = envname,
    method = install_method,
    pip = pip
  )

  if (install_method == "conda") {
    reticulate::use_condaenv(envname, required = TRUE)
  } else if (install_method == "virtualenv") {
    reticulate::use_virtualenv(envname, required = TRUE)
  } else {
    # "auto" chooses the best backend available; try conda first, then virtualenv.
    try(reticulate::use_condaenv(envname, required = FALSE), silent = TRUE)
    try(reticulate::use_virtualenv(envname, required = FALSE), silent = TRUE)
  }

  invisible(check_bertopic_environment(verbose = TRUE))
}
