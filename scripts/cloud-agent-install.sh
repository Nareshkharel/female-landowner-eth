#!/usr/bin/env bash
# Idempotent Cloud Agent bootstrap: install R and CRAN packages used by fies_rasch.R.
# Do not run the analysis scripts here. They depend on local ESS data that is not
# in this repository, and they use machine-specific paths.
set -euo pipefail

export DEBIAN_FRONTEND=noninteractive
CRAN_REPO="${CRAN_REPO:-https://cloud.r-project.org}"

if ! command -v Rscript >/dev/null 2>&1; then
  sudo apt-get update
  sudo apt-get install -y --no-install-recommends \
    r-base \
    r-base-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libicu-dev \
    zlib1g-dev
fi

Rscript --version

sudo Rscript -e "
pkgs <- c('dplyr', 'haven', 'RM.weights')
repos <- '${CRAN_REPO}'
missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing)) {
  install.packages(missing, repos = repos, dependencies = TRUE)
}
cat('R packages ready:', paste(pkgs, collapse = ', '), '\n')
"
