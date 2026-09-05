# female-landowner-eth

Statistical analysis code for a thesis on female land ownership, food security, and
agricultural practices using the Ethiopia 2021 ESPS Wave 5 (LSMS) household survey.

Contents:
- `fies_rasch.R` — R script: weighted Rasch model of the 8 FIES food-insecurity items via `RM.weights`.
- `*.do` — Stata scripts (`ethiopia_landowner.do`, `female landowner analysis.do`, `Pca_wealthindex.do`): data cleaning, wealth-index PCA, and logit/OLS regressions.

There is no application, build system, or test suite — these are run-by-hand analysis scripts.

## Cursor Cloud specific instructions

- **R is the only reproducible toolchain here.** The required CRAN packages (`dplyr`, `haven`,
  `RM.weights`) are installed into the system site-library by the startup update script, so
  `Rscript fies_rasch.R`-style work runs without per-session installs.
- **The survey data is NOT in this repo.** Both the R and Stata scripts hardcode absolute
  paths under `/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/...`
  (proprietary LSMS `.dta` files). Running a script as-is fails at the `read_dta`/`use` step.
  To exercise the R pipeline end-to-end without the real data, build a synthetic 8-item FIES
  matrix (items recorded 1=yes/2=no, recoded to 1/0) and feed it to `RM.w()` — this mirrors
  `fies_rasch.R` exactly. `RM.w()` returns `$infit`, `$outfit`, and `$res.corr`.
- **Stata `.do` files cannot run here.** Stata is proprietary commercial software with no
  free Linux install, so `ethiopia_landowner.do`, `female landowner analysis.do`, and
  `Pca_wealthindex.do` cannot be executed in this environment. They also depend on the
  missing local `.dta` files and the `outreg2`/`factortest` SSC packages.
- The `fs` R package (pulled in transitively by `RM.weights` → `Hmisc`) compiles against
  libuv; `libuv1-dev` plus `libcurl4-openssl-dev`/`libssl-dev`/`libxml2-dev` are required
  system build deps and are baked into the VM snapshot.
- Lint/syntax check an R script without running it: `Rscript -e 'parse("fies_rasch.R")'`.
