# randomMachines

## Package conventions

The package uses a layered design: an **S4 core** (formal classes, validity,
multiple dispatch) wrapped by a functional, S3-friendly user API. S4 is the
natural fit for the core because `kernlab` — which the package builds on — is
itself an S4 package, and S4 provides validity checks and dispatch for the
model ensemble. The user-facing surface follows the
[Tidyverse style guide](https://style.tidyverse.org/) so the package can
integrate cleanly with `tidymodels` (mostly S3). The two coexist because S4
objects carry an S3-compatible class. Concretely: S4 class names use
`UpperCamelCase` (neutral to both Bioconductor and the tidyverse), while
functions, arguments, and slots use `snake_case`, and existing generics are
reused before new ones are defined.

| Entity | Convention | Example | Notes |
|---|---|---|---|
| Files | kebab-case, topic-prefixed | `sampling-bootstrap.R`, `utils-math.R` | Class files: `RandomMachines-class.R` |
| S4 classes | `UpperCamelCase`, noun | `RandomMachines`, `RMEnsemble` | Same in Bioconductor and tidyverse — no conflict |
| Virtual/base classes | `UpperCamelCase` | `RMModel` (virtual parent) | |
| Generics | `snake_case`, lowercase | `predict`, `fit`, `sample_partitions` | Reuse existing generics (`predict`, `coef`, `show`) before inventing |
| Methods | follow the generic | — | Documented with `@rdname`, not separately |
| Exported functions | `snake_case`, verb-led | `compute_entropy()`, `scale_exponential()` | |
| Main constructor | matches package name | `randomMachines()` | Sanctioned exception to snake_case; keep its arguments snake_case |
| Internal helpers | `snake_case`, no prefix | `nearest_enemy_distance()` | Don't `@export`; tag `@noRd` to skip man pages |
| Slots | `snake_case`, noun | `kernels`, `class_weights` | Never accessed via `@` outside the class's own file |
| Accessors | noun = slot name | `kernels(object)` | Setter: `kernels(object) <- value` |
| Arguments / locals | `snake_case` | `n_boot`, `train_data` | |
| Method's primary arg | `object` (S4) / `x` (`predict`) | — | Match the generic's existing signature exactly |
| Constants | `UPPER_SNAKE` | `DEFAULT_KERNELS` | Keep few; prefer function defaults |
| Test files | `test-` + source file | `test-sampling-bootstrap.R` | |

### File naming — precedent

The kebab-case file convention above is verified against the following
packages (their `R/` directories were inspected directly):

| Package | Filename convention observed |
|---|---|
| **dplyr** 1.2.1 (tidyverse) | kebab — `bind-cols.R`, `case-when.R`, `compute-collect.R`, `compat-dbplyr.R` |
| **ggplot2** 4.0.3 (tidyverse) | kebab — `aes-colour-fill-alpha.R`, `annotation-borders.R`, `all-classes.R` |
| **GenomicRanges** 1.64.0 (Bioconductor S4) | kebab class/method files — `GRanges-class.R`, `GRangesList-class.R`, `findOverlaps-methods.R` |
| **SummarizedExperiment** 1.42.0 (Bioconductor S4) | `SummarizedExperiment-class.R`, `Assays-class.R`, `combine-methods.R`, `zzz.R` |
| **sp** 2.2-1 (CRAN S4) | `Class-SpatialPoints.R`, `Class-SpatialGrid.R`, `CRS-methods.R` |

For honesty: **parsnip** and **bonsai** (parsnip-extension packages within
tidymodels) use snake_case files instead — an internal divergence within the
tidyverse ecosystem, not a rule to follow.

**Why kebab-case:**

1. The `-class.R` and `-methods.R` suffixes for S4 files are already kebab
   and universal across Bioconductor — that part is fixed regardless.
2. Adopting kebab for the rest of `R/` avoids mixing two separator styles in
   the same directory.
3. Kebab matches the flagship tidyverse packages (dplyr, ggplot2) and every
   modern Bioconductor S4 package inspected.
4. Filename casing and identifier casing are independent namespaces — file
   `utils-math.R` containing function `compute_entropy()` is not a
   contradiction.

**Sources:**
[Tidyverse style guide — package files](https://style.tidyverse.org/package-files.html);
[Bioconductor coding style](https://contributions.bioconductor.org/r-code.html);
package sources from
[CRAN](https://cloud.r-project.org/src/contrib/) and
[Bioconductor](https://bioconductor.org/packages/release/bioc/src/contrib/).

### Suggested file layout

The table below is a suggested refactor of the current `R/` files to match the
conventions above. Files are renamed to kebab-case with a topic prefix, and
related helpers are grouped. Everything about a class — `setClass`,
`setValidity`, accessor generics, the constructor, accessors, and the `show`
method — lives in a single `<Class>-class.R` file. `RM_classifier.R` is split
because S4 requires classes and generics to be defined *before* the methods
that use them; that ordering is declared with roxygen2 `@include` tags (e.g.
`fit.R` and `predict.R` each carry `@include RandomMachines-class.R`), which
`devtools::document()` turns into the package's `Collate:` field.

| Current file | Proposed file(s) |
|---|---|
| `RM_classifier.R` | `RandomMachines-class.R`, `fit.R`, `predict.R` |
| `bootstrap_sampler.R` | `sampling-bootstrap.R` |
| `localized_sampling.R` | `sampling-localized.R` |
| `nearest_enemy_sampling.R`, `nearest_enemy_distance.R` | `sampling-nearest-enemy.R` |
| `create_partitions.R`, `stratified_kfold.R` | `partitions.R` |
| `accuracy.R` | `utils-metrics.R` |
| `class_entropy.R`, `class_props.R`, `log_normalizing.R`, `scale_exponential.R` | `utils-math.R` |
| `sys_o2.R`, `sys_pc.R` | `utils-math.R` or `kernel-weights.R` (depending on contents) |

## References

### Package development

- [Advanced R](https://adv-r.hadley.nz/environments.html?q=package#search-path)
- [R Packages](https://r-pkgs.org/)
- [Writing R extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html)
- [S7 (sucessor to S3 and S4) reference](https://rconsortium.github.io/S7/)_

### OOP/Software Development
- [Mastering Software Development in R](https://bookdown.org/rdpeng/RProgDA/)
- [Bioconductor course material](https://bioconductor.org/help/course-materials/)
- [Clean Architecture (Good for quick introduction to other paradigms as well)](https://github.com/GunterMueller/Books-3/blob/master/Clean%20Architecture%20A%20Craftsman%20Guide%20to%20Software%20Structure%20and%20Design.pdf)

## Adopted conventions
- [Diataxis Documentation](https://diataxis.fr/)
- [Bioconductor guidelines - S4](https://contributions.bioconductor.org/)
- [Tidyverse style guide](https://style.tidyverse.org/)
