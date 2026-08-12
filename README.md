# randomMachines

`randomMachines` fits a weighted ensemble of kernel support vector machines. It
combines several kernels rather than committing to one, in two stages:

1. **Kernel lambdas.** Cross-validate every candidate kernel and map its mean
   out-of-fold performance to a selection probability (λ).
2. **Bootstrap omegas.** Draw `B` bootstrap replicates, fit one λ-sampled kernel
   per replicate, and weight the fitted models by their out-of-bag performance (ω).

Prediction scores new data with every bootstrap model and combines the results by
their ω, giving a weighted majority vote, an averaged class-probability matrix, or
a weighted mean for binary, multiclass, probabilistic and regression tasks.

```r
rm   <- random_machines(iris, formula = Species ~ ., task = "multiclass")
pred <- predict(rm, iris)
```

The single entry point `random_machines()` builds a specification and fits the
ensemble, returning a self-contained `RandomMachines` object. See `ARCHITECTURE.md`
for the class model and the fit and prediction pipelines.

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
