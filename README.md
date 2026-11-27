
<!-- README.md is generated from README.Rmd. Please edit that file -->

# misha.ext

<!-- badges: start -->

[![Lifecycle:
retired](https://img.shields.io/badge/lifecycle-retired-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#superseded)
<!-- badges: end -->

**misha.ext is retired.** Its functions still work and give the same
results, but each one warns, once per session, with what to use instead.
New code should use [misha](https://github.com/tanaylab/misha) (R) or
[pymisha](https://github.com/tanaylab/pymisha) (Python) directly.

## What to use instead

| misha.ext | Use instead |
|----|----|
| `get_promoters()` | `misha::gintervals.promoters("intervs.global.tss", upstream, downstream)` (misha \>= 5.21.0). Windows are `upstream + downstream` bp; strand 0 covers both directions |
| `gintervals.expand()` | `misha::gintervals.expand()` (misha \>= 5.21.0) |
| `gintervals.remove_overlaps()` | `misha::gintervals.remove_overlaps()` (misha \>= 5.21.0); keeps the input row order |
| `gintervals.centers(x)` | `misha::gintervals.normalize(x, 1)` |
| `gintervals.neighbors1(...)` | `misha::gintervals.neighbors(..., na.if.notfound = TRUE)` |
| `gintervals.normalize()` | `misha::gintervals.normalize()`; an odd size gives `size` bp, not `size - 1` |
| `gintervals.mark_overlaps()` | `misha::gintervals.mark_overlaps()` |
| `gextract.left_join(...)` | `misha::gextract(..., intervals_join = "intervals")` |
| `intervs_to_mat()` / `mat_to_intervs()` | `misha::gintervals.to_mat()` / `misha::gintervals.from_mat()` |
| `grandom_genome()` | `misha::gintervals.random()` |
| `gtrack.import_mappedseq_bam()` | `misha::gtrack.import_mappedseq()`, which reads BAM files itself; merge several with `samtools cat` first |
| `gcluster.run2()` | `misha::gcluster.run()`, with SGE resources in `opt.flags` |
| `gset_genome(genome)` | `misha::gsetroot(misha::gdb.genome_info(genome, registry = "~/.misha.yaml")$recipe$path)` |
| `genome_exists()`, `find_params_yaml()` | `misha::gdb.list_genomes(registry = "~/.misha.yaml")` |
| `gtrack.create_kmer()`, `gtrack.marginal_sum()` | `misha::gtrack.create()` over virtual tracks |
| `gvtrack.array_extract()` | `misha::gvtrack.array.slice()` virtual tracks with `misha::gextract()` |
| `gextract_meth()` | `func = "sum"` virtual tracks of the `.meth` and `.cov` tracks, divided |
| `fwrite_ucsc()`, `fwrite_igv()`, `fwrite_header()`, `gintervals.filter()`, `gintervals.align()`, `gintervals.distance()`, `convert_10x_peak_names_to_misha_intervals()`, `convert_misha_intervals_to_10x_peak_names()` | no replacement |
| `psum()`, `pmean()` | `tgutil::psum()`, `tgutil::pmean()` |

The Python versions of the misha functions have the same names with `_`
for `.` (`pymisha.gintervals_promoters`, …).

## Installation

``` r
remotes::install_github("tanaylab/misha.ext")
```
