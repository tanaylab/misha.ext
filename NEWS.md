# misha.ext 0.1.3

* Fixed a bug in `gtrack.import_mappedseq_bam` where the `track` parameter was not being passed to `gtrack.import_mappedseq`.
* Fixed a bug in `fwrite_ucsc` where the `span` parameter caused intervals to exceed chromosome boundaries.

# misha.ext 0.1.2

* Added `gintervals.align` function.

# misha.ext 0.1.1

* Added `gintervals.mark_overlaps` and `gintervals.remove_overlaps` functions.

# misha.ext 0.1.0

* Added the `gdb.create_genome` function.

# misha.ext 0.0.10

* Changed the default of `force` parameter in `gset_genome` to `TRUE`

# misha.ext 0.0.6

* Added `grandom_genome` function
* Added a `NEWS.md` file to track changes to the package.
