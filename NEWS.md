# misha.ext 0.1.5

* **Database corruption fix:** `gset_genome()` always calls `gsetroot()` now. Its memoized fast path (`force = FALSE`) restored only part of the misha session - leaving the previous genome's chromosome aliases and dataset maps in place, and replaying a track listing that went stale as soon as a track was added. That stale listing could then be written into the current database's `.db.cache`, so every other user of that database saw the wrong tracks. The `force` argument is now ignored and kept only for backward compatibility.
* Added `gtrack.create_kmer()`: builds a dense k-mer count/fraction track in one call. Whole-hg38 GC-content tracks complete in seconds. Supports multi-kmer summation (e.g. `c("G", "C")` for GC content) and an optional sliding `window`.
* `gseq.create_track` is now defunct. Use `gtrack.create_kmer` instead - it is faster, produces a dense track, and supports multi-kmer summation and sliding windows.

# misha.ext 0.1.4

* fix: `fwrite_ucsc` did not remove chroms outside the boundries when `span=NULL`

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
