#' Runs R commands on a cluster that supports SGE
#'
#' @inheritParams misha::gcluster.run
#' @param command_list list of strings with R commands
#' @param packages packages to load for each command
#' @param jobs_title title of job names. names would have the title followed
#' by a serial number
#' @param job_names vector with the names of the jobs
#' @param collapse_results collapse return values of the jobs to a data frame.
#' if not possible - would return the usual list.
#'
#' @param memory memory requirments (would be called using \code{memory_flag})
#' @param mem_tot memory requirments for the machine (non-consumable)
#' @param threads threads requirments (would be called using \code{threads_flag})
#' @param io_saturation io_saturation requirments (would be called using \code{io_saturation_flag})
#' @param memory_flag flag for memory requirment (formatted as in glue)
#' @param threads_flag flag for threads requirment (formatted as in glue)
#' @param io_saturation_flag flag for io_saturation requirment (formatted as in glue)
#' @param verbose verbose output
#'
#' @return if collapse_results is TRUE: data frame with the results of all jobs (rbinded).
#' if collapse_results is FALSE returns the same as: \link[misha]{gcluster.run}
#'
#' @export
#' @seealso \link[misha]{gcluster.run}
gcluster.run2 <- function(...,
                          command_list = NULL,
                          opt.flags = "",
                          max.jobs = 400,
                          debug = FALSE,
                          R = paste0(R.home(component = "bin"), "/R"),
                          packages = NULL,
                          jobs_title = NULL,
                          job_names = NULL,
                          collapse_results = FALSE,
                          queue = NULL,
                          memory = NULL,
                          mem_tot = NULL,
                          threads = NULL,
                          io_saturation = NULL,
                          num_proc = NULL,
                          queue_flag = "-q {queue}",
                          memory_flag = "-l mem_free={memory}G",
                          mem_tot_flag = "-l mem_total={mem_tot}G",
                          threads_flag = "-pe threads {threads}",
                          io_saturation_flag = "-l io_saturation={io_saturation}",
                          num_proc_flag = "-l num_proc={num_proc}",
                          script = system.file("bin", "sgjob.sh", package = "misha.ext"),
                          verbose = FALSE) {
    if (!is.null(command_list)) {
        commands <- purrr::map(command_list, function(x) parse(text = x))
    } else {
        commands <- as.list(substitute(list(...))[-1L])
    }

    if (!is.null(queue)) {
        opt.flags <- paste(opt.flags, glue(queue_flag))
    }

    if (!is.null(memory)) {
        opt.flags <- paste(opt.flags, glue(memory_flag))
    }

    if (!is.null(mem_tot)) {
        opt.flags <- paste(opt.flags, glue(mem_tot_flag))
    }

    if (!is.null(threads)) {
        opt.flags <- paste(opt.flags, glue(threads_flag))
    }
    if (!is.null(io_saturation)) {
        opt.flags <- paste(opt.flags, glue(io_saturation_flag))
    }

    if (!is.null(num_proc)) {
        opt.flags <- paste(opt.flags, glue(num_proc_flag))
    }

    if (length(commands) < 1) {
        stop("Usage: gcluster.run2(..., command_list = NULL, opt.flags = \"\" max.jobs = 400, debug = FALSE)",
            call. = F
        )
    }
    if (!length(system("which qsub", ignore.stderr = T, intern = T))) {
        stop("gcluster.run2 must run on a host that supports Sun Grid Engine (qsub)",
            call. = F
        )
    }
    misha:::.gcheckroot()
    tmp.dirname <- ""
    submitted.jobs <- c()
    tryCatch(
        {
            tmp.dirname <- tempfile(pattern = "", tmpdir = paste(get("GROOT", envir = .misha),
                "/tmp",
                sep = ""
            ))
            if (!dir.create(tmp.dirname, recursive = T, mode = "0777")) {
                stop(sprintf("Failed to create a directory %s", tmp.dirname),
                    call. = F
                )
            }
            if (verbose) {
                cat(paste0("Temp directory: ", tmp.dirname, "\n"))
            }

            cat("Preparing for distribution...\n")
            save(.GLIBDIR, envir = .misha, file = paste(tmp.dirname, "libdir", sep = "/"))
            vars <- ls(all.names = TRUE, envir = parent.frame())
            envir <- parent.frame()
            while (!identical(envir, .GlobalEnv)) {
                envir <- parent.env(envir)
                if (!isNamespace(envir)) {
                    vars <- union(vars, ls(all.names = TRUE, envir = envir))
                }
            }

            suppressWarnings(save(list = vars, file = paste(tmp.dirname, "envir",
                sep = "/"
            ), envir = parent.frame()))
            .GSGECMD <- commands
            save(.GSGECMD, file = paste(tmp.dirname, "commands",
                sep = "/"
            ))
            opts <- options()
            save(opts, file = paste(tmp.dirname, "opts", sep = "/"))
            if (!is.null(packages)) {
                .GPACKAGES <- as.list(packages)
            } else {
                .GPACKAGES <- as.list(.packages())
            }
            save(.GPACKAGES, file = paste(tmp.dirname, "packages", sep = "/"))

            cat("Running the commands...\n")
            completed.jobs <- c()
            progress <- -1
            repeat {
                num.running.jobs <- length(submitted.jobs) - length(completed.jobs)
                if (length(submitted.jobs) < length(commands) &&
                    num.running.jobs < max.jobs) {
                    istart <- length(submitted.jobs) + 1
                    iend <- min(length(commands), istart + (max.jobs -
                        num.running.jobs) - 1)
                    for (i in istart:iend) {
                        out.file <- sprintf(
                            "%s/%d.out", tmp.dirname,
                            i
                        )
                        err.file <- sprintf(
                            "%s/%d.err", tmp.dirname,
                            i
                        )
                        if (!is.null(job_names)) {
                            job.name <- job_names[i]
                        } else if (!is.null(jobs_title)) {
                            job.name <- sprintf("%s_%s", jobs_title, i)
                        } else {
                            job.name <- sprintf("sgjob_%s", i)
                        }
                        command <- sprintf(
                            "qsub -terse -cwd -S /bin/bash -N %s -o %s -e %s -V %s %s %d '%s' '%s'",
                            job.name, out.file, err.file, opt.flags, script, i,
                            tmp.dirname, R
                        )
                        jobid <- system(command, intern = TRUE)
                        if (length(jobid) != 1) {
                            stop("Failed to run qsub", call. = FALSE)
                        }
                        if (debug) {
                            cat(sprintf(
                                "\tSubmitted job %d (id: %s)\n",
                                i, jobid
                            ))
                        }
                        submitted.jobs <- c(submitted.jobs, jobid)
                    }
                }
                Sys.sleep(3)
                running.jobs <- misha:::.gcluster.running.jobs(submitted.jobs)
                old.completed.jobs <- completed.jobs
                completed.jobs <- setdiff(submitted.jobs, running.jobs)
                if (debug) {
                    delta.jobs <- setdiff(completed.jobs, old.completed.jobs)
                    if (length(delta.jobs) > 0) {
                        for (jobid in delta.jobs) {
                            cat(sprintf(
                                "\tJob %d (id: %s) completed\n",
                                match(jobid, submitted.jobs), jobid
                            ))
                        }
                    }
                    if (!length(running.jobs) && length(submitted.jobs) ==
                        length(commands)) {
                        break
                    }
                    new.progress <- length(completed.jobs)
                    if (new.progress != progress) {
                        progress <- new.progress
                        cat(sprintf(
                            "\t%d job(s) still in progress\n",
                            length(commands) - progress
                        ))
                    }
                } else {
                    if (!length(running.jobs) && length(submitted.jobs) ==
                        length(commands)) {
                        break
                    }
                    new.progress <- as.integer(100 * length(completed.jobs) / length(commands))
                    if (new.progress != progress) {
                        progress <- new.progress
                        cat(sprintf("%d%%...", progress))
                    } else {
                        cat(".")
                    }
                }
            }
            if (!debug && progress != -1 && progress != 100) {
                cat("100%\n")
            }
        },
        interrupt = function(interrupt) {
            cat("\n")
            stop("Command interrupted!", call. = FALSE)
        },
        finally = {
            if (length(submitted.jobs) > 0) {
                running.jobs <- misha:::.gcluster.running.jobs(submitted.jobs)
                answer <- c()
                for (i in 1:length(commands)) {
                    res <- list()
                    res$exit.status <- NA
                    res$retv <- NA
                    res$stdout <- NA
                    res$stderr <- NA
                    if (submitted.jobs[i] %in% running.jobs) {
                        res$exit.status <- "interrupted"
                    } else {
                        fname <- sprintf(
                            "%s/%d.retv", tmp.dirname,
                            i
                        )
                        if (file.exists(fname)) {
                            load(fname)
                            res$exit.status <- "success"
                            res$retv <- retv
                        } else {
                            res$exit.status <- "failure"
                        }
                    }
                    out.file <- sprintf(
                        "%s/%d.out", tmp.dirname,
                        i
                    )
                    if (file.exists(out.file)) {
                        f <- file(out.file, "rc")
                        res$stdout <- readChar(f, 1000000)
                        close(f)
                    }
                    err.file <- sprintf(
                        "%s/%d.err", tmp.dirname,
                        i
                    )
                    if (file.exists(err.file)) {
                        f <- file(err.file, "rc")
                        res$stderr <- readChar(f, 1000000)
                        close(f)
                    }
                    answer[[i]] <- res
                }
                for (job in running.jobs) {
                    system(sprintf(
                        "qdel %s",
                        job
                    ), ignore.stderr = T, intern = T)
                }
                unlink(tmp.dirname, recursive = TRUE)

                if (collapse_results) {
                    canswer <- tryCatch(
                        purrr::map_df(answer, function(x) x$retv),
                        error = function(e) {
                            message("returning original output due to an error. collapse your reults manually (are all the parts data frames?)")
                            return(NULL)
                        }
                    )

                    if (!is.null(canswer)) {
                        return(canswer)
                    }
                }

                return(answer)
            }
            unlink(tmp.dirname, recursive = TRUE)
        }
    )
}
