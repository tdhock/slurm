library(slurm)
library(testthat)
context("sacct")

slurm.txt <- system.file(
  "extdata", "slurm-unrecognized-unit.txt", package="slurm", mustWork=TRUE)
test_that("sacct_fread works for prev unrecognized unit", {
  dt <- sacct_fread(slurm.txt)
  expect_is(dt, "data.table")
})


test_that("job ID field parsed correctly", {
  sacct.csv.gz <- system.file(
    "data", "sacct-multitype.csv.gz", package="slurm", mustWork=TRUE)
  cmd <- paste("zcat", sacct.csv.gz)
  task.dt <- sacct_fread(cmd=cmd)
  ord.dt <- task.dt[order(job, task)]
  expect_identical(ord.dt$job, as.integer(c(
    13937810, 14022192, 14022192, 14022192, 14022204)))
  expect_identical(ord.dt$task, as.integer(c(
    25, 1, 2, 3, 4)))
})

test_that("job ID suffixes both optional", {
  sacct.csv.gz <- system.file(
    "data", "sacct-2019-11-21.csv.gz", package="slurm", mustWork=TRUE)
  cmd <- paste("zcat", sacct.csv.gz)
  task.dt <- sacct_fread(cmd=cmd)
  task.uniq <- unique(task.dt[, .(job, task)])
  expect_equal(nrow(task.dt), nrow(task.uniq))
})

test_that("all NA column read as character", {
  sacct.txt <- system.file(
    "extdata", "slurm-multistep.txt", package="slurm", mustWork=TRUE)
  task.dt <- sacct_fread(file=sacct.txt)
  expect_equal(task.dt$task, 1:999)
})

test_that("multi user data", {
  sacct.txt <- system.file(
    "extdata", "slurm-multiuser.txt", package="slurm", mustWork=TRUE)
  task.dt <- sacct_fread(file=sacct.txt)
  expect_equal(task.dt$task, 1:999)
})

test_that("no tasks is OK", {
  task.dt <- sacct_fread(text="JobID|ExitCode|State|MaxRSS|Elapsed\n")
  expect_equal(nrow(task.dt), 0)
})

