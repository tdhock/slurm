library(slurm)
library(testthat)
context("sacct")

slurm.txt <- system.file(
  "extdata", "sacct-unrecognized-unit.txt", package="slurm", mustWork=TRUE)
test_that("sacct_fread works for prev unrecognized unit", {
  dt <- sacct_fread(slurm.txt, sep="|")
  expect_is(dt, "data.table")
})

test_that("job ID field parsed correctly", {
  sacct.csv.gz <- system.file(
    "data", "sacct-multitype.csv.gz", package="slurm", mustWork=TRUE)
  cmd <- paste("zcat", sacct.csv.gz)
  sacct.dt <- sacct_fread(cmd=cmd, sep="|")
  task.dt <- sacct_tasks(sacct.dt)
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
  sacct.dt <- sacct_fread(cmd=cmd, sep="|")
  task.dt <- sacct_tasks(sacct.dt)
  task.uniq <- unique(task.dt[, .(job, task)])
  expect_equal(nrow(task.dt), nrow(task.uniq))
})

test_that("all NA column read as character", {
  sacct.txt <- system.file(
    "extdata", "sacct-multistep.txt", package="slurm", mustWork=TRUE)
  sacct.dt <- sacct_fread(file=sacct.txt, sep="|")
  task.dt <- sacct_tasks(sacct.dt)
  expect_equal(task.dt$task, 1:999)
})

test_that("multi user data", {
  sacct.txt <- system.file(
    "extdata", "sacct-multiuser.txt", package="slurm", mustWork=TRUE)
  sacct.dt <- sacct_fread(file=sacct.txt, sep="|")
  task.dt <- sacct_tasks(sacct.dt)
  expect_is(task.dt, "data.table")
})

test_that("lots of columns", {
  sacct.txt <- system.file(
    "extdata", "sacct-manyfields.txt", package="slurm", mustWork=TRUE)
  sacct.dt <- sacct_fread(file=sacct.txt, sep="|")
  expect_equal(nrow(sacct.dt), 3)
})

test_that("no tasks is OK", {
  sacct.dt <- sacct_fread(text="JobID|ExitCode|State|MaxRSS|Elapsed\n", sep="|")
  expect_equal(nrow(sacct.dt), 0)
})

test_that("sacct works with all columns", {
  sacct.txt <- system.file(
    "extdata", "sacct-all-cols.txt", package="slurm", mustWork=TRUE)
  sacct.dt <- sacct_fread(sacct.txt)
})
