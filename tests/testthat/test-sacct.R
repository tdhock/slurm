library(slurm)
library(testthat)
context("sacct")

slurm.txt <- system.file(
  "extdata", "sacct-unrecognized-unit.txt", package="slurm", mustWork=TRUE)
test_that("sacct_fread works for prev unrecognized unit", {
  dt <- sacct_fread(slurm.txt)
  expect_is(dt, "data.table")
})

test_that("multi user data", {
  sacct.txt <- system.file(
    "extdata", "sacct-multiuser.txt", package="slurm", mustWork=TRUE)
  sacct.dt <- sacct_fread(file=sacct.txt)
  task.dt <- sacct_tasks(sacct.dt)
  expect_is(task.dt, "data.table")
})

test_that("lots of columns", {
  sacct.txt <- system.file(
    "extdata", "sacct-manyfields.txt", package="slurm", mustWork=TRUE)
  sacct.dt <- sacct_fread(file=sacct.txt)
  expect_equal(nrow(sacct.dt), 3)
})

test_that("no tasks is OK", {
  sacct.dt <- sacct_fread(text="JobID|ExitCode|State|MaxRSS|Elapsed\n")
  expect_equal(nrow(sacct.dt), 0)
})

test_that("sacct works with all columns", {
  sacct.txt <- system.file(
    "extdata", "sacct-all-cols.txt", package="slurm", mustWork=TRUE)
  sacct.dt <- sacct_fread(sacct.txt)
  expect_is(sacct.dt, "data.table")
})

test_that("sacct works with all columns", {
  raw.csv <- system.file(
    "extdata", "sacct_tasks_input_multi_state.csv", package="slurm", mustWork=TRUE)
  raw.dt <- data.table::fread(raw.csv)
  task.dt <- sacct_tasks(raw.dt)
  expect_is(task.dt$State_blank, "character")
  summary.dt <- sjob_dt(task.dt)
  expect_equal(summary.dt$job, rep(8046989L, 2))
})

