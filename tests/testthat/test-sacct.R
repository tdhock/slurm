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
  ord.dt <- task.dt[order(JobID.job, task)]
  expect_identical(ord.dt$JobID.job, as.integer(c(
    13937810, 14022192, 14022192, 14022192, 14022204)))
  expect_identical(ord.dt$task, as.integer(c(
    25, 1, 2, 3, 4)))
})
