library(slurm)
library(testthat)
context("sacct")

slurm.txt <- system.file(
  "extdata", "sacct-unrecognized-unit.txt", package="slurm", mustWork=TRUE)
test_that("sacct_fread works for prev unrecognized unit", {
  dt <- sacct_fread(slurm.txt)
  expect_is(dt, "data.table")
})

test_that("all NA column read as character", {
  sacct.txt <- system.file(
    "extdata", "sacct-multistep.txt", package="slurm", mustWork=TRUE)
  sacct.dt <- sacct_fread(file=sacct.txt)
  task.dt <- sacct_tasks(sacct.dt)
  expect_equal(task.dt$task, 1:999)
})

test_that("dcast only keeps RUNNING", {
  sacct.txt <- system.file(
    "extdata", "slurm-dcast-agg.txt", package="slurm", mustWork=TRUE)
  sacct.dt <- sacct_fread(file=sacct.txt)
  task.dt <- sacct_tasks(sacct.dt)
  st.tab <- table(task.dt$State_blank)
  expect_identical(names(st.tab), c("FAILED", "OUT_OF_MEMORY", "PENDING", "RUNNING"))
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

if(requireNamespace("R.utils")){
  test_that("job ID field parsed correctly", {
    sacct.csv.gz <- system.file(
      "data", "sacct-multitype.csv.gz", package="slurm", mustWork=TRUE)
    sacct.dt <- sacct_fread(sacct.csv.gz)
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
    sacct.dt <- sacct_fread(sacct.csv.gz)
    task.dt <- sacct_tasks(sacct.dt)
    task.uniq <- unique(task.dt[, .(job, task)])
    expect_equal(nrow(task.dt), nrow(task.uniq))
    summary.dt <- sjob_dt(task.dt)
    expect_equal(summary.dt$job[1:3], c(26534569L, 26534608L, 26534608L))
    expect_equal(summary.dt$State_batch[1:3], c(NA,NA,"COMPLETED"))
  })
}

