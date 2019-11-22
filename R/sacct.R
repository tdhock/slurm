### Run sacct with args and parse output as a data.table
sacct <- structure(function(args){
   cmd <- paste(
    "sacct",
    args,
    "--format=JobID%30,ExitCode,State%30,MaxRSS,Elapsed -P")
  ## DerivedExitCode: The highest exit code returned by the job's job
  ## steps (srun invocations). Following the colon is the signal that
  ## caused the process to terminate if it was terminated by a signal.
  ## The DerivedExitCode can be modified by invoking sacctmgr modify
  ## job or the specialized sjobexitmod command.
  sacct_fread(cmd=cmd)
}, ex=function(){

  if(FALSE){
    sacct("-j123,456")
  }

})

### Run fread on the output of sacct.
sacct_fread <- structure(function(...){
  taskN <- task1 <- JobID <- task <- task.id <- unit <-
    megabytes <- amount <- type <- type <- hours <-
      days.only <- hours.only <- minutes.only <- seconds.only <-
        job <- task <- Elapsed <- NULL
  ## above to avoid CRAN NOTE
  sacct.dt <- fread(..., fill=TRUE, sep="|")
  ## ExitCode The exit code returned by the job script or salloc,
  ## typically as set by the exit() function.  Following the colon is
  ## the signal that caused the process to terminate if it was
  ## terminated by a signal.
  na.as.zero <- function(int.or.empty){
    ifelse(int.or.empty=="", 0L, as.integer(int.or.empty))
  }
  optional.end <- list(
    list("-", taskN="[0-9]+", as.integer),
    "?")
  range.pattern <- list(
    "\\[",
    task1="[0-9]+", as.integer,
    optional.end,
    "\\]")
  task.pattern <- list(
    task.id="[0-9]+", as.integer,
    "|",#either one task(above) or range(below)
    range.pattern)
  optional.type <- list(
    list("[.]", type=".*"),
    "?")
  optional.task <- list(
    list("_", task.pattern),
    "?")
  match.dt <- nc::capture_first_df(
    sacct.dt,
    JobID=list(
      job="[0-9]+", as.integer,
      optional.task,
      optional.type),
    ExitCode=list(
      before="[0-9]+", as.integer,
      ":",
      after="[0-9]+", as.integer),
    Elapsed=list(
      "(?:",
      days.only="[0-9]+", na.as.zero,
      "-)?",
      "(?:",
      hours.only="[0-9]+", na.as.zero,
      ":)?",
      minutes.only="[0-9]+", as.integer,
      ":",
      seconds.only="[0-9]+", as.integer),
    MaxRSS=list(
      amount="[.0-9]+", as.numeric,
      unit=".*",
      nomatch.error=FALSE))
  range.dt <- match.dt[!is.na(taskN)]
  task.dt <- rbind(
    if(nrow(range.dt))range.dt[, {
      data.table(.SD, task=seq(task1, taskN))
    }, by=list(JobID)],
    match.dt[is.na(taskN), {
      data.table(.SD, task=ifelse(is.na(task.id), task1, task.id))
    }])
  amount.per.megabyte <- c(
    K=1024,
    M=1)
  task.dt[, megabytes := ifelse(
    amount==0, 0, amount/amount.per.megabyte[paste(unit)]
  )]
  bad.unit.dt <- task.dt[is.na(megabytes) & !is.na(unit)]
  if(nrow(bad.unit.dt)){
    bad.str <- paste(
      sprintf("'%s'", unique(bad.unit.dt$unit)),
      collapse=", ")
    stop("unrecognized unit: ", bad.str)
  }
  task.dt[, type := ifelse(type=="", "blank", type)]
  task.dt[, hours := {
    days.only * 24 +
      hours.only +
      minutes.only/60 +
      seconds.only/60/60
  }]
  wide.dt <- dcast(
    task.dt,
    job + task ~ type,
    value.var=c("State", "ExitCode"))
  rss.dt <- task.dt[type=="batch", {
    list(job, task, megabytes)
  }][wide.dt, on=list(job, task)]
  time.dt <- task.dt[type=="blank", {
    list(job, task, Elapsed, hours)
  }][rss.dt, on=list(job, task)]
  time.dt
### data.table with one row per job/task.
}, ex=function(){

  library(slurm)
  sacct.csv.gz <- system.file(
    "data", "sacct-job13936577.csv.gz", package="slurm", mustWork=TRUE)
  cmd <- paste("zcat", sacct.csv.gz)
  task.dt <- sacct_fread(cmd=cmd)
  task.dt[State_batch != "COMPLETED"]

  if(require(ggplot2)){
    ggplot()+
      geom_point(aes(
        hours, megabytes, fill=State_batch),
        shape=21,
        data=task.dt)+
      scale_fill_manual(values=c(
        COMPLETED=NA,
        FAILED="red"))+
      scale_x_log10()+
      scale_y_log10()
  }

})

### Run sacct and summarize State/ExitCode values for given job IDS
sjob <- function(job.id=sq.jobs(), tasks.width=11){
  sacct.dt <- sacct(paste0("-j", job.id))
  sjob_dt(sacct.dt, tasks.width=tasks.width)
}

### Get summary dt from sacct dt.
sjob_dt <- structure(function(time.dt, tasks.width=11){
  ExitCodes <- task <- NULL
  ## above to avoid CRAN NOTE
  suffix.vec <- c("batch", "blank", "extern")
  col.name.list <- list()
  for(prefix in c("ExitCode", "State")){
    possible.vec <- paste0(prefix, "_", suffix.vec)
    col.name.list[[prefix]] <- possible.vec[possible.vec %in% names(time.dt)]
  }
  paste.args <- as.list(time.dt[, col.name.list$ExitCode, with=FALSE])
  time.dt[, ExitCodes := do.call(paste, paste.args)]
  by.vars <- c(
    col.name.list$State,
    "ExitCodes",
    if(1 < length(unique(time.dt$JobID.job))){
      "JobID.job"
    })
  time.dt[, {
    list(
      count=.N,
      tasks={
        tasks.long <- paste(task, collapse=",")
        ifelse(
          tasks.width < nchar(tasks.long),
          sub("[0-9]+$", "", substr(tasks.long, 1, tasks.width-1)),
          tasks.long)
      }
    )
  }, by=by.vars]
### data.table summarizing State/ExitCode distribution over jobs
}, ex=function(){

  library(slurm)
  sacct.csv.gz <- system.file(
    "data", "sacct-job13936577.csv.gz", package="slurm", mustWork=TRUE)
  cmd <- paste("zcat", sacct.csv.gz)
  task.dt <- sacct_fread(cmd=cmd)
  (summary.dt <- sjob_dt(task.dt))

})

### get currently running jobs
sq.jobs <- function(args="-u $USER"){
  cmd <- paste('squeue -h', args, '-o "%F"|sort|uniq')
  jid.vec <- system(cmd, intern=TRUE)
  paste(jid.vec, collapse=",")
### character scalar: job1,job2,etc
}

