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
  JobID.taskN <- JobID.task1 <- JobID <- JobID.task <- MaxRSS.unit <-
    MaxRSS.megabytes <- MaxRSS.amount <- type <- JobID.type <- hours <-
      Elapsed.days <- Elapsed.hours <- Elapsed.minutes <- Elapsed.seconds <-
        JobID.job <- task <- Elapsed <- NULL
  ## above to avoid CRAN NOTE
  sacct.dt <- fread(..., fill=TRUE, sep="|")
  ## ExitCode The exit code returned by the job script or salloc,
  ## typically as set by the exit() function.  Following the colon is
  ## the signal that caused the process to terminate if it was
  ## terminated by a signal.
  na.as.zero <- function(int.or.empty){
    ifelse(int.or.empty=="", 0L, as.integer(int.or.empty))
  }
  pattern.list <- list(
    ExitCode=list(
      before="[0-9]+", as.integer,
      ":",
      after="[0-9]+", as.integer),
    Elapsed=list(
      "(?:",
      days="[0-9]+", na.as.zero,
      "-)?",
      "(?:",
      hours="[0-9]+", na.as.zero,
      ":)?",
      minutes="[0-9]+", as.integer,
      ":",
      seconds="[0-9]+", as.integer),
    JobID=list(
      job="[0-9]+", as.integer,
      "_",
      "(?:",#begin alternate
      task="[0-9]+", as.integer,
      "|",#either one task(above) or range(below)
      "[[]",
      task1="[0-9]+", as.integer,
      "(?:-",#begin optional end of range.
      taskN="[0-9]+", as.integer,
      ")?", #end is optional.
      "[]]",
      ")",#end alternate
      "(?:[.]",
      type=".*",
      ")?"),
    MaxRSS=list(
      amount="[.0-9]+", as.numeric,
      unit=".*"))
  match.dt.list <- list(sacct.dt)
  for(col.name in names(pattern.list)){
    col.pattern <- pattern.list[[col.name]]
    subject <- paste(sacct.dt[[col.name]])
    all.args <- c(list(subject), col.pattern)
    match.dt.list[[col.name]] <- do.call(str_match_variable, all.args)
  }
  match.dt <- do.call(data.table, match.dt.list)
  range.dt <- match.dt[!is.na(JobID.taskN)]
  task.dt <- rbind(
    if(nrow(range.dt))range.dt[, {
      data.table(.SD, task=seq(JobID.task1, JobID.taskN))
    }, by=list(JobID)],
    match.dt[is.na(JobID.taskN), {
      data.table(.SD, task=ifelse(is.na(JobID.task), JobID.task1, JobID.task))
    }])
  tomega.vec <- c(
    K=1/1024,
    M=1)
  unit.vec <- task.dt[!is.na(MaxRSS.unit), unique(MaxRSS.unit)]
  bad.unit <- ! unit.vec %in% names(tomega.vec)
  if(any(bad.unit)){
    print(unit.vec[bad.unit])
    stop("unrecognized unit")
  }
  task.dt[, MaxRSS.megabytes := MaxRSS.amount*tomega.vec[paste(MaxRSS.unit)] ]
  task.dt[, type := ifelse(JobID.type=="", "blank", JobID.type)]
  task.dt[, hours := {
    Elapsed.days * 24 +
      Elapsed.hours +
      Elapsed.minutes/60 +
      Elapsed.seconds/60/60
  }]
  wide.dt <- dcast(
    task.dt,
    JobID.job + task ~ type,
    value.var=c("State", "ExitCode"))
  rss.dt <- task.dt[JobID.type=="batch", {
    list(JobID.job, task, MaxRSS.megabytes)
  }][wide.dt, on=list(JobID.job, task)]
  time.dt <- task.dt[type=="blank", {
    list(JobID.job, task, Elapsed, hours)
  }][rss.dt, on=list(JobID.job, task)]
  time.dt
### data.table with one row per job/task.
}, ex=function(){

  library(slurm)
  sacct.csv <- system.file(
    "data", "sacct-job13936577.csv", package="slurm", mustWork=TRUE)
  task.dt <- sacct_fread(sacct.csv)
  task.dt[State_batch != "COMPLETED"]

  if(require(ggplot2)){
    ggplot()+
      geom_point(aes(
        hours, MaxRSS.megabytes, fill=State_batch),
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
  sacct.csv <- system.file(
    "data", "sacct-job13936577.csv", package="slurm", mustWork=TRUE)
  task.dt <- sacct_fread(sacct.csv)
  (summary.dt <- sjob_dt(task.dt))
  
})

### get currently running jobs
sq.jobs <- function(args="-u $USER"){
  cmd <- paste('squeue -h', args, '-o "%F"|sort|uniq')
  jid.vec <- system(cmd, intern=TRUE)
  paste(jid.vec, collapse=",")
### character scalar: job1,job2,etc
}

