### Convert empty string to zero and use as.integer otherwise.
na.as.zero <- function(int.or.empty){
  ifelse(int.or.empty=="", 0L, as.integer(int.or.empty))
### Integer vector.
}

### Match one or more digits and convert to integer.
int.pattern <- list("[0-9]+", as.integer)

### Pattern for either one task or a range.
task.pattern <- list(
  task.id=int.pattern,
  "|",#either one task(above) or range(below)
  "\\[",
  task1=int.pattern,
  nc::quantifier("-", taskN=int.pattern, "?"),
  "\\]")

### Named list of patterns for parsing sacct fields.
sacct.pattern.list <- list(
  JobID=list(
    job=int.pattern,
    nc::quantifier("_", task.pattern, "?"),
    nc::quantifier("[.]", type=".*", "?")),
  ExitCode=list(
    ## DerivedExitCode: The highest exit code returned by the job's job
    ## steps (srun invocations). Following the colon is the signal that
    ## caused the process to terminate if it was terminated by a signal.
    ## The DerivedExitCode can be modified by invoking sacctmgr modify
    ## job or the specialized sjobexitmod command.
    before=int.pattern,
    ":",
    after=int.pattern,
    nomatch.error=FALSE),
  Elapsed=list(
    nc::quantifier(days.only="[0-9]+", na.as.zero, "-", "?"),
    nc::quantifier(hours.only="[0-9]+", na.as.zero, ":", "?"),
    minutes.only=int.pattern,
    ":",
    seconds.only=int.pattern),
  MaxRSS=list(
    amount="[.0-9]+", as.numeric,
    unit=".*",
    nomatch.error=FALSE))

### Get current fields from sacct.
sacct_fields <- function(){
  fields.dt <- fread(cmd="sacct -e", header=FALSE)
  melt(fields.dt, measure.vars=names(fields.dt))$value
### character vector.
}

### Run sacct_lines then sacct_tasks.
sacct <- function(...){
  sacct.dt <- sacct_lines(...)
  sacct_tasks(sacct.dt)
### Same as result of sacct_tasks.
}

sacct_lines <- function
### Run sacct with args and parse output as a data.table
(args,
### character string passed to sacct command line, e.g. -j123 for
### selecting job ID 123.
  format.fields=c("JobID","ExitCode","State","MaxRSS","Elapsed"),
### character vector of field names to pass to sacct --format. Use
### sacct_fields to get all fields.
  delimiter="\t"
### passed as --delimiter.
){
  cmd <- sprintf(
    "sacct -P %s --delimiter='%s' --format=%s",
    args, delimiter, paste(format.fields, collapse=","))
  line.vec <- system(cmd, intern=TRUE)
  sacct_fread(text=line.vec, sep=delimiter)
### Same as sacct_fread.
}

### Run fread on the output of sacct.
sacct_fread <- structure(function(...){
  name.dt <- fread(..., nrows=0)
  colClasses <- list(character=names(name.dt))
  sacct.dt <- fread(..., colClasses=colClasses, fill=TRUE)
  if(nrow(sacct.dt)==0)return(sacct.dt)
  some.names <- intersect(names(sacct.dt), names(sacct.pattern.list))
  arg.list <- c(
    list(sacct.dt),
    sacct.pattern.list[some.names])
  do.call(nc::capture_first_df, arg.list)
### Data table with the same number of rows as the output of the sacct
### command, and additional columns that result from parsing the
### columns with sacct.pattern.list. This table typically has more
### than one row per task, so typically this table is used as input to
### sacct_tasks.
}, ex=function(){
  library(slurm)
  sacct_fread(text="JobID|ExitCode|State|MaxRSS|Elapsed
18473217_1|0:0|RUNNING||00:03:47
18473217_1.extern|0:0|RUNNING||00:03:47")
})

### Use output of sacct_fread to compute a table with one row per task.
sacct_tasks <- structure(function(match.dt){
  taskN <- task1 <- JobID <- task <- task.id <- unit <-
    megabytes <- amount <- type <- type <- hours <- State <- 
      days.only <- hours.only <- minutes.only <- seconds.only <-
        job <- task <- Elapsed <- State_blank <- NULL
  ## above to avoid CRAN NOTE
  range.dt <- match.dt[!is.na(taskN)]
  task.dt <- rbind(
    if(nrow(range.dt))range.dt[, {
      data.table(.SD, task=seq(task1, taskN))
    }, by=list(JobID)],
    match.dt[is.na(taskN), {
      data.table(.SD, task=ifelse(is.na(task.id), task1, task.id))
    }])
  amount.per.megabyte <- c(
    G=1/1024,
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
  pending.dt <- task.dt[State=="PENDING"]
  not.pending <- task.dt[State!="PENDING"]
  only.pending <- pending.dt[!not.pending, on=c("job","task")]
  uniq.tasks <- rbind(only.pending, not.pending)
  wide.dt <- dcast(
    uniq.tasks,
    job + task ~ type,
    value.var=c("State", "ExitCode"))
  if(is.numeric(wide.dt$State_blank)){
    bad.jobs <- wide.dt[State_blank>1, job]
    stop("some jobs were not parsed correctly inside sacct_tasks. Please use dput() on the data table output from sacct_lines(), and report as an issue on the github issue tracker")
  }
  rss.dt <- uniq.tasks[type=="batch", {
    list(job, task, megabytes)
  }][wide.dt, on=list(job, task)]
  time.dt <- uniq.tasks[type=="blank", {
    list(job, task, Elapsed, hours)
  }][rss.dt, on=list(job, task)]
  time.dt
### data.table with one row per job/task, typically used as input to
### sjob_dt.
}, ex=function(){

  library(slurm)
  sacct.csv.gz <- system.file(
    "data", "sacct-job13936577.csv.gz", package="slurm", mustWork=TRUE)
  if(requireNamespace("R.utils")){
    sacct.dt <- sacct_fread(sacct.csv.gz)
    task.dt <- sacct_tasks(sacct.dt)
    print(task.dt[State_batch != "COMPLETED"])
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
  }

})

### Run sacct and summarize State/ExitCode values for given job IDS
sjob <- function(job.id=sq.jobs(), tasks.width=11){
  sacct.dt <- sacct(paste0("-j", job.id))
  sjob_dt(sacct.dt, tasks.width=tasks.width)
### Data table from sjob_dt.
}

### Summarize task table from sacct_tasks.
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
    "job",
    col.name.list$State,
    "ExitCodes")
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
  }, keyby=by.vars]
### data.table with one row per unique value of job/State/ExitCode
### (and keyed on these columns).
}, ex=function(){

  library(slurm)
  sacct.csv.gz <- system.file(
    "data", "sacct-job13936577.csv.gz", package="slurm", mustWork=TRUE)
  if(requireNamespace("R.utils")){
    task.dt <- sacct_fread(sacct.csv.gz)
    print(summary.dt <- sjob_dt(task.dt))
  }

})

### get currently running jobs
sq.jobs <- function(args="-u $USER"){
  cmd <- paste('squeue -h', args, '-o "%F"|sort|uniq')
  jid.vec <- system(cmd, intern=TRUE)
  paste(jid.vec, collapse=",")
### character scalar: job1,job2,etc
}

