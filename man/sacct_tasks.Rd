\name{sacct_tasks}
\alias{sacct_tasks}
\title{sacct tasks}
\description{Use output of \code{\link{sacct_fread}} to compute a table with one row per task.}
\usage{sacct_tasks(match.dt)}
\arguments{
  \item{match.dt}{match.dt }
}

\value{data.table with one row per job/task, typically used as input to
\code{\link{sjob_dt}}.}

\author{Toby Hocking <toby.hocking@r-project.org> [aut, cre]}




\examples{

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

}
