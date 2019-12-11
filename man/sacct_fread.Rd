\name{sacct_fread}
\alias{sacct_fread}
\title{sacct fread}
\description{Run fread on the output of \code{\link{sacct}}.}
\usage{sacct_fread(...)}
\arguments{
  \item{\dots}{
}
}

\value{data.table with one row per job/task.}

\author{Toby Dylan Hocking}




\examples{

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

}
