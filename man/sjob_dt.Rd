\name{sjob_dt}
\alias{sjob_dt}
\title{sjob dt}
\description{Summarize task table from \code{\link{sacct_tasks}}.}
\usage{sjob_dt(time.dt, tasks.width = 11)}
\arguments{
  \item{time.dt}{time.dt }
  \item{tasks.width}{tasks.width }
}

\value{data.table with one row per unique value of job/State/ExitCode
(and keyed on these columns).}

\author{Toby Hocking <toby.hocking@r-project.org> [aut, cre]}




\examples{

library(slurm)
sacct.csv.gz <- system.file(
  "data", "sacct-job13936577.csv.gz", package="slurm", mustWork=TRUE)
if(requireNamespace("R.utils")){
  task.dt <- sacct_fread(sacct.csv.gz)
  print(summary.dt <- sjob_dt(task.dt))
}

}
