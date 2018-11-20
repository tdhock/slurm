\name{sjob_dt}
\alias{sjob_dt}
\title{sjob dt}
\description{Get summary dt from sacct dt.}
\usage{sjob_dt(time.dt, tasks.width = 11)}
\arguments{
  \item{time.dt}{
}
  \item{tasks.width}{
}
}

\value{data.table summarizing State/ExitCode distribution over jobs}

\author{Toby Dylan Hocking}




\examples{

library(slurm)
sacct.csv <- system.file(
  "data", "sacct-job13936577.csv", package="slurm", mustWork=TRUE)
task.dt <- sacct_fread(sacct.csv)
(summary.dt <- sjob_dt(task.dt))

}
