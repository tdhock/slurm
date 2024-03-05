\name{sjob_dt}
\alias{sjob_dt}
\title{sjob dt}
\description{Get summary dt from \code{\link{sacct}} dt.}
\usage{sjob_dt(time.dt, tasks.width = 11)}
\arguments{
  \item{time.dt}{time.dt }
  \item{tasks.width}{tasks.width }
}

\value{data.table summarizing State/ExitCode distribution over jobs}

\author{Toby Dylan Hocking}




\examples{

library(slurm)
sacct.csv.gz <- system.file(
  "data", "sacct-job13936577.csv.gz", package="slurm", mustWork=TRUE)
if(requireNamespace("R.utils")){
  task.dt <- sacct_fread(sacct.csv.gz)
  print(summary.dt <- sjob_dt(task.dt))
}

}
