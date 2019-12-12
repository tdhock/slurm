\name{sacct_fread}
\alias{sacct_fread}
\title{sacct fread}
\description{Run fread on the output of \code{\link{sacct}}.}
\usage{sacct_fread(...)}
\arguments{
  \item{\dots}{
}
}

\value{Data table with the same number of rows as the output of the \code{\link{sacct}}
command, and additional columns that result from parsing the
columns with \code{\link{sacct.pattern.list}}.}

\author{Toby Dylan Hocking}




\examples{
  library(slurm)
  sacct_fread(text="JobID|ExitCode|State|MaxRSS|Elapsed
18473217_1|0:0|RUNNING||00:03:47
18473217_1.extern|0:0|RUNNING||00:03:47")
}
