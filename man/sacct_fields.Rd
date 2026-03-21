\name{sacct_fields}
\alias{sacct_fields}
\title{sacct fields}
\description{Get current fields from \code{\link{sacct}}.}
\usage{sacct_fields(cmd = "sacct -e")}
\arguments{
  \item{cmd}{command to run and capture output, default "sacct -e" to query
slurm, can be changed to cat a file for testing.}
}

\value{character vector, items that can be used as format.fields argument to sacct_lines().}

\author{Toby Hocking <toby.hocking@r-project.org> [aut, cre]}





