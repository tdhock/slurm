\name{sacct_lines}
\alias{sacct_lines}
\title{sacct lines}
\description{Run \code{\link{sacct}} with \code{args} and parse output as a data.table}
\usage{sacct_lines(args, format.fields = c("JobID", 
    "ExitCode", "State", 
    "MaxRSS", "Elapsed"))}
\arguments{
  \item{args}{character string passed to \code{\link{sacct}} command line, e.g. -j123 for
selecting job ID 123.}
  \item{format.fields}{character vector of field names to pass to \code{\link{sacct}} --format. Use
\code{\link{sacct_fields}} to get all fields.}
}

\value{Same as \code{\link{sacct_fread}}.}

\author{Toby Dylan Hocking}




\examples{

if(FALSE){
  sacct("-j123,456")
}

}
