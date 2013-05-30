\name{rzReloadData}
\alias{rzReloadData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reload data.set from globalenv
}
\description{
Reload data.set from globalenv
}
\usage{
rzReloadData(data.set.name = NULL, ask = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.set.name}{
character: dataset's name to be reloaded. If NULL, current dataset is reloaded.
}
  \item{ask}{
logical: confirm execution if TRUE
}
}
\details{
Reload data.set from globalenv
}
\author{
Masahiro Hayashi
}
\seealso{
\code{\link{rzAddData}}, \code{\link{rzAddItem}}
}
