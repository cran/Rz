\name{rzAddItem}
\alias{rzAddItem}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add a variable to a dataset
}
\description{
Add a variable to a dataset
}
\usage{
rzAddItem(item, name = as.character(substitute(item)),
          data.set.name = NULL, description = name,
          measurement = c("auto", "nominal", "ordinal", "interval", "ratio"),
          overwrite = FALSE, ask = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{item}{
an \code{item} object or a variable such as factor or numeric.
}
  \item{name}{
character: the variable name.
}
  \item{data.set.name}{
character: a name of a dataset in Rz. \code{NULL} means the current dataset.
}
  \item{description}{
character: the variable label.
}
  \item{measurement}{
character: measurement of the variable.
}
  \item{overwrite}{
logical: if \code{TRUE} will overwrite an existing variable.
}
  \item{ask}{
logical: confirm execution if \code{TRUE}.
}
}
\author{
Masahiro Hayashi
}
\seealso{
\code{\link{rzAddData}}, \code{\link{rzReloadData}}
}
