\name{crossTable}
\alias{crossTable}
\alias{summary.CrossTable}
\title{
Cross Tabulation
}
\description{
Output well-formatted cross tabulation. Also can genarate latex syntax of cross tabulation.
}
\usage{
crossTable(..., deparse.level = 2)
\method{summary}{CrossTable}(object, digits=3, latex=FALSE, ...)
}
\arguments{
  \item{deparse.level}{
passed to \code{\link{table}}
}
  \item{\dots}{
passed to \code{\link{table}}
}
  \item{object}{
crossTable object
}
  \item{digits}{
integer, used for number formatting
}
  \item{latex}{
logical, generate latex syntax if \code{TRUE}
}
}
%\details{
%}
\value{
\code{crossTable} returns a object which belongs to CrossTable class and table class.
}
\author{
Masahiro Hayashi
}
\examples{
sex    <- factor(rbinom(1:1000, 1, 0.5), labels=c("male" , "female"))
age    <- factor(rbinom(1:1000, 2, 0.4), labels=c("young", "middle", "old"))
weight <- factor(rbinom(1:1000, 2, 0.6), labels=c("light", "middle", "heavy"))

cross.table1 <- crossTable(sex, age)
summary(cross.table1)

cross.table2 <- crossTable(sex, age, weight)
summary(cross.table2)

summary(cross.table2, latex = TRUE)

}
\keyword{ cross tabulation }
