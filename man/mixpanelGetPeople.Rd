\name{mixpanelGetPeople}
\alias{mixpanelGetPeople}

\title{
Get user (people) from Mixpanel API.
}
\description{
Get a matrix filled with users taken from the Mixpanel People API. 
}
\usage{
mixpanelGetPeople(account, where = "", select, maxPage = 1e+05, verbose=TRUE)
}

\arguments{
  \item{account}{
A mixpanel account, as defined in \code{mixpanelCreateAccount}.
}
  \item{where}{
Expression, see \url{https://mixpanel.com/docs/api-documentation/data-export-api#segmentation-expressions}.
}
  \item{select}{
If \code{"select"} is an array of column names, only the specified properties are selected. If \code{TRUE}, all columns are selected. 
}
  \item{maxPage}{
The API delivers usually about 1000 users per page. Setting this parameter can limit the response for testing purposes.
}
\item{verbose}{
If \code{TRUE} more output is generated. 
}
}

\value{
A character matrix showing one user per line. The first column represents the distinct ID of the user, the other column the user properties.  
}

\references{
\url{https://mixpanel.com/docs/api-documentation/data-export-api#engage-default}
}
\author{
Meinhard Ploner}


\seealso{
\code{\link{mixpanelGetEvents}}
}

\keyword{mixpanel}
\keyword{people}
