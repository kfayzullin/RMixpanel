\name{mixpanelGetPeopleCount}
\alias{mixpanelGetPeopleCount}

\title{
Get user (people) count from Mixpanel API.
}
\description{
Get the number of users conforming to the requested query from the Mixpanel People API. 
}
\usage{
mixpanelGetPeopleCount(account, where = "", verbose=TRUE)
}

\arguments{
  \item{account}{
A mixpanel account, as defined in \code{mixpanelCreateAccount}.
}
  \item{where}{
Expression, see \url{https://mixpanel.com/docs/api-documentation/data-export-api#segmentation-expressions}.
}
\item{verbose}{
If \code{TRUE} more output is generated. 
}
}

\value{
The number of users conforming to the request.  
}

\references{
\url{https://mixpanel.com/docs/api-documentation/data-export-api#engage-default}
}
\author{
Meinhard Ploner}


\seealso{
\code{\link{mixpanelGetPeople}}
}

\keyword{mixpanel}
\keyword{people}
