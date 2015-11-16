\name{peopleJson2RMatrix}
\alias{peopleJson2RMatrix}

\title{
Convert people data in json format to R matrix.
}
\description{
Convenience function to convert json result of engage API to R matrix holding people data.
}
\usage{
peopleJson2RMatrix(jsonData, select = TRUE)
}

\arguments{
  \item{jsonData}{
Result of request to Mixpanel's Engage API.
}
  \item{select}{
If \code{"select"} is an array of column names, only the specified properties are selected. If \code{TRUE}, all columns are selected. 
}
}
\details{
Ths helper function takes a json result as of the Mixpanel Engage API and creates a R matrix holding the data. Array user properties are collapsed using a dash ('-') as separator.   
}
\value{
A character matrix showing one user per line. The first column represents the distinct ID of the user, the other column the user properties.  
}
\author{
Meinhard Ploner
}

\seealso{
\code{\link{mixpanelGetPeople}}
}
\keyword{mixpanel}
\keyword{parse}