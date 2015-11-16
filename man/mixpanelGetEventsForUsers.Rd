\name{mixpanelGetEventsForUsers}
\alias{mixpanelGetEventsForUsers}


\title{
Get events from Mixpanel API.
}
\description{
Get a matrix filled with events taken from the Mixpanel API. 
}
\usage{
mixpanelGetEventsForUsers(account, args)
}

\arguments{
  \item{account}{
A mixpanel account, as defined in \code{mixpanelCreateAccount}.
}
  \item{args}{
List of arguments being included in the Mixpanel API request. E.g. args=list(
    distinct_ids='AAF8...', from_date="2015-01-01", to_date="2015-06-01"). }
}

\value{
A character matrix showing one event per line. The first column represents the name of the event, the other column the event properties.  
}

\author{
Meinhard Ploner}

\seealso{
\code{\link{mixpanelGetPeople}},
\code{\link{mixpanelGetEvents}}
}


\keyword{mixpanel}
\keyword{events}
