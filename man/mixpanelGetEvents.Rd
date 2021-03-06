\name{mixpanelGetEvents}
\alias{mixpanelGetEvents}

\title{Get Events From Mixpanel API}

\description{Get a matrix filled with event data from the Mixpanel API.}

\usage{mixpanelGetEvents(account, event, from, to, daysPerBlock, select=TRUE, verbose=TRUE, ...)}

\arguments{
\item{account}{A mixpanel account, as defined in \code{mixpanelCreateAccount}.}
\item{event}{Array of event names. If empty, all events are returned.}
\item{from}{Start date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive. }
\item{to}{End date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive.}
\item{daysPerBlock}{Data is downloaded in blocks of fixed size.}
\item{select}{If \code{"select"} is an array of column names, only the specified properties are selected. If \code{TRUE}, all columns are selected.}
\item{verbose}{If \code{TRUE} more output is generated.}
\item{...}{Additional arguments to be included in the Mixpanel API request. E.g. where = "properties[\"$os\"]==\"iPhone OS\""). See also \url{https://mixpanel.com/docs/api-documentation/data-export-api#events-default}.}
}

\value{A character matrix showing one event per line. The first column represents the name of the event, the other column the event properties.}

\author{Meinhard Ploner}

\seealso{
\code{\link{mixpanelGetProfiles}},
\code{\link{mixpanelGetEventsForProfiles}}
}


\keyword{mixpanel}
\keyword{events}
