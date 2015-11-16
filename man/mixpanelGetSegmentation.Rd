\name{mixpanelGetSegmentation}
\alias{mixpanelGetSegmentation}

\title{Get events segmented by 1 or 2 properties.}

\description{Get a matrix of events segmented by properties. Event data is taken from the Mixpanel Segmentation API.}

\usage{mixpanelGetSegmentation(account, event, from, to, unit, type, on, ...)}

%- maybe also 'usage' for other objects documented here.

\arguments{
\item{account}{A mixpanel account, as defined in \code{mixpanelCreateAccount}.}
\item{event}{Event name. If empty, all events are returned.}
\item{from}{Start date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive. }
\item{to}{End date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive.}
\item{unit}{Defaults to 'day'.}
\item{type}{This can be "general", "unique", or "average".}
\item{on}{Array of up to 2 property names for segmentation. An empty array returns un-segmented counts. }
\item{...}{ Additional arguments to Mixpanel API. See also \url{https://mixpanel.com/docs/api-documentation/data-export-api#segmentation-default.}}
}

\value{A matrix or 3-dimensional array showing event counts. The first dimension represents the time in the specified unit. The other dimension(s) are determined by the segmention properties. }

\references{\url{https://mixpanel.com/docs/api-documentation/data-export-api#segmentation-default}}

\author{Meinhard Ploner}

\keyword{mixpanel}
\keyword{segmentation}
