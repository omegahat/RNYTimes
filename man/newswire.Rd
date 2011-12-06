\name{newswire}
\alias{newswire}
\title{Access NY Times articles meta-data}
\description{
This gets links and metadata for Times articles and blog posts.
}
\usage{
newswire(n = 100, period = "recent", offset = 0, ..., key = getOption("NYTimesAPI")["Newswire"])
}
\arguments{
  \item{n}{maximum number of elements in the result set}
  \item{period}{either "recent" or "last24hours"}
  \item{offset}{an integer multiple of 20. This gives the offset of the result set at which to start}
  \item{\dots}{additional parameters}
  \item{key}{the NY Times API key for accessing the Newswire service.}
}
\value{
A list of \code{news_item} objects containing information about the item,
e.g. the headline, byline, summary, when it was created published, updated, ...
}

\references{
\url{http://developer.nytimes.com/docs/times_newswire_api}
}
\author{
Duncan Temple Lang
}

%\seealso{}
\examples{
 if("NYTimesAPI" \%in\% names(options()) && "Best Sellers" \%in\% names(options("Newswire"))) {
    vals = newswire()
    sapply(vals, slot, "headline")
 }
}
\keyword{IO}
