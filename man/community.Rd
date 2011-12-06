\name{community}
\alias{community}
\title{Access user-generated NYTimes.com content}
\description{
This function allows us to access user-generate content on the NY Times,
e.g. comments on articles.
}
\usage{
community(what = "recent", match = "exact-match", ..., key = getOption("NYTimesAPI")["Community"])
}
\arguments{
  \item{what}{a string identifying which comments to retrieve, e.g.
        "recent", "random", "by-date/YYYY-MM-DD", "user/id/1234"}
  \item{match}{either "exact-match" or "closest-stem-match"}
  \item{\dots}{additional name=value pairs passed to the Web service request}
  \item{key}{the NY Times API key to access to the service}
}
\value{

}
\references{
\url{http://developer.nytimes.com/docs/community_api}
}
\author{
Duncan Temple Lang
}

%\seealso{}
\examples{

}
\keyword{IO}
