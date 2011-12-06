\name{nyTags}
\alias{nyTags}
\title{Access NY Times tag set for controlled vocabularies}
\description{
 This function retrieves information about the NY Times tag sets which are
used for NY Times meta data.
For a given string, the service returns a ranked list of suggested terms
from the tags/vocabularies.
}
\usage{
nyTags(query, filter = character(), ..., key = getOption("NYTimesAPI")["Tags"])
}
\arguments{
  \item{query}{the string to map to tags}
  \item{filter}{character vector giving the names of dictionaries
     to which to limit the results, e.g. Geo, Des, Per, Org. 
     See the service API for a description of these. 
     These should not be contained with parentheses.
    These are added by the function.}
  \item{\dots}{additional parameters to be submitted as part of the form}
  \item{key}{the NY Times API key for the Tags service}
}
\value{
A named character vector.
The values are the tags.
The names give the dictionary from which the tag came.
}
\references{
\url{http://developer.nytimes.com/docs/timestags_api}
}
\author{
Duncan Temple Lang
}


\seealso{}
\examples{
if("NYTimesAPI" \%in\% names(options()) && "Tags" \%in\% names(options("NYTimes"))) {
  nyTags("climate")
}
}
\keyword{IO}

