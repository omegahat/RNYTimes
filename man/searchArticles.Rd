\name{searchArticles}
\alias{searchArticles}
\title{Search NY Times articles}
\description{
This allows one to search NY Times articles from 1981 to the present
day, looking for particular text. One can also specify a range of dates.
}
\usage{
searchArticles(query, begin_date = NA, end_date = NA, ..., key = getOption("NYTimesAPI")["Article Search"],
                fields = character(), facets = character())
}
\arguments{
  \item{query}{the phrases to search for. This can be a character vector which is collapsed with white space.}
  \item{begin_date,end_date}{optional start and end dates, given as strings  of the form "YYYYMMDD"}
  \item{\dots}{additional name = value parameters passed in the form query}
  \item{key}{a string giving the NY Times API key for the Article Search service}
  \item{fields}{character vector giving the names of the fields to return.}
  \item{facets}{character vector specifying up to 5 facets }
}
\value{
A list with an element for each element of the result set. Each element 
is a list with a body, byline, date, title and url field
unless the caller specifies a value for \code{fields}.
}
\references{
\url{http://developer.nytimes.com/docs/times_newswire_api}
}
\author{
Duncan Temple Lang
}


%\seealso{}
\examples{
if("NYTimesAPI" \%in\% names(options()) && "Article Search" \%in\% names(options("NYTimes")))  {
  searchArticles("title:bailout", begin_date = "20081001",
                  end_date = '20081201',
                  fields = 'org_facet', facets = "geo_facet")
} else
  cat("You need to specify the API key for the NY Times API, specifically for Article Search.\n",
      "Set this in the options() as options(NYTimesAPI = c('Article Search' = key)\n")
}
\keyword{IO}

