\name{getBestSellerCategories}
\alias{getBestSellerCategories}
\alias{getBestSellers}
\title{Get information from the NY Times best sellers lists}
\description{

\code{getBestSellerCategories} allows you to retrieve the list
of available categories in the  New York Times best-seller lists. 
\code{getBestSellers} retrieves the data for a particular category.
}
\usage{
getBestSellerCategories(key = getOption("NYTimesAPI")["Best Sellers"])
getBestSellers(list = "Paperback-Books", n = 10, date = NA, offset = 0, 
                 key = getOption("NYTimesAPI")["Best Sellers"])
}
\arguments{
  \item{key}{character string, the API key that authorizes the request}
  \item{list}{the name of the category or best-seller list of interest}
  \item{n}{the number of items to retrieve}
  \item{offset}{the offset into the result set at which to start. This is an integer multiple of 20}
  \item{date}{the date for which the best sellers list is sought. This can be
     approximate and the NY Times services looks at that date and after for the "nearest" list.
  This can be a string in the form of YYYY-MM-DD or a POSIXt object or Date object.}
}
\value{

\code{getBestSellerCategories} returns a character vector.
These include
 "Hardcover Fiction"       "Hardcover Nonfiction"    "Hardcover Advice"        "Paperback Nonfiction"    "Paperback Advice"       
 "Trade Fiction Paperback" "Picture Books"           "Chapter Books"           "Paperback Books"         "Series Books"           
 "Mass Market Paperback".

\code{getBestSellers} returns a data frame
with 11 columns named
rank, list_name, bestsellers_date, published_date, weeks_on_list, rank_last_week, asterisk, dagger, book_details, isbns, reviews.
}
\references{
\url{http://developer.nytimes.com/docs/best_sellers_api}
}
\author{
Duncan Temple Lang
}

\seealso{
}
\examples{
if("NYTimesAPI" \%in\% names(options()) && "Best Sellers" \%in\% names(options("NYTimes"))) {

   categories = getBestSellerCategories()
   sapply(categories, getBestSellers, n = 30)
}
}

\keyword{IO}
\concept{REST}

