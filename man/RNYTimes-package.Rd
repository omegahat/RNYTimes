\name{RNYTimes-package}
\alias{RNYTimes-package}
\alias{RNYTimes}
\docType{package}
\title{Access to the New York Times REST services
}
\description{
This package provides R functions to access content
from the New York Times using its REST services.
}
\details{
\tabular{ll}{
Package: \tab RNYTimes\cr
Version: \tab 0.1-0\cr
Imports: \tab XML, RJSONIO\cr
License: \tab BSD\cr
Built: \tab R 2.11.0; ; 2009-10-03 16:54:38 UTC; unix\cr
}

The function provides a function for each of several of the NY Times Web Services.
One can search articles


You need a (different) key  to access each API. These are obtained by 
requesting a key via the NY Times site \url{developer.nytimes.com}.
You can specify the relevant key in each call, but it is more convenient
to set them in the R options object and have the functions
retrieve them as they are needed. 
You set the option named \code{NYTimesAPI} to a named character
vector with entries for each of the APIs for which you have a key.
The names should be "Best Sellers", "Newswire", "Tags", "Community", "Article Search".
An example of setting these is 
\code{
options("NYTimesAPI" = c("Best Sellers" = "xxxxxxxxxxx:17:12345678",
                         "Newswire" = "xadsda:10:372324576",
                         "Tags" = "xxxxxxxxxxxxxx:14:12345678",
                         Community = "xxxxxxxxxxxxx:9:12345678",
                         "Article Search" = "xxxxxxxxxxxx:17:12345678"))
}
}
\author{
Duncan Temple Lang
Maintainer: Duncan Temple Lang <duncan@r-project.org>
}
\references{
NY Times developer page
}

\keyword{ package }
%\seealso{}
\examples{

}
