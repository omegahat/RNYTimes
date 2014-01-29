\name{book-class}
\Rdversion{1.1}
\docType{class}
\alias{book-class}
\alias{book_details-class}
\alias{book_detail-class}
\alias{news_item-class}
\alias{reviews-class}
\alias{review-class}
\alias{URLx-class}


\title{Class for representing results entities from NYTimes API}
\description{
  These classes allow us to turn XML content into structured, formal
  R objects to describe entities or concepts such as a book,
  a collection of reviews, an individual review and so on.
  The primary purpose is to provide structure from the XML content.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("book", ...)}.
Instances, however, are typically created when the XML content
returned from an NY Times API request is processed within the functions.
}
\section{Slots}{
  \describe{
    \item{\code{rank}:}{Object of class \code{"integer"} the ranking of
      the book on the best seller list}
    \item{\code{list_name}:}{Object of class \code{"character"} the name
      of the best seller list, i.e. the type of book}
    \item{\code{bestsellers_date}:}{Object of class \code{"Date"} the
      date of the entry on the best seller list.}
    \item{\code{published_date}:}{Object of class \code{"Date"} the date
    the book was published}
    \item{\code{weeks_on_list}:}{Object of class \code{"integer"} the
      number of weeks on the best  seller list.}
    \item{\code{rank_last_week}:}{Object of class \code{"integer"} the
      rank on the list the week prior to this entry, allowing us to see
      if it going up or down the rankings or recently enetered. }
    \item{\code{asterisk}:}{Object of class \code{"logical"} ? }
    \item{\code{dagger}:}{Object of class \code{"logical"} ? }
    \item{\code{book_details}:}{Object of class \code{"book_details"}
      details about the book }
    \item{\code{isbns}:}{Object of class \code{"character"} the ISBN
      string for the book, or potentially several of such strings }
    \item{\code{reviews}:}{Object of class \code{"reviews"}  }
  }
}
\section{Methods}{
No methods defined with class "book" in the signature.
}
\references{
  See the NY Times API documentation
}
\author{
 Duncan Temple Lang
}

%\seealso{}
%\examples{}
\keyword{classes}
