getBestSellerCategories =
function(key = getOption("NYTimesAPI")["Best Sellers"])
{  
  ans = getForm("http://api.nytimes.com/svc/books/v2/lists/names.xml", 'api-key' = key)
  as.character(getResults(ans, xmlValue))
}


getBestSellers =
  #
  # x = getBestSellers('Hardcover-Fiction')
  #
function(list = 'Paperback-Books', n = 10, date = NA, offset = 0, key = getOption("NYTimesAPI")["Best Sellers"])
{
  list = gsub(" ", "-", list)
  u = if(is.na(date))
          sprintf("http://api.nytimes.com/svc/books/v2/lists/%s.xml", list)
      else {
          if(!is.character(date))
            date = format(date, "%Y-%m-%d")
          sprintf("http://api.nytimes.com/svc/books/v2/lists/%s/%s.xml", date, list)
      }
  tmp = getForm(u, 'api-key' = key)
#  getResults(tmp, xmlToS4)
  doc = xmlParse(tmp, asText = TRUE)
  xmlToDataFrame(nodes = xmlChildren(xmlRoot(doc)[["results"]]))
}

##########################################

newswire =
  # z = newswire(50)
function(n = 100, period = 'recent', offset = 0, ..., key =  getOption("NYTimesAPI")["Newswire"])
{
  period = match.arg(period, c("recent", "last24hours"))
  u = sprintf("http://api.nytimes.com/svc/news/v2/all/%s.xml", period)
  ans = list()
  while(TRUE) {
    tmp = getForm(u, limit = n, offset = offset, 'api-key' = key, binary = FALSE)
    tmp = getResults(tmp, xmlToS4)
    ans = c(ans, tmp)
    if(length(ans) >= n)
      break
    offset = offset + length(tmp)
  }
  ans[seq(length = n)]
}

searchArticles =
  #
  #  See http://developer.nytimes.com/docs/article_search_api/
  #
  #  z = searchArticles("iowa caucus", .opts = list(verbose = TRUE))
  # searchArticles("title:bailout", facets = 'org_facet', begin_date = "20081001", end_date = '20081201', fields = 'org_facet')
function(query, begin_date = NA, end_date = NA, ..., key = getOption("NYTimesAPI")["Article Search"],
           fields = character(), facets = character())
{  
  if(!is.na(begin_date)) {
    if(is.na(end_date))
         end_date = Sys.Date()
    if(inherits(begin_date, "POSIXt") )
       begin_date = format(begin_date, "%Y%m%d")
    if(inherits(end_date, "POSIXt") )
       end_date = format(end_date, "%Y%m%d")    
  }

  args = list(...)
  args$q = paste(query, collapse = " ")

  if(length(fields))
      args$fields = paste(fields, collapse = ",")

  if(length(facets)) {
     if(length(facets) > 5)
         stop("Only 5 facets allowed")
     args$facets = paste(facets, collapse = ",")
  }

  if(!is.na(begin_date)) {
     args$begin_date = begin_date
     args$end_date = end_date     
  }
  args[['api-key']] = key
     
  ans = getForm("http://api.nytimes.com/svc/search/v2/articlesearch.json", .params = args)

 fromJSON(ans) # $results
}


nyTags =
function(query, filter = character(), ..., key = getOption("NYTimesAPI")["Tags"])
{
  u = "http://api.nytimes.com/svc/timestags/suggest"
  args = list(query = query, ..., 'api-key' = key)

  if(length(filter)) {
    filter = match.arg(filter, c("Geo", "Des", "Per", "Org"), TRUE)
    filter = paste("(", filter, ")", sep = "", collapse = ",")
    args$filter = filter    
  }    

  ans = getForm(u, .params = args, binary = FALSE)
#  library(RJSONIO)
  ans = fromJSON(ans)  

  vals = ans$results
   # Take out the category and put it as a name.
  names(vals) = gsub(".* \\(([^\\)]+)\\)$", "\\1", vals)
  vals = gsub(" \\([^\\)]+\\)$", "", vals)
  vals
}

community =
function(what = "recent", match = "exact-match", ..., key = getOption("NYTimesAPI")["Community"])
{
 match = match.arg(match, c("exact-match", "closest-stem-match"))
 u = sprintf("http://api.nytimes.com/svc/community/v2/comments/%s.xml", what)
 args =  list(..., 'api-key' = key)
 getForm(u, .params = args)
}


##############################


getResults =
  #
  # Read the XML and get the <results> node.
  # If func is specified, we apply this to each of the nodes under results
  # to convert the XML elements to R objects.
  #
function(txt, func = NULL)
{
  doc = xmlParse(txt, asText = TRUE)
  res = xmlRoot(doc)[["results"]]
  if(is.null(res))
     stop("no results")
  
  if(!is.null(func))
    xmlSApply(res, func)
  else
    res
}


############################

setClass("URLx", contains = "character")

setAs("XMLInternalElementNode", "URLx", function(from) new("URLx", xmlValue(from)))

setClass('review',
         representation(book_review_link = 'URLx',
                        first_chapter_link = 'URLx',
                        sunday_review_link = 'URLx',
                        article_chapter_link = 'URLx'))

setClass('book_detail',
         representation(
                        title = 'character',
                        description = 'character',
                        contributor = 'character',
                        author = 'character',
                        contributor_note = 'character',
                        price = 'numeric',
                        age_group = 'character',
                        publisher = 'character'))


setClass("book_details", contains = "list")
setClass("reviews", contains = "list")

setClass("book",
         representation(rank = "integer",
                        list_name = "character",
                        bestsellers_date = 'Date',
                        published_date = 'Date',
                        weeks_on_list = 'integer',
                        rank_last_week = 'integer',
                        asterisk = 'logical',
                        dagger = 'logical',
                        book_details = 'book_details',
                        isbns = 'character',
                        reviews = 'reviews'))


setAs("XMLAbstractNode", "book_details", function(from) XML:::xmlToS4List(from))
setAs("XMLAbstractNode", "reviews",  function(from) XML:::xmlToS4List(from))
setAs("XMLAbstractNode", "book_detail", function(from) XML:::xmlToS4(from))
setAs("XMLAbstractNode", "review", function(from) XML:::xmlToS4(from))


# We generate classes using the code in reflection.R now in the XML package.
# x = xmlParse(getForm("http://api.nytimes.com/svc/news/v2/all/recent.xml", 'api-key' = getOption("NYTimesAPI")["Newswire"]))
# x = xmlRoot(x)[["results"]]
#
# dd = makeXMLClasses(x, c("results", "num_results", "result_set"), TRUE)
# k = makeClassTemplate(x[[1]], c('created' = 'POSIXct', updated = 'POSIXct', subsection = "character", subheadline = "character"))
# cat(k$def, k$coerce, sep = "\n\n")
#


setClass('news_item',
    representation(section = 'character',
	subsection = 'character',
	headline = 'character',
	summary = 'character',
	byline = 'character',
	platform = 'character',
	id = 'character',
	type = 'character',
	source = 'character',
	updated = 'POSIXct',
	created = 'POSIXct',
	pubdate = 'character',
	subtype = 'ANY',
	kicker = 'ANY',
	subheadline = 'character',
	terms = 'ANY',
	organizations = 'ANY',
	people = 'ANY',
	locations = 'ANY',
	indexing_terms = 'ANY',
	related_urls = 'ANY',
	categories_tags = 'ANY',
	blog_name = 'character',
	media = 'ANY',
	url = 'character'))

setAs('XMLAbstractNode', 'news_item', function(from) xmlToS4(from))
