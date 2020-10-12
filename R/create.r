#' Upload new articles to AmCAT
#' 
#' @description
#' Upload a data.frame with article data to an articleset in AmCAT. Article data must be provided as a data.frame, 
#' in which every row represents an article, and every column represents a data field. All columns will be uploaded to AmCAT.
#' 
#' There are some \strong{special fields}, and it is recommended to use specific column names if you have this type of data.
#' Most importantly, the \strong{date} column is mandatory. Other common fields are \strong{text}, \strong{title}, \strong{byline}, \strong{url}, \strong{medium}, \strong{section} and \strong{author}.
#' See details (below) for more information.
#' 
#' All other columns in data will also be uploaded. By default, these columns will be stored as text, but specific field types (int, date, id, etc.) can be specified in the column name.
#' See details for the most imporant field types and how to use them.
#' 
#' @param data         A data.frame in which rows represent articles and columns represent data fields. See details for information about column names.
#' @param project      The id of the project to add the articles to.
#' @param articleset   The article set id of an existing set, 
#' @param new_set      The name of a new set to create a new articles. Note that passing the name of an existing set will create a new set with the same name. For uploading to an existing set, use the 'articleset' argument.
#' @param provenance   If a new articleset is created (articleset is a name), an optional provenance string to store with the new set. If NULL, a default string mentioning the number of articles and creation date is used.
#' @param conn         The connection object from \code{\link{amcat_connect}}.
#' @param verbose      If true, report progress
#' 
#' @return the articleset id
#' 
#' @details 
#' 
#' The following field names have special meaning in AmCAT, and are recommended if you have this type of data.
#' 
#' \tabular{ll}{
#'   \strong{_date}    \tab Calendar date. Use a date type vector (Date, POSIXct or POSIXlt) or character type in format "%Y-%m-%d" or "%Y-%m-%d %H:%S:%M".   \cr
#'   \strong{text}     \tab The main body. Indexed for query searches. \cr
#'   \strong{title}    \tab The document title. Displayed above the text and indexed. \cr
#'   \strong{byline}   \tab A byline. Displayed below the title and indexed. \cr
#'   \strong{url}      \tab The URL, stored as a (clickable) keyword. \cr
#'   \strong{medium}   \tab The source of a document. \cr 
#'   \strong{section}  \tab A sub-category \cr
#'   \strong{author}   \tab The author of the document \cr
#' }
#' 
#' For alternative fields, it is possible (not required) to specify field types. To do so, add the type to the field name (column name in 'data'), separated by an underscore.
#' For example, to make the column "page" an integer column, use "page_int". The most important field types are:
#' 
#' \tabular{ll}{
#'   \strong{_int}     \tab Integers          \cr
#'   \strong{_date}    \tab Calendar date. Use a date type vector (Date, POSIXct or POSIXlt) or character type in format "%Y-%m-%d" or "%Y-%m-%d %H:%S:%M".   \cr
#'   \strong{_num}     \tab Numeric (double)  \cr
#'   \strong{_id}      \tab Keyword. For names and structured content such as zip codes \cr
#'   \strong{_url}     \tab Special keyword for URLs          \cr
#'   \strong{_text}    \tab Full text              \cr
#'   \strong{default}  \tab Without an explicit field type, field is stored as text \cr
#' }
#' 
#' @examples
#' \dontrun{
#' conn = amcat_connect('https://amcat.nl', 'username')
#' 
#' set_id = create_articles(project = 1, articleset = 'example set', 
#'                          text = 'example', date = as.POSIXct('2010-01-01'), medium = 'example', headline = 'headline')
#'                          
#' get_articles(project = 1, articleset = set_id, 
#'              columns = c('headline','text','date','medium'))
#' }
#' @export
create_articles <- function(data, project, articleset=NULL, new_set=NULL, provenance=NULL, conn=conn_from_env(), verbose=T) {
  if (is.null(conn)) stop('conn not specified. Either provide conn as argument or run amcat_connect in the current session')
  if (is.null(articleset) + is.null(new_set) != 1) stop("Either 'articleset' OR 'new_set' must be specified")
  
  ## check mandatory columns
  if (!'date' %in% colnames(data)) stop('data must contains a "date" column')
  
  for (f in names(data)) {
    if (is.factor(data[[f]])) data[[f]] = as.character(data[[f]])
    if (grepl('_(int|num)', f)) data[[f]] = as.numeric(data[[f]])
    if (f == 'date' || grepl('_date$', f)) data[[f]] = prepare_date(data[[f]], f)
  }
  
  ## check date
  n = nrow(data)
  if (!is.null(new_set)) {
    if (is.null(provenance)) provenance=paste("Uploaded", n, "articles from R on", format(Sys.time(), "%FT%T"))
    articleset = create_articleset(project, new_set, provenance, conn=conn)
  }

  chunk_index = split(1:n, ceiling((1:n)/100))
  if (verbose) pb = utils::txtProgressBar(min = 0, max = length(chunk_index), style = 3)
  for(i in seq_along(chunk_index)) {
    json_data = jsonlite::toJSON(data[chunk_index[[i]],,drop=F])
    r = request(c('projects',project,'articlesets',articleset,'articles'), conn=conn, json_data=json_data, post=TRUE) 
    if (verbose) pb$up(i)
  }
  invisible(articleset)
}

#' Create a new articleset on AmCAT
#'
#' @param project     The project id
#' @param name        The name of the new set
#' @param provenance  A provenance message. If NULL, a default message noting R and the time of creation is used.
#' @param conn        The connection object from \code{\link{amcat_connect}}. 
#'
#' @return The id of the new articleset
#' @export
create_articleset <- function(project, name, provenance=NULL, conn=conn_from_env()) {
  if (is.null(conn)) stop('conn not specified. Either provide conn as argument or run amcat_connect in the current session')
  if (is.null(provenance)) provenance = sprintf('Created in R on %s', Sys.time())
  
  r = request(c('projects',project,'articlesets'), param=list(name=name, provenance=provenance), conn=conn, post=TRUE) 
  
  articleset = r$id
  message("Created articleset ", articleset, ": ", name," in project ", project)
  invisible(articleset)
}

#' Create a new project in AmCAT
#'
#' @param name        The name of the new set
#' @param description A brief description
#' @param owner       Specify the user that owns the project. Either provide an existing username (character) or user id (numeric). If NULL, 
#'                    the username of the current connection is used.
#' @param active      If TRUE, set project as active (which makes a lot of sense for a new project)
#' @param guest_role  Determine the priviledges of guests (i.e. users not assigned to the project). See details for options.
#' @param conn        The connection object from \code{\link{amcat_connect}}. 
#'
#' \tabular{ll}{
#'   \strong{metareader}  \tab Guest can see and query articlesets, but only sees meta data and snippets (not full texts) \cr
#'   \strong{reader}      \tab The above, plus read articles, and copy articlesets to their own project \cr
#'   \strong{read/write}  \tab The above, plus create and modify articlesets  \cr
#'   \strong{admine}      \tab The above, plus managing user privileges and deleting articlesets \cr
#'   \strong{none}        \tab Give no access. \cr
#' }
#' 
#' @return The id of the new project
#' @export
create_project <- function(name, description, owner=NULL, active=T, guest_role=c('metareader','reader','read/write','admin','none'), conn=conn_from_env()) {
  guest_role = match.arg(guest_role)
  
  if (is.null(conn)) stop('conn not specified. Either provide conn as argument or run amcat_connect in the current session')
  guest_role_ids = list('metareader' = '10', 'reader' = '11', 'read/write' = '12', 'admin' = '13', 'none' = '')
  
  if (is.null(owner)) owner = conn$username
  if (is.character(owner)) {
    users = get_users(conn=conn)
    owner = users$id[users$username == owner]
    if (length(owner) == 0) stop("'owner' is not an existing username")
  }
  
  r = request('projects', param=list(name=name, description=description, favourite=1, owner=owner, guest_role=guest_role_ids[[guest_role]]), conn=conn, post=TRUE) 
  articleset = r$id
  message("Created articleset ", articleset, ": ", name," in project ", project)
  invisible(articleset)
}


#' Create a new user on AmCAT
#' 
#' If the current user is a superuser, new users can be added.
#'
#' @param username    The username on AmCAT. Max 30 characters. Letters, digits and [@.+-_] only.
#' @param password    The password of the new user
#' @param first_name  The first name of the new user
#' @param last_name   The last name of the new user
#' @param email       The user's email adress
#' @param active      If False, users are not considered 'active'. Unselect this instead of deleting accounts.
#' @param superuser   If TRUE, make user a superuser
#' @param staff       If TRUE, give user staff status
#' @param conn        The connection object from \code{\link{amcat_connect}}. 
#'
#' @return The id of the new articleset
#' @export
create_user <- function(username, password, first_name, last_name, email, active=T, superuser=F, staff=F, conn=conn_from_env()) {
  if (is.null(conn)) stop('conn not specified. Either provide conn as argument or run amcat_connect in the current session')
  
  r = request('users', username=username, password=password, 
              first_name=first_name, last_name=last_name, email=email,
              is_staff=staff, is_active=active, is_superuser=superuser, conn=conn, post=TRUE) 
  
  request(c('user', 2), username='non', post=T)
  
  user_id = r$id
  message("Created user ", username, ' with id ', user_id)
  invisible(user_id)
}


prepare_date <- function(date, columnname) {
  date_classes = c('character','factor','Date','POSIXlt','POSIXct')
  if (!any(sapply(date_classes, function(x) is(date, x)))) stop(paste0('date type column (', columnname, ') must either a date type vector (Date, POSIXct or POSIXlt) or character type in format "%Y-%m-%d" or "%Y-%m-%d %H:%S:%M"'))
  if (is(date, 'factor')) date = as.character(date)
  if (is(date, 'character')) {
    date = as.POSIXct(date)
    if (any(is.na(date))) stop(paste0('date type column (', columnname, ') must either a date type vector (Date, POSIXct or POSIXlt) or character type in format "%Y-%m-%d" or "%Y-%m-%d %H:%S:%M"'))
  }
  if (is(date, 'Date')) date = format(date, "%Y-%m-%d")  
  if (is(date, 'POSIXct') || is(date, 'POSIXlt')) date = format(date, "%Y-%m-%dT%H:%M:%S") 
  date
}

