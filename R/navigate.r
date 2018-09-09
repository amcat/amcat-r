#' View projects
#' 
#' @param only_visited If TRUE, only projects that the user visited will be shown
#' @param only_favourite If TRUE, only "favourite" projects are shown
#' @param columns the names of columns to retrieve, e.g. date, medium, text, headline. If set to NULL, all available columns are retrieved
#' @param conn the connection object from \code{\link{amcat_connect}}
#'
#' @return A data.frame with projects
#' 
#' @examples
#' \dontrun{
#' conn = amcat_connect('https://amcat.nl', 'username')
#' get_projects()
#' }
#' @export
get_projects <- function(only_visited=T, only_favourite=F, columns=c('id','name','active','favourite','weeks_ago','guest_role'), conn=conn_from_env()){
  if (is.null(conn)) stop('conn not specified. Either provide conn as argument or run amcat_connect in the current session')
  
  result = get_pages('projects', conn=conn, verbose = F)
  
  if (only_visited) result = result[!grepl('Never', result$last_visited_at),]
  if (only_favourite) result = result[result$favourite,]
  
  ## weeks since last visit
  result$weeks_ago = suppressWarnings(as.numeric(gsub(' .*', '', result$last_visited_at)))
  result$weeks_ago[grepl('day', result$last_visited_at)] = floor(result$weeks_ago[grepl('day', result$last_visited_at)] / 7)
  result$weeks_ago[grepl('hour|minute|second', result$last_visited_at)] = 0
  is_date = !grepl('day|week|hour|minute|second|Never', result$last_visited_at)
  result$weeks_ago[is_date] = floor(difftime(Sys.time(), as.POSIXct(result$last_visited_at[is_date]), units = 'week'))
  result = result[order(result$weeks_ago),]
  
  if (!is.null(columns)) {
    if (!all(columns %in% colnames(result))) warning(paste0("Unavailable columns specified (", paste(setdiff(columns, colnames(result)), collapse=','), ")"))
    result[,columns[columns %in% colnames(result)]]
  } else result
}

#' View projects
#' 
#' @param project The project id
#' @param only_favourite If TRUE, only "favourite" projects are shown
#' @param columns the names of columns to retrieve, e.g. date, medium, text, headline. If set to NULL, all available columns are retrieved
#' @param conn the connection object from \code{\link{amcat_connect}}
#'
#' @return A data.frame with articlesets
#' 
#' @examples
#' \dontrun{
#' conn = amcat_connect('https://amcat.nl', 'username')
#' get_articlesets(project=1)
#' }
#' @export
get_articlesets <- function(project, only_favourite=F, columns=c('id','project','name','featured','favourite','provenance'), conn=conn_from_env()){
  if (is.null(conn)) stop('conn not specified. Either provide conn as argument or run amcat_connect in the current session')
  
  result = get_pages(c("projects",project,"articlesets"), conn=conn, col=columns, verbose = F)
  if (nrow(result) == 0) return(result)
  if (only_favourite) result = result[result$favourite,]
  if (nrow(result) == 0) return(result)
  result = result[order(-result$id),]
  
  if (!is.null(columns)) {
    if (!all(columns %in% colnames(result))) warning(paste0("Unavailable columns specified (", paste(setdiff(columns, colnames(result)), collapse=','), ")"))
    result[,columns[columns %in% colnames(result)]]
  } else result
}

get_users <- function(username, conn=conn_from_env()) {
  get_pages('user', conn=conn)
} 