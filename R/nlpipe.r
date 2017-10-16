#' Process an articleset with nlpipe
#' 
#' This requires vanatteveldt/nlpiper to be install as well
#' 
#' @param project the amcat project id
#' @param articleset the amcat articleset id
#' @param module the nlpipe module name
#' @param conn the amcatr connection object
#' @param nlpipe_server the nlpipe server name or folder
#'
#' @export
nlpipe <- function(project, articleset, module, conn=conn_from_env(), nlpipe_server=getOption("nlpiper.server", default="http://localhost:5001")) {
  if (is.null(conn)) stop('conn not specified. Either provide conn as argument or run amcat_connect in the current session')
  
  if(!requireNamespace('nlpiper', quietly = T)) stop("Please install_github('/vanatteveldt/nlpiper')")
  ids = get_articles(project, articleset, conn=conn, columns=NULL)$id
  status = nlpiper::status(module, ids, nlpipe_server)
  todo = ids[status == "UNKNOWN"]
  if (length(todo) == 0) {
    message("All ", length(ids), " articles from ", conn$host, " set ", articleset, " are already assigned with ", module, " at ", nlpipe_server)
  } else {
    message("Assigning ", length(todo), " articles from ", conn$host, " set ", articleset, " for processing with ", module, " at ", nlpipe_server)
    chunks = split(todo, ceiling(seq_along(todo)/1000))
    for (chunk in chunks) {
      articles = get_articles(project, conn=conn, articles=chunk, columns=c("headline", "text"))
      texts = paste(articles$headline, articles$text, sep="\n\n")
      nlpiper::process_async(module, texts, ids=articles$id, server = nlpipe_server)
    }
  }
  return(ids) # also return ids that were already on queue
}
