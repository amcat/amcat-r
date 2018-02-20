#' Create a new codingjob
#' @param conn the connection object from \code{\link{amcat.connect}}
#' @param project the project to add the articles to
#' @param articleset the article set id of an existing set, or the name of a new set to create
#' @param coder the ID of the coder to assign the job to
#' @param articleschema the ID of the articleschema (may be ommitted)
#' @param unitschema the ID of the unit (sentence) schema (may be ommitted)
#' @export
amcat.create.codingjob <- function(conn, project, codingjobname, articleset=articleset, coder=coder, articleschema=NULL, unitschema=NULL) {
  data = list(name=codingjobname, project=project, coder=coder, articleschema=articleschema, unitschema=unitschema, articleset=setid)
  json_data = toJSON(data)
  url = paste(conn$host, "api", "v4", "projects", project, "codingjobs", "", sep = "/")
  message("Creating codingjob for coder ",coder)
  resp = httr::POST(url, body = json_data, content_type_json(), accept_json(), add_headers(Authorization = paste("Token", conn$token)))
  if (resp$status_code != 201) stop("Error on POST to ",url,": ", resp$status_code, " ", http_status(resp)$reason,"\n", content(resp))
  invisible(content(resp)$id)
}
