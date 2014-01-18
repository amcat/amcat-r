
#' Get the hierarchy from an AmCAT codebook
#' 
#' Get the labels and parents for all codes in a codebook
#' 
#' @param conn the connection object from \code{\link{amcat.connect}}
#' @param codebook_id the id of the codebook
#' @param languages which languages to retrieve
#' @return A data frame with code, parent, and one column per language
#' @export
amcat.gethierarchy <- function(conn, codebook_id, languages=NULL) {
  
  hierarchy = amcat.getobjects(conn, "codebookcode", filters=list(codebook__id=codebook_id))
  hierarchy = hierarchy[,c("code", "parent")]
  
  for (lang in languages) {
    r = amcat.getobjects(conn, "language", filters=list(label=lang))
    if (nrow(r) != 1) stop(paste("Searching for language", lang, "gave", nrow(r), "results"))
    lang_id = r$id
    labels = amcat.getobjects(conn, "label", filters=list(language__id=lang_id, 
                                                          code__codebook_codes__codebook__id=codebook_id))
    labels = labels[, c("code", "label")]
    colnames(labels) = c("code", paste("label", lang, sep="."))  
    hierarchy = merge(hierarchy, labels, all.x=T)
  }
  
  if (!is.null(languages)) {
    labels = rep(NA, nrow(hierarchy))
    for (lang in languages) {
      labelcol = paste("label", lang, sep=".")
      labels[is.na(labels)] = as.character(hierarchy[is.na(labels), labelcol])
    }
    labels = factor(labels)
    hierarchy$parent = labels[match(hierarchy$parent, hierarchy$code)]
    hierarchy$code = labels
    
  }
  
  
  
  return(hierarchy)
}

