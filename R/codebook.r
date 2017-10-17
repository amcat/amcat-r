
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





.codebookcat <- function(hierarchy, depth=0) {
  # depth 0 is root, depth 1 is cat, etc
  get.ancestors <- function (hierarchy){
    p = data.frame(c=hierarchy$code, p1 = hierarchy$parent)
    for (i in 1:nrow(p)) { #nrow(p) is max number of required merges, but we'll break sooner
      m = p[, c("c", "p1")]
      colnames(m) = paste("p", c(i, i+1), sep="")
      p = merge(p, m, all.x=T)
      if (all(is.na(p[, ncol(p)]))) {
        # last column is all NA, so we are done. Drop the column and break to return
        p = p[, -ncol(p)]
        break
      }
    }
    p = p[, sort(colnames(p))]
    return(p)
  }
  anc = get.ancestors(hierarchy)
  x = rep(NA, nrow(anc))
  for (i in 1:(ncol(anc) - depth)) {
    col = anc[, i]
    parent = anc[, i+depth]
    x[!is.na(parent)] = as.character(col[!is.na(parent)])
  }
  
  return(x[match(hierarchy$code, anc$c)])
}

#' Add categories (ancestors) to a codebook 'hierarchy'
#' 
#' Adds one or more categories to codebook codes. Suppose that you have a hierarchy like
#' 
#' code, parent
#' issue, NA
#' economy, issue
#' unemployment, economy
#' environment, issue
#' 
#' The first category or 'root' for all objects will be 'issue'. The second category would be
#' 'economy' for economy and unemployment, and 'environment' for environment. For 'issue', the second
#' category would simply be issue:
#' 
#' code, parent, cat1, cat2
#' issue, NA, issue, issue
#' economy, issue, issue, economy
#' unemployment, economy, issue, economy
#' environment, issue, issue, environment
#' #' 
#' @param hierarchy the hierarchy data frame from \code{\link{amcat.gethierarchy}}
#' @param maxdepth the maxium number of ancestors per code
#' @return The hierarchy data frame with a column added for each code
#' @export
amcat.hierarchy.cats <- function(hierarchy, maxdepth=2) {
  for(depth in 0:maxdepth) {
    target = paste("cat", (depth+1), sep=".")
    hierarchy[, target] = .codebookcat(hierarchy, depth)  
    if (depth > 0) {
      fallback = paste("cat", (depth), sep=".")
      hierarchy[is.na(hierarchy[,target]), target] = hierarchy[is.na(hierarchy[, target]), fallback]
    }
  }
  # Thanks, http://stackoverflow.com/questions/16441952/sort-a-data-frame-by-multiple-columns-whose-names-are-contained-in-a-single-obje
  sortnames = paste("cat", (0:maxdepth) + 1, sep=".")
  hierarchy[do.call("order", hierarchy[, sortnames]),]
}





.codebookcat <- function(hierarchy, depth=0) {
  # depth 0 is root, depth 1 is cat, etc
  get.ancestors <- function (hierarchy){
    p = data.frame(c=hierarchy$code, p1 = hierarchy$parent)
    for (i in 1:nrow(p)) { #nrow(p) is max number of required merges, but we'll break sooner
      m = p[, c("c", "p1")]
      colnames(m) = paste("p", c(i, i+1), sep="")
      p = merge(p, m, all.x=T)
      if (all(is.na(p[, ncol(p)]))) {
        # last column is all NA, so we are done. Drop the column and break to return
        p = p[, -ncol(p)]
        break
      }
    }
    p = p[, sort(colnames(p))]
    return(p)
  }
  anc = get.ancestors(hierarchy)
  x = rep(NA, nrow(anc))
  for (i in 1:(ncol(anc) - depth)) {
    col = anc[, i]
    parent = anc[, i+depth]
    x[!is.na(parent)] = as.character(col[!is.na(parent)])
  }
  
  return(x[match(hierarchy$code, anc$c)])
}

#' Change a (simple) lucene query to a regular expression
#' 
#' It is assumed that this will be used to filter single words/lemmata, so the query should only consist of a list of synonyms with optional wildcards.
#' E.g. a query like (many* words*) is fine, but many AND words or "many words" will give an error.
#' 
#' @param queries a character vector containing the queries
#' @param names an optional vector of the same length as queries containing names for the concepts
#' @return a character vector of regular expressions, with names added if given
#' @export
lucene_to_re <- function(queries, names=NULL) {
  # remove trailing/leading parens
  queries = gsub("^\\s*\\(|\\)\\s*$", "", queries)
  # check no weird syntax
  if (any(grepl(' (OR|AND|NOT) |\\(|\\)|"', queries)))
    stop("Queries should only contain disjunctions and wildcards")
  # replace space by | and * by .*
  queries = gsub("[, ]+", "|", queries)
  queries = gsub("\\*", ".*", queries)
  # add parens and initial/final bind and return
  queries = paste("^(", queries, ")$", sep="")
  if (!is.null(names))
    names(queries) = names
  queries
}



#' Matches a regular expression pattern on multiple strings
#' 
#' This is intended as a 'drop-in' replacement for grepl and should give identical results
#' This function first matches on the unique strings and then matches back to the original,
#' so it will give a speedup iff length(unique(words)) << length(words)
#' 
#' @param pattern a regular expression pattern to use
#' @param words a vector of words, this should probably be a factor to achieve speedup
#' @param ignore.case passed to grepl, defaults to TRUE for this function
#' @return a vector of booleans indicating which words matched the pattern
#' @export
match_words <- function(pattern, words, ignore.case=T) {
  u = unique(words)
  hits = u[grepl(pattern, u, ignore.case=ignore.case)]
  words %in% hits
}