codebook = amcat.getcodebook(c, 1)
query_lang = 'query'
concept_lang = 'en'

for (n in result) {
  print(n$code_id)
  print("-----")
}

get_queries <- function(codebook, concept_lang, query_lang) {
  result = list()
  for (node in codebook) {
    concept = node$label[[concept_lang]]
    query = get_query(node, query_lang)
    result[[concept]] = query
  }
}

node = n
get_query <- function(node, query_lang) {
    result = node$label[[query_lang]]
    for(child in node$children) 
      result = c(result, get_query(child, query_lang))
    return(result)
}

get_query(result[[3]], 'query')