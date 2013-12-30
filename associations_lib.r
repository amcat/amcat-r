associations.calcProb <- function(x, calc_type, alpha = 2){
  x[is.na(x)] = 0
  if(calc_type == 'cp') x = 1 - ((1/alpha) ^ x)
  if(calc_type == 'jacard') x[x > 0] = 1
  as.numeric(x)
}

associations.calcConProb <- function(meta, a_scores, b_scores, calc_type='jacard', byMedium=F, byDateinterval=NA){
  ab_data = merge(a_scores[,c('id','hits')],b_scores[,c('id','hits')],by='id',all.x=T)
  ab_data$hits.y[is.na(ab_data$hits.y)] = 0
  ab_data$score.x = associations.calcProb(ab_data$hits.x, calc_type)
  ab_data$score.y = associations.calcProb(ab_data$hits.y, calc_type)
  ab_data$xy = ab_data$score.x * ab_data$score.y
  
  breaks = c()
  if(byMedium == T) breaks=c(breaks, 'medium')
  if(!is.na(byDateinterval)) breaks=c(breaks, 'date')
  
  if(length(breaks) > 0){
    ab_data = merge(ab_data, meta[,c('id',breaks)], by='id',all.x=T, all.y=T)
    ab_data$xy[is.na(ab_data$xy)] = 0
    ab_data$score.x[is.na(ab_data$score.x)] = 0
    
    if('date' %in% breaks) ab_data$date = associations.dateToPeriod(as.Date(format(ab_data$date)), byDateinterval)
    
    break_list = as.list(data.frame(ab_data[,breaks]))
    names(break_list) = breaks
    
    ab_data = aggregate(ab_data[,c('score.x','xy')], by=break_list, FUN='sum')
    ab_data$cp = round((ab_data$xy / ab_data$score.x),2)
    ab_data$cp[ab_data$score.x == 0] = 0
    cp = ab_data[,c(breaks, 'cp')]
  } else cp = round(data.frame(cp=c(sum(ab_data$xy) / sum(ab_data$score.x))),2)
  cp
}

associations.conProb <- function(hits, meta, codes=c(), variable='hits', only_from=c(), only_to=c(), byMedium=F, byDateinterval=NA, calc_type='cp'){
  if(length(codes) == 0) codes = unique(hits$code)
  if(length(only_from) == 0) only_from = codes
  if(length(only_to) == 0) only_to = codes
  
  if(calc_type == 'jacard') dicho = T else dicho = F
  
  results = data.frame()
  for(a in only_from){
    print(a)
    a_scores = hits[hits$code == a,c('id','code',variable)]
    colnames(a_scores) = c('id','code','hits')
    for(b in only_to){
      if(a == b) next
      print(paste('    -->', b))
      b_scores = hits[hits$code == b,c('id','code',variable)]
      colnames(b_scores) = c('id','code','hits')
      
      cp = associations.calcConProb(meta, a_scores, b_scores, calc_type, byMedium, byDateinterval)
      cp$x = a
      cp$y = b
      results = rbind(results, cp)
      
    }
  }  
  results
}

associations.dateToPeriod <- function(date, date_interval){
  if(date_interval == 'trimester'){
    date = associations.formatDateTrimester(as.Date(date)) # omdat er geen standaard format voor kwartaal is...
  } else date = format(as.Date(date), associations.getDateFormat(date_interval))
  date
} 

associations.formatDateTrimester <- function(date){
  year = format(date, '%Y')
  month = format(date, '%m')
  month = ceiling(as.numeric(month) / 3)
  paste(year, '-', month, sep='')
}

associations.getDateFormat <- function(date_interval){
  if(date_interval == 'year') dateformat='%Y' 
  if(date_interval == 'month') dateformat='%Y-%m' 
  if(date_interval == 'week') dateformat='%Y-%U' 
  if(date_interval == 'day') dateformat='%Y-%m-%d'
  dateformat
}
