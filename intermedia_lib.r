library(reshape)
library(lme4)
library(sandwich)


intermedia.organizeData <- function(hits, meta, variable='hits', codes=c(), split_hours=c(), skip_weekdays=c(), mediameta=data.frame()){
  if(length(codes) == 0) codes = unique(hits$code)
  h = hits[hits$code %in% codes & hits$id %in% meta$id,]
  
  meta$day = as.Date(format(meta$date))
  meta$hour = intermedia.getPublicationHour(meta, mediameta=mediameta)
  
  alldayparts = c()
  for(i in 1:length(c(0,split_hours))) alldayparts = c(alldayparts,paste(c(0,split_hours)[i],'h-',c(0,split_hours,24)[i+1],'h',sep=''))   
  meta$daypart = alldayparts[1]
  for(i in 1:length(split_hours)) meta$daypart[meta$hour >= split_hours[i]] = alldayparts[i+1]
  meta$daypart = as.factor(meta$daypart)
  
  if(nrow(mediameta) == 0) mediameta = data.frame(medium_id=unique(meta$medium), medium_string=amcat.getMediumString(unique(meta$medium)))
  meta = merge(meta, mediameta[,c('medium_id','medium_string')], by.x='medium', by.y='medium_id',all.x=T)
  
  d = merge(data.frame(id=h$id, hits=h[,variable], code=h$code), meta, by='id', all.x=T)
  d = aggregate(hits ~ medium_string + code + day + daypart, data=d, FUN='length')
  d = reshape(d[,c('medium_string','code','day','hits','daypart')], timevar=c('medium_string'), idvar=c('code','day','daypart'), direction='wide')
  
  alldays = seq(min(d$day), max(d$day), by='day')
  dayXdaypart = data.frame(day=rep(alldays, each=length(alldayparts)),daypart=rep(alldayparts, times=length(alldays)))
  
  dayXdaypart$weekday = as.numeric(format(dayXdaypart$day, '%w'))
  dayXdaypart$weekday[dayXdaypart$weekday == 0] = 7
  dayXdaypart = dayXdaypart[!dayXdaypart$weekday %in% skip_weekdays,]
  
  dayXdaypart$t = 1:nrow(dayXdaypart)
  
  allcodes = unique(d$code)
  allcases = NULL
  for(code in unique(d$code)) allcases = rbind(allcases, cbind(dayXdaypart, code=rep(code, nrow(dayXdaypart))))
  
  d = d[d$day %in% allcases$day,]
  d = merge(allcases, d, by=c('day','daypart','code'), all.x=T)
  narts = aggregate(cbind(total=meta$id) ~ medium_string + day + daypart, meta, FUN='length')
  narts = reshape(narts, timevar=c('medium_string'), idvar=c('day','daypart'), direction='wide')
  d = merge(d, narts, by=c('day','daypart'), all.x=T)
  
  d[is.na(d)] = 0 
  d = d[order(d$code,d$t),]
  
  d
}

intermedia.mediumXweekday <- function(meta) {
  meta$weekday = as.numeric(format(meta$date, '%w'))
  meta$weekday[meta$weekday == 0] = 7
  reshape(aggregate(id ~ weekday + medium, meta, FUN='length'), timevar='medium',idvar=c('weekday'), direction='wide')
}

intermedia.prepareData <- function(hits, meta, day_lag=1, split_hours=c(), skip_weekdays=c(), skip_missingdays=TRUE, mediameta=mediameta){
  d = intermedia.organizeData(hits, meta, split_hours=split_hours, skip_weekdays=skip_weekdays, mediameta=mediameta)
  lagscores = intermedia.getLag(d, day_lag=day_lag)
  d = d[d$t %in% lagscores$t,]
  d = merge(d, lagscores, by=c('t','code'), all.x=T)
  if(skip_missingdays == TRUE) {
    missingdays = intermedia.missingDays(d, skip_weekdays)
    if(length(missingdays) > 0){
      d = intermedia.deleteMissingDays(d, missingdays, day_lag=day_lag) 
      print('Deleted missing days (and consecutive days to account for lag)')
    }
  }
  d[order(d$code,d$day,d$daypart),]
}

intermedia.missingDays <- function(d, skip_weekdays=c()) {
  d = d[d$code == d$code[1] & !d$weekday %in% skip_weekdays,]
  agg = aggregate(d[,colnames(d)[grep('total.',colnames(d))]], by=list(day=d$day), FUN='sum')
  allmissingdays = c()
  for(column in colnames(d)[grep('total.',colnames(d))]) {
    missingdays = as.character(agg$day[agg[,column] == 0])
    print(paste(column,': ', length(missingdays), ' days missing',sep=''))
    allmissingdays = unique(c(allmissingdays, missingdays))
  }
  print(paste("Total of unique days missing:", length(allmissingdays)))
  as.Date(allmissingdays)
}

intermedia.deleteMissingDays <- function(d, missingdays, day_lag=1){
  missingdays = missingdays[order(missingdays, decreasing=T)]
  for(mis in missingdays){
    ts = unique(d$t[d$day == mis])
    maxts = max(ts) + day_lag*length(ts)  
    d = d[!d$t %in% (min(ts):maxts),]
  }
  d
}

intermedia.getLag <- function(d, day_lag=1) {  
  ndayparts = length(unique(d$daypart)) #(max(d$t) / length(unique(d$day)) for dubbelcheck
  lag = day_lag * ndayparts
  
  media = unique(as.character(sapply(colnames(d)[grep('hits', colnames(d))],intermedia.getMediumName)))
  hitsvars = paste('hits',media,sep='.')
  lagvars = paste('lag',media,sep='.')
  
  codes = unique(d$code)
  for(lagvar in lagvars) d[,lagvar] = NA
  first_t = min(d$t)

  d = reshape(d[,c('code','t',hitsvars,lagvars)], timevar=c('code'), idvar=c('t'), direction='wide')
  N = nrow(d)
  sufficient_lag_cases = rep(0,nrow(d))   
  
  hitsvarsXcodes = paste(rep(as.character(hitsvars),each=length(codes)),codes,sep='.')
  lagvarsXcodes = paste(rep(lagvars,each=length(codes)),codes,sep='.')
  
  print('Calculating lag:')
  for(i in 1:N){
    if(i %% 100 == 0) print(paste(i, ' / ', N))
    r = d[i,]
    min_t = r$t - lag
    if(min_t < first_t) next # als de lag niet helemaal berekend kan worden, dan blijft lagscore NA    
    sufficient_lag_cases[i] = 1
    d[i,lagvarsXcodes] = colSums(d[d$t >= min_t & d$t < r$t, hitsvarsXcodes])
  }
  d = d[sufficient_lag_cases == 1,]
  d = reshape(d[,c('t',lagvarsXcodes)], idvar='t', ids=d$t, times=lagvarsXcodes, varying=list(lag=lagvarsXcodes), v.names='lag',direction='long')
  d$medium = sapply(d$time, function(txt) strsplit(as.character(txt), fixed=T, '.')[[1]][2])
  d$code = sapply(d$time, function(txt) strsplit(as.character(txt), fixed=T, '.')[[1]][3])
  d = reshape(d[,c('t','lag','medium','code')], timevar='medium', idvar=c('t','code'), direction='wide')
  d = data.frame(t=d$t,code=d$code,d[,lagvars],row.names=NULL)
  colnames(d) = c('t','code',lagvars)
  d
}

intermedia.runAnalysis <- function(d, b_digits=2, se_digits=2, randomintercepts=F, binomial=F, control_for_daytotal=F, use_ar=T){  
  modelindex = intermedia.getModelIndex(d)
  media = unique(modelindex$medium)
  models_output = NULL
  #if(randomintercepts==TRUE) print('Using random intercepts for codes') else print("Using pooled issues. Robuse SE's are used")
  for(i in 1:nrow(modelindex)){
    medium = modelindex$medium[i]
    daypart = modelindex$daypart[i]
    print(paste('Analyzing model for', medium,'at daypart', daypart))
    
    d$autoregression = d[,paste('lag',medium,sep='.')]
    d$hits = d[,paste('hits',medium,sep='.')]
    
    fullformula = 'hits ~ '
    if(control_for_daytotal == TRUE) {
      d$daytotal = d[,paste('total',medium,sep='.')]
      fullformula = paste(fullformula, 'daytotal + ', sep='')
    }
    if(use_ar == TRUE) fullformula = paste(fullformula, 'autoregression +', sep='')
    independent_media = paste('lag',media[!media == medium],sep='.')
    fullformula = paste(fullformula, paste(independent_media, collapse=' + '), sep='')
    
    if(binomial == T) {
      d$hits[d$hits > 1] = 1
      family = 'binomial'
    } else family = 'poisson'
    
    if(randomintercepts==TRUE) {
      fullformula = paste(fullformula, '+ (1|code)') 
      m_base = glmer(hits ~ (1|code), family=family,data=d[d$daypart == daypart,])
      m = glmer(as.formula(fullformula), family=family,data=d[d$daypart == daypart,])
      mo = intermedia.getLmerOutput(m, b_digits=b_digits, se_digits=se_digits)   
      modelindex$delta.loglik[i] = intermedia.getGlmerFit(m, m_base)
      
    } else {   
      m_base = glm(hits ~ 1, family=family,data=d[d$daypart == daypart,])
      m = glm(as.formula(fullformula), family=family,data=d[d$daypart == daypart,])
      
      # having fun with intercepts
      #base_intercept = as.numeric((coef(m_base)['(Intercept)']))
      #full_intercept = as.numeric((coef(m)['(Intercept)']))
      # if(binomial == T){
      #  p_base = 1 / (1 + exp(-base_intercept))
      #  p_controlled = 1 / (1 + exp(-full_intercept))
      #} else{
      #  p_base = exp(base_intercept)
      #  p_controlled = exp(full_intercept)
      #}
      #print(paste(medium, ': ', p_base, ' - ', p_controlled, sep=''))
      
      if(binomial == T){ 
        mo = intermedia.getGlmOutput(m, b_digits=b_digits, se_digits=se_digits)
      } else mo = intermedia.getRobustSePoissonOutput(m, b_digits=b_digits, se_digits=se_digits)
      modelindex$delta.deviance[i] = intermedia.getGlmFit(m, m_base)
    }    
    mo$model = paste(medium,daypart)
    mo$medium = medium
    mo$daypart = daypart
    models_output = rbind(models_output, mo)
  }
  list(coefficients=models_output,models=modelindex)
}

intermedia.getLmerOutput <- function(m, b_digits=2, se_digits=3){
  vc <- vcov(m, useScale = FALSE)
  b <- fixef(m)
  se <- sqrt(diag(vc))
  z <- b / sqrt(diag(vc))
  P <- 2 * (1 - pnorm(abs(z)))
  P[is.na(P)] = 1
  fe = format(round(b,b_digits), digits=b_digits)
  fe[P < 0.05] = paste(fe[P < 0.05], '*', sep='') ; fe[P >= 0.05] = paste(fe[P >= 0.05], ' ', sep='')
  fe[P < 0.01] = paste(fe[P < 0.01], '*', sep='') ; fe[P >= 0.01] = paste(fe[P >= 0.01], ' ', sep='')
  fe[P < 0.001] = paste(fe[P < 0.001], '*', sep='') ; fe[P >= 0.001] = paste(fe[P >= 0.001], ' ', sep='')
  fe_se = paste(fe, ' (',format(round(se,se_digits),digits=se_digits), ')', sep='')
  data.frame(variable = names(b), b=b, se=se, p=P, label=fe, label_se=fe_se)
}

intermedia.getRobustSePoissonOutput <- function (model, b_digits=2, se_digits=2) {
  output = intermedia.getRobustSe(model)
  varnames = rownames(summary(model)$coef)
  b = output[,'Estimate']
  P = output[,'Pr(>|z|)']
  P[is.na(P)] = 1
  se = output[,'Robust SE']
  fe = format(round(b,b_digits), digits=b_digits)
  fe[P < 0.05] = paste(fe[P < 0.05], '*', sep='') ; fe[P >= 0.05] = paste(fe[P >= 0.05], ' ', sep='')
  fe[P < 0.01] = paste(fe[P < 0.01], '*', sep='') ; fe[P >= 0.01] = paste(fe[P >= 0.01], ' ', sep='')
  fe[P < 0.001] = paste(fe[P < 0.001], '*', sep='') ; fe[P >= 0.001] = paste(fe[P >= 0.001], ' ', sep='')
  fe_se = paste(fe, ' (',format(round(se,se_digits),digits=se_digits), ')', sep='')
  data.frame(variable = rownames(output), label_se = fe_se, label=fe, b=b, p=P)
}

intermedia.getGlmOutput <- function (model, b_digits=2, se_digits=2) {
  b = coef(model)
  vc <- vcov(model, useScale = FALSE)
  se <- sqrt(diag(vc))
  P = 2 * pnorm(abs(b/se), lower.tail = FALSE)
  P[is.na(P)] = 1  
  varnames = rownames(summary(model)$coef)
  fe = format(round(b,b_digits), digits=b_digits)
  fe[P < 0.05] = paste(fe[P < 0.05], '*', sep='') ; fe[P >= 0.05] = paste(fe[P >= 0.05], ' ', sep='')
  fe[P < 0.01] = paste(fe[P < 0.01], '*', sep='') ; fe[P >= 0.01] = paste(fe[P >= 0.01], ' ', sep='')
  fe[P < 0.001] = paste(fe[P < 0.001], '*', sep='') ; fe[P >= 0.001] = paste(fe[P >= 0.001], ' ', sep='')
  fe_se = paste(fe, ' (',format(round(se,se_digits),digits=se_digits), ')', sep='')
  
  data.frame(variable = varnames, label_se = fe_se, label=fe, b=b, p=P)
}

intermedia.getRobustSe <-function(model){
  ##http://www.ats.ucla.edu/stat/r/dae/poissonreg.htm
  cov.model <- vcovHC(model, type = "HC0")
  std.err <- sqrt(diag(cov.model))
  coef.model = coef(model)
  #class(coef.model)
  coef.model[is.na(coef.model)] = 0
  missing = names(coef.model[!names(coef.model) %in% names(std.err)])
  coef.model = coef.model[!names(coef.model) %in% missing]
  r.est <- cbind(Estimate = coef.model, `Robust SE` = std.err, `Pr(>|z|)` = 2 * 
                   pnorm(abs(coef.model/std.err), lower.tail = FALSE), LL = coef.model - 1.96 * 
                   std.err, UL = coef.model + 1.96 * std.err)
  if(length(missing) > 0){
    missing_line = data.frame(dit=rep(0,length(missing)),is=0,mega=1,stom=NA,hoor=NA)
    rownames(missing_line) = missing
    colnames(missing_line) = colnames(r.est)
    r.est = rbind(r.est, missing_line)
  }
  r.est
}

intermedia.getGlmerFit <- function(m, m_base){
  comp=anova(m, m_base, test = "Chisq") 
  dll = round((comp$logLik[1] - comp$logLik[2]),1)
  P = comp[,"Pr(>Chisq)"][2]
  P[is.na(P)] = 1
  for(star_thres in c(0.05,0.01,0.001)) if(P < star_thres) dll = paste(dll,'*',sep='')
  dll
}
  
intermedia.getGlmFit <- function(m, m_base){
  comp=anova(m, m_base, test = "Chisq")  
  dev = round(comp$Deviance[2],1)
  P = comp[,'Pr(>Chi)'][2]
  P[is.na(P)] = 1
  for(star_thres in c(0.05,0.01,0.001)) if(P < star_thres) dev = paste(dev,'*',sep='')
  dev
}

intermedia.getRegressionTable <- function(results, dayparts=NA, media=NA, values='label'){
  if(is.na(dayparts)) dayparts = unique(results$daypart)
  if(is.na(media)) media = unique(results$medium)
  rtable = reshape(results[results$daypart %in% dayparts & results$medium %in% media,c('variable',values,'model')], timevar='model', idvar='variable', direction='wide')
  colnames(rtable) = sapply(names(rtable),intermedia.cleanName)
  rtable
}

intermedia.cleanName <- function(txt) strsplit(as.character(txt), fixed=T, '.')[[1]][2]

intermedia.getPublicationHour <- function(d, mediameta=data.frame()){
  #print("Getting hour for available timestamps")
  d$hour = as.numeric(format(d$date, '%H'))
  #format(as.POSIXct(d$date), '%H')
  if(!nrow(mediameta) == 0){
    #print("Getting preset hours from mediafile:")
    #mediameta$pub_hour = as.numeric(format(as.POSIXct(mediameta$publication_time, format='%H:%M:%S'), '%H'))
    for(m in mediameta$medium_id[!is.na(mediameta$pub_hour)]){
      #print(paste('    ', m))
      d$hour[d$medium == m] = as.numeric(mediameta$pub_hour[mediameta$medium_id == m])
    }
  }
  d$hour 
}

intermedia.getMediumName <- function(txt) strsplit(as.character(txt), fixed=T, '.')[[1]][2]

intermedia.getModelIndex <- function(d){
  media = as.character(sapply(colnames(d)[grep('hits', colnames(d))],intermedia.getMediumName))
  models = NULL
  for(medium in media){
    agg = aggregate(cbind(N=d[,paste('total',medium,sep='.')]), by=list(daypart=d$daypart), FUN='sum')
    agg$medium = medium
    models = rbind(models, agg)
  }
  models[models$N > 0,c('medium','daypart','N')]
}


intermedia.prepareSlData <- function(codings, meta, attribute, mediameta, attrib_values=c(), lag_hours=24, variable='hits', codes=c(), timesplit='hours', skip_weekdays=c()){
  if(length(codes) == 0) codes = unique(codings$code) # if not codes are specified, use all codes
  
  h = codings[codings$code %in% codes & codings$id %in% meta$id,c('id','code',attribute)] # make basis for results table
  colnames(h) = c('id','code','attrib')
  
  # transform date to day-hour format
  meta$day = as.Date(format(meta$date))
  meta$time = intermedia.getPublicationHour(meta, mediameta=mediameta)
  meta$date = as.POSIXct(paste(format(meta$day), ' ', meta$time, ':00:00',sep=''))  
  timerange = date=seq(min(meta$date), max(meta$date), "hour")
  meta$t = match(meta$date, timerange) # change day-hour format to integer (t)
  
  h = unique(merge(h, meta[,c('id','medium','t')], by='id', all.x=T)) # match metadata 
  h$medium = mediameta$medium_string[match(h$medium, mediameta$medium_id)] # change medium_id to medium_string
  
  r = intermedia.getSlLag(h, lag_hours) # get lag
  
  r$hit = 1 # dependent variable. All cases are 1 (hit)
  r = intermedia.addzeros(r, attrib_values) # dependent variable, add no-hit cases (using alternative attributes) 
  r
}

intermedia.getSlLag <- function(h, lag_hours){  
  media = NULL
  for(m in unique(h$medium)) media = c(media, m)
  
  r = h[,c('id','code','attrib','medium','t')] # make results table
  for(m in media) r[,paste('lag',m,sep='.')] = NA # make empty values to fill with calculated lag values
  for(code in unique(h$code)){ # loop through codes
    print(code)
    hs = h[h$code == code,] # select subset of h, containing only the code in the loop
    hs$hit = 1 # set value to 1 (since each case is a hit)
    hs = cast(hs, id + code + attrib + t ~ medium, value='hit') # transform dataframe to wide format with media as columns
    for(m in media[!media %in% colnames(hs)]) hs[,m] = NA # if certain media do not occur in hs, add these as NA (required for matching later on)
    hs[is.na(hs)] = 0 # set all NA values to 0
    
    all_t = unique(hs$t) # get unique points in time (t) for hs
    all_t = all_t[all_t > lag_hours] # only calculate lag for t with sufficient history
    for(t in all_t){
      hs_lag = hs[hs$t < t & hs$t > (t-lag_hours),] # select cases in hs that represent the lag for time t
      if(nrow(hs_lag) == 0) next # if no lag, continue
      agg = aggregate(hs_lag[,media], by=list(hss$attrib), FUN='sum') # aggregate lag scores, break by attributes
      for(i in 1:nrow(agg)){
        l = agg[i,media] # get lag scores seperately for each attribute
        attrib = agg$Group.1[i] # get attribute name
        r[r$code == code & r$attrib == attrib & r$t == t,paste('lag',media,sep='.')] = l # fill lag values in results table for matching code, attribute and t
      }
    }
  }
  r[is.na(r)] = 0 # set NA values to 0
  r = r[r$t > lag_hours,] # delete cases whith insufficient history for indicated lag time
  r
}

intermedia.addzeros <- function(r, attribs=c()){
  ## Adds not-coded alternative attributes to results, in order to compare to coded attributes.
  ## The attribs parameter is optional. If given, only these attributes are used as alternatives. Otherwise, all code attributes that have a least been used once, by any medium, are used.
  
  r = r[!r$attrib == '',] # ignore non-coded attributes
  attribs = attribs[!attribs == ''] 
  all_codes = unique(r[,c('code','attrib')])
  for(i in 1:nrow(r)){
    l = r[i,]
    if(length(attribs) == 0) {
      missing_attrib = all_codes$attrib[all_codes$code == l$code & !all_codes$attrib == l$attrib]
    } else missing_attrib = attribs[!attibs == l$attrib]
    addlines = NULL
    for(attrib in missing_attrib){
      new_l = l
      new_l$attrib = attrib
      new_l$hit = 0
      addlines = rbind(addlines, new_l)
    }
    r = rbind(r, addlines)
  }
  r
}

x = data.frame(a = c(1,2,3),b=c(4,5,6))

getPseudoR <- function(m, m_intercept){
  llm=logLik(m)
  ll0=logLik(m_intercept)
  n = length(m$residuals)
  CoxSnell.R2 <- 1 - exp(-2/n * (llm[1]-ll0[1]))
  Nagelkerke.R2 <- CoxSnell.R2/(1-exp(2/n * ll0[1]))
  R2fit <-rbind(CoxSnell.R2, Nagelkerke.R2)
  R2fit
}


