library(ROracle)
library(plyr)

innovation_login <- function(){

  dbConnect(
      Oracle(),
      username=getOption('Oracle.username'),
      password=getOption('Oracle.password'),
      dbname='innovation'
    )
}

trim_ws <- function(x){
  sub(' *$','',sub('^ *','',x))
}

trim_zeros <- function(x){
  sub('^0+','',as.character(x))
}

transition_data <- function(con){
  x <- dbGetQuery(
        con,
        'select 
          a.encounter_number as eid,
          a.mrid as mrn,
          a.targeted_chf as chf,
          a.targeted_ami as ami,
          a.targeted_copd as copd,
          a.targeted_pna as pna,
          b.admit_date as admit_date
        from 
        cmsmhtadm.mht_transition_poc_xf a, 
        cmsmhtadm.encounter b 
        where 
          a.encounter_number=b.local_enc_id'
      )

  names(x) <- tolower(names(x))

  x$admit_date <- as.Date(x$admit_date)
  x$eid <- trim_ws(trim_zeros(x$eid))
  x$mrn <- trim_ws(trim_zeros(x$mrn))
  for (i in c('chf','ami','copd','pna'))
    x[[i]][which(is.na(tolower(x[[i]])))] <- 'N'

  # Order by mrn, admit_date
  x <- x[order(x$mrn,x$admit_date),]

  # Select first mrn
  x <- x[!duplicated(x$mrn),]

  x
}

golden_data <- function(con){
  y <- dbGetQuery(
        con,
        'select
          encounter_number as eid,
          mrn as mrn,
          diaggroup as cons,
          admit_date as admit_date
        from
          CAOA.CTPOP_25Mar2014'
        )
  names(y) <- tolower(names(y))
  y$eid <- trim_ws(trim_zeros(y$eid))
  y$mrn <- trim_ws(trim_zeros(y$mrn))

  y$cons <- trim_ws(y$cons)
  y$cons[which(y$cons=='PN')] <- 'PNA'
  x <- data.frame(eid=y$eid,mrn=y$mrn,
        chf=y$cons,ami=y$cons,copd=y$cons,pna=y$cons,
        admit_date=as.Date(y$admit_date),stringsAsFactors=FALSE)

  for (i in c('chf','ami','copd','pna')){
    x[[i]][ which( x[[i]] == toupper(i)) ] <- 'Y'
    x[[i]][ which( x[[i]] != 'Y'       ) ] <- 'N'
  }

  # Order by mrn, admit_date
  x <- x[order(x$mrn,x$admit_date),]

  # Select first mrn
  x <- x[!duplicated(x$mrn),]

  x
}

task1_summary <- function(t,g){
  months <- seq(as.Date('2013-01-01'),by='month',length=13)
  adply(
    1:12,
    .margins=1,
    .fun=function(i){
      t_s <- subset(t,admit_date >= months[i] & admit_date < months[i+1])
      g_s <- subset(g,admit_date >= months[i] & admit_date < months[i+1])
      if (nrow(t_s) == 0 || nrow(g_s) == 0){
        data.frame(month=months[i],t_n=nrow(t_s), g_n=nrow(g_s), inter=0, t_m_g=0, g_m_t=0)
      } else {
        t_s <- t_s$mrn
        g_s <- g_s$mrn
        data.frame(month=months[i],t_n=length(t_s), g_n=length(g_s), 
          inter=length(intersect(t_s,g_s)), 
          t_m_g=length(setdiff(t_s,g_s)), 
          g_m_t=length(setdiff(g_s,t_s)))
      }
    }
  )

}
