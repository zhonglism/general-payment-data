library(magrittr)
rm(list = ls())
gen<-read.csv('gen_pymnt_sam.csv',
              stringsAsFactors=FALSE)

memory.limit()

gen_final<-gen[,c(2,6:9,11:21,26:35,40,46,49,59)]
set.seed(1234)
gen_final1<-gen_final[sample(seq(1,90000),4500),]

View(gen_final1)
# dim(gen_final)

subset_colclasses <- function(df, 
                              colclasses="character") {
  df[,sapply(df,
             function(vec, test) class(vec) %in% test,
             test=colclasses)]
}

gen_str<-subset_colclasses(gen_final1,c("character"))
#View(gen_str)
#colnames(gen_str)
to_upper<-colnames(gen_str)[c(8,14,17,23:24)]
to_upper

for(n in to_upper){
  gen_final1[,n]<-toupper(gen_final1[,n])
}
View(gen_final1)

rm(list=c('gen','gen_final','gen_str'))

# colnames(gen_final)
to_cap<-colnames(gen_final1)[c(1,3:8,11,14:15,21,27)]
# to_cap
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

for(n in to_cap){
  gen_final1[,n]<-capwords(gen_final1[,n],
                          strict = TRUE)
}
View(gen_final1)

for (n in to_cap) {
  for (i in 1:length(gen_final1[,n])) {
    if (gen_final1[,n][i]=='NANA'){
      gen_final1[,n][i]<-NA
    }
  }
}


to_string<-colnames(gen_final1[,c(2,18,28)])
#to_string
class(gen_final1[,22])

for(n in to_string){
  gen_final1[,n]<-as.character(gen_final1[,n])
}

class(gen_final1[,28])
View(gen_final1)

insert_comma<-colnames(gen_final1[,c(17,19)])
insert_comma

#regex here is better...
for(n in insert_comma){
  gen_final1[,n]<-gsub('INC','Inc',gen_final1[,n])
  gen_final1[,n]<-gsub('Incporated','Inc',gen_final1[,n])
  gen_final1[,n]<-gsub('USA','U.S.',gen_final1[,n])
  gen_final1[,n]<-gsub('U.S..','U.S.',gen_final1[,n])
  gen_final1[,n]<-gsub('L.L.C.','LLC',gen_final1[,n])
  gen_final1[,n]<-gsub('LLC.','LLC',gen_final1[,n])
  gen_final1[,n]<-gsub('L.P.','LP',gen_final1[,n])
  gen_final1[,n]<-gsub('Inc.','Inc',gen_final1[,n])
  gen_final1[,n]<-gsub('Corporation','Co.',gen_final1[,n])
  gen_final1[,n]<-gsub('Corp.','Co.',gen_final1[,n])

}

for (n in insert_comma) {
  for (i in 1:length(gen_final1[,n])) {
    for(com in c('LP','Inc','LLC')){
      if (grepl(com,gen_final1[,n][i])) {
        if (!grepl(',',gen_final1[,n][i])) {
          pos<-gregexpr(pattern =com,gen_final1[,n][i])
          gen_final1[,n][i]<-paste0(substr(gen_final1[,n][i],1,pos[[1]][1]-2),', ',
                                    substr(gen_final1[,n][i],
                                           pos,
                                           length(strsplit(gen_final1[,n][i],'')[[1]])))
          
        }
      }
    }
  }
}


write.csv(gen_final,'gen_final.csv')