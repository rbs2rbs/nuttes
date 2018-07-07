library(DBI)
library(RPostgreSQL)
library(jsonlite)
cone<-function(){
  pw <- {
    "6097dbcfa7d4051e676d6259c286fdc04dc1fa58688dabdedf517785101e6935"
  }

  # loads the PostgreSQL driver
  drv <-  RPostgreSQL::PostgreSQL()
  # creates a connection to the postgres database
  # note that "con" will be used later in each connection to the database
  con <- DBI::dbConnect(drv, dbname = "d81pdf7aaq99dj",
                        host = "ec2-54-243-213-188.compute-1.amazonaws.com", port = 5432,
                        user = "jgdhyextboduau", password = pw)
  return(con)
  }
que<-function(pac,con,primeira){
  if(primeira){

  }
  if(!primeira){
    t0=DBI::dbGetQuery(con,sprintf("SELECT MAX(data_inicio) FROM paciente WHERE email ='%s'",pac))
    dft<-difftime(Sys.time(),t0[[1]],units = 'hours')
    if(dft>24){
      tipo_sort=list()
      for(i in seq_len(trunc(dft/24)+1)){
        tipo_sort[[i]]=sample(seq_len(4),3,replace=T)
      }
      id_per=t(sapply(tipo_sort,function(x){
        sapply(x, function(y){
          switch (y,
                  '1' = {
                    return( sample(seq_len(10),1))
                  },
                  '2' = {
                    return(sample(seq(11,15),1))
                  },
                  '3' = {
                    return(sample(seq(16,21),1))
                  },
                  '4' = {
                    return(sample(seq(22,24),1))
                  }
          )})
      }))
      id_per_sem_resposta=as.vector(id_per[-nrow(id_per),])
      data_sem_resposta=t0[[1]]-rev(rep(seq(length(id_per_sem_resposta)/3),each=3))
      id_per=as.vector(id_per[nrow(id_per),])
      DBI::dbSendQuery(con,sprintf("INSERT INTO respostas (id_pergunta,email,data,resposta)
                                   VALUES %s;",paste(sprintf('(%s)',
                                                             paste(id_per_sem_resposta,sprintf("'%s'",pac),sprintf("to_date('%s','YYYY-MM-DD')",data_sem_resposta),-1,sep=',')),
                                                     collapse=',')))
      return(jsonlite::toJSON(DBI::dbGetQuery(con,sprintf("SELECT * FROM perguntas WHERE id IN(%s)",paste(id_per,collapse = ',')))))
    }else return("Perguntas de hoje já respondidas\nAmanhã novas perguntas serão geradas")
  }else{
    tipo_sort=sample(seq_len(4),3,replace=T)
    id_per=apply(matrix(tipo_sort),1,function(x){
      switch (x,
              '1' = {
                return( sample(seq_len(10),1))
              },
              '2' = {
                return(sample(seq(11,15),1))
              },
              '3' = {
                return(sample(seq(16,21),1))
              },
              '4' = {
                return(sample(seq(22,24),1))
              }
      )
    })
    return(jsonlite::toJSON(DBI::dbGetQuery(con,sprintf("SELECT * FROM perguntas WHERE id IN(%s)",paste(id_per,collapse = ',')))))
  }
}

respotas<-function(email,id_perguntas,respostas){
  # respotas<-jsonlite::fromJSON(email,idperguntas,respostas)
  con<-cone()
  DBI::dbSendQuery(con,sprintf("INSERT INTO respsostas (email,id_pergunta,resposta) VALUES
                                      ('%s','%s','%s'),('%s','%s','%s'),('%s','%s','%s')",
                              email,id_perguntas[1],resposta[1],
                              email,id_perguntas[2],resposta[2],
                              email,id_perguntas[3],resposta[3]))
  return(DBI::dbGetQuery(con,"SELECT*FROM respostas"))
}
