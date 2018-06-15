raw_byte_stream <- function(buf = as.raw(0:255),offset=0){
  read <- function(l){
    v <- buf[(offset+1):(offset+l)]
    offset<<-offset+l
    return(v)
  }
  seek <- function(o){
    offset<<-o
  }
  move <- function(r){
    offset<<-offset+move
    if (offset<0){
      offset <<- 0
    }
    if (offset>baseenv()$length(buf)){
      offset<<-baseenv()$length(buf)
    }
  }
  length <- function(){
    baseenv()$length(buf)
  }
  remaining <- function(){
    baseenv()$length(buf) - offset
  }
  environment()
}

capturedValue <- function(){
  value <- NA
  capture<-function(v){
    value<<-v
  }
  environment()
}

tunnel <- function(){
  value <-NA
  capture<-function(v){
    value<<-v
  }
  environment()
}

vt <- function(t){
  t0 <-t
  
}

origin <- function(v){
  list(a=1,b=2,t=v)
}

destination <-function(v){
  list(a=2,b=3,t=v)
}
