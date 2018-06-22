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
    offset<<-offset+r
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
  obj <- environment()
  class(obj) <- "raw_byte_stream"
  return (obj)
}

ones <- function(rows = 1, cols = 1){
  v <- seq(from = 1, to =1, length.out= rows*cols)
  if (rows >1 || cols >1)
    v <- matrix(v,rows,cols)
  return (v)
}

zeros <- function(rows = 1, cols = 1){
  v <- seq(from = 0, to =0, length.out= rows*cols)
  if (rows >1 || cols >1)
    v <- matrix(v,rows,cols)
  return (v)
}

capturedValue <- function(){
  value <- NA
  capture<-function(v){
    value<<-v
  }
  environment()
}

PassiveTunnel <- function(){
  data <-NA
  set<-function(v){
    data<<-v
  }
  get<-function(){
    return (data)
  }
  obj <- environment()
  class(obj) <- "PassiveTunnel"
  return (obj)
}


