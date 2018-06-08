decode <- function(obj,buffer,offset){
  UseMethod("decode")
}

encode <- function(obj,buffer,offset,...){
  UseMethod("encode")
}

encode.default <- function(obj) {
  cat("This is a generic function\n")
}

encode.default <- function(obj) {
  cat("This is a generic function\n")
}


UnsignedCodec <- function (len){
  self <- list(l=len,m=as.matrix(256.^(0:(len-1))),d=t(as.matrix(256.^(len:1))),scaler=TRUE,encodes=TRUE)
  class(self)<-"UnsignedCodec"
  return (self)
}

SignedCodec <- function (len,offset_binary=FALSE){
  self <- list(l=len
               ,m=as.matrix(256.^(0:(len-1)))
               ,d=t(as.matrix(256.^(len:1)))
               ,comp = 2^(8*len-1)
               ,sub  = 2^(8*len)
               ,scaler=TRUE
               ,encodes=TRUE)
  if (offset_binary){
    self$sub  = self$comp    
    self$comp = 0;
  }
  class(self)<-"SignedCodec"
  return (self)
}

decode.UnsignedCodec <- function(self,dor){
  o = dor[[2]]
  v = as.numeric(dor[[1]][(o+1):(o+self$l)]) %*% self$m
  o=o+self$l
  dor[[2]]<-o
  dor[3]<- c(dor[[3]],v)
  return (dor)
}

decode.SignedCodec <- function(self,dor){
  o = dor[[2]]
  v = as.numeric(dor[[1]][(o+1):(o+self$l)]) %*% self$m
  if (v>self$comp){
    v <- v-self$sub
  }
  o=o+self$l
  dor[[2]]<-o
  dor[3]<- c(dor[[3]],v)
  return (dor)
}

u8 <- UnsignedCodec(1)
u16 <- UnsignedCodec(2)
u32 <- UnsignedCodec(4)
u64 <- UnsignedCodec(8)

s8<-SignedCodec(1)
s16<-SignedCodec(2)
s32<-SignedCodec(4)
s64<-SignedCodec(8)

AtomicCodecs <- list(B=u8,b=s8,S=u16,s=s16,L=u32,l=s32,Q=u64,q=s64)

testbuf <-as.raw(0:255)
testdor <- list(testbuf,0,list())




