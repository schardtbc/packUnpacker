decode <- function(obj,buffer,offset){
  UseMethod("decode")
}

encode <- function(obj,buffer,offset,...){
  UseMethod("encode")
}

permute_to_array<- function(obj,n){
  UseMethod("permute_to_array")
}

permute_to_forward_reference<- function(obj){
  UseMethod("permute_to_forward_reference")
}

permute_to_constant <- function(self){
  UseMethod("permute_to_constant")
}

encode.default <- function(obj) {
  cat("This is a generic function\n")
}

decode.default <- function(obj) {
  cat("This is a generic function\n")
}

permute_to_array.default <- function(obj,n){
  cat("This is a generic function\n")
}

permute_to_forward_reference.default <- function(obj){
  cat("This is a generic function\n")
}

permute_to_constant.default <- function(self){
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

permute_to_array.UnsignedCodec <- function (self,n){
  obj <- UnSignedArrayCodec(self$l,n)
  return (obj)
}
permute_to_forward_reference.UnsignedCodec <- function(self){
  obj <- UnsignedForwardReferenceCodec(self$l)
  return (obj)
}

permute_to_constant.UnsignedCodec <- function(self){
  obj <- UnsignedConstantCodec(self$l,0);
  return (obj)
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

permute_to_array.SignedCodec <- function(self,n){
  obj <- SignedArrayCodec(self.len,n)
  return (obj)
}
permute_to_onstant.SignedCodec <- function(self){
  obj <- SignedConstantCodec(self.len,0)
} 

RelativePositioner <- function(obj,n){
  obj <- list(N = n)
  return (obj)
}

decode.RelativePostioner <- function(self,dor){
  dor[[2]] <- dor[[2]] + self$N
  return (dor)
}

encode.RelativePostioner <- function(self,dor){
  dor[[2]] <- dor[[2]] + self$N
  return (dor)
}

permute_to_array.RelativePositioner <- function(self,n){
  self$N <- n
  return (self)
}

AbsolutePositioner <- function(obj,n){
  obj <- list(N = n)
  return (obj)
}

decode.AbsolutePostioner <- function(self,dor){
  dor[[2]] <- self$N
  return (dor)
}

encode.AbsolutePostioner <- function(self,dor){
  dor[[2]] <- self$N
  return (dor)
}

permute_to_array.AbsolutePositioner <- function(self,n){
  self$N <- n
  return (self)
}

CharCodec <- function(){
  obj <- list(N = 1,l =1,scaler = TRUE, encodes = TRUE)
  return (obj)
}

decode.CharCodec <- function(self,dor){
  o <- dor[[2]]
  v <- rawToChar(dor[[1]][[o+1]])
  dor[[2]] <- o+self$l
  dor[3] <- c(dor[[3]],v)
  return (dor)
}

permute_to_array.CharCodec <- function(self,n){
  obj <- StringCodec(n)
  return (obj)
}

StringCodec <- function(n){
  obj <- list(N = n,scaler=FALSE, encodes=TRUE)
}

decode.StringCodec <- function(self,dor){
  
  return (dor)
}

AtomicCodec <- list(
 x = RelativePositioner(1)
,o = AbsolutePositioner(0)
,O = CurrentOffsetDecorator()
,c = CharCodec()
,Z = NullTerminatedStringCodec() 
,b = SignedCodec(1)
,B = UnsignedCodec(1)
,s = SignedCodec(2)
,S = UnsignedCodec(2)
,j = SignedCodec(-3)
,J = UnsignedCodec(-3)
,m = SignedCodec(3)
,M = UnsignedCodec(3)
,i = SignedCodec(4)
,I = UnsignedCodec(4)
,l = SignedCodec(4)
,L = UnsignedCodec(4)
,t = SignedCodec(6) 
,T = UnsignedCodec(6) 
,q = int64Codec()
,Q = uint64Codec() 
,f = FloatCodec()
,d = DoubleCodec()
,v = IndexDecorator(1)
,V = ValueDecorator(0)
,R = RandomDataGenerator(1)
)
testbuf <-as.raw(0:255)
testdor <- list(testbuf,0,list())
