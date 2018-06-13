decode <- function(obj,rbs){
  UseMethod("decode")
}

encode <- function(obj,rbs,...){
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

decode.UnsignedCodec <- function(self,rbs){
  v = as.numeric(rbs$read(self.l)) %*% self$m
  return (v)
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

decode.SignedCodec <- function(self,rbs){
  v = as.numeric(rbs$read(self$l)) %*% self$m
  if (v>self$comp){
    v <- v-self$sub
  }
  return (v)
}

permute_to_array.SignedCodec <- function(self,n){
  obj <- SignedArrayCodec(self.len,n)
  return (obj)
}
permute_to_constant.SignedCodec <- function(self){
  obj <- SignedConstantCodec(self.len,0)
} 

RelativePositioner <- function(obj,n){
  obj <- list(N = n)
  return (obj)
}

decode.RelativePostioner <- function(self,rbs){
  rbs$move(self$N)
}

encode.RelativePostioner <- function(self,rbs){
  rbs$move(self$N)
}

permute_to_array.RelativePositioner <- function(self,n){
  self$N <- n
  return (self)
}

AbsolutePositioner <- function(obj,n){
  obj <- list(N = n)
  return (obj)
}

decode.AbsolutePostioner <- function(self,rbs){
  rbs$seek(self$N)
}

encode.AbsolutePostioner <- function(self,rbs){
  rbs$seek(self$N)
}

permute_to_array.AbsolutePositioner <- function(self,n){
  self$N <- n
  return (self)
}

CharCodec <- function(){
  obj <- list(N = 1,l =1,scaler = TRUE, encodes = TRUE)
  return (obj)
}

decode.CharCodec <- function(self,rbs){
  v <- rawToChar(rbs$read(self.l))
  return (v)
}

permute_to_array.CharCodec <- function(self,n){
  obj <- StringCodec(n)
  return (obj)
}

StringCodec <- function(n){
  obj <- list(N = n,scaler=FALSE, encodes=TRUE)
}

decode.StringCodec <- function(self,rbs){
  v <- rawToChar(rbs$read(self.l))
  return (v)
}

RandomDataGenerator <- function(n){
  obj <- list(N = n, codec = UnsignedCodec(1),scaler=TRUE,encodes = TRUE)
  if (n>1){
    obj$codec <- UnsignedArrayCodec(1,n)
    obj$scaler <- FALSE
  }
  class(obj) <- "RandomDataGenerator"
}

decode.RandomDataGenerator <- function(self,rbs){
  v = floor(255*rand(self.N))
  return (v)
}

permute_to_array.RandomDataGenerator <- function(self,n){
  obj <- RandomDataGenerator(n)
  return (obj)
}

IndexDecorator <- function(v){
   obj <- list(scaler = TRUE, encodes <- TRUE, N=0, l=0,value = v)
   class(obj) <- "IndexDecorator"
}

decode.IndexDecorator <- function(self,rbs){
   v = self$value
   self$value<-value+1
   return (v)
}

ValueDecorator <- function(v){
  obj <- list(scaler = TRUE, encodes <- TRUE, N=0, l=0,value = v)
  class(obj) <- "ValueDecorator"
}

decode.ValueDecorator <- function(self,rbs){
  v = self$value
  return (v)
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

UnsignedArrayCodec <- function(len,N){
  obj <- list(
    l = len
    ,N = N
    ,lR = N*len
    ,multiplier = (256.^(0:len-1))
    ,divider = 256.^(1:len)
    ,scaler = FALSE
    ,encodes = TRUE
  )
  class(obj) <- "UnsignedArrayCodec"
  return (obj)
}

decode.UnsignedArrayCodec <- function(self,rbs){
  v = as.numeric(rbs$read(self.lR))
  v = matrix(v,self.len,self.N)
  v = v %*% self.Multiplier;
  rbs$move(self.lR)
  return (v)
}

SignedArrayCodec <- function(len,N){
  obj <- list(
    l = len
    ,N = N
    ,lR = N*len
    ,multiplier = (256.^(0:len-1))
    ,divider = 256.^(1:len)
    ,comp = 2^(8*self.l-1)
    ,sub = 2^(8*self.l)
    ,scaler = FALSE
    ,encodes = TRUE
  )
  class(obj) <- "SignedArrayCodec"
  return (obj)
}

decode.SignedArrayCodec <- function(self,rbs){
  v = as.numeric(rbs$read(self.lR))
  v = matrix(v,self.len,self.N)
  v = v %*% self.Multiplier
  np <-  v>=self.Comp;
  if (any(np)){
     v[np]=v[np] - self.sub
  }
  rbs$move(self.lR)
  return (v)
}

RecordCodec <- function(Codec,n){
  obj = list(
    N = n
    ,fmt = NA
    ,codec = Codec
    ,uniform = FALSE
    ,scaler = FALSE
    ,encodes = TRUE
  )
  class(obj) <- "RecordCodec"
}

decode.RecordCodec <- function(self,rbs){
  for (ri in 1:self.N){
    cnt<-0
    for (ci in 1:length(self$codec)){
      r <- decode(self$codec[ci],rbs)
      if (isempty(r)) next
      if (cnt==0){
        cnt<-cnt+1
        r=list(t)
      }
      else{
        cnt<-cnt+1
        r=c(r,t)
      }
    }
    if (ri==1){
      v=r
    }
    else{
      v[ri,]=r
    }
  }
  return (v)
}
  
purmute_to_array.RecordCodec <- function(self,n){
  self$N = n
  return (self)
}
  
