# PackUnpackR/core.R
#
# Author: Bruce C. Schardt
#  email: schardt.bruce.curtis@gmail.com
#
# git clone https://github.com/schardtbc/packUnpacker
# copyright 2018

# packUnpacker
# R package to Unpack(parse) a binary file based on a file format definition
# example format:  format = 'x8Sx38LL>nLL>aLLLBBBBBccc8c8c8c8c8c8c20bbo<a[OptiRecordv1]<n'



#################################################################################
#                                                                               #
# GENERIC FUNCTIONS                                                             #
#                                                                               #
#################################################################################

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

add_reference <- function(obj){
  UseMethod("add_reference")
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

add_reference.default <- function(ob){
  cat("this is a generic function")
}

permute_to_constant.default <- function(self){
  cat("This is a generic function\n")
}

#################################################################################
#                                                                               #
# UnsignedCodec                                                                 #
#                                                                               #
#################################################################################

UnsignedCodec <- function (len){
  self <- list(l=len,m=as.matrix(256.^(0:(len-1))),d=t(as.matrix(256.^(len:1))),scaler=TRUE,encodes=TRUE)
  class(self)<-"UnsignedCodec"
  return (self)
}

decode.UnsignedCodec <- function(self,rbs){
  v <- as.numeric(rbs$read(self$l)) %*% self$m
  v <- v[1,1]
  return (v)
}

permute_to_array.UnsignedCodec <- function (self,n){
  obj <- UnsignedArrayCodec(self$l,n)
  return (obj)
}

permute_to_forward_reference.SignedCodec <- function(self,cvalue){
  obj <- self
  obj$tunnel <- cvalue
  class(obj) <- "UnsignedForwardReferenceCodec"
  return (obj)
}


permute_to_constant.UnsignedCodec <- function(self){
  obj <- UnsignedConstantCodec(self$l,0);
  return (obj)
}

#################################################################################
#                                                                               #
# SignedCodec                                                                   #
#                                                                               #
#################################################################################

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

decode.SignedCodec <- function(self,rbs){
  v <- as.numeric(rbs$read(self$l)) %*% self$m
  v <- v[1,1]
  if (v>=self$comp){
    v <- v-self$sub
  }
  return (v)
}

permute_to_array.SignedCodec <- function(self,n){
  obj <- SignedArrayCodec(self$l,n)
  return (obj)
}

permute_to_forward_reference.SignedCodec <- function(self,cvalue){
  obj <- self
  obj$tunnel <- cvalue
  class(obj) <- "SignedForwardReferenceCodec"
  return (obj)
}


permute_to_constant.SignedCodec <- function(self){
  obj <- SignedConstantCodec(self$len,0)
}

#################################################################################
#                                                                               #
# UnsignedForwardReferenceCodec                                                   #
#                                                                               #
#################################################################################


decode.UnsignedForwardReferenceCodec <- function(self,rbs){
  v <- as.numeric(rbs$read(self$l)) %*% self$m
  v <- v[1,1]
  if (v>=self$comp){
    v <- v-self$sub
  }
  self$tunnel$set(v)
  return (v)
}

#################################################################################
#                                                                               #
# SignedForwardReferenceCodec                                                   #
#                                                                               #
#################################################################################


decode.SignedForwardReferenceCodec <- function(self,rbs){
  v <- as.numeric(rbs$read(self$l)) %*% self$m
  v <- v[1,1]
  if (v>=self$comp){
    v <- v-self$sub
  }
  self$tunnel$set(v)
  return (v)
}

#################################################################################
#                                                                               #
# RelativePositioner                                                            #
#                                                                               #
#################################################################################

RelativePositioner <- function(n = 0){
  obj <- list(N = n,scaler=TRUE,encodes=TRUE)
  class(obj)<-"RelativePositioner"
  return (obj)
}

decode.RelativePositioner <- function(self,rbs){
  rbs$move(self$N)
  return (NA)
}

encode.RelativePositioner <- function(self,rbs){
  rbs$move(self$N)
  return (NA)
}

permute_to_array.RelativePositioner <- function(self,n){
  self$N <- n
  return (self)
}

#################################################################################
#                                                                               #
# AbsolutePositioner                                                            #
#                                                                               #
#################################################################################

AbsolutePositioner <- function(n = 0){
  obj <- list(N = n, tunnel = NA)
  class(obj)<-"AbsolutePositioner"
  return (obj)
}

decode.AbsolutePositioner <- function(self,rbs){
  if (is.na(self$tunnel))
    rbs$seek(self$N-1)
  else
    rbs$seek(self$tunnel$get())
  return (NA)
}

encode.AbsolutePositioner <- function(self,rbs){
  if (is.na(self$tunnel))
    rbs$seek(self$N-1)
  else
    rbs$seek(self$tunnel$get())
  return (NA)
}

permute_to_array.AbsolutePositioner <- function(self,n){
  self$N <- n
  return (self)
}

add_reference.AbsolutePositioner <- function(self,cvalue){
  self$tunnel <- cvalue
  return (self)
}

#################################################################################
#                                                                               #
# CurrentOffsetDecorator                                                        #
#                                                                               #
#################################################################################

CurrentOffsetDecorator <- function(){
  obj <- list(l = 0, N=NA, scaler = TRUE, encodes = FALSE)
  class(obj) <- "CurrentOffsetDecorator"
  return (obj)
}

decode.CurrentOffsetDecorator <- function(self, rbs){
  return (rbs$offset)
}


#################################################################################
#                                                                               #
# CharCodec                                                                     #
#                                                                               #
#################################################################################

CharCodec <- function(){
  obj <- list(N = 1,l =1,scaler = TRUE, encodes = TRUE)
  class(obj)<-"CharCodec"
  return (obj)
}

decode.CharCodec <- function(self,rbs){
  v <- rawToChar(as.raw(rbs$read(self$l)))
  return (v)
}

permute_to_array.CharCodec <- function(self,n){
  obj <- StringCodec(n)
  return (obj)
}

#################################################################################
#                                                                               #
# StringCodec                                                                   #
#                                                                               #
#################################################################################

StringCodec <- function(n){
  obj <- list(l = n,scaler=FALSE, encodes=TRUE)
  class(obj) <- "StringCodec"
  return (obj)
}

decode.StringCodec <- function(self,rbs){
  v <- rawToChar(as.raw(rbs$read(self$l)))
  return (v)
}

#################################################################################
#                                                                               #
# RandomDataGenerator                                                           #
#                                                                               #
#################################################################################

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

#################################################################################
#                                                                               #
# IndexDecorator                                                            #
#                                                                               #
#################################################################################

IndexDecorator <- function(v){
   obj <- list(scaler = TRUE, encodes = TRUE, N=0, l=0,idx = capturedValue())
   obj$idx$capture(v)
   class(obj) <- "IndexDecorator"
   return (obj)
}

decode.IndexDecorator <- function(self,rbs){
   v = self$idx$value
   self$idx$capture(v+1)
   return (v)
}

#################################################################################
#                                                                               #
# ValueDecorator                                                                #
#                                                                               #
#################################################################################

ValueDecorator <- function(v){
  obj <- list(scaler = TRUE, encodes = TRUE, N=0, l=0,value = v)
  class(obj) <- "ValueDecorator"
}

decode.ValueDecorator <- function(self,rbs){
  v = self$value
  return (v)
}

#################################################################################
#                                                                               #
# NullTerminatedStringCodec                                                     #
#                                                                               #
#################################################################################

NullTerminatedStringCodec <- function(){
  obj <- list(l=0,N=0,encodes=TRUE, scaler = FALSE)
  class(obj) <- "NullTerminatedStringCodec"
}

decode.NullTerminatedStringCodec <- function(self,rbs){
  o <- rbs$offset
  e <- which(rbs$buf[(o+1):length(rbs$buf)]==0)[1]
  v <- rawToChar(rbs$read(e-o+1))
  return (v)
}


AtomicCodecs <- list(
 x = RelativePositioner()
,o = AbsolutePositioner()
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
#,q = int64Codec()
#,Q = uint64Codec()
#,f = FloatCodec()
#,d = DoubleCodec()
,v = IndexDecorator(1)
,V = ValueDecorator(0)
,R = RandomDataGenerator(1)
)

#################################################################################
#                                                                               #
# UnsignedArrayCodec                                                            #
#                                                                               #
#################################################################################

UnsignedArrayCodec <- function(len,N){
  obj <- list(
    l = len
    ,N = N
    ,lR = N*len
    ,multiplier = (256.^(0:(len-1)))
    ,divider = 256.^(1:len)
    ,scaler = FALSE
    ,encodes = TRUE
    ,tunnel = NA
  )
  class(obj) <- "UnsignedArrayCodec"
  return (obj)
}

decode.UnsignedArrayCodec <- function(self,rbs){
  if (!is.na(self$tunnel)){
    self$N <- self$tunnel$get()
    self$lR <- self$N*self$l
  }
  v <- as.numeric(rbs$read(self$lR))
  v = matrix(v,self$l,self$N)
  v <- t(v) %*% as.matrix(self$multiplier)
  v<-v[,1]
  return (v)
}

add_reference.UnsignedArrayCodec <- function(self,cvalue){
  self$tunnel <- cvalue
  return(self)
}

#################################################################################
#                                                                               #
# SignedArrayCodec                                                              #
#                                                                               #
#################################################################################

SignedArrayCodec <- function(len,N){
  obj <- list(
    l = len
    ,N = N
    ,lR = N*len
    ,multiplier = (256.^(0:(len-1)))
    ,divider = 256.^(1:len)
    ,comp = 2^(8*len-1)
    ,sub = 2^(8*len)
    ,scaler = FALSE
    ,encodes = TRUE
    ,tunnel = NA
  )
  class(obj) <- "SignedArrayCodec"
  return (obj)
}

decode.SignedArrayCodec <- function(self,rbs){
  if (!is.na(self$tunnel)){
    self$N <- self$tunnel$get()
    self$lR <- self$N*self$l
  }
  v <- as.numeric(rbs$read(self$lR))
  v <- matrix(v,self$l,self$N)
  v <- t(v) %*% as.matrix(self$multiplier)
  v <- v[,1]
  np <-  v>=self$comp
  if (any(np)){
     v[np] <- v[np] - self$sub
  }
  return (v)
}

add_reference.SignedArrayCodec <- function(self,cvalue){
  self$tunnel <- cvalue
  return(self)
}

#################################################################################
#                                                                               #
# RecordCodec                                                                   #
#                                                                               #
#################################################################################

RecordCodec <- function(Codec,n){
  obj = list(
    N = n
    ,fmt = NA
    ,codecs = Codec
    ,uniform = FALSE
    ,scaler = FALSE
    ,encodes = TRUE
    ,tunnel = NA
  )
  class(obj) <- "RecordCodec"
  return (obj)
}

decode.RecordCodec <- function(self,rbs){
  if (!is.na(self$tunnel))
    self$N <- self$tunnel$get()
  for (ri in 1:self$N){
    cnt<-0
    for (codec in self$codecs){
      t <- decode(codec,rbs)
      if (is.na(t) || is.null(t) || length(t)==0) next
      if (cnt==0){
        cnt<-cnt+1
        r<-t
      }
      else{
        cnt<-cnt+1
        r<-c(r,t)
      }
    }
    if (ri==1){
      v<-r
    }
    else{
      v<-c(v,r)
    }
  }
  v <- matrix(v,self$N,length(v)/self$N,byrow = TRUE)
  return (v)
}

permute_to_array.RecordCodec <- function(self,n){
  self$N = n
  return (self)
}

add_reference.RecordCodec <- function(self,cvalue){
  self$tunnel <- cvalue
  return(self)
}

#################################################################################
#                                                                               #
# StructureCodec                                                                #
#                                                                               #
#################################################################################

StructureCodec <- function(fmt,name_list){
  obj <- list( format = fmt
              ,codec = BufferCodec(fmt)
              ,properties = name_list)
  class(obj) <- "StructureCodec"
}

decode.StructureCodec <- function(self,rbs){
  v <- decode(self$codec,rbs)
  names(v) <- self$properties
}

#################################################################################
#                                                                               #
# BufferCodec                                                                   #
#                                                                               #
#################################################################################

BufferCodec <- function(fmt){
  obj <- list(
    codecs = NA
    ,uniform = FALSE
  )
  if (class(fmt)=="character")
    obj$codecs <- BufferCodec.Factory(fmt)
  else
    obj$codecs <- fmt
  obj$uniform <- all(sapply(obj$codecs,function(x) x$scaler));
  class(obj)<-"BufferCodec"
  return (obj)
}

decode.BufferCodec <- function(self,rbs){
  if (self$uniform){
    cnt<-0
    for (codec in self$codecs){
      t <- decode(codec,rbs)
      if (is.na(t) || is.null(t) || length(t)==0){
        next
      }
      if (cnt ==0){
        cnt<-cnt+1
        v <-t
      }
      else {
        cnt<-cnt+1
        v<-c(v,t)
      }
    }
  }
  else{
    cnt<-0
    for (codec in self$codecs){
      t <- decode(codec,rbs)
      if (is.na(t) || is.null(t) || length(t)==0){
        next
      }
      if (cnt==0){
        cnt <-cnt+1
        v <- list(t)
      }
      else {
        cnt<-cnt+1
        v <- c(v,list(t))
      }
    }
  }
  if (is.list(v) && length(v)==1)
    v <- v[[1]]
  return (v)
}

BufferCodec.Factory <- function(fmt){
  offsetbinary <- FALSE
  forwardlinks <- list()
  codecs <- list()
  nac <- names(AtomicCodecs)
  digits = "0123456789"
  digits = strsplit(digits,split="")[[1]]
  cnt<-0
  ci<-1
  cv <- strsplit(fmt,split="")[[1]]
  while (ci<=length(cv)){
    c <- cv[ci]
    if (c=="K"){
      offsetbinary <- TRUE
      ci<-ci+1
    }
    else if (c == 'k') {
      offsetbinary <- FALSE
      ci<-ci+1
    }
    else if (c == '('){
      lp <- cv[ci:length(cv)]=="("
      rp <- cv[ci:length(cv)]==")"
      mp <- cumsum(lp-rp)
      imp <- which(mp==0)[1]
      rfmt <- cv[(ci+1):(ci+imp-2)]
      rfmt <- paste(rfmt,collapse="")
      rcodecs <- BufferCodec.Factory(rfmt)
      cnt<-cnt+1
      codecs[[cnt]]<-RecordCodec(rcodecs,1)
      ci <- ci + imp
    }
    else if (any(nac == c)){
      cnt<-cnt+1
      codecs[[cnt]] <- AtomicCodecs[[c]]
      ci<-ci+1
    }
    else if (any(digits == c)){
      ld <- which(sapply(cv[ci:length(cv)],is.element,digits,USE.NAMES = FALSE))[1]
      recn <- as.numeric(cv[ci:(ci+ld-1)])
      codecs[[cnt]]<-permute_to_array(codecs[[cnt]],recn)
      ci<-ci+ld
    }
    else if (c==">") {
      #forward link
      name <- cv(ci+1);
      cvalue <- passive_tunnel()
      codecs[cnt] = permute_to_forward_reference(codecs[cnt],cvalue);
      forwardLinks[[name]] <- cvalue;
      ci<-ci+2;
    }
    else if (c=="<"){
      name <- cv[ci+1]
      cvalue <- forwardLinks[[name]];
      codecs[cnt] = purmute_to_array(codecs[[cnt]],1)
      codecs[cnt] = add_reference(codecs[[cnt]],cvalue)
      ci<-ci+2;
    }
    else if (c=="["){
      ld <- which(cv[ci:length(cv)]=="]")[1]
      codec_name <- paste(cv[(ci+1):(ci+ld-1)],sep="")
      codec <- parse(text = paste(codec_name,"()",sep=""))
      cnt <- cnt+1
      codecs[[cnt]]<-eval(codec)
      ci = ci + ld
    }
    else
      ci<-ci+1
  }
  return (codecs)
}



