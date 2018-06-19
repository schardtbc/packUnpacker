# PackUnpackR/core.R
#
# Author: Bruce C. Schardt
#  email: bruce.schardt@gmail.com
#
# git clone https://github.com/bruce-schardt/PackUnpackR.git
# copyright 2018

# PackUnpackR
# R package to Unpack(parse) a binary file based on a file format definition
# example format:  format = 'x8Sx38LL>nLL>aLLLBBBBBccc8c8c8c8c8c8c20bbo<a[OptiRecordv1]<n'


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
  v <- as.numeric(rbs$read(self$l)) %*% self$m
  v <- v[1,1]
  return (v)
}

permute_to_array.UnsignedCodec <- function (self,n){
  obj <- UnSignedArrayCodec(self$l,n)
  return (obj)
}
permute_to_forward_reference.UnsignedCodec <- function(self,cvalue){
  obj <- UnsignedForwardReferenceCodec(self$l,cvalue)
  return (obj)
}

add_reference.UnsignedCodec <- function(self,cvalue){
  self$capturedValue <-cvalue
  return (self)
}

permute_to_constant.UnsignedCodec <- function(self){
  obj <- UnsignedConstantCodec(self$l,0);
  return (obj)
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
  obj <- SignedArrayCodec(self.len,n)
  return (obj)
}

permute_to_forward_reference.SignedCodec <- function(self,cvalue){
  obj <- SignedForwardReferenceCodec(self$l,cvalue)
  return (obj)
}

add_reference.SignedCodec <- function(self,cvalue){
  self$capturedValue <-cvalue
  return (self)
}
permute_to_constant.SignedCodec <- function(self){
  obj <- SignedConstantCodec(self.len,0)
}

RelativePositioner <- function(n){
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

AbsolutePositioner <- function(n){
  obj <- list(N = n)
  class(obj<-"AbsolutePositioner")
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

CurrentOffsetDecorator <- function(){
  obj <- list(l = 0, N=NA, scaler = TRUE, encodes = FALSE)
  class(obj) <- "CurrentOffsetDecorator"
  return (obj)
}

decode.CurrentOffsetDecorator <- function(self, rbs){
  return (rbs$offset)
}

permute_to_array.CurrentOffsetDecorator <- function(self,n){
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
   obj <- list(scaler = TRUE, encodes = TRUE, N=0, l=0,value = v)
   class(obj) <- "IndexDecorator"
}

decode.IndexDecorator <- function(self,rbs){
   v = self$value
   self$value<-value+1
   return (v)
}

ValueDecorator <- function(v){
  obj <- list(scaler = TRUE, encodes = TRUE, N=0, l=0,value = v)
  class(obj) <- "ValueDecorator"
}

decode.ValueDecorator <- function(self,rbs){
  v = self$value
  return (v)
}

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
#,q = int64Codec()
#,Q = uint64Codec()
#,f = FloatCodec()
#,d = DoubleCodec()
,v = IndexDecorator(1)
,V = ValueDecorator(0)
,R = RandomDataGenerator(1)
)

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
    self$N <- self$tunnel$value
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

SignedArrayCodec <- function(len,N){
  obj <- list(
    l = len
    ,N = N
    ,lR = N*len
    ,multiplier = (256.^(0:(len-1)))
    ,divider = 256.^(1:len)
    ,comp = 2^(8*self$l-1)
    ,sub = 2^(8*self$l)
    ,scaler = FALSE
    ,encodes = TRUE
  )
  class(obj) <- "SignedArrayCodec"
  return (obj)
}

decode.SignedArrayCodec <- function(self,rbs){
  if (!is.na(self$tunnel)){
    self$N <- self$tunnel$value
    self$lR <- self$N*self$l
  }
  v = as.numeric(rbs$read(self$lR))
  v = matrix(v,self.len,self$N)
  v = v %*% self$Multiplier
  np <-  v>=self$Comp;
  if (any(np)){
     v[np]=v[np] - self$sub
  }
  rbs$move(self$lR)
  return (v)
}

add_reference.SignedArrayCodec <- function(self,cvalue){
  self$tunnel <- cvalue
  return(self)
}

RecordCodec <- function(Codec,n){
  obj = list(
    N = n
    ,fmt = NA
    ,codec = Codec
    ,uniform = FALSE
    ,scaler = FALSE
    ,encodes = TRUE
    ,tunnel = NA
  )
  class(obj) <- "RecordCodec"
}

decode.RecordCodec <- function(self,rbs){
  if (!is.na(self$tunnel))
    self$N <- self$tunnel$value
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

add_reference.RecordCodec <- function(self,cvalue){
  self$tunnel <- cvalue
  return(self)
}

BufferCodec <- function(fmt){
  obj <- list(
    codecs <- NA
    ,uniform = FALSE
  )
  if (ischar(fmt))
    obj$codecs <- BufferCodec.Factory(fmt)
  else
    obj$codecs <- fmt
  obj$uniform <- all(lapply(obj$codecs,function(x) x$scaler));
  class(obj)<-"BufferCodec"
  return (obj)
}

decode.BufferCodec <- function(self,rbs){
  if (self$unform){
    cnt<-0
    for (codec in self$codecs){
      t <- decode(codec,rbs)
      if (is.NA(t) || is.null(t) || length(t)==0){
        next
      }
      if (cnt ==0){
        cnt<-cnt+1
        v <-t
      }
      else {
        cnt<-cnt+1
        v[cnt]<-t
      }
    }
  }
  else{
    cnt<-0
    for (codec in self$codecs){
      t <- decode(codec,rbs)
      if (is.NA(t) || is.null(t) || length(t)==0){
        next
      }
      if (cnt==0){
        cnt <-cnt+1
        v <- list(t)
      }
      else {
        cnt<-cnt+1
        v[cnt]<-t
      }
    }
  }
  if (is.list(v) && length(a)==1)
    v <- v[[1]]
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
  while (ci<length(cv)){
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
      codecs[[cnt]] <- AtomicCodec[[c]]
      ci<-ci+1
    }
    else if (any(digit == c)){
      ld <- which(digit == cv[ci:end])[1]
      recn <- as.numeric(cv[ci:(ci+ld-1)])
      codecs[cnt]<-permute_to_array(codecs[[cnt]],recn)
      ci<-ci+ld
    }
    else if (c==">") {
      #forward link
      name <- cv(ci+1);
      cvalue <- tunnel()
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
    else
      ci<-ci+1
  }
  return (codecs)
}

# function codecs = Factory(str)
# % Static Factory Method Compiles Format String into an cell array of format descriptors for later use.
# digits =  {'0' '1' '2' '3' '4' '5' '6' '7' '8' '9' '+' '-'};
# %#function AtomicCodec
# halswitch('-PUP_OFFSETBINARY');
# [m,s]=enumeration('AtomicCodec');

# p  = find(str(ci+2:end)==mc,1,'first');
# fielddesc = str(ci+1:ci+p+1);
# fielddesc = eval(fielddesc);
# if codecs{cnt}.Scaler
# if any(fielddesc<0)
# if codecs{cnt}.len == 8
# fc=uint64SignedBitFieldCodec(codecs{cnt},fielddesc);
# else
#   fc = SignedBitFieldCodec(codecs{cnt},fielddesc,offsetbinary);
# end
# else
#   if codecs{cnt}.len == 8
# fc=uint64BitFieldCodec(codecs{cnt},fielddesc);
# else
#   fc = BitFieldCodec(codecs{cnt},fielddesc);
# end
# end
# nfields = length(fielddesc);
# for ii=0:nfields-1
# codecs{cnt+ii}=fc;
# end
# cnt=cnt+nfields-1;
# else
#   if mc==']'
# if any(fielddesc<0)
# if codecs{cnt}.len==8
# codecs{cnt} = int64BitFieldArrayCodec(codecs{cnt},fielddesc,offsetbinary);
# else
#   codecs{cnt} = SignedBitFieldArrayCodec(codecs{cnt},fielddesc,offsetbinary);
# end
# else
#   if codecs{cnt}.len==8
# codecs{cnt} = uint64BitFieldArrayCodec(codecs{cnt},fielddesc);
# else
#   codecs{cnt} = BitFieldArrayCodec(codecs{cnt},fielddesc);
# end
# end
# else
#   if  any(cellfun(@(x) any(x<0),fielddesc))
# codecs{cnt} = SignedBitFieldMatrixCodec(codecs{cnt},fielddesc);
# else
#   codecs{cnt} = BitFieldMatrixCodec(codecs{cnt},fielddesc);
# end
# end
# end
# ci=ci+p+2;
# case '['
# p  = find(str(ci+1:end)==']',1,'first');
# classname = str(ci+1:ci+p-1);
# cnt=cnt+1;
# codecs{cnt} = ClassCodec(classname,1);
# ci=ci+p;
# case '('
# p  = (str(ci:end)=='(') - (str(ci:end)==')');
# if sum(p)~=0
# error('BufferCodec.Factory, format string, %s, has unbalanced parentheses',str);
# end
# fp = find(p~=0);
# sp = fp;
# for pi=2:length(fp); sp(pi)=sp(pi-1)+p(fp(pi)); end;
# lp = fp(find(sp==0,1,'first'));
# rfmt = BufferCodec.Factory(str(ci+1:ci+lp-2));
# isUniform = all(cellfun(@(x) x.Scaler,rfmt));
# cnt=cnt+1;
# if isUniform
# codecs{cnt}=UniformRecordCodec(rfmt,1);
# else
#   codecs{cnt}=RecordCodec(rfmt,1);
# end
# ci=ci+lp;
# case s
# cnt=cnt+1;
# codecs{cnt} = AtomicCodec.(str(ci)).codec;
# ci=ci+1;
# case digits
# cstr = cellfun(@(x) {char(x)},(num2cell(int8(str(ci:end)))));
# nums = ismember(cstr,digits);
# nums = [nums 0];
# ld = find(nums==0,1,'first')-1;
# recn = str2num(str(ci:ci+ld-1));
# if isa(codecs{cnt},'UniformRecordCodec')
# codecs{cnt} = codecs{cnt}.Optimize();
# end
# codecs{cnt} = codecs{cnt}.PermuteToArray(recn);
# ci=ci+ld;
# case '>' %forward link
# name = str(ci+1);
# codecs{cnt} = codecs{cnt}.PermuteToForwardReference();
# forwardLinks.(name) = codecs{cnt};
# ci=ci+2;
# case '<' %consume forward link
# name = str(ci+1);
# hSource = forwardLinks.(name);
# if isa(codecs{cnt},'UniformRecordCodec')
# codecs{cnt} = codecs{cnt}.Optimize();
# end
# codecs{cnt} = codecs{cnt}.PermuteToArray(1);
# hTarget=codecs{cnt};
# addlistener(hSource,'DataParsed',@hTarget.ProcessForwardReference);
# ci=ci+2;
# case ':'
# codecs{cnt} = codecs{cnt}.PermuteToConstant();
# ci=ci+1;
# otherwise
# ci=ci+1;
# end
# end
# end

# classdef BufferCodec < handle
# %BufferCodec Used to Code/Decode (Pack/Unpack) byte buffers
# %   Detailed explanation goes here
# %
# % Format specifier
# % Scalers return a single number
# %   'c' character
# %   'b' signed    1-byte integer
# %   'B' unsigned  1-byte integer
# %   's' signed    2-byte integer
# %   'S' unsigned  2-byte integer
# %   'l' signed    4-byte integer
# %   'L' unsigned  4-byte integer
# %   't' signed    6-byte integer
# %   'T' unsigned  6-byte integer
# %   'q' signed    8-byte integer
# %   'Q' unsigned  8-byte integer
# %   'f' float 32
# %   'd' float 64 (double)
# %
# % Positioners - do not return value; just changes offset
# %   'x' pad byte
# %   'x8' 8 pad bytes
# %   'o' absolute offset
# %   'o1024' sets current offset to 1024, next value parsed
# %           starting at byte 1024
# %           offsets use 0 based indexing
# %
#
#
# %  Arrays - return a list of numbers
# %    array = Scaler number
# %    'S8' returns an array of 8 2-byte integers
# %
# %  Uniform Records
# %    (Scaler {Scaler})
# %    '(SSbBL)' return array of 5 numbers
# %
# %  Uniform Record Array
# %    all elements of record are Scalers
# %    (Uniform Record) number
# %    '(SSbBL)n' return an nx5 matrix
# %
# %  Records - Non Uniform
# %    All elements of record are not Scalers
# %    '(bSs32)'   will return a    3 element cell array
# %    '(bSs32)n' will reutrn an nx3 element cell array
# %
# %  Class
# %    '[ClassName]'
# %    returns instance of specified class
# %    calls the classes decode method to parse buffer
# %    '[ClassName]n'
# %    returns an array length(n) of the specified class
# %    calls classes decode method to parse buffer
# %
# %    Usually the class specified by classname will be a subclass
# %    of the Recepticle class, but can be of any handle class that implements
# %    a decode function with the pattern:
#   %    function [b,o]=decode(self,b,o);
# %
# %  BitFields
# %   'S_[1 3 7 3 1]'
# %   breaks an Unsigned short into 5 bitfields
# %   returning them from low-order to high order bits
# %
# %  BitField Arrays
# %   'S10_[1 3 7 3 1]'
# %   breaks an Unsigned short array of ten elements into 5 bitfields
# %   returning them from low-order  to hi-order bits
# %   bitfields are columns rows are each number from original array
# %
# %  BitField Matrix
# %   '(SS)10_{8 4 4; 4 4 4 2 2}'
# %   In this example 10 records constisting of two unsigned shorts
# %   are split into 8 fields; the first short into 3 fields and the
# %   second short of each record into 5 fields.
# %   10 rows will be returned.
#
# %   Written By: Bruce C. Schardt
# %   Primary Contact:
#   %       Dr. Bruce C. Schardt
# %       Sr. Staff Electronic Design Engineer
# %       Magnetic Recoding Technology
# %       Advanced Read Channel Optimization
# %       bruce.schardt@wdc.com
#
# %   Copyright 2011 Western Digital Corporation
# %   $URL: http://svn.wdc.com/svn/matexsdk/matex/trunk/UtilityClasses/BufferCodec.m $
#   %   $Date: 2015-08-20 21:48:00 -0700 (Thu, 20 Aug 2015) $
#   %   $Author: maguire_b@sc.wdc.com $
#   %   $Rev: 27876 $
#   %   $Id: BufferCodec.m 27876 2015-08-21 04:48:00Z maguire_b@sc.wdc.com $
#
#
#   %#function CurrentOffsetDecoraor
# %#function CurrentOffsetForwardReference
# %#function AbsolutePositioner
# %#function RelativePositioner
# %#function CharCodec
# %#function StringCodec
# %#function SignedCodec
# %#function UnsignedCodec
# %#function UnsignedForwardReferenceCodec
# %#function UnsignedArrayCodec
# %#function SignedArrayCodec
# %#function uint64Codec
# %#function int64Codec
# %#function uint64ArrayCodec
# %#function int64ArrayCodec
# %#function FloatCodec
# %#function DoubleCodec
# %#function FloatArrayCodec
# %#function DoubleArrayCodec
# %#function RecordCodec
# %#function UniformRecordCodec
# %#function OptimizedUniformRecordCodec
# %#function ClassCodec
#
# properties
# codecs
# Uniform
# end
#
# methods
# function self = BufferCodec(fmt)
# %Class Constructor given format string
# if ischar(fmt)
# self.codecs = BufferCodec.Factory(fmt);
# else
#   self.codecs = fmt;
# end
# self.Uniform = all(cellfun(@(x) x.Scaler,self.codecs));
#
#
# end
# function delete(self)
# for ci=1:length(self.codecs)
# self.codecs{ci}=[];
# end
# end
# function [v,b,o]=decode(self,b,o)
# %decode based on compiled format
# if self.Uniform
# cnt=0;
# for ci=1:length(self.codecs)
# [t,b,o]=self.codecs{ci}.decode(b,o);
# if isempty(t)
# continue;
# end
# if cnt==0
# cnt=cnt+1;
# v=t;
# else
#   cnt=cnt+1;
# v(cnt)=t;
# end
# end
# else
#   cnt=0;
# for ci=1:length(self.codecs)
# [t,b,o]=self.codecs{ci}.decode(b,o);
# if isempty(t)
# continue;
# end
# if cnt==0
# cnt=cnt+1;
# v={t};
# else
#   cnt=cnt+1;
# v{cnt}=t;
# end
# end
# end
# if iscell(v) && length(v)==1
# v=v{1};
# end
# end
# function [b,o]=encode(self,b,o,varargin)
# if length(self.codecs)==1
# [b,o]=self.codecs{1}.encode(b,o,varargin{:});
# return;
# elseif length(varargin)==1
# v=varargin{1};
# else
#   v=varargin;
# end
# if iscell(v)
# cnt=0;
# for ci=1:length(self.codecs)
# if self.codecs{ci}.encodes
# cnt=cnt+1;
# [b,o]=self.codecs{ci}.encode(b,o,v{cnt});
# else
#   [b,o]=self.codecs{ci}.encode(b,o,[]);
# end
# end
# else
#   cnt=0;
# for ci=1:length(self.codecs)
# if self.codecs{ci}.encodes
# cnt=cnt+1;
# [b,o]=self.codecs{ci}.encode(b,o,v(cnt));
# else
#   [b,o]=self.codecs{ci}.encode(b,o,[]);
# end
# end
# end
# end
# end
# methods (Static)
# function codecs = Factory(str)
# % Static Factory Method Compiles Format String into an cell array of format descriptors for later use.
# digits =  {'0' '1' '2' '3' '4' '5' '6' '7' '8' '9' '+' '-'};
# %#function AtomicCodec
# halswitch('-PUP_OFFSETBINARY');
# [m,s]=enumeration('AtomicCodec');
# offsetbinary = false;
#
# codecs = {};
# cnt = 0;
# ci=1;
# while ci<=length(str)
# switch str(ci)
# case 'K'
# offsetbinary=true;
# ci=ci+1;
# halswitch('+PUP_OFFSETBINARY');
# case 'k'
# offsetbinary=false;
# halswitch('-PUP_OFFSETBINARY');
# ci=ci+1;
# case '_'
# mc = str(ci+1);
# if mc=='['
# mc=']';
# elseif mc=='{';
# mc='}';
# end
# p  = find(str(ci+2:end)==mc,1,'first');
# fielddesc = str(ci+2:ci+p+1);
# fielddesc = eval(fielddesc);
# if codecs{cnt}.Scaler
# if any(fielddesc<0)
# if codecs{cnt}.len == 8
# fc=uint64SignedBitFieldCodec(codecs{cnt},fielddesc);
# else
#   fc = SignedBitFieldCodec(codecs{cnt},fielddesc,offsetbinary);
# end
# else
#   if codecs{cnt}.len == 8
# fc=uint64BitFieldCodec(codecs{cnt},fielddesc);
# else
#   fc = BitFieldCodec(codecs{cnt},fielddesc);
# end
# end
# nfields = length(fielddesc);
# for ii=0:nfields-1
# codecs{cnt+ii}=fc;
# end
# cnt=cnt+nfields-1;
# else
#   if mc==']'
# if any(fielddesc<0)
# if codecs{cnt}.len==8
# codecs{cnt} = int64BitFieldArrayCodec(codecs{cnt},fielddesc,offsetbinary);
# else
#   codecs{cnt} = SignedBitFieldArrayCodec(codecs{cnt},fielddesc,offsetbinary);
# end
# else
#   if codecs{cnt}.len==8
# codecs{cnt} = uint64BitFieldArrayCodec(codecs{cnt},fielddesc);
# else
#   codecs{cnt} = BitFieldArrayCodec(codecs{cnt},fielddesc);
# end
# end
# else
#   if  any(cellfun(@(x) any(x<0),fielddesc))
# codecs{cnt} = SignedBitFieldMatrixCodec(codecs{cnt},fielddesc);
# else
#   codecs{cnt} = BitFieldMatrixCodec(codecs{cnt},fielddesc);
# end
# end
# end
# ci=ci+p+2;
# case '['
# p  = find(str(ci+1:end)==']',1,'first');
# classname = str(ci+1:ci+p-1);
# cnt=cnt+1;
# codecs{cnt} = ClassCodec(classname,1);
# ci=ci+p;
# case '('
# p  = (str(ci:end)=='(') - (str(ci:end)==')');
# if sum(p)~=0
# error('BufferCodec.Factory, format string, %s, has unbalanced parentheses',str);
# end
# fp = find(p~=0);
# sp = fp;
# for pi=2:length(fp); sp(pi)=sp(pi-1)+p(fp(pi)); end;
# lp = fp(find(sp==0,1,'first'));
# rfmt = BufferCodec.Factory(str(ci+1:ci+lp-2));
# isUniform = all(cellfun(@(x) x.Scaler,rfmt));
# cnt=cnt+1;
# if isUniform
# codecs{cnt}=UniformRecordCodec(rfmt,1);
# else
#   codecs{cnt}=RecordCodec(rfmt,1);
# end
# ci=ci+lp;
# case s
# cnt=cnt+1;
# codecs{cnt} = AtomicCodec.(str(ci)).codec;
# ci=ci+1;
# case digits
# cstr = cellfun(@(x) {char(x)},(num2cell(int8(str(ci:end)))));
# nums = ismember(cstr,digits);
# nums = [nums 0];
# ld = find(nums==0,1,'first')-1;
# recn = str2num(str(ci:ci+ld-1));
# if isa(codecs{cnt},'UniformRecordCodec')
# codecs{cnt} = codecs{cnt}.Optimize();
# end
# codecs{cnt} = codecs{cnt}.PermuteToArray(recn);
# ci=ci+ld;
# case '>' %forward link
# name = str(ci+1);
# codecs{cnt} = codecs{cnt}.PermuteToForwardReference();
# forwardLinks.(name) = codecs{cnt};
# ci=ci+2;
# case '<' %consume forward link
# name = str(ci+1);
# hSource = forwardLinks.(name);
# if isa(codecs{cnt},'UniformRecordCodec')
# codecs{cnt} = codecs{cnt}.Optimize();
# end
# codecs{cnt} = codecs{cnt}.PermuteToArray(1);
# hTarget=codecs{cnt};
# addlistener(hSource,'DataParsed',@hTarget.ProcessForwardReference);
# ci=ci+2;
# case ':'
# codecs{cnt} = codecs{cnt}.PermuteToConstant();
# ci=ci+1;
# otherwise
# ci=ci+1;
# end
# end
# end
# end
# end

