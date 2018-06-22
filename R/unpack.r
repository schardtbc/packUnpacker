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

unpack <- function(fmt,b,o = NA){
  codec <- BufferCodec(fmt)
  if (class(b)=="raw_byte_stream"){
    rbs <- b
    if (!is.na(o))
      rbs$seek(o)
  }
  else
    rbs <- raw_byte_stream(b,o)
  r <- decode(codec,rbs)
  return (r)
}


