context("SignedArrayCodec")
library(PackUnpackR)

d <- raw_byte_stream()

s8 <- SignedArrayCodec(1,128)

test_that("s8 array", {
   expect_equal(decode(s8,d),0:127)
   expect_equal(decode(s8,d),seq(from = -128, to -1, length.out = 128))
})

d<-raw_byte_stream(list(0, 0, 1, 0, 255, 0, 255, 127, 0, 128, 1, 128, 254, 255, 255, 255, 253, 255))
s16<-SignedArrayCodec(2,3)
test_that("s16 array",{
  expect_equal(decode(s16,d),c(0,1,255))
  expect_equal(decode(s16,d),c(256,2^16-1,0))
})

d<-raw_byte_stream(list(0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 255, 255, 255))
u32<-UnsignedArrayCodec(4,2)
test_that("u32 array",{
  expect_equal(decode(u32,d),c(0,1))
  expect_equal(decode(u32,d),c(257,2^32-255))
})
