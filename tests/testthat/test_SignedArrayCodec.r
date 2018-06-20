context("SignedArrayCodec")
library(PackUnpackR)

d <- raw_byte_stream()
s8 <- SignedArrayCodec(1,128)

test_that("s8 array", {
   expect_equal(decode(s8,d),0:127)
   expect_equal(decode(s8,d),seq(from = -128, to = -1, length.out = 128))
})

d<-raw_byte_stream(list(0, 0, 1, 0, 255, 0, 255, 127, 0, 128, 1, 128, 254, 255, 255, 255, 253, 255))
s16<-SignedArrayCodec(2,3)
test_that("s16 array",{
  expect_equal(decode(s16,d),c(0,1,255))
  expect_equal(decode(s16,d),c(2^15-1,-2^15,-2^15+1))
  expect_equal(decode(s16,d),c(-2,-1,-3))
})

d<-raw_byte_stream(list(0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 255, 255, 255, 255, 255, 255, 255, 127,0,0,0,128))
s32<-SignedArrayCodec(4,2)
test_that("s32 array",{
  expect_equal(decode(s32,d),c(0,1))
  expect_equal(decode(s32,d),c(257,-1))
  expect_equal(decode(s32,d),c(2^31-1,-2^31))
})

s32 <- SignedCodec(4)
sa32 <-SignedArrayCodec(4,10)
pas32 <- permute_to_array(s32,10)
test_that("permute_to_array",{
  expect_equal(pas32,sa32)
})
