context("UnsignedArrayCodec")
library(PackUnpackR)

d <- raw_byte_stream()

u8 <- UnsignedArrayCodec(1,128)

test_that("u8 array", {
   expect_equal(decode(u8,d),0:127)
   expect_equal(decode(u8,d),128:255)
})

d<-raw_byte_stream(list(0, 0, 1, 0, 255, 0, 0, 1, 255, 255, 0, 0))
u16<-UnsignedArrayCodec(2,3)
test_that("u16 array",{
  expect_equal(decode(u16,d),c(0,1,255))
  expect_equal(decode(u16,d),c(256,2^16-1,0))
})

d<-raw_byte_stream(list(0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 255, 255, 255))
u32<-UnsignedArrayCodec(4,2)
test_that("u32 array",{
  expect_equal(decode(u32,d),c(0,1))
  expect_equal(decode(u32,d),c(257,2^32-255))
})

u32 <- UnsignedCodec(4)
ua32 <-UnsignedArrayCodec(4,10)
pau32 <- permute_to_array(u32,10)
test_that("permute_to_array",{
  expect_equal(pau32,ua32)
})
