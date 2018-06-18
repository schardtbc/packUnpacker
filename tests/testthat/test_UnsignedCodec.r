context("UnsignedCodec")
library(PackUnpackR)

u8 <- UnsignedCodec(1)
d <- raw_byte_stream()
test_that("u8",{
          expect_equal(decode(u8,d),0)
          expect_equal(decode(u8,d),1)
          expect_equal(decode(u8,d),2)
          })

u16 <- UnsignedCodec(2)
test_that("u16",{
          expect_equal(decode(u16,d),4*256+3)
          expect_equal(decode(u16,d),6*256+5)
          expect_equal(decode(u16,d),8*256+7)
          })

u32 <-UnsignedCodec(4)
test_that("u32",{
          expect_equal(decode(u32,d),12*256^3+11*256^2+10*256+9)
          expect_equal(decode(u32,d),16*256^3+15*256^2+14*256+13)
          expect_equal(decode(u32,d),20*256^3+19*256^2+18*256+17)
          })
