context("SignedCodec")
library(PackUnpackR)

s8 <- SignedCodec(1)
d <- raw_byte_stream()
test_that("s8",{
          expect_equal(decode(s8,d),0)
          expect_equal(decode(s8,d),1)
          expect_equal(decode(s8,d),2)
          })

s16 <- SignedCodec(2)
test_that("s16",{
          expect_equal(decode(s16,d),4*256+3)
          expect_equal(decode(s16,d),6*256+5)
          expect_equal(decode(s16,d),8*256+7)
          })

s32 <-SignedCodec(4)
test_that("s32",{
          expect_equal(decode(s32,d),12*256^3+11*256^2+10*256+9)
          expect_equal(decode(s32,d),16*256^3+15*256^2+14*256+13)
          expect_equal(decode(s32,d),20*256^3+19*256^2+18*256+17)
          })

d<-raw_byte_stream(as.raw(seq(from = 255, to = 0, length.out=256)))

test_that("s8",{
  expect_equal(decode(s8,d),-1)
  expect_equal(decode(s8,d),-2)
  expect_equal(decode(s8,d),-3)
})

d<-raw_byte_stream(as.raw(list(255, 255, 254 ,255, 253, 255)))
test_that("s16",{
  expect_equal(decode(s16,d),-1)
  expect_equal(decode(s16,d),-2)
  expect_equal(decode(s16,d),-3)
})
d<-raw_byte_stream(as.raw(list(255, 255, 255 ,255, 254, 255, 255, 255, 253, 255, 255, 255)))
s32 <-SignedCodec(4)
test_that("s32",{
  expect_equal(decode(s32,d),-1)
  expect_equal(decode(s32,d),-2)
  expect_equal(decode(s32,d),-3)
})

d<-raw_byte_stream(as.raw(seq(from = 126, to = 129, length.out=4)))

test_that("s8",{
  expect_equal(decode(s8,d),126)
  expect_equal(decode(s8,d),127)
  expect_equal(decode(s8,d),-128)
  expect_equal(decode(s8,d),-127)
})

d<-raw_byte_stream(as.raw(list(255, 127, 0 ,128, 1, 128)))
test_that("s16",{
  expect_equal(decode(s16,d),32767)
  expect_equal(decode(s16,d),-32768)
  expect_equal(decode(s16,d),-32767)
})

d<-raw_byte_stream(as.raw(list(255, 255, 255, 127, 0, 0, 0 , 128, 1, 0, 0, 128)))
test_that("s32",{
  expect_equal(decode(s32,d),2147483647)
  expect_equal(decode(s32,d),-2147483648)
  expect_equal(decode(s32,d),-2147483647)
})
