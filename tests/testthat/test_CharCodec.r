context("CharCodec,StringCodec")
library("PackUnpackR")


c <- CharCodec()
d <- raw_byte_stream(32:128)

test_that("c",{
  expect_equal(decode(c,d)," ")
  expect_equal(decode(c,d),"!")
  expect_equal(decode(c,d),"\"")
  expect_equal(decode(c,d),"#")
})

c26 <- StringCodec(26)
d<-raw_byte_stream(charToRaw("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
test_that("c26",{
  expect_equal(decode(c26,d),"abcdefghijklmnopqrstuvwxyz")
  expect_equal(decode(c26,d),"ABCDEFGHIJKLMNOPQRSTUVWXYZ")
})


pc26 <- permute_to_array(c,26)

d<-raw_byte_stream(charToRaw("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
test_that("pc26",{
  expect_equal(pc26,c26)
  expect_equal(decode(pc26,d),"abcdefghijklmnopqrstuvwxyz")
  expect_equal(decode(pc26,d),"ABCDEFGHIJKLMNOPQRSTUVWXYZ")
})
