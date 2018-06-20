context("Positioners")
library(PackUnpackR)


d <- raw_byte_stream()
O <- CurrentOffsetDecorator()
x  <- RelativePositioner()
x4 <- RelativePositioner(4)
a100 <- AbsolutePositioner(100)
a256 <- AbsolutePositioner(256)
u8 <- UnsignedCodec(1)

decode(x4,d)

test_that("RelativePositioner",{
  expect_equal(decode(u8,d),4)
  expect_equal(decode(O,d),5)
})

decode(a100,d)
test_that("AbsolutePositioner",{
  expect_equal(decode(u8,d),99)
  expect_equal(decode(O,d),100)
})

decode(a256,d)
test_that("AbsolutePositioner",{
  expect_equal(decode(O,d),255)
  expect_equal(decode(u8,d),255)
  expect_equal(decode(O,d),256)
})

px4 <- permute_to_array(x,4)
test_that("permute_to_array",{
  expect_equal(px4,x4)
})
