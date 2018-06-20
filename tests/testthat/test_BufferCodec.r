context("BufferCodec")

u8 <- UnsignedCodec(1)
u16<- UnsignedCodec(2)
u32<- UnsignedCodec(4)

s8 <- SignedCodec(1)
s16<- SignedCodec(2)
s32<- SignedCodec(4)

cl <- list(u32,u8,u16,s8,s32,s16)

bc <- BufferCodec.Factory("LBSbls")

test_that("BufferCodec.Factory",{
  expect_equal(bc,cl)
})
