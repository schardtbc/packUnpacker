context("unpack")
library(PackUnpackR)

d <- list(0,0,255,0,128,0,255,128,255,255,255,255,0,128,0,0,255,0,128,0,255,128,255,255,255,255,0,128)
vd <- c(255*256^2,128,255*256+0,-2^7,-1,-2^15)
r <- matrix(vd,1,6)

result <- unpack("LBSlbs",d,0)

test_that("unpack uniform record",{
  expect_equal(result,r)
})
