context("unpack")
library(PackUnpackR)

d <- list(0,0,255,0,128,0,255,128,255,255,255,255,0,128,0,0,255,0,128,0,255,128,255,255,255,255,0,128
         ,0,0,255,0,128,0,255,128,255,255,255,255,0,128,0,0,255,0,128,0,255,128,255,255,255,255,0,128)
vd <- c(255*256^2,128,255*256+0,-2^7,-1,-2^15)
r <- vd

result <- unpack("LBSlbs",d,0)

test_that("unpack simple numerics",{
  expect_equal(result,r)
})


vd <- c( 255*256^2,128,255*256+0,-2^7,-1,-2^15
        ,255*256^2,128,255*256+0,-2^7,-1,-2^15
        ,255*256^2,128,255*256+0,-2^7,-1,-2^15
        ,255*256^2,128,255*256+0,-2^7,-1,-2^15)
r <- matrix(vd,4,6,byrow = TRUE)
result <- unpack("(LBSlbs)4",d,0)

test_that("unpack uniform record array",{
  expect_equal(result,r)
})


vd <- c( 128,255*256+0,-2^7,-1,-2^15
        ,128,255*256+0,-2^7,-1,-2^15
        ,128,255*256+0,-2^7,-1,-2^15
        ,128,255*256+0,-2^7,-1,-2^15)

r <- matrix(vd,4,5,byrow = TRUE)
result <- unpack("(x4BSlbs)4",d,0)

test_that("unpack uniform record array with pad bytes",{
  expect_equal(result,r)
})

vd <- c( 255*256^2,128,255*256+0,-2^7,-1,-2^15
         ,255*256^2,128,255*256+0,-2^7,-1,-2^15
         ,255*256^2,128,255*256+0,-2^7,-1,-2^15)
r <- matrix(vd,3,6,byrow = TRUE)
hdr <- list(255*256^2,128,255*256+0,-2^7,-1,-2^15)
cr <- hdr
cr[[7]]<-r

result <-unpack("LBSlbs(LBSlbs)3",d,0)

test_that("unpack compex stucture",{
  expect_equal(result,cr)
})
