library(testthat)
library(dogmRtism)

test_df <- data.frame("txt" = c("always constant nonsense nonsense nonsense",
                                "perfect quite radical nonsense nonsense",
                                "various frequent rare very doubt",
                                "plenty doubt nonsense nonsense nonsense"))

test_df$txt <- as.character(test_df$txt)


test_that("correct count close-mind", {
  dog_df <- dogmrtism(test_df)
  expect_equal(dog_df$close_mind,c(2/5, 3/5,0,0))

})


test_that("correct count open-mind", {
  dog_df <- dogmrtism(test_df)
  expect_equal(dog_df$open_mind, c(0,0, 5/5, 2/5))

})