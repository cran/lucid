# test_vc.R
# Time-stamp: <23 Apr 2019 15:05:49 c:/x/rpack/lucid/tests/testthat/test_vc.R>

require(lucid)
data(Rail, package="nlme")

# ----------------------------------------------------------------------------

test_that("default", {
  expect_error(vc(1))
})

test_that("nlme", {
  require(nlme)
  m1n <- lme(travel~1, random=~1|Rail, data=Rail)
  expect_equal(
    vc(m1n),
      structure(list(effect = 
                       structure(1:2, 
                                 .Label = c("(Intercept)", "Residual"), 
                                 class = "factor"), 
                     variance = c(615.31, 16.17), 
                     stddev = c(24.81, 4.02)), 
                .Names = c("effect", "variance", "stddev"), 
                row.names = c(NA, -2L), class = c("vc.lme", "data.frame")),
    tolerance=1e-1)

  # print method
  print(vc(m1n))
})

# ----------------------------------------------------------------------------

test_that("lmer", {
  require("lme4")
  m1l <- lmer(travel~1 + (1|Rail), data=Rail)
  expect_equal(
    vc(m1l),
    structure(list(grp = c("Rail", "Residual"), 
                   var1 = c("(Intercept)", NA), 
                   var2 = c(NA_character_, NA_character_), 
                   vcov = c(615.32, 16.17), 
                   sdcor = c(24.81, 4.02)), 
              row.names = c(NA, -2L), 
              class = c("vc.lmerMod", "data.frame")),
    tolerance=1e-1)

  # print method
  print(vc(m1l))
})

# ----------------------------------------------------------------------------

test_that("glmer", {
  require("lme4")
  m1g <- glmer(travel~1 + (1|Rail), data=Rail, family=gaussian(link="log"))
  expect_equal(
    vc(m1g),
    structure(list(grp = c("Rail", "Residual"), 
                   var1 = c("(Intercept)", NA), 
                   var2 = c(NA_character_, NA_character_), 
                   vcov = c(1.64, 11.11), 
                   sdcor = c(1.28, 3.33)), 
              .Names = c("grp", "var1", "var2", "vcov", "sdcor"), 
              row.names = c(NA, -2L), class = c("vc.lmerMod", "data.frame")),
    tolerance=1e-1)
  # print
  print(vc(m1g))
})

# ----------------------------------------------------------------------------


test_that("asreml", {
  if(require("asreml")){
    m1a <- asreml(travel~1, random=~Rail, data=Rail)
    expect_equal(
      vc(m1a),
      structure(list(effect = structure(1:2, .Label = c("Rail", "units!R"), 
        class = "factor"), 
        component = c(615.74, 16.18), 
        std.error = c(391.58, 6.61), 
        z.ratio = c(1.57, 2.45), 
        bound=c("P", "P"),
        `%ch` = c(0.2, 0)), 
        class = c("vc.asreml", "data.frame"), 
        row.names = c(NA, -2L)),
      tolerance=1e-1)
  # print method
  print(vc(m1a))
  }})
  
# ----------------------------------------------------------------------------

test_that("mmer",{  
  require("sommer")
  m1s <- mmer(travel~1, random = ~ Rail, data=Rail)
  expect_equal(
    vc(m1s),
    structure(list(effect = c("Rail.travel-travel", "units.travel-travel"),
                   VarComp = c(615.26, 16.17), 
                   VarCompSE = c(392.28, 6.60), 
                   Zratio = c(1.57, 2.45), 
                   Constraint = c("Positive", "Positive")), 
              row.names = c(NA, -2L), 
              class = c("vc.mmer", "data.frame")),
    tolerance=1e-1)
  # print
  print(vc(m1s))
  
})
