## ----SETUP, echo=FALSE, results="hide"--------------------------------------------------
library("knitr")
opts_chunk$set(fig.path="figs/lucid-", fig.align="center",
               fig.width=7, fig.height=7)
options(width=90)
if(!file.exists("figs")) dir.create("figs")

## ----echo=FALSE-------------------------------------------------------------------------
df1 <- data.frame(effect=c(-13.5, 4.5,  24.5, 6.927792e-14, -1.75,
                    16.5, 113.5000))
rownames(df1) <- c("A","B","C","C1","C2","D","(Intercept)")
print(df1)

## ----message=FALSE----------------------------------------------------------------------
require("lucid")
options(digits=7) # knitr defaults to 4, R console uses 7
lucid(df1)

## ---------------------------------------------------------------------------------------
print(antibiotic)

## ---------------------------------------------------------------------------------------
lucid(antibiotic)

## ----echo=FALSE, message=FALSE, fig.height=4, fig.width=4-------------------------------
require(lattice)
anti=antibiotic # make a copy of the data to reverse the levels
anti$bacteria <- factor(anti$bacteria, levels=rev(anti$bacteria))
dotplot(bacteria~ -log10(penicillin), anti,
        cex=1, xlim=c(-4,4), #xlab="variance component (log10 scale)",
        scales=list(x=list(at= c(-2,0,2), lab=c('100','1','.01')))
        )

## ----message=FALSE----------------------------------------------------------------------
require("nlme")
data(Rail)
mn <- lme(travel~1, random=~1|Rail, data=Rail)
vc(mn)

require("lme4")
m4 <- lmer(travel~1 + (1|Rail), data=Rail)
vc(m4)

#require("asreml")
#ma <- asreml(travel~1, random=~Rail, data=Rail)
#vc(ma)
##         effect component std.error z.ratio constr
##  Rail!Rail.var    615.3      392.6     1.6    pos
##     R!variance     16.17       6.6     2.4    pos

## ----echo=FALSE-------------------------------------------------------------------------
d1 <- structure(list(Groups = c("new.gen", "one", "one.1", "one.2", 
"one.3", "one.4", "one.5", "one.6", "one.7", "one.8", "one.9", 
"one.10", "one.11", "one.12", "one.13", "Residual"), Name = c("(Intercept)", 
"r1:c3", "r1:c2", "r1:c1", "c8", "c6", "c4", "c3", "c2", "c1", 
"r10", "r8", "r4", "r2", "r1", ""), Variance = c(2869.45, 5531.6, 
58225.75, 128003.6, 6455.77, 1399.73, 1791.65, 2548.89, 5941.8, 
0, 1132.95, 1355.23, 2268.73, 241.79, 9199.94, 4412.11), Std.Dev. = c(53.567, 
74.375, 241.3, 357.776, 80.348, 37.413, 42.328, 50.486, 77.083, 
0, 33.659, 36.813, 47.631, 15.55, 95.916, 66.424)), .Names = c("Groups", 
"Name", "Variance", "Std.Dev."), class = "data.frame", row.names = c(NA, 
-16L))
d2 <- structure(list(Groups = c("new.gen", "one", "one.1", "one.2", 
"one.3", "one.4", "one.5", "one.6", "one.7", "one.8", "one.9", 
"one.10", "one.11", "one.12", "one.13", "Residual"), Name = c("(Intercept)", 
"r1:c3", "r1:c2", "r1:c1", "c8", "c6", "c4", "c3", "c2", "c1", 
"r10", "r8", "r4", "r2", "r1", ""), Variance = c(3230, 7690, 
69800, 107000, 6790, 1640, 12300, 2690, 7640, 0.000956, 1980, 
1240, 2810, 928, 10400, 4130), Std.Dev. = c(56.81831, 87.675211, 
264.123506, 327.750047, 82.381314, 40.446339, 110.764195, 51.831045, 
87.435345, 0.030918, 44.446766, 35.234043, 53.020431, 30.465617, 
101.80296, 64.240858)), .Names = c("Groups", "Name", "Variance", 
"Std.Dev."), class = "data.frame", row.names = c(NA, -16L))

## ---------------------------------------------------------------------------------------
cbind(d1,d2[,3:4])

## ---------------------------------------------------------------------------------------
lucid(cbind(d1,d2[,3:4]))

## ----finish, echo=FALSE, results="asis"-------------------------------------------------
# knit_hooks$set(output = function(x, options) { x })
toLatex(sessionInfo(), locale=FALSE)

