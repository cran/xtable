### Test of implementation of request #2104
### Use of \centering rather that center environment when centering tables
### DJS, 16/8/2012
require(xtable)
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2,10,20, labels=c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)
class(lm.D9)

xtable(lm.D9, caption="\\tt latex.environment=\"center\"")

## % latex table generated in R 2.15.0 by xtable 1.7-1 package
## % Thu Aug 16 15:44:09 2012
## \begin{table}[ht]
## \centering
## \begin{tabular}{rrrrr}
##   \hline
##  & Estimate & Std. Error & t value & Pr($>$$|$t$|$) \\
##   \hline
## (Intercept) & 5.0320 & 0.2202 & 22.85 & 0.0000 \\
##   groupTrt & -0.3710 & 0.3114 & -1.19 & 0.2490 \\
##    \hline
## \end{tabular}
## \caption{\tt latex.environment="center"}
## \end{table}


### General testing removed from xtable.Rd, 15/02/2026
### Was leaving detritus in check directory
sessionDir <- getwd()
print(sessionDir)
  for(i in c("latex","html")) {
    outFileName <- paste("xtable.", ifelse(i=="latex", "tex", i), sep = "")
    data(tli)
    tli.table <- xtable(tli[1:20, ])
    print(tli.table, type = i, file = outFileName, append = FALSE)
    design.matrix <- model.matrix(~ sex*grade, data = tli[1:20, ])
    design.table <- xtable(design.matrix, auto = TRUE)
    print(design.table, type = i, file = outFileName, append = TRUE)
    fm1 <- aov(tlimth ~ sex + ethnicty + grade + disadvg, data = tli)
    fm1.table <- xtable(fm1)
    print(fm1.table, type = i, file = outFileName, append = TRUE)
    fm2 <- lm(tlimth ~ sex*ethnicty, data = tli)
    fm2.table <- xtable(fm2)
    print(fm2.table, type = i, file = outFileName, append = TRUE)
    print(fm2.table, type = i, file = outFileName, append = TRUE,
          math.style.negative = TRUE)
    print(xtable(anova(fm2)), type = i, file = outFileName, append = TRUE)
    fm2b <- lm(tlimth ~ ethnicty, data = tli)
    print(xtable(anova(fm2b, fm2)), type = i, file = outFileName, append = TRUE)
    fm3 <- glm(disadvg ~ ethnicty*grade, data = tli, family = binomial())
    fm3.table <- xtable(fm3)
    print(fm3.table, type = i, file = outFileName, append = TRUE)
    print(xtable(anova(fm3)), type = i, file = outFileName, append = TRUE)
    N <- c(0,1,0,1,1,1,0,0,0,1,1,0,1,1,0,0,1,0,1,0,1,1,0,0)
    P <- c(1,1,0,0,0,1,0,1,1,1,0,0,0,1,0,1,1,0,0,1,0,1,1,0)
    K <- c(1,0,0,1,0,1,1,0,0,1,0,1,0,1,1,0,0,0,1,1,1,0,1,0)
    yield <- c(49.5,62.8,46.8,57.0,59.8,58.5,55.5,56.0,62.8,55.8,69.5,55.0,
               62.0,48.8,45.5,44.2,52.0,51.5,49.8,48.8,57.2,59.0,53.2,56.0)
    npk <- data.frame(block = gl(6,4), N = factor(N), P = factor(P),
                      K = factor(K), yield = yield)
    npk.aov <- aov(yield ~ block + N*P*K, npk)
    print(xtable(npk.aov), type = i, file = outFileName, append = TRUE)
    print(xtable(anova(npk.aov)), type = i, file = outFileName, append = TRUE)
    print(xtable(summary(npk.aov)), type = i, file = outFileName, append = TRUE)
    op <- options(contrasts = c("contr.helmert", "contr.treatment"))
    npk.aovE <- aov(yield ~  N*P*K + Error(block), npk)
    print(xtable(npk.aovE), type = i, file = outFileName, append = TRUE)
    options(op)
    print(xtable(summary(npk.aovE)),
          type = i, file = outFileName, append = TRUE)
    if(i=="latex") cat("\\\\clearpage\n", file = outFileName, append = TRUE)
    ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
    trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
    group <- gl(2,10,20, labels = c("Ctl","Trt"))
    weight <- c(ctl, trt)
    lm.D9 <- lm(weight ~ group)
    print(xtable(lm.D9),
          type = i, file = outFileName, append = TRUE, latex.environment = NULL)
    print(xtable(lm.D9),
          type = i, file = outFileName, append = TRUE, latex.environment = "")
    print(xtable(lm.D9),
          type = i, file = outFileName, append = TRUE,
          latex.environment = "center")
    print(xtable(anova(lm.D9)), type = i, file = outFileName, append = TRUE)
    counts <- c(18,17,15,20,10,20,25,13,12)
    outcome <- gl(3,1,9)
    treatment <- gl(3,3)
    d.AD <- data.frame(treatment, outcome, counts)
    glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
    print(xtable(glm.D93), type = i, file = outFileName, append = TRUE)
    print(xtable(anova(glm.D93, test = "Chisq")),
          type = i, file = outFileName, append = TRUE)
    print(xtable(glm.D93, align = "r|llrc"),
          include.rownames = FALSE, include.colnames = TRUE,
          type = i, file = outFileName, append = TRUE)
    print(xtable(glm.D93, align = "r||llrc"),
          include.rownames = TRUE, include.colnames = FALSE,
          type = i, file = outFileName, append = TRUE)
    print(xtable(glm.D93, align = "|r||llrc"),
          include.rownames = FALSE, include.colnames = FALSE,
          type = i, file = outFileName, append = TRUE)
    print(xtable(glm.D93, align = "|r||llrc|"),
          type = i, file = outFileName, append = TRUE)
    print(xtable(anova(glm.D93)),
          hline.after = c(1), size = "small",
          type = i, file = outFileName, append = TRUE)
    if(require(stats, quietly = TRUE)) {
      data(USArrests)
      pr1 <- prcomp(USArrests)
      print(xtable(pr1), type = i, file = outFileName, append = TRUE)
      print(xtable(summary(pr1)), type = i, file = outFileName, append = TRUE)
      # print(xtable(pr2), type = i, file = outFileName, append = TRUE)
    }
    temp.table <- xtable(ts(cumsum(1+round(rnorm(100), 2)),
                            start = c(1954, 7), frequency = 12))
    caption(temp.table) <- "Time series example"
    print(temp.table, type = i, file = outFileName,
          append = TRUE, caption.placement = "top", table.placement = "h")
    print(temp.table, type = i, file = outFileName,
          append = TRUE, caption.placement = "bottom", table.placement = "htb")
  }
