## ---- echo = FALSE-------------------------------------------------------
library(knitr)
kable(data.frame(row = seq(9), 
                 time = as.Date("2017-01-02") + seq(9),
                 target = c(16443, 3013, 1643, rep(NA, 6)),
                 temp = c(72, 72, 68, rep(NA, 6))))

## ---- echo = FALSE-------------------------------------------------------
library(knitr)
kable(data.frame(row = seq(9), 
                 time = as.Date("2017-01-02") + seq(9),
                 target = c(16443, 3013, 1643, rep(NA, 6)),
                 holiday = c(TRUE, rep(FALSE, 5), TRUE, rep(FALSE, 2))))

