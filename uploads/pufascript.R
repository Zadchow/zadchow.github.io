pufaMETA<-read.csv("PUFA.csv", header=TRUE)
pufaMETA
library(metafor)

## Meta-Analysis
dat <- escalc(measure="RR", ai=P_Events, n1i=P_Total,
              ci=C_Events, n2i=C_Total, data=pufaMETA)
dat
res <- rma(yi, vi, data=dat)
confint(res)

par(mar=c(4,4,1,2))

res <- rma(ai=P_Events, n1i=P_Total, ci=C_Events, 
           n2i=C_Total, data=dat, measure="RR",
           slab=paste(Study, sep=", "), method="DL")

## Data Visualization - Forest Plot Structure

forest(res, xlim=c(-16, 6), at=log(c(0.05, 0.25, 1, 2)), atransf=exp,
       ilab=cbind(dat$P_Events, dat$P_Total, dat$C_Events, dat$C_Total),
       ilab.xpos=c(-9.5,-8,-6,-4.5), cex=0.85, ylim=c(-1, 8.6),
       xlab="Risk Ratio", mlab="", psize=1.7, pch=16)

## Heterogeneity 

text(-16, -1, pos=4, cex=0.85, bquote(paste("Random Effects (Q = ",
                                            .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                            ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res$I2, digits=1, format="f")), "%)")))

op <- par(cex=0.85, font=4)
### Bold Font
par(font=2)

### Columns
text(c(-9.5,-8,-6,-4.5), 7.5, c("Events ", " Total", "Events ", " Total"))
text(c(-8.75,-5.25),     8.5, c("PUFA Diet", "Control Diet"))
text(-16,                7.5, "Study Name",  pos=4)
text(6,                  7.5, "Risk Ratio [95% CI]", pos=2)
