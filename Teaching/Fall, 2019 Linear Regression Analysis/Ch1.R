# Slide # 6 ---------------------------------------------------------------

setEPS()
postscript('Downloads/Reg/6.eps', width = 8*2, height = 8)
par(mfrow=c(1,2))
panel1 <- c(-1,-1) ; panel2 <- c(3,3)
plot(c(panel1[1],panel2[1]), c(panel1[2],panel2[2]), type = 'n',axes = F, xlab = "X=father's height", ylab = "Y=son's height")
arrows(panel1[1], 0, panel2[1], 0, length = .1);arrows(0, panel1[2], 0, panel2[2], length = .1)
lines(c(0,2),c(1,1),lty=2);lines(c(2,2),c(0,1),lty=2)
points(2, 1, pch = 19, cex=.5);text(2, -.25, 'x');text(-.25, 1, 'y');text(2,1.25,'(x,y)')
library(UsingR);data(father.son)
plot(father.son, xlab = "Father's Height (Inches)", main = "Galton's data",
     ylab = "Son's Height (Inches)", cex=.25)
dev.off()


# Slide # 7 ---------------------------------------------------------------

setEPS()
postscript('Downloads/Reg/7.eps', width = 8, height = 8)
par(mfrow=c(1,1))
hist(father.son$sheight, main = "Histogram of Son's height", xlab = '')
axis(side=1, at=68, labels=68)
dev.off()

# Slide # 8 ---------------------------------------------------------------

setEPS()
postscript('Downloads/Reg/8.eps', width = 16, height = 8)
par(mfrow=c(1,2))
fs.withg <- father.son[order(father.son$fheight),]
fs.withg <- cbind(fs.withg, g=factor(round(seq(1,13,length.out = nrow(fs.withg)))))
attach(fs.withg)
plot(fs.withg[,-3], xlab = "Father's Height (Inches)", ylab = "Son's Height (Inches)",
     main = "Plot for subgroups", col=g, cex=.5)
hist(fs.withg[g==10,"sheight"], main = "Histogram of Son's height with Father's height is about 72", xlab = '',
     breaks = 7)
axis(side=1, at=71, labels=71)
dev.off()

# Slide # 9 ---------------------------------------------------------------

setEPS()
postscript('Downloads/Reg/9.eps', width = 8, height = 8)
par(mfrow=c(1,1))
plot(fs.withg[,-3], xlab = "Father's Height (Inches)", ylab = "Son's Height (Inches)",
     main = "local means and regression line", col=g, cex=.5)
abline(lm(sheight~fheight))
for(i in 2:12){
  text(mean(fs.withg[g==i,"fheight"]),mean(fs.withg[g==i,"sheight"]),'m')
}
dev.off()