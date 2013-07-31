
my_acf = function(x, lag_max) {
	endx = length(x)
	mx = matrix(nrow=1, ncol=2)
	for (i in 1:lag_max){
		this_lag = x[(i+1):endx]
		full_ts = x[1:length(this_lag)]
		a = cor(full_ts, this_lag)
		mx = rbind(mx, c(as.integer(i),a))
	}
	mx = mx[2:dim(mx)[1],]
	mx
}


c1 = my_acf(ts_data, 25)

CG = data.frame(lag=c1[,1], r2 = c1[,2])

op = par(mar=c(4,3,2,2), col.axis="#1A4876", cex.axis=0.7)

plot(CG$lag, CG$r2, ann=F, axes=F, col="#D68A59", lwd=1.3, type="h")

xt = seq(CG$lag[1], CG$lag[nrow(CG)], 1)

axis(1, at=xt, tick=T, labels=T, lwd.ticks = 1.3, col.ticks = "maroon",
	 padj=-1.5, tcl=-.25, line=1)

mtext("lag (days)", side=1, line=3, cex=.9, col="navy")


