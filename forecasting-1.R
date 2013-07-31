#---------------------------- filters -------------------------------#

# linear filter
lf1 = filter( x, filter, method=c("convolution", "recursive"),
				sides=2, circular=F, init)

# three-point simple moving average
# ie, if the points reprsent days, then this is a 3-day moving avg
# just take a given day, and the day before, and the day after, then average them (/3)

mv_avg = function(x) { 
	y = numeric(length(x)-2)
	for (i in 2:(length(x)-1)) {
		y[i] = (x[i-1] + x[i] + x[i+1])/3
	}
	y
}
# periods in current time series (xts)
nmonths(TSR)

# to roll-up hourly xts series:
# step 1 get endpoints
np_day = endpoints(TSR, on='days')
np_week = endpoints(TSR, on='weeks')
np_month = endpoints(TSR, on='months')


ts_split = function(x) { 
	as.Date(unlist(strsplit(as.character(x), split=' ', 
							fixed=TRUE))[1], 
							format="%Y-%m-%d", 
							origin="1970-01-01")[1] 
}


myacf = acf(trs_day,
		   type="correlation",
		   lag.max=70, 
		   drop.lag.0=T, 
		   plot=F 
)


#---------------------------- plots -------------------------------#

op = par(mar=c(3,3,1.5,1), mfrow=c(2,1), cex=0.8, 
		 col.lab="steelblue4", col="#D96A14")


x = myacf[['lag']]
y = myacf[['acf']]
y = y[1:length(x),1,1]

plot(x, y, 
	 ann=F, cex.axis=.8, xlab=F, 
	 ylab=F, main=NULL, col="#D96A14", 
	 type='h')


data(UKDriverDeaths)

ux = UKDriverDeaths[1:36]

my_acf = function(x, lag_max){
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

# this_lag: takes items off top, one by one
# full_ts: takes items off bottom, so lengths of the two args for 'cor' are equal
ux1 = diff(log(ux))
c1 = my_acf(ux)
c2 = my_acf(ux1)

r1 = acf(ux, plot=F)$acf[1:15]
r2 = acf(ux1, plot=F)$acf[1:15]

dfx = data.frame(Lag = c1[,1], bln=r1, mybln=c1[,2], difflg=r2, mydifflg=c2[,2])
dfx = round(dfx, 3)
print(dfx)


# print( c(length(c1[,2]), length(c2[,2]), length(r1), length(r2)) )


dev.new(res=128, units="inches", width=7, height=4)  # for two plots
op = par(mar=c(4,3,2,2), mfrow=c(2,1), col.axis="#1A4876", cex.axis=0.7)

plot.ts(any_ts, ann=F, axes=F, col="#D68A59", lwd=1.3, type="h");
box(lty="solid", lwd=1.6, col="grey60")

xt = seq(start(AP)[1], (end(AP)[1])+1, length.out=10);
yt = seq(100, 650, 50)

axis(1, at=xt, tick=T, lwd.ticks = 1.3, col.ticks = "maroon",
	 padj=-1.5, tcl=-.25);
axis(2, at=yt, tick=T, lwd.ticks = 1.3, col.ticks = "maroon",
	 las=1, tcl=-.25);
abline(h=yt, v=xt, col="grey80", lty="dotted", lwd=0.9)





