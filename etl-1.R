
require(xts)

fname = "~/dougybarbo/Desktop/okl_rev_ts.csv"
V = read.csv(fname, header=T, sep=";")

q = strsplit(a, "-", fixed=F, perl=T)
q = unlist(q)
yr = q[1]
mn = q[2]
dy = q[3]
hr = q[4]

fnx = function(x) {
	x = unlist(strsplit(as.character(x), "-", fixed=F, perl=T));
	yr = x[1];
	mn = x[2];
	dy = x[3];
	hr = x[4];
	day = paste(yr, mn, dy, sep='-');
	timeOfDay = paste(hr, ":00", sep="");
	datetime = paste(day, timeOfDay, sep=" ")
	as.numeric(as.POSIXct(datetime))}

#start
st = "2011-03-01 00:00"
#end
nd = "2013-04-30 23:00"

ndx = sapply(V$date_hour, fnx)
attr(ndx, "names") = NULL
ndx = structure(ndx, class=c("POSIXct", "POSIXt"))

# convert to xts time series 
X = xts(V$revenue, order.by=ndx)

# grid the time series
fnx1 = function(x){structure(x, class=c("POSIXct", "POSIXt"))}
st = as.numeric(ndx[1])
nd = as.numeric(ndx[nrow(V)])
mt = seq(from=st, to=nd, by=60*60)
mt = fnx1(mt)

mt = zoo(,mt)
XG = merge(X, mt, all=T)

# handle NA vals (arise during gridding, or merge w/ zero length series) by interpolation
XG = na.approx(XG)

names(XG) = "revenue"

# create endpoints to roll-up series
np_day = endpoints(XG, on='days')
np_week = endpoints(XG, on='weeks')
np_month = endpoints(XG, on='months')

# note: 'XG' is XG_hour
XG_day = period.apply(x=XG, INDEX=np_day, FUN=sum, na.rm=T)
XG_week = period.apply(x=XG, INDEX=np_week, FUN=sum, na.rm=T)
XG_month = period.apply(x=XG, INDEX=np_month, FUN=sum, na.rm=T)

# for each of these convert index to remove the time portion
XG_day = xts( coredata(XG_day), order.by=as.Date(index(XG_day)) )
#--------------- plot -----------------#

DG_week = data.frame(date=index(XG_week), revenue=coredata(XG_week))

px = ggplot(DG_week, aes(date, revenue)) + geom_line(col="#326178") +
	scale_x_date(labels=date_format("%d-%b-%y"), breaks=date_breaks(width="1 month")) +
	theme(axis.text.x=element_text(angle=30, hjust=1)) +
	theme(axis.title.x=element_blank()) +
	scale_y_continuous(labels=comma)
	







