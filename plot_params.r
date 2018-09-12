library(gitBasedProjects)
sourceAllLibs('../rasterextrafuns/rasterPlotFunctions/R/')

paramFile = 'outputs/params'
datFile   = 'data/frpVariables'

mnth = 6

logistic <- function(x, x0, k) 
	1/(1+exp(-k * (x - x0)))
	
month_names = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
	
openParams <- function(mnth) {
	paramFile_mn = paste(paramFile, mnth-1, '.csv', sep = '')
	params = read.csv(paramFile_mn, stringsAsFactors = FALSE)
	nit =  nrow(params)/2
	start = ((nit/2)+1)
	index = c(start:nit, (nit + start):(2* nit))
	params = params[index,]
	return(params)
}


plot_mnth <- function(mnth, col, indep = 'tree_fraction', xmax = 100, x0 = 'wood_0', k = 'wood_k', ksc = -1) {
	
	paramFile_mn = paste(paramFile, mnth-1, '.csv', sep = '')
	datFile_mn   = paste(datFile, mnth-1, '.csv', sep = '')
	print(paramFile_mn)
	params = read.csv(paramFile_mn, stringsAsFactors = FALSE)
	dat    = read.csv(datFile_mn  , stringsAsFactors = FALSE)

	nit =  nrow(params)/2

	start = ((nit/2)+1)
	index = c(start:nit, (nit + start):(2* nit))
	params = params[index,]

	mask = dat[, 'ba_km'] > 0.0 & dat[,'sm'] > 0.0
	x = dat[,indep][mask]
	frp = dat[,'frp'][mask]

	it_sample = runif(50, 1, nit)
	x0 = params[, x0]
	k = params[, k]
	a = 0.05#params[, 'a']
	frp_e = exp(-a * frp)
	Dmax = params[, 'D_max']
	col = "black"
	
	
	plot_iteration <- function(i) {
		#frp_e = exp(-a[i] * frp)

		
		#points(x, frp_e , pch = 19, cex = 0.1, col = make.transparent(col, 0.95), ylim = c(0,1))
		
		xi = seq(0, xmax, length.out = 1000)
		yi = logistic(xi, x0[i], ksc * k[i]) #* Dmax[i]
		lines(xi, yi, lwd = 4, col = make.transparent(col, 0.95))
	}
	
	browser()
	plot(c(0, xmax), c(0, 1), type ='n', xaxt = 'n')
	points(x, frp_e , pch = 19, cex = 0.5, col = make.transparent(col, 0.95), ylim = c(0,1))
	mtext(month_names[mnth])
	lapply(it_sample, plot_iteration)
}

png("figs/month_stuff.png", height = 18, width = 16, unit = 'in', res = 300)

lmat = rbind(1:4, 5:8, 9:12)
lmat = rbind(lmat, 0, lmat + 12)
layout(lmat, heights = c(1,1,1, 0.3, 1, 1,1))

cols =  make_col_vector(c("blue", "cyan", "green", "brown", "red", "magenta", "blue"), ncol = 13)[-1]
plotControl <- function(...) 
	mapply(plot_mnth, 1:12, cols, MoreArgs = list(...))

mapply(plotControl, c('tree_fraction', 'sm'), c(100, 1), c('wood_0', 'moisture_0'), c('wood_k', 'moisture_k'), c(-1, 1))
dev.off()

