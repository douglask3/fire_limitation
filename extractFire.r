library(raster)

dir = 'data/data_libfire/'
outFile = 'data/frpVars.csv'

files = list.files(dir, full.names = TRUE)
files = files[grepl('.nc', files)]	

whichFrp = grepl('frp', files)

frp = brick(files[whichFrp])

vars = lapply(files, brick)

temporal = sapply(vars, nlayers) == nlayers(frp)
varT = vars[ temporal]
varS = vars[!temporal]
varS = lapply(varS, function(i) {extent(i) == extent(frp[[1]]); i})

listFRPevents <- function(i) {
	mask = frp[[i]] > 0
	vVarT = sapply(varT, function(j) j[[i]][mask])
	vVarS = sapply(varS, function(j) j[[1]][mask[]])
	out = cbind(vVarT, vVarS)
	out =  out[!apply(is.na(out), 1, any),]
	return(out)
}

dat = lapply(1:nlayers(frp), listFRPevents)	#

out = c()
for ( i in dat) out = rbind(out, i)

cnames = unlist(strsplit(sapply(files, function(i) strsplit(i, dir)[[1]][2]), '.nc'))
cnames = c(cnames[temporal], cnames[!temporal])
colnames(out) = cnames

write.csv(out, outFile, row.names = FALSE)
