summary.samplefwl <-
function(x)
{
	cat("\nMeans: \n")
	print(round(sapply(x, mean), 4))

	cat("\nStandard deviations: \n")
	print(round(sapply(x, mean), 4))

	cat("\nFirst five modes: \n")
	print(round(x[1:5, ],4))
}
