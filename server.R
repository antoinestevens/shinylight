shinyServer(function(input, output,session) {

	# source base functions
	source('shinyspec.R', local = TRUE)

	# source tools
	flist <- sourceDirectory('tools', recursive = TRUE)
	# print(flist)
})