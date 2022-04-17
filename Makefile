launch: 
	start http://127.0.0.1:6645
	Rscript -e "renv::restore()"
	Rscript -e "shiny::runApp('multilex-app', host = 'http://127.0.0.1', port = 6645)"
