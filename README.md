# EPO_HR_Dashboard

#This is a Shiny dashboard containing visualizations of key HR statistics from the European Patent Office. 

#The data are publically available on the EPO website (https://www.epo.org/about-us/annual-reports-statistics/social-reports.html). They are to be found in 11 reports, published annualy by the EPO, containing key statistics about EPO staff. 
The reports are in PDF format and the data in question are in the form of tables in the PDF files. 
In order to extract the data, I used the pdftools R package, which converts PDFs to vectors. I then wrote a number of functions, catering to the various tables whose data I wanted to scrape. 

Following data scraping, cleaning and manipulation, I created 13 interactive visualizations using plotly and compiled everything into a simple Shiny dashboard
