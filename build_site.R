#Set our working directory. 
#This helps avoid confusion if our working directory is 
#not our site because of other projects we were 
#working on at the time. 

# actually remove the files
rmarkdown::clean_site()
#render your sweet site. 
rmarkdown::render_site()

