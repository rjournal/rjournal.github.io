library(tools)
texi2pdf(file="C:/Users/Usuario/Desktop/RJtemplate/RJwrapper.tex", clean = F)
library(rjtools)

initial_check_article(path = here::here("C:/Users/Usuario/Desktop/RJtemplate"))

check_wrappers(path = here::here("C:/Users/Usuario/Desktop/RJtemplate"))
check_filenames(path = here::here("C:/Users/Usuario/Desktop/RJtemplate"))
check_unnecessary_files(path = here::here("C:/Users/Usuario/Desktop/RJtemplate")) 
check_title(path = here::here("C:/Users/Usuario/Desktop/RJtemplate")) 
check_section(path = here::here("C:/Users/Usuario/Desktop/RJtemplate")) 
check_spelling(path = here::here("C:/Users/Usuario/Desktop/RJtemplate")) 
