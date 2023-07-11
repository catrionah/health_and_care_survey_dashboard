###############################################
#
# Define server for the Shiny app
#
##############################################


function(input, output, session) {
  
  # Source files with server code for each tab -----------------------------------------
  source(file.path("summary_server.R"),  local = TRUE)$value # Summary tab
  source(file.path("pos_neg_server.R"),  local = TRUE)$value # Indicator definitions tab
}


## END 