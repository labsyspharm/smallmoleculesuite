function(input, output, session) {
  selectivity <- callModule(
    selectivityServer, "select"
  )  
  
  onBookmarked(function(url) {
    
  })
}
