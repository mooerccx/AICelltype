GenerateComparisonTable <- function(CelltypeList, model="gpt-4.1-mini", api_key="", base_url=""){
  if(api_key==""){
    api_key=APP_API_KEY
  }
  if(base_url==""){
    base_url=APP_BASE_URL
  }
  
  # Preprocess input data
  input_cells <- strsplit(CelltypeList, '\n')[[1]]
  input_cells <- input_cells[input_cells != ""] # Remove blank lines
  input_count <- length(input_cells)
  
  
  # Build request message
  request_body <- list(
    model = model,
    messages = list(
      list(
        role    = "system",
        content = paste("You must return exactly", input_count, "cell type names, one per line. Do not include any explanations or additional text.")
      ),
      list(
        role    = "user",
        content = paste0("I am currently annotating cell types and need you to provide their corresponding broad categories. Please ensure your response follows these rules strictly:

You must return exactly the same number of lines as the input provided (", input_count, " lines).
Each line must contain only one broad category corresponding to the input.
Do not include any explanations, extra text, or formatting beyond the required output.
Here are some example mappings for reference:

activated cd4-positive, alpha-beta t cell → t/nk cell
strial intermediate cell → epithelial cell
plasmablast → B cell
cancer cell → malignant cell
Below are the cell types that need categorization. Please provide their corresponding broad categories:",
                        paste(input_cells, collapse="\n"))
      )
    ),
    temperature = 0.1  # Reduce temperature to achieve more consistent output
  )

  # Send a request and receive a response
  tryCatch({
    response <- request(base_url) %>%
      req_headers(
        "Content-Type" = "application/json",
        "Authorization" = paste("Bearer", api_key)
      ) %>%
      req_body_json(request_body) %>%
      req_perform()
    
    # Analyze response
    response_content <- response %>%
      resp_body_json()
    
    # Extract and process message content
    message_content <- response_content$choices[[1]]$message$content
    message_content <- gsub('[ ]{2,3}', '', message_content)
    message_content <- strsplit(message_content, '\n')[[1]]
    message_content <- message_content[message_content != ""] # Remove blank lines
    
    # Check if the output length matches
    if(length(message_content) != input_count) {
      warning(sprintf("Length mismatch: expected %d, got %d lines", 
                     input_count, length(message_content)))
      # If there is less output, use NA to make up for it
      if(length(message_content) < input_count) {
        message_content <- c(message_content, 
                           rep(NA, input_count - length(message_content)))
      }
      # If there are many outputs, truncate them
      if(length(message_content) > input_count) {
        message_content <- message_content[1:input_count]
      }
    }
    
    # Create result data box
    result <- data.frame(
      CLname = input_cells,
      broadtype = message_content,
      stringsAsFactors = FALSE
    )
    
    return(result$broadtype)  # Only return the broadtype list
    
  }, error = function(e) {
    warning(paste("Error in API call:", e$message))
    return(rep(NA, input_count))  # Return NA when an error occurs
  })
}
