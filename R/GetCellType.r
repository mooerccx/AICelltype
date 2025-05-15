library(httr2)
library(jsonlite)
library(dplyr)

#' Enter the markergenes list to obtain the corresponding cell type
#' @param markergenes: A list of genes in string form, separated by rows, separated by genes within the same cluster
#' @param model      : The model name you want to use
#' @param tissuename : Cell derived tissue
#' @param api_key    : Key for API interface
#' @param base_url   : API interface address
#' @return The List of cell types
#' @export
url = ""
GetCellType <- function(markergenes, model, tissuename, api_key="", base_url="",temperature=0.1){
  if(api_key==""){
    api_key=APP_API_KEY
  }
  if(base_url==""){
    base_url=APP_BASE_URL
  }
  print(model)
  # Build request message
  request_body <- list(
    model = model,
    messages = list(
      list(
        role    = "system",
        content = paste(
      "You are an expert cell type annotation assistant.",
      "Your task is to identify cell types based on provided marker genes for a specific tissue.",
      "You MUST return the response ONLY as a valid JSON object.",
      "The JSON object should have sequential integers starting from 1 as keys, corresponding to the input marker rows.",
      "The value for each key MUST be the identified English cell type name ONLY. or 'unknown' if uncertain.",
      "The number of key-value pairs in the JSON MUST exactly match the number of input marker rows provided in the user prompt.",
      "Do NOT include ANY markdown formatting (like ```json ... ```), introductory text, concluding remarks, explanations, apologies, or any other text outside the JSON object itself.",
      sep = " "
    )
  ),
    list(
      role    = "user",
      content = paste0(
        'Identify cell types for ', tissuename, ' tissue based on the following marker genes. Each row represents a distinct cell population:\n\n',
        markergenes, '\n\n',
        'Provide the output strictly as a JSON object as described in the system prompt.' 
      )
    )
    ),
  temperature = temperature
   )

  # Send a request and receive a response
  tryCatch({
    response <- request(base_url) %>%
      req_headers(
        "Content-Type" = "application/json",
        "Authorization" = paste("Bearer", api_key)
      ) %>%
      req_timeout(seconds = 200) %>%
      req_body_json(request_body) %>%
      req_perform()
  },error = function(e){
    print(paste("httr2 API call error:", e$message, "\n"))
    cat(paste("httr2 API call error:", e$message, "\n"), file=log.file, append=TRUE)
    return('')

  })
  # Analyze response
  response_content <- response %>%
    resp_body_json()
  print(response_content)
  message_content <- c()
  cat(format(Sys.time(),  "%Y-%m-%d %H:%M:%S"))
  cat(paste(paste0("模型：",model), paste0("marker基因：", markergenes), paste0("返回信息：", response_content$choices[[1]]$message$content),sep="\n") )
  tryCatch({
     tmp <- response_content$choices[[1]]$message$content
     tmp <- gsub('```'         , '', tmp)
     tmp <- gsub('[\t|\n]{1,2}', '', tmp)
     tmp <- gsub('^.*json'     , '', tmp)
     tmp <- gsub("^.*</think>" , '', tmp)
     message_content$content <- fromJSON(tmp)
  },error = function(e){
      content <- response_content$choices[[1]]$message$content
      content <- gsub("<think>.*</think>"       , ""  , content, perl = TRUE)
      content <- gsub('^.*.?:'                  , ''  , content)
      content <- sub('[\n]{2}[\\*]{1,2}'        , ''  , content)
      content <- gsub("[0-9]{1,2}[.]{1}[ ]{0,1}", ""  , content) 
      content <- gsub('[ ]{2,8}'                , ''  , content)
      content <- gsub('[\n*]{2,9}'              , '\n', content)
      content <- gsub('|'                       , ' ' , content)
      content <- gsub('\t'                      , ' ' , content)
      content <- gsub('^\n'                     , ''  , content)
      content <- strsplit(content, '\n')
      content <- as.list(content)
      names(content) <- seq(1:length(content))
      message_content$content <- content
  })
  message_content$tokens  <- list(
                                prompt_tokens     = response_content$usage$prompt_tokens,
                                completion_tokens = response_content$usage$completion_tokens,
                                total_tokens      = response_content$usage$total_tokens
                                
                              )
  cat("The parsed information:",  paste(message_content, collapse=", "))
  cat("\n--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n")
  return(message_content)
}