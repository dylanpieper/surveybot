library(shiny)
library(shinychat)
library(bslib)
library(coro)
library(udpipe)
library(dplyr)
library(googlesheets4)
library(wordcloud)
library(RColorBrewer)

# Service account authentication for deployment
gs4_auth(path = "gen-lang-client-0181100734-a34fde113722.json")

# survey questions (in order) ----
questions <- list(
  list(
    id = "name",
    text = "What's your name?"
  ), 
  list(
    id = "ice_cream",
    text = "Hey {name}! Now let's talk ice cream. 🍦 I'm curious—what's your favorite flavor?"
  ),
  list(
    id = "brand_shop",
    text = "Ooh, {ice_cream} sounds delicious! Where's your go-to spot to get it? 👀"
  ),
  list(
    id = "why_favorite",
    text = "Nice choice! What is it about {ice_cream} that makes it your favorite?"
  ),
  list(
    id = "when_eat",
    text = "Love it! When do you find yourself craving {ice_cream} the most? 🤔"
  ),
  list(
    id = "anything_else",
    text = "This has been fun! Any other ice cream secrets you want to share?"
  )
)

# Google Sheets setup ----
SHEET_ID <- "1bOW8rtww5aIBZlD93OWyeTlTNBrJbWT1bE7MpATlt5Y"

# load udpipe model (assumes model file exists)
model <- NULL

# initialize model function
init_udpipe_model <- function() {
  if (is.null(model)) {
    model <<- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")
  }
  return(model)
}

# simple text processing helpers ----
extract_name <- function(text) {
  if (is.null(model)) {
    model <<- init_udpipe_model()
  }
  
  parsed <- udpipe_annotate(model, text) |>
    as.data.frame()
  
  names <- parsed |>
    filter(upos == "PROPN") |>
    pull(token)
  
  if (length(names) > 0) {
    return(names[1])
  } else {
    return("")
  }
}

extract_ice_cream_flavor <- function(text) {
  if (is.null(model)) {
    model <<- init_udpipe_model()
  }
  
  parsed <- udpipe_annotate(model, text) |>
    as.data.frame()
  
  # keep conjunctions when they connect flavor words
  flavor_tokens <- parsed |>
    mutate(
      is_flavor_word = upos %in% c("NOUN", "ADJ", "PROPN") &
        !tolower(token) %in% c("ice", "flavor", "favourite", "favorite", 
                               "like", "love", "prefer", "good", "delicious", 
                               "yum", "yummy"),
      is_connector = upos == "CCONJ" | tolower(token) == "cream"
    ) |>
    # keep sequence of flavor words and connectors
    mutate(keep = is_flavor_word | (is_connector & 
                                      lag(is_flavor_word, default = FALSE) & 
                                      lead(is_flavor_word, default = FALSE)))
  
  kept_tokens <- flavor_tokens |>
    filter(keep) |>
    pull(token)
  
  if (length(kept_tokens) > 0) {
    # take the first continuous sequence (stop at punctuation or long gaps)
    flavor <- paste(kept_tokens[1:min(5, length(kept_tokens))], collapse = " ")
    return(tolower(flavor))
  } else {
    return("")
  }
}

is_response_clear <- function(text, question_type = "general") {
  text <- trimws(text)
  if (nchar(text) == 0) return(FALSE)
  
  if (question_type == "name") {
    extracted <- extract_name(text)
    return(nchar(extracted) > 0)
  }
  
  if (question_type == "ice_cream") {
    extracted <- extract_ice_cream_flavor(text)
    return(nchar(extracted) > 0)
  }
  
  return(TRUE)
}

personalize_text <- function(text, responses) {
  result <- text
  for (key in names(responses)) {
    if (key %in% c("answered_clearly") || grepl("_raw$", key)) next
    pattern <- sprintf("\\{%s\\}", key)
    result <- gsub(pattern, responses[[key]], result)
  }
  result
}

bot_response <- async_generator(function(message) {
  await(async_sleep(0.3))
  chars <- strsplit(as.character(message), "", useBytes = FALSE)[[1]]
  for (char in chars) {
    yield(char)
    await(async_sleep(0.02))
  }
})

# Google Sheets functions ----
write_to_sheets <- function(responses) {
  tryCatch({
    # Create extracted data frame
    extracted_data <- data.frame(
      timestamp = Sys.time(),
      name = as.character(responses$name %||% NA),
      ice_cream = as.character(responses$ice_cream %||% NA),
      brand_shop = as.character(responses$brand_shop %||% NA),
      why_favorite = as.character(responses$why_favorite %||% NA),
      when_eat = as.character(responses$when_eat %||% NA),
      anything_else = as.character(responses$anything_else %||% NA),
      stringsAsFactors = FALSE
    )
    
    # Create raw data frame
    raw_data <- data.frame(
      timestamp = Sys.time(),
      name_raw = as.character(responses$name_raw %||% NA),
      ice_cream_raw = as.character(responses$ice_cream_raw %||% NA),
      brand_shop_raw = as.character(responses$brand_shop_raw %||% NA),
      why_favorite_raw = as.character(responses$why_favorite_raw %||% NA),
      when_eat_raw = as.character(responses$when_eat_raw %||% NA),
      anything_else_raw = as.character(responses$anything_else_raw %||% NA),
      stringsAsFactors = FALSE
    )
    
    # Append to both sheets
    sheet_append(SHEET_ID, extracted_data, sheet = "extracted")
    
    # Try raw sheet, but don't fail if it doesn't work
    tryCatch({
      sheet_append(SHEET_ID, raw_data, sheet = "raw")
    }, error = function(e) {
      warning("Could not write to raw sheet: ", e$message)
    })
    
  }, error = function(e) {
    warning("Failed to write to Google Sheets: ", e$message)
  })
}

read_from_sheets <- function() {
  tryCatch({
    # Read extracted data from Google Sheets (used for wordclouds)
    data <- read_sheet(SHEET_ID, sheet = "extracted")
    return(data)
  }, error = function(e) {
    warning("Failed to read from Google Sheets: ", e$message)
    return(data.frame())
  })
}

# Wordcloud generation functions ----
create_wordcloud <- function(text_data) {
  # Check if we have valid data
  if (length(text_data) == 0 || all(is.na(text_data)) || all(text_data == "")) {
    par(mar = c(1, 1, 1, 1))
    plot.new()
    text(0.5, 0.5, "No data yet!\nComplete a survey to see results.", 
         cex = 1.2, col = "gray60", adj = c(0.5, 0.5))
    return(invisible(NULL))
  }
  
  # Clean and prepare text
  text_clean <- text_data[!is.na(text_data) & text_data != "" & nchar(text_data) > 0]
  if (length(text_clean) == 0) {
    par(mar = c(1, 1, 1, 1))
    plot.new()
    text(0.5, 0.5, "No valid text data", 
         cex = 1.2, col = "gray60", adj = c(0.5, 0.5))
    return(invisible(NULL))
  }
  
  # Process each response separately to get proper word frequencies
  all_words <- c()
  
  for(response in text_clean) {
    # Clean each response
    clean_response <- tolower(response)
    clean_response <- gsub("[^a-zA-Z ]", "", clean_response)  # Remove punctuation/numbers but keep spaces
    clean_response <- gsub("\\s+", " ", clean_response)  # Clean multiple whitespace to single space
    clean_response <- trimws(clean_response)
    
    # Split into words
    words <- unlist(strsplit(clean_response, " "))
    words <- words[nchar(words) > 1]  # Remove very short words
    words <- words[words != ""]  # Remove empty strings
    
    # Add to all words
    all_words <- c(all_words, words)
  }
  
  # Remove common stopwords manually
  stopwords_custom <- c("the", "and", "for", "are", "but", "not", "you", "all", "can", 
                       "had", "her", "was", "one", "our", "out", "day", "get", "has", 
                       "him", "his", "how", "its", "may", "new", "now", "old", "see",
                       "two", "who", "boy", "did", "she", "use", "way", "will", "with",
                       "ice", "cream", "like", "love", "really", "just", "very", "good",
                       "its", "that", "this", "have", "from", "they", "been", "were",
                       "said", "each", "which", "their", "time", "what", "when", "where")
  
  words_list <- all_words[!all_words %in% stopwords_custom]
  words_list <- words_list[words_list != "" & !is.na(words_list)]
  
  if (length(words_list) == 0) {
    par(mar = c(1, 1, 1, 1))
    plot.new()
    text(0.5, 0.5, "No meaningful words found", 
         cex = 1.2, col = "gray60", adj = c(0.5, 0.5))
    return(invisible(NULL))
  }
  
  # Count word frequencies
  word_freq <- table(words_list)
  word_freq <- sort(word_freq, decreasing = TRUE)
  
  # Limit to reasonable number of words for wordcloud
  top_words <- head(word_freq, min(50, length(word_freq)))
  
  # Create wordcloud with error handling
  tryCatch({
    par(mar = c(0, 0, 0, 0))
    
    # Custom color palette
    colors <- brewer.pal(8, "Dark2")
    
    wordcloud(words = names(top_words), 
              freq = as.numeric(top_words),
              min.freq = 1,
              max.words = 50,
              random.order = FALSE,
              rot.per = 0.3,
              colors = colors,
              scale = c(3, 0.5),
              family = "sans")
              
  }, error = function(e) {
    # Fallback if wordcloud fails
    par(mar = c(1, 1, 1, 1))
    plot.new()
    text(0.5, 0.5, "Wordcloud generation failed\nTry refreshing data", 
         cex = 1, col = "gray60", adj = c(0.5, 0.5))
  })
}

# UI ----
ui <- page_navbar(
  title = "Ice Cream Survey",
  nav_panel(
    title = "Survey",
    page_fillable(
      card(
        card_header("SurveyBot"),
        chat_ui(
          id = "chat",
          messages = "Hi! I'd love to learn about your ice cream preferences. 🍨"
        )
      )
    )
  ),
  nav_panel(
    title = "Results",
    fluidPage(
      div(
        style = "text-align: center; margin: 20px;",
        actionButton("refresh_data", "Refresh Visualizations", 
                     class = "btn-primary")
      ),
      h3("🍦 Favorite Flavors", style = "text-align: center;"),
      plotOutput("flavor_frequency", height = "400px"),
      hr(),
      h3("🏪 Favorite Shops/Brands", style = "text-align: center;"),
      plotOutput("shop_frequency", height = "400px"),
      hr(),
      h3("💭 Why It's Their Favorite", style = "text-align: center;"),
      plotOutput("why_frequency", height = "400px"),
      hr(),
      h3("⏰ When They Crave It", style = "text-align: center;"),
      plotOutput("when_frequency", height = "400px"),
      hr(),
      h3("💬 Additional Comments", style = "text-align: center;"),
      plotOutput("comments_frequency", height = "400px")
    )
  )
)

# Server ----
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    q_num = 1,
    responses = list(),
    initialized = FALSE,
    processing = FALSE,
    retry_count = 0,
    survey_data = data.frame()
  )
  
  # Load data on startup
  observe({
    rv$survey_data <- read_from_sheets()
  })
  
  # Refresh data when button is clicked
  observeEvent(input$refresh_data, {
    rv$survey_data <- read_from_sheets()
    showNotification("Data refreshed!", type = "message")
  })
  
  # send first question ----
  observe({
    if (!rv$initialized) {
      rv$initialized <- TRUE
      Sys.sleep(0.5)
      chat_append("chat", bot_response(questions[[1]]$text))
    }
  })
  
  # handle user responses ----
  observeEvent(input$chat_user_input, {
    if (rv$processing) return()
    rv$processing <- TRUE
    
    user_input <- input$chat_user_input
    current_q <- questions[[rv$q_num]]
    
    raw_field_name <- paste0(current_q$id, "_raw")
    rv$responses[[raw_field_name]] <- user_input
    
    extracted_value <- switch(current_q$id,
      "name" = extract_name(user_input),
      "ice_cream" = extract_ice_cream_flavor(user_input),
      user_input
    )
    
    answered_clearly <- is_response_clear(user_input, current_q$id)
    
    if (!answered_clearly && rv$retry_count < 1) {
      rv$retry_count <- rv$retry_count + 1
      chat_append("chat", bot_response("Sorry, I had trouble understanding that. 🤔 Could you try again?"))
      rv$processing <- FALSE
      return()
    }
    
    rv$retry_count <- 0
    rv$responses[[current_q$id]] <- extracted_value
    rv$responses[[paste0(current_q$id, "_answered_clearly")]] <- answered_clearly
    
    rv$q_num <- rv$q_num + 1
    
    if (rv$q_num <= length(questions)) {
      next_q <- questions[[rv$q_num]]
      next_text <- personalize_text(next_q$text, rv$responses)
      chat_append("chat", bot_response(next_text))
      rv$processing <- FALSE
    } else {
      # Survey complete - save to Google Sheets
      write_to_sheets(rv$responses)
      
      msg <- sprintf(
        "Thanks for sharing, %s! I recorded your love for %s! 🤓📊 
        See the 'Results' tab to see how your responses compare with others! 🍦✨",
        rv$responses$name %||% "friend",
        rv$responses$ice_cream %||% "ice cream"
      )
      chat_append("chat", bot_response(msg))
      
      # Refresh data to include new response
      rv$survey_data <- read_from_sheets()
      
      rv$processing <- FALSE
    }
  })
  
  # Frequency plot outputs ----
  output$flavor_frequency <- renderPlot({
    if (nrow(rv$survey_data) > 0 && "ice_cream" %in% names(rv$survey_data)) {
      create_wordcloud(rv$survey_data$ice_cream)
    } else {
      create_wordcloud(NULL)
    }
  })
  
  output$shop_frequency <- renderPlot({
    if (nrow(rv$survey_data) > 0 && "brand_shop" %in% names(rv$survey_data)) {
      create_wordcloud(rv$survey_data$brand_shop)
    } else {
      create_wordcloud(NULL)
    }
  })
  
  output$why_frequency <- renderPlot({
    if (nrow(rv$survey_data) > 0 && "why_favorite" %in% names(rv$survey_data)) {
      create_wordcloud(rv$survey_data$why_favorite)
    } else {
      create_wordcloud(NULL)
    }
  })
  
  output$when_frequency <- renderPlot({
    if (nrow(rv$survey_data) > 0 && "when_eat" %in% names(rv$survey_data)) {
      create_wordcloud(rv$survey_data$when_eat)
    } else {
      create_wordcloud(NULL)
    }
  })
  
  output$comments_frequency <- renderPlot({
    if (nrow(rv$survey_data) > 0 && "anything_else" %in% names(rv$survey_data)) {
      create_wordcloud(rv$survey_data$anything_else)
    } else {
      create_wordcloud(NULL)
    }
  })
}

# Run ----
shinyApp(ui, server)