library(shiny)
library(shinychat)
library(bslib)
library(coro)
library(ellmer)
library(promises)

# survey questions with extraction schemas (in order) ----
questions <- list(
  list(
    id = "name",
    text = "What's your name?",
    schema = type_object(
      name = type_string("Just the person's name, e.g. 'Dylan' from 'call me Dylan'"),
      answered_clearly = type_boolean("TRUE if they provided any first and/or last name, 
                                      FALSE only if completely off-topic, only a nickname, or no name given")
    )
  ), 
  list(
    id = "ice_cream",
    text = "Hey {name}! Now let's talk ice cream. 🍦 I'm curious—what's your favorite flavor?",
    schema = type_object(
      ice_cream = type_string("The ice cream flavor, e.g. 'mint chocolate chip'"),
      answered_clearly = type_boolean("TRUE if they mentioned any flavor or description, 
                                      FALSE only if completely off-topic")
    )
  ),
  list(
    id = "brand_shop",
    text = "Where's your go-to spot to get it? 👀",
    fun_fact = TRUE,
    schema = type_object(
      brand_shop = type_string("Brand or shop name EXACTLY as stated by user, no inference. 
                               If vague like 'the store', extract that literally"),
      answered_clearly = type_boolean("TRUE if they named a specific brand/shop/location, 
                                      FALSE if too vague (like 'the store', 'somewhere') or off-topic")
    )
  ),
  list(
    id = "why_favorite",
    text = "Nice choice! What is it about {ice_cream} that makes it your favorite?",
    schema = type_object(
      why_favorite = type_string("Brief summary of why they like this flavor"),
      answered_clearly = type_boolean("TRUE if they gave ANY reason or preference (taste, texture, memory, etc.), 
                                      even if brief, FALSE only if completely off-topic or no reason given")
    )
  ),
  list(
    id = "adaptive_response",
    text = NULL,  # generated dynamically
    adaptive = TRUE,
    schema = type_object(
      adaptive_response = type_string("The answer to the question from the user's response to the adaptive question"),
      answered_clearly = type_boolean("TRUE if they provided ANY relevant answer to the question, 
                                      even if brief or simple, FALSE only if completely off-topic or nonsensical")
    )
  ),
  list(
    id = "when_eat",
    text = "Love it! When do you find yourself craving {ice_cream} the most? 🤔",
    schema = type_object(
      when_eat = type_string("Brief summary of when they eat ice cream"),
      answered_clearly = type_boolean("TRUE if they described any timing/occasion/situation, 
                                      FALSE only if completely off-topic")
    )
  ),
  list(
    id = "anything_else",
    text = "This has been fun! Any other ice cream secrets you want to share?",
    schema = type_object(
      anything_else = type_string("Any additional preferences or comments", required = FALSE),
      answered_clearly = type_boolean("TRUE if they shared something or declined (like 'no', 'nope'), 
                                      FALSE only if completely off-topic")
    )
  )
)

# setup claude chat ----
chat <- chat_claude(echo = "none")

# helper to get fresh context
fresh_chat <- function() {
  chat$clone()$set_turns(list())
}

# funfact schema ----
funfact_schema <- type_object(
  fact = type_string("Brief enthusiastic fun fact (1-2 sentences), no 'Fun fact:' prefix")
)

# async helpers ----
extract_response_async <- function(user_response, schema) {
  fresh_chat()$chat_structured_async(user_response, type = schema)
}

get_funfact_async <- function(flavor) {
  prompt <- sprintf("Share a fun fact about %s ice cream", flavor)
  fresh_chat()$chat_structured_async(prompt, type = funfact_schema) %...>%
    (\(result) result$fact)
}

generate_adaptive_question_async <- function(responses) {
  context <- sprintf(
    "User '%s' likes %s ice cream because: %s",
    responses$name,
    responses$ice_cream,
    responses$why_favorite
  )
  
  prompt <- paste(
    context,
    "Generate ONE curious, specific follow-up question about their ice cream preference based on what they said.",
    "Make it conversational and natural (like a friend asking).",
    "Examples: If they mentioned texture, ask about toppings. If they mentioned nostalgia, ask about memories.",
    "Return ONLY the question text with an emoji, no explanation or preamble.",
    sep = "\n"
  )
  
  fresh_chat()$chat_async(prompt)
}

personalize_text <- function(text, responses) {
  result <- text
  for (key in names(responses)) {
    # skip metadata fields
    if (key %in% c("adaptive_question_text", "adaptive_question_response", "answered_clearly")) next
    # skip raw input fields
    if (grepl("_raw$", key)) next
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

# ui ----
ui <- page_fillable(
  card(
    card_header("SurveyBot"),
    chat_ui(
      id = "chat",
      messages = "Hi! I'd love to learn about your ice cream preferences. 🍨"
    )
  )
)

# server ----
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    q_num = 1,
    responses = list(),
    initialized = FALSE,
    processing = FALSE,
    retry_count = 0  # track retries for current question
  )
  
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
    
    # store raw input for this question
    raw_field_name <- paste0(current_q$id, "_raw")
    rv$responses[[raw_field_name]] <- user_input
    
    # extract structured data using this question's schema
    extract_response_async(user_input, current_q$schema) %...>%
      (\(extracted_data) {
        field_name <- current_q$id
        answered_clearly <- extracted_data$answered_clearly
        
        # check if answer was clear
        if (!answered_clearly && rv$retry_count < 1) {
          # give one chance to clarify
          rv$retry_count <- rv$retry_count + 1
          chat_append("chat", bot_response("Sorry, I had trouble understanding that. 🤔 Could you try again?"))
          rv$processing <- FALSE
          return()
        }
        
        # reset retry count and store response
        rv$retry_count <- 0
        rv$responses[[field_name]] <- extracted_data[[field_name]]
        rv$responses[[paste0(field_name, "_answered_clearly")]] <- answered_clearly
        
        # for adaptive questions, also store the raw user input with alternate name
        if (!is.null(current_q$adaptive) && current_q$adaptive) {
          rv$responses$adaptive_question_response <- user_input
        }
        
        # move to next question
        rv$q_num <- rv$q_num + 1
        
        if (rv$q_num <= length(questions)) {
          next_q <- questions[[rv$q_num]]
          
          # handle adaptive questions
          if (!is.null(next_q$adaptive) && next_q$adaptive) {
            generate_adaptive_question_async(rv$responses) %...>%
              (\(adaptive_q) {
                # store the question that was asked
                rv$responses$adaptive_question_text <- adaptive_q
                chat_append("chat", bot_response(adaptive_q))
                rv$processing <- FALSE
              }) %...!%
              (\(err) {
                # fallback: skip to next question on error
                message("adaptive question generation failed, skipping...")
                rv$q_num <- rv$q_num + 1
                if (rv$q_num <= length(questions)) {
                  fallback_q <- questions[[rv$q_num]]
                  fallback_text <- personalize_text(fallback_q$text, rv$responses)
                  chat_append("chat", bot_response(fallback_text))
                }
                rv$processing <- FALSE
              })
          } else {
            next_text <- personalize_text(next_q$text, rv$responses)
            
            # add fun fact after ice cream question
            if (!is.null(next_q$fun_fact) && next_q$fun_fact) {
              ice_cream_flavor <- rv$responses$ice_cream
              
              if (is.null(ice_cream_flavor) || is.na(ice_cream_flavor)) {
                chat_append("chat", bot_response(next_text))
                rv$processing <- FALSE
              } else {
                get_funfact_async(ice_cream_flavor) %...>%
                  (\(fact) {
                    msg <- sprintf("Ooh, %s! %s\n\n%s", ice_cream_flavor, fact, next_text)
                    chat_append("chat", bot_response(msg))
                    rv$processing <- FALSE
                  }) %...!%
                  (\(err) {
                    chat_append("chat", bot_response(next_text))
                    rv$processing <- FALSE
                  })
              }
            } else {
              chat_append("chat", bot_response(next_text))
              rv$processing <- FALSE
            }
          }
        } else {
          # survey complete
          msg <- sprintf(
            "Thanks for sharing, %s! I recorded your love for %s! 🤓📊 
            I hope you enjoy your next scoop soon! 🍦✨",
            rv$responses$name %||% "friend",
            rv$responses$ice_cream %||% "ice cream"
          )
          chat_append("chat", bot_response(msg))
          
          # create summary dataframes
          survey_results <- list(
            extracted = tibble::tibble(
              question = c("name", "ice_cream", "brand_shop", "why_favorite", 
                           "adaptive_question", "adaptive_response", "when_eat", "anything_else"),
              value = c(
                as.character(rv$responses$name %||% NA),
                as.character(rv$responses$ice_cream %||% NA),
                as.character(rv$responses$brand_shop %||% NA),
                as.character(rv$responses$why_favorite %||% NA),
                as.character(rv$responses$adaptive_question_text %||% NA),
                as.character(rv$responses$adaptive_response %||% NA),
                as.character(rv$responses$when_eat %||% NA),
                as.character(rv$responses$anything_else %||% NA)
              ),
              answered_clearly = c(
                rv$responses$name_answered_clearly %||% NA,
                rv$responses$ice_cream_answered_clearly %||% NA,
                rv$responses$brand_shop_answered_clearly %||% NA,
                rv$responses$why_favorite_answered_clearly %||% NA,
                rv$responses$adaptive_question_text_answered_clearly %||% NA,
                rv$responses$adaptive_response_answered_clearly %||% NA,
                rv$responses$when_eat_answered_clearly %||% NA,
                rv$responses$anything_else_answered_clearly %||% NA
              )
            ),
            raw = tibble::tibble(
              question = c("name", "ice_cream", "brand_shop", "why_favorite", 
                           "adaptive_question", "adaptive_response", "when_eat", "anything_else"),
              raw_input = c(
                as.character(rv$responses$name_raw %||% NA),
                as.character(rv$responses$ice_cream_raw %||% NA),
                as.character(rv$responses$brand_shop_raw %||% NA),
                as.character(rv$responses$why_favorite_raw %||% NA),
                as.character(rv$responses$adaptive_question_text %||% NA),
                as.character(rv$responses$adaptive_question_response %||% NA),
                as.character(rv$responses$when_eat_raw %||% NA),
                as.character(rv$responses$anything_else_raw %||% NA)
              )
            )
          )
          
          print(survey_results)
          
          rv$processing <- FALSE
        }
      }) %...!%
      (\(err) {
        rv$processing <- FALSE
        chat_append("chat", bot_response("Sorry, I had trouble understanding that. 🤔 Could you try again?"))
      })
  })
}

# run ----
shinyApp(ui, server)