library(shiny)
library(shinychat)
library(bslib)
library(ellmer)
library(DBI)
library(RSQLite)
library(promises)

# Load functions module
fns <- modules::use("functions.R")

# Messages ----
messages <- list(
  welcome = "Hello! I'd love to learn about your ice cream preferences. 🍨",
  retry = "Sorry, I had trouble understanding that. 🤔 Could you try again?",
  completion = "Thanks, %s! I recorded your love for %s! 🤓📊
                I hope you enjoy your next scoop soon! 🍦✨"
)

# Content Templates ----
content_templates <- list(
  funfact = list(
    prompt = "Share a fun fact about %s ice cream",
    schema = type_object(
      fact = type_string("Brief fun fact (1-2 sentences) with no 'Fun fact:' prefix; include a relevant emoji")
    ),
    response_field = "fact",
    context_fields = list("ice_cream"),
    intro_template = "Oh, %s! %s\n\n%s",
    intro_fields = list("ice_cream", "generated_content", "next_question")
  ),
  follow_up = list(
    prompt = "User '%s' likes %s ice cream because: %s. Acknowledge their reason briefly. Then, generate one curious follow-up question about their ice cream preference based on what they said. Examples: If they mentioned texture, ask about toppings. If they mentioned nostalgia, ask about memories. Return ONLY the question text with no preamble.",
    schema = type_object(
      question = type_string("Just the question text")
    ),
    response_field = "question",
    context_fields = list("name", "ice_cream", "why_favorite")
  ),
  recommendation = list(
    prompt = "Suggest a %s pairing for someone who likes %s ice cream",
    schema = type_object(
      suggestion = type_string("Brief suggestion with emoji")
    ),
    response_field = "suggestion",
    context_fields = list("food_type", "ice_cream"),
    intro_template = "Here's a great pairing idea: %s\n\n%s",
    intro_fields = list("generated_content", "next_question")
  )
)

# Survey questions with extraction schemas ----
questions <- list(
  list(
    id = "name",
    text = "What's your name?",
    schema = type_object(
      name = type_string("Just the person's name, e.g. 'Dylan' from 'call me dylan'"),
      answered_clearly = type_boolean("TRUE if they provided any first and/or last name,
                                      FALSE only if completely off-topic, only a nickname, or no name given")
    )
  ),
  list(
    id = "ice_cream",
    text = "Hey {name}! Let's talk ice cream. 🍦 What's your favorite flavor?",
    schema = type_object(
      ice_cream = type_string("The ice cream flavor, e.g. 'mint chocolate chip'"),
      answered_clearly = type_boolean("TRUE if they mentioned any flavor,
                                      FALSE only if completely off-topic")
    )
  ),
  list(
    id = "why_favorite",
    text = "What about {ice_cream} makes it your favorite ice cream flavor?",
    content_generation = "funfact",
    schema = type_object(
      why_favorite = type_string("The reason why they like this flavor"),
      answered_clearly = type_boolean("TRUE if they mentioned ANY positive feeling, emotion, memory, or reason related to the flavor, even if very brief like 'I love it' or 'It reminds me of something'.
                                      FALSE only if completely off-topic, rude, or makes no sense")
    )
  ),
  list(
    id = "fu_favorite",
    text = NULL,
    content_generation = "follow_up",
    schema = type_object(
      fu_favorite = type_string("The core answer to the adaptive question"),
      answered_clearly = type_boolean("TRUE if they provided ANY answer that shows engagement with the question,
                                      even if brief, somewhat vague, or unconventional.
                                      FALSE only if completely off-topic or rude")
    )
  ),
  list(
    id = "brand_shop",
    text = "Love it! Where's your go-to spot to get {ice_cream} ice cream? 👀",
    schema = type_object(
      brand_shop = type_string("Brand or shop name EXACTLY as stated by user, no inference.
                               If vague like 'the store', extract that literally"),
      answered_clearly = type_boolean("TRUE if they named a specific brand/shop/location,
                                      FALSE if too vague (like 'the store', 'somewhere') or off-topic")
    )
  ),
  list(
    id = "when_eat",
    text = "{brand_shop} is a great choice! When do you crave {ice_cream} the most? 🤔",
    schema = type_object(
      when_eat = type_string("Brief summary of when they eat ice cream"),
      answered_clearly = type_boolean("TRUE if they described any timing, occasion, situation, or context for eating ice cream, even if somehwat vague or unconventional.
                                      FALSE only if completely off-topic or rude")
    )
  )
)

# Setup chat ----
chat <- chat_claude(echo = "none")

# UI ----
ui <- page_fillable(
  card(
    card_header("SurveyBot"),
    chat_ui(
      id = "chat",
      messages = messages$welcome
    )
  )
)

# Server ----
server <- function(input, output, session) {
  # Database connection
  db_path <- "survey.db"
  if (!file.exists(db_path)) {
    fns$init_database(db_path)
  }

  con <- dbConnect(SQLite(), db_path)

  onStop(\() {
    dbDisconnect(con)
  })

  # Reactive values
  rv <- reactiveValues(
    q_num = 1,
    responses = list(),
    initialized = FALSE,
    processing = FALSE,
    retry_count = 0,
    session_id = NULL
  )

  # Send first question and initialize session
  observe({
    if (!rv$initialized) {
      rv$initialized <- TRUE

      # Start database session
      rv$session_id <- fns$start_session(con, version = "1.0")

      chat_append("chat", fns$bot_response(questions[[1]]$text))
    }
  })

  # Handle user responses
  observeEvent(input$chat_user_input, {
    if (rv$processing) {
      return()
    }
    rv$processing <- TRUE

    user_input <- input$chat_user_input
    current_q <- questions[[rv$q_num]]

    # Store raw input in reactive values
    raw_field_name <- paste0(current_q$id, "_raw")
    rv$responses[[raw_field_name]] <- user_input

    # Extract structured data synchronously (faster)
    extracted_data <- fns$extract_response(chat, user_input, current_q$schema)
    field_name <- current_q$id
    answered_clearly <- extracted_data$answered_clearly

    # Determine question text for database (handle content generation)
    question_text <- if (!is.null(current_q$content_generation) && current_q$content_generation == "follow_up") {
      rv$responses$adaptive_question_text
    } else {
      fns$personalize_text(current_q$text, rv$responses)
    }

    # Save to database
    fns$save_response(
      con,
      session_id = rv$session_id,
      question_id = current_q$id,
      question_order = rv$q_num,
      question_text = question_text,
      raw_input = user_input,
      extracted_value = extracted_data[[field_name]],
      answered_clearly = answered_clearly,
      retry_attempt = rv$retry_count
    )

    # Check if answer was clear
    if (!answered_clearly && rv$retry_count < 1) {
      rv$retry_count <- rv$retry_count + 1
      fns$increment_retry(con, rv$session_id)
      chat_append("chat", fns$bot_response(messages$retry))
      rv$processing <- FALSE
      return()
    }

    # Store in reactive values
    rv$retry_count <- 0
    rv$responses[[field_name]] <- extracted_data[[field_name]]
    rv$responses[[paste0(field_name, "_answered_clearly")]] <- answered_clearly

    # Store adaptive question response
    if (!is.null(current_q$adaptive) && current_q$adaptive) {
      rv$responses$adaptive_question_response <- user_input
    }

    # Move to next question
    rv$q_num <- rv$q_num + 1

    if (rv$q_num <= length(questions)) {
      next_q <- questions[[rv$q_num]]

      # Handle content generation (adaptive questions, funfacts, etc.)
      if (!is.null(next_q$content_generation)) {
        template_config <- content_templates[[next_q$content_generation]]
        context_data <- lapply(template_config$context_fields, \(field) rv$responses[[field]])

        tryCatch(
          {
            generated_content <- fns$generate_content(chat, template_config, context_data)

            if (next_q$content_generation == "follow_up") {
              # Adaptive question generation
              rv$responses$adaptive_question_text <- generated_content
              chat_append("chat", fns$bot_response(generated_content))
            } else {
              # Other content types - add to next question with intro if configured
              next_question <- fns$personalize_text(next_q$text, rv$responses)

              if (!is.null(template_config$intro_template)) {
                intro_data <- list()
                for (field in template_config$intro_fields) {
                  intro_data[[length(intro_data) + 1]] <- switch(field,
                    "generated_content" = generated_content,
                    "next_question" = next_question,
                    rv$responses[[field]]
                  )
                }
                msg <- do.call(sprintf, c(list(template_config$intro_template), intro_data))
                chat_append("chat", fns$bot_response(msg))
              } else {
                chat_append("chat", fns$bot_response(next_question))
              }
            }
          },
          error = function(err) {
            message(paste("Content generation failed:", next_q$content_generation, "- falling back to regular question"))
            if (next_q$content_generation == "follow_up") {
              # Skip adaptive question, move to next
              rv$q_num <- rv$q_num + 1
              if (rv$q_num <= length(questions)) {
                fallback_q <- questions[[rv$q_num]]
                fallback_text <- fns$personalize_text(fallback_q$text, rv$responses)
                chat_append("chat", fns$bot_response(fallback_text))
              }
            } else {
              # Show question without generated content
              next_question <- fns$personalize_text(next_q$text, rv$responses)
              chat_append("chat", fns$bot_response(next_question))
            }
          }
        )
      } else {
        # Regular question without content generation
        next_question <- fns$personalize_text(next_q$text, rv$responses)
        chat_append("chat", fns$bot_response(next_question))
      }
    } else {
      # Survey complete - mark in database
      fns$complete_session(con, rv$session_id)
      rv$session_id <- NULL

      # Show completion message
      msg <- sprintf(
        messages$completion,
        rv$responses$name %||% "friend",
        rv$responses$ice_cream %||% "ice cream"
      )
      chat_append("chat", fns$bot_response(msg))
    }

    rv$processing <- FALSE
  })
}

# Run ----
shinyApp(ui, server)
