library(shiny)
library(shinychat)
library(bslib)
library(ellmer)
library(DBI)
library(RSQLite)
library(promises)

# Load functions module
fns <- modules::use("functions.R")

# Configuration ----
config <- list(
  tries = 1,
  response_delay = 0,
  character_delay = 0.02,
  version = "1.0",
  db_path = "survey.db",
  db_driver = rlang::expr(RSQLite::SQLite())
)

# Messages ----
messages <- list(
  welcome = "Hello! I'd love to learn about your ice cream preferences. 🍨",
  retry = "Sorry, I had trouble understanding that. 🤔 Could you try again?",
  completion = paste(
    "Thanks, {name}! I recorded your love for {ice_cream}! 🤓📊",
    "I hope you enjoy your next scoop soon! 🍦✨"
  )
)

# Content Templates ----
content_templates <- list(
  funfact = list(
    prompt = "Share a fun fact about {ice_cream} ice cream. Return a brief fun fact (1-2 sentences) with no 'Fun fact:' prefix; include a fun emoji (but don't use the ice cream cone or dish).",
    response_field = "fact",
    intro_template = "Oh, {ice_cream}! {generated_content}\n\n{next_question}"
  ),
  follow_up = list(
    prompt = paste(
      "User '{name}' likes {ice_cream} ice cream because: {why_favorite}.",
      "Acknowledge their reason briefly. Then, generate one curious follow-up question",
      "about their ice cream preference based on what they said.",
      "Examples: If they mention texture, ask about their experience or if they add extra toppings.",
      "If they mentioned nostalgia, ask about memories.",
      "Return ONLY the question text with no preamble."
    ),
    response_field = "question"
  ),
  recommendation = list(
    prompt = "Suggest a {food_type} pairing for someone who likes {ice_cream} ice cream. Return a brief suggestion with emoji.",
    response_field = "suggestion",
    intro_template = "Here's a great pairing idea: {generated_content}\n\n{next_question}"
  )
)

# Survey questions with extraction schemas ----
questions <- list(
  list(
    id = "name",
    text = "What's your name?",
    schema = type_object(
      name = type_string("Just the person's name, e.g. 'Dylan' from 'call me dylan'"),
      answered_clearly = type_boolean(paste(
        "TRUE if they provided any reasonable first and/or last name,",
        "FALSE only if completely off-topic, only a nickname is given, or no name is given"
      ))
    )
  ),
  list(
    id = "ice_cream",
    text = "Hey {name}! Let's talk ice cream. 🍦 What's your favorite flavor?",
    schema = type_object(
      ice_cream = type_string("The ice cream flavor, e.g. 'mint chocolate chip'"),
      answered_clearly = type_boolean(paste(
        "TRUE if they mentioned any flavor,",
        "FALSE only if completely off-topic"
      ))
    )
  ),
  list(
    id = "why_favorite",
    text = "What about {ice_cream} makes it your favorite ice cream flavor?",
    content_generation = "funfact",
    schema = type_object(
      why_favorite = type_string("The reason why they like this flavor"),
      answered_clearly = type_boolean(paste(
        "TRUE if they mentioned ANY positive feeling, emotion, memory, or reason",
        "related to the flavor, even if very brief like 'I love it' or 'It reminds me of something'.",
        "FALSE only if completely off-topic, rude, or makes no sense"
      ))
    )
  ),
  list(
    id = "fu_favorite",
    text = NULL,
    content_generation = "follow_up",
    schema = type_object(
      fu_favorite = type_string("The core answer to the adaptive question"),
      answered_clearly = type_boolean(paste(
        "TRUE if they provided ANY answer that shows engagement with the question,",
        "even if brief, somewhat vague, or unconventional.",
        "FALSE only if completely off-topic or rude"
      ))
    )
  ),
  list(
    id = "brand_shop",
    text = "Love it! Where's your go-to spot to get {ice_cream} ice cream? 👀",
    schema = type_object(
      brand_shop = type_string(paste(
        "Brand or shop name EXACTLY as stated by user, no inference.",
        "If vague like 'the store', extract that literally"
      )),
      answered_clearly = type_boolean(paste(
        "TRUE if they named a specific brand/shop/location,",
        "FALSE if too vague (like 'the store', 'somewhere') or off-topic"
      ))
    )
  ),
  list(
    id = "when_eat",
    text = "{brand_shop} is a great choice! When do you crave {ice_cream} the most? 🤔",
    schema = type_object(
      when_eat = type_string("Brief summary of when they eat ice cream"),
      answered_clearly = type_boolean(paste(
        "TRUE if they described any timing, occasion, situation, or context for eating ice cream,",
        "even if somewhat vague or unconventional.",
        "FALSE only if completely off-topic or rude"
      ))
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
  if (!file.exists(config$db_path)) {
    fns$init_database(config$db_path, eval(config$db_driver))
  }

  con <- dbConnect(eval(config$db_driver), config$db_path)

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
      rv$session_id <- fns$start_session(con, version = config$version)

      chat_append("chat", fns$bot_response(questions[[1]]$text, config$response_delay, config$character_delay))
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
      input_raw = user_input,
      input_extracted = extracted_data[[field_name]],
      answered_clearly = answered_clearly,
      retry_attempt = rv$retry_count
    )

    # Check if answer was clear
    if (!answered_clearly && rv$retry_count < config$tries) {
      rv$retry_count <- rv$retry_count + 1
      fns$increment_retry(con, rv$session_id)
      chat_append("chat", fns$bot_response(messages$retry, config$response_delay, config$character_delay))
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
        # Auto-extract required fields from template
        required_fields <- fns$extract_variables(template_config$prompt)
        context_data <- setNames(
          lapply(required_fields, \(field) rv$responses[[field]]),
          required_fields
        )

        tryCatch(
          {
            generated_content <- fns$generate_content(chat, template_config, context_data)

            if (next_q$content_generation == "follow_up") {
              # Adaptive question generation
              rv$responses$adaptive_question_text <- generated_content
              chat_append("chat", fns$bot_response(generated_content, config$response_delay, config$character_delay))
            } else {
              # Other content types - add to next question with intro if configured
              next_question <- fns$personalize_text(next_q$text, rv$responses)

              if (!is.null(template_config$intro_template)) {
                intro_data <- c(
                  list(
                    generated_content = generated_content,
                    next_question = next_question
                  ),
                  rv$responses
                )
                msg <- fns$interpolate(template_config$intro_template, intro_data)
                chat_append("chat", fns$bot_response(msg, config$response_delay, config$character_delay))
              } else {
                chat_append("chat", fns$bot_response(next_question, config$response_delay, config$character_delay))
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
                chat_append("chat", fns$bot_response(fallback_text, config$response_delay, config$character_delay))
              }
            } else {
              # Show question without generated content
              next_question <- fns$personalize_text(next_q$text, rv$responses)
              chat_append("chat", fns$bot_response(next_question, config$response_delay, config$character_delay))
            }
          }
        )
      } else {
        # Regular question without content generation
        next_question <- fns$personalize_text(next_q$text, rv$responses)
        chat_append("chat", fns$bot_response(next_question, config$response_delay, config$character_delay))
      }
    } else {
      # Survey complete - mark in database
      fns$complete_session(con, rv$session_id)
      rv$session_id <- NULL

      # Show completion message
      msg <- fns$interpolate(messages$completion, rv$responses)
      chat_append("chat", fns$bot_response(msg))
    }

    rv$processing <- FALSE
  })
}

# Run ----
shinyApp(ui, server)
