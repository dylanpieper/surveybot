import("DBI")
import("RSQLite")
import("coro")
import("ellmer")
import("promises")
import("cli")
import("shiny")

# Database Setup ----

#' Initialize SQLite database with schema
#' @param db_path Path to database file
#' @param db_driver Database driver (default RSQLite::SQLite())
init_database <- \(db_path = "survey.db", db_driver = RSQLite::SQLite()) {
  con <- dbConnect(db_driver, db_path)

  dbExecute(con, "PRAGMA foreign_keys = ON")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS sessions (
      session_id INTEGER PRIMARY KEY AUTOINCREMENT,
      started_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      completed_at TIMESTAMP,
      completed BOOLEAN NOT NULL DEFAULT FALSE,
      retry_count INTEGER DEFAULT 0,
      question_set_version TEXT DEFAULT '1.0',
      duration_seconds INTEGER
    )
  ")

  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_sessions_completed ON sessions(completed)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_sessions_started ON sessions(started_at)")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS responses (
      response_id INTEGER PRIMARY KEY AUTOINCREMENT,
      session_id INTEGER NOT NULL,
      question_id TEXT NOT NULL,
      question_order INTEGER NOT NULL,
      question_text TEXT,
      input_raw TEXT NOT NULL,
      input_extracted TEXT,
      answered_clearly BOOLEAN,
      responded_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      retry_attempt INTEGER DEFAULT 0,
      question_duration_seconds INTEGER,

      FOREIGN KEY (session_id) REFERENCES sessions(session_id) ON DELETE CASCADE
    )
  ")

  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_responses_session ON responses(session_id)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_responses_question ON responses(question_id)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_responses_order ON responses(session_id, question_order)")

  dbDisconnect(con)
  cli_alert_success("Database initialized at {.file {db_path}}")
}

# Database Operations ----

#' Start a new survey session
#' @param con Database connection
#' @param version Question set version
#' @return session_id
start_session <- \(con, version = "1.0") {
  dbExecute(
    con,
    "INSERT INTO sessions (question_set_version) VALUES (?)",
    params = list(version)
  )

  dbGetQuery(con, "SELECT last_insert_rowid() as id")$id
}

#' Save a question response
#' @param con Database connection
#' @param session_id Integer session ID
#' @param question_id Question identifier
#' @param question_order Order in survey
#' @param question_text Actual question text shown
#' @param input_raw User's raw input
#' @param input_extracted Cleaned/extracted value
#' @param answered_clearly Boolean quality flag
#' @param retry_attempt Which attempt (0 = first)
#' @param question_duration_seconds Duration in seconds for this question
save_response <- \(con, session_id, question_id, question_order, question_text,
  input_raw, input_extracted = NULL, answered_clearly = NULL,
  retry_attempt = 0, question_duration_seconds = NULL) {
  dbExecute(
    con,
    "INSERT INTO responses
     (session_id, question_id, question_order, question_text, input_raw,
      input_extracted, answered_clearly, retry_attempt, question_duration_seconds)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(
      as.integer(session_id),
      as.character(question_id),
      as.integer(question_order),
      as.character(question_text),
      as.character(input_raw),
      if (is.null(input_extracted)) NULL else as.character(input_extracted),
      if (is.null(answered_clearly)) NULL else as.logical(answered_clearly),
      as.integer(retry_attempt),
      if (is.null(question_duration_seconds)) NULL else as.integer(question_duration_seconds)
    )
  )
}

#' Update session duration
#' @param con Database connection
#' @param session_id Integer session ID
update_session_duration <- \(con, session_id) {
  dbExecute(
    con,
    "UPDATE sessions SET 
       duration_seconds = CAST((julianday(CURRENT_TIMESTAMP) - julianday(started_at)) * 86400 AS INTEGER)
     WHERE session_id = ?",
    params = list(session_id)
  )
}

#' Mark session as completed
#' @param con Database connection
#' @param session_id Integer session ID
complete_session <- \(con, session_id) {
  dbExecute(
    con,
    "UPDATE sessions SET 
       completed = TRUE, 
       completed_at = CURRENT_TIMESTAMP,
       duration_seconds = CAST((julianday(CURRENT_TIMESTAMP) - julianday(started_at)) * 86400 AS INTEGER)
     WHERE session_id = ?",
    params = list(session_id)
  )
}

#' Increment retry count for session
#' @param con Database connection
#' @param session_id Integer session ID
increment_retry <- \(con, session_id) {
  dbExecute(
    con,
    "UPDATE sessions SET retry_count = retry_count + 1 WHERE session_id = ?",
    params = list(session_id)
  )
}

# Async Chat Operations ----


#' Extract structured response from user input (synchronous)
#' @param chat Chat object
#' @param user_response User's text input
#' @param schema Extraction schema
#' @return Extracted data
extract_response <- \(chat, user_response, schema) {
  chat$clone()$set_turns(list())$chat_structured(user_response, type = schema)
}

#' Generate content using templates
#' @param chat Chat object
#' @param template_config Template configuration with prompt, intro (optional)
#' @param context_data Named list of values for template placeholders
#' @return Generated content
generate_content <- \(chat, template_config, context_data = list()) {
  prompt <- interpolate(template_config$prompt, context_data)
  schema <- create_generic_schema("content")
  result <- chat$clone()$set_turns(list())$chat_structured(prompt, type = schema)
  result[["content"]]
}

#' Build context string from template and field mapping
#' @param template Template string with %s placeholders
#' @param field_mapping List of field names to extract from responses
#' @param responses Response data
#' @return Formatted context string
build_context <- \(template, field_mapping, responses) {
  context_values <- lapply(field_mapping, \(field) responses[[field]])
  do.call(sprintf, c(list(template), context_values))
}

#' Generate adaptive follow-up question (synchronous)
#' @param chat Chat object
#' @param responses Current response data
#' @param context_template Template for building context from responses
#' @param prompt Prompt template for question generation
#' @return Question text
#' @deprecated Use generate_content with follow_up template instead
generate_adaptive_question_async <- \(chat, responses, context_template, prompt) {
  context <- sprintf(
    context_template,
    responses$name,
    responses$ice_cream,
    responses$why_favorite
  )

  full_prompt <- paste(context, prompt, sep = "\n")

  chat$clone()$set_turns(list())$chat(full_prompt)
}

# Utility Functions ----

#' String interpolation with context-aware capitalization
#' @param template String with {variable} placeholders
#' @param data Named list or environment with variable values
#' @param fallback_value Value to use for missing variables (default: variable name)
#' @return Interpolated string with proper capitalization
interpolate_with_context <- \(template, data, fallback_value = NULL) {
  result <- template
  if (is.null(template)) {
    return(template)
  }

  # Extract all {variable} patterns
  matches <- gregexpr("\\{([^}]+)\\}", template)[[1]]
  if (matches[1] == -1) {
    return(template)
  }

  # Process matches in reverse order to preserve positions
  match_starts <- as.numeric(matches)
  match_lengths <- attr(matches, "match.length")

  for (i in length(match_starts):1) {
    start <- match_starts[i]
    length <- match_lengths[i]

    # Extract variable name
    var_text <- substr(template, start, start + length - 1)
    var_name <- gsub("\\{|\\}", "", var_text)

    # Get replacement value
    replacement <- if (!is.null(data[[var_name]])) {
      as.character(data[[var_name]])
    } else if (!is.null(fallback_value)) {
      as.character(fallback_value)
    } else {
      var_name
    }

    # Check if variable starts a sentence (at beginning or after ". ", "! ", "? ")
    is_sentence_start <- start == 1 || 
      grepl("[\\.\\!\\?]\\s*$", substr(result, 1, start - 1))

    # Capitalize if at sentence start
    if (is_sentence_start && !is.null(data[[var_name]])) {
      replacement <- capitalize_first(replacement)
    }

    # Replace in template
    result <- paste0(
      substr(result, 1, start - 1),
      replacement,
      substr(result, start + length, nchar(result))
    )
  }

  result
}

#' String interpolation with named placeholders
#' @param template String with {variable} placeholders
#' @param data Named list or environment with variable values
#' @param fallback_value Value to use for missing variables (default: variable name)
#' @return Interpolated string
interpolate <- \(template, data, fallback_value = NULL) {
  result <- template
  if (is.null(template)) {
    return(template)
  }

  # Extract all {variable} patterns
  matches <- gregexpr("\\{([^}]+)\\}", template)[[1]]
  if (matches[1] == -1) {
    return(template)
  }

  # Process matches in reverse order to preserve positions
  match_starts <- as.numeric(matches)
  match_lengths <- attr(matches, "match.length")

  for (i in length(match_starts):1) {
    start <- match_starts[i]
    length <- match_lengths[i]

    # Extract variable name
    var_text <- substr(template, start, start + length - 1)
    var_name <- gsub("\\{|\\}", "", var_text)

    # Get replacement value
    replacement <- if (!is.null(data[[var_name]])) {
      as.character(data[[var_name]])
    } else if (!is.null(fallback_value)) {
      as.character(fallback_value)
    } else {
      var_name
    }

    # Replace in template
    result <- paste0(
      substr(result, 1, start - 1),
      replacement,
      substr(result, start + length, nchar(result))
    )
  }

  result
}

#' Extract variable names from template strings
#' @param template Template string with {variable} placeholders
#' @return Character vector of variable names
extract_variables <- \(template) {
  if (is.null(template)) {
    return(character(0))
  }

  matches <- regmatches(template, gregexpr("\\{([^}]+)\\}", template))[[1]]
  if (length(matches) == 0) {
    return(character(0))
  }

  # Extract variable names without braces
  gsub("\\{|\\}", "", matches)
}

#' Generate generic schema for content extraction
#' @param field_name Name of the field to extract
#' @return Schema object for structured data
create_generic_schema <- \(field_name = "content") {
  schema_list <- list()
  schema_list[[field_name]] <- type_string("Extract the generated content")
  do.call(type_object, schema_list)
}

#' Capitalize first letter of a string
#' @param text String to capitalize
#' @return String with first letter capitalized
capitalize_first <- \(text) {
  if (is.null(text) || nchar(text) == 0) {
    return(text)
  }
  paste0(toupper(substr(text, 1, 1)), substr(text, 2, nchar(text)))
}

#' Personalize question text with user responses
#' @param text Question text with {placeholders}
#' @param responses List of response values
#' @return Personalized text
personalize_text <- \(text, responses) {
  if (is.null(text)) {
    return(text)
  }

  # Filter out internal fields
  filtered_responses <- responses[!names(responses) %in% c(
    "adaptive_question_text", "adaptive_question_response", "answered_clearly"
  )]

  interpolate_with_context(text, filtered_responses)
}

#' Create streaming bot response generator
#' @param message Message to stream
#' @param response_delay Initial delay before streaming (default 0)
#' @param character_delay Delay between characters (default 0.02)
#' @return Async generator
bot_response <- async_generator(function(message, response_delay = 0, character_delay = 0.02) {
  await(async_sleep(response_delay))
  chars <- strsplit(as.character(message), "", useBytes = FALSE)[[1]]
  for (char in chars) {
    yield(char)
    await(async_sleep(character_delay))
  }
})

# Survey Configuration ----

#' Create complete survey configuration
#' @param topic The main topic of the survey (e.g., "ice cream", "coffee", "music")
#' @param topic_emoji Emoji representing the topic
#' @param subject_field Name of the field storing the subject preference
#' @param location_context Context for location question (e.g., "spot", "place", "store")
#' @return List containing messages, content_templates, and questions
survey_config <- \(topic = "ice cream", topic_emoji = "🍨", subject_field = "ice_cream", location_context = "spot") {
  
  messages <- list(
    welcome = paste("Hello! I'd love to learn about your", topic, "preferences.", topic_emoji),
    retry = "Sorry, I had trouble understanding that. 🤔 Could you try again?",
    completion = paste(
      "Thanks, {name}! I recorded your love for {", subject_field, "}! 🤓📊",
      "I hope you enjoy your next", topic, "soon!", topic_emoji, "✨"
    )
  )
  
  content_templates <- list(
    funfact = list(
      prompt = paste("Share a fun fact about {", subject_field, "} ", topic, ". Return a brief fun fact (1-2 sentences) with no 'Fun fact:' prefix; include a fun emoji (but don't use the ", topic, " emoji)."),
      response_field = "fact",
      intro_template = paste("Oh, {", subject_field, "}! {generated_content}\n\n{next_question}")
    ),
    follow_up = list(
      prompt = paste(
        "User '{name}' likes {", subject_field, "} ", topic, " because: {why_favorite}.",
        "Acknowledge their reason briefly. Then, generate one curious follow-up question",
        "about their", topic, "preference based on what they said.",
        "Examples: If they mention texture, ask about their experience or if they add extra toppings/additions.",
        "If they mentioned nostalgia, ask about memories.",
        "Return ONLY the question text with no preamble."
      ),
      response_field = "question"
    ),
    recommendation = list(
      prompt = paste("Suggest a {food_type} pairing for someone who likes {", subject_field, "} ", topic, ". Return a brief suggestion with emoji."),
      response_field = "suggestion",
      intro_template = "Here's a great pairing idea: {generated_content}\n\n{next_question}"
    )
  )
  
  questions <- list(
    list(
      id = "name",
      text = "What's your name?",
      schema = type_object(
        name = type_string("Just the person's name, e.g. 'Dylan' from 'call me dylan'"),
        answered_clearly = type_boolean("TRUE if they provided any reasonable first and/or last name, FALSE only if completely off-topic, only a nickname is given, or no name is given")
      )
    ),
    list(
      id = subject_field,
      text = paste("Hey {name}! Let's talk", topic, ".", topic_emoji, "What's your favorite", if(topic == "ice cream") "flavor" else "type", "?"),
      schema = if(subject_field == "ice_cream") {
        type_object(
          ice_cream = type_string("The ice cream flavor, e.g. 'mint chocolate chip'"),
          answered_clearly = type_boolean("TRUE if they mentioned any flavor, FALSE only if completely off-topic")
        )
      } else {
        type_object(
          preference = type_string(paste("The", topic, "type, e.g. 'rock'", collapse = " ")),
          answered_clearly = type_boolean(paste("TRUE if they mentioned any", topic, "type, FALSE only if completely off-topic", collapse = " "))
        )
      }
    ),
    list(
      id = "why_favorite",
      text = if(topic == "ice cream") "What about {ice_cream} makes it your favorite ice cream flavor?" else paste0("What about {", subject_field, "} makes it your favorite ", topic, " type?"),
      content_generation = "funfact",
      schema = type_object(
        why_favorite = type_string("The reason why they like this preference"),
        answered_clearly = type_boolean("TRUE if they mentioned ANY positive feeling, emotion, memory, or reason related to their preference, even if very brief like 'I love it' or 'It reminds me of something'. FALSE only if completely off-topic, rude, or makes no sense")
      )
    ),
    list(
      id = "fu_favorite",
      text = NULL,
      content_generation = "follow_up",
      schema = type_object(
        fu_favorite = type_string("The core answer to the adaptive question"),
        answered_clearly = type_boolean("TRUE if they provided ANY answer that shows engagement with the question, even if brief, somewhat vague, or unconventional. FALSE only if completely off-topic or rude")
      )
    ),
    list(
      id = "brand_shop",
      text = paste("Love it! Where's your go-to", location_context, "to get {", subject_field, "} ", topic, "? 👀"),
      schema = type_object(
        brand_shop = type_string(paste(
          "Brand or shop name EXACTLY as stated by user, no inference.",
          "If vague like 'the store', extract that literally"
        )),
        answered_clearly = type_boolean("TRUE if they named a specific brand/shop/location, FALSE if too vague (like 'the store', 'somewhere') or off-topic")
      )
    ),
    list(
      id = "when_eat",
      text = paste("{brand_shop} is a great choice! When do you crave {", subject_field, "} the most? 🤔"),
      schema = type_object(
        when_eat = type_string(paste("Brief summary of when they", if(topic %in% c("ice cream", "coffee", "tea")) "consume" else "enjoy", topic)),
        answered_clearly = type_boolean("TRUE if they described any timing, occasion, situation, or context for consuming/enjoying their preference, even if somewhat vague or unconventional. FALSE only if completely off-topic or rude")
      )
    )
  )
  
  list(
    messages = messages,
    content_templates = content_templates,
    questions = questions
  )
}

# Configuration ----

#' Default survey configuration
#' @param db_path Database file path
#' @param db_driver Database driver expression
#' @param tries Maximum retry attempts for unclear responses
#' @param response_delay Delay before bot response (seconds)
#' @param character_delay Delay between characters in streaming (seconds)
#' @param version Survey version
#' @return Configuration list
default_config <- \(db_path = "survey.db", 
                   db_driver = rlang::expr(RSQLite::SQLite()),
                   tries = 2,
                   response_delay = 0,
                   character_delay = 0.02,
                   version = "1.0") {
  list(
    db_path = db_path,
    db_driver = db_driver,
    tries = tries,
    response_delay = response_delay,
    character_delay = character_delay,
    version = version
  )
}

# Survey Class ----

#' Create a Survey class instance
#' @param chat Chat object for AI interactions
#' @param questions List of survey questions
#' @param messages Message templates
#' @param content Content generation templates
#' @param config Application configuration (optional, uses defaults)
#' @return Survey class instance
Survey <- function(chat, questions, messages, content, config = default_config()) {
  # Initialize database connection
  if (!file.exists(config$db_path)) {
    init_database(config$db_path, eval(config$db_driver))
  }
  con <- dbConnect(eval(config$db_driver), config$db_path)
  
  self <- list(
    # State
    chat = chat,
    con = con,
    questions = questions,
    messages = messages,
    content = content,
    config = config,
    
    # Survey state
    q_num = 1,
    responses = list(),
    retry_count = 0,
    session_id = NULL,
    processing = FALSE,
    question_start_time = NULL
  )
  
  # Initialize database session and return first question
  self$init <- function() {
    self$session_id <<- start_session(self$con, version = self$config$version)
    self$question_start_time <<- Sys.time()
    return(self$questions[[1]]$text)
  }
  
  # Cleanup database connection
  self$cleanup <- function() {
    if (!is.null(self$con)) {
      dbDisconnect(self$con)
      self$con <<- NULL
    }
  }
  
  # Process user input and return message to send
  self$process_input <- function(user_input) {
    if (self$processing) {
      return(list(message = NULL, complete = FALSE))
    }
    
    self$processing <<- TRUE
    
    # Get current question
    current_q <- self$questions[[self$q_num]]
    
    # Extract and save response
    extracted_data <- extract_response(self$chat, user_input, current_q$schema)
    field_name <- current_q$id
    answered_clearly <- extracted_data$answered_clearly
    
    # Determine question text for database
    question_text <- if (!is.null(current_q$content_generation) && 
                        current_q$content_generation == "follow_up") {
      self$responses$adaptive_question_text
    } else {
      personalize_text(current_q$text, self$responses)
    }
    
    # Calculate question duration
    question_duration <- if (!is.null(self$question_start_time)) {
      as.integer(difftime(Sys.time(), self$question_start_time, units = "secs"))
    } else {
      NULL
    }
    
    # Save to database
    save_response(
      self$con,
      session_id = self$session_id,
      question_id = current_q$id,
      question_order = self$q_num,
      question_text = question_text,
      input_raw = user_input,
      input_extracted = extracted_data[[field_name]],
      answered_clearly = answered_clearly,
      retry_attempt = self$retry_count,
      question_duration_seconds = question_duration
    )
    
    # Update session duration
    update_session_duration(self$con, self$session_id)
    
    # Check if retry needed
    if (!answered_clearly && self$retry_count < self$config$tries) {
      self$retry_count <<- self$retry_count + 1
      increment_retry(self$con, self$session_id)
      self$processing <<- FALSE
      return(list(message = self$messages$retry, complete = FALSE))
    }
    
    # Store response data
    self$responses[[field_name]] <<- extracted_data[[field_name]]
    self$responses[[paste0(field_name, "_raw")]] <<- user_input
    self$responses[[paste0(field_name, "_answered_clearly")]] <<- answered_clearly
    self$retry_count <<- 0
    
    # Move to next question
    self$q_num <<- self$q_num + 1
    
    # Reset question start time for next question
    self$question_start_time <<- Sys.time()
    
    # Check if survey complete
    if (self$q_num > length(self$questions)) {
      complete_session(self$con, self$session_id)
      completion_message <- interpolate(self$messages$completion, self$responses)
      self$processing <<- FALSE
      return(list(message = completion_message, complete = TRUE))
    }
    
    # Get next question and generate content
    next_q <- self$questions[[self$q_num]]
    generated_content <- self$generate_content(next_q)
    
    # Handle failed adaptive questions
    if (is.null(generated_content) && !is.null(next_q$content_generation) && next_q$content_generation == "follow_up") {
      # Skip to next question
      self$q_num <<- self$q_num + 1
      self$question_start_time <<- Sys.time()
      
      if (self$q_num > length(self$questions)) {
        complete_session(self$con, self$session_id)
        completion_message <- interpolate(self$messages$completion, self$responses)
        self$processing <<- FALSE
        return(list(message = completion_message, complete = TRUE))
      }
      
      fallback_q <- self$questions[[self$q_num]]
      fallback_message <- personalize_text(fallback_q$text, self$responses)
      self$processing <<- FALSE
      return(list(message = fallback_message, complete = FALSE))
    }
    
    # Store adaptive question text for database
    if (!is.null(generated_content) && next_q$content_generation == "follow_up") {
      self$responses$adaptive_question_text <<- generated_content
    }
    
    # Build and return final message
    message <- self$build_message(next_q, generated_content)
    self$processing <<- FALSE
    return(list(message = message, complete = FALSE))
  }
  
  # Generate content for question
  self$generate_content <- function(question) {
    if (is.null(question$content_generation)) {
      return(NULL)
    }
    
    template_config <- self$content[[question$content_generation]]
    
    # Auto-extract required fields from template
    required_fields <- extract_variables(template_config$prompt)
    context_data <- list()
    for (field in required_fields) {
      context_data[[field]] <- self$responses[[field]]
    }
    names(context_data) <- required_fields
    
    tryCatch(
      {
        generate_content(self$chat, template_config, context_data)
      },
      error = function(err) {
        message(paste("Content generation failed:", question$content_generation))
        NULL
      }
    )
  }
  
  # Build message for question
  self$build_message <- function(question, generated_content = NULL) {
    question_text <- personalize_text(question$text, self$responses)
    
    if (!is.null(generated_content) && !is.null(question$content_generation)) {
      template_config <- self$content[[question$content_generation]]
      
      if (question$content_generation == "follow_up") {
        # For adaptive questions, return the generated content directly
        return(generated_content)
      } else if (!is.null(template_config$intro)) {
        # For other content types, combine with intro template
        intro_data <- c(
          list(
            content = generated_content,
            next_question = question_text
          ),
          self$responses
        )
        return(interpolate(template_config$intro, intro_data))
      }
    }
    
    return(question_text)
  }
  
  # Return the survey instance
  class(self) <- "Survey"
  return(self)
}

# Server Function ----

#' Survey in Shiny server
#' @param input Shiny input object
#' @param output Shiny output object  
#' @param session Shiny session object
#' @param chat Chat object for AI interactions
#' @param questions List of survey questions
#' @param messages Message templates
#' @param content Content generation templates
#' @param config Optional configuration (uses defaults if not provided)
survey <- function(input, output, session, chat, questions, messages, content, config = default_config()) {
  survey <- NULL
  initialized <- FALSE
  
  # Initialize survey and send first question
  observe({
    if (!initialized) {
      initialized <<- TRUE
      survey <<- Survey(chat, questions, messages, content, config)
      
      # Setup cleanup on session end
      onStop(\() {
        if (!is.null(survey)) {
          survey$cleanup()
        }
      })
      
      # Send first question
      first_question <- survey$init()
      shinychat::chat_append("chat", bot_response(first_question), session = session)
    }
  })
  
  # Handle user responses
  observeEvent(input$chat_user_input, {
    if (is.null(survey)) return()
    
    user_input <- input$chat_user_input
    result <- survey$process_input(user_input)
    
    if (!is.null(result$message)) {
      if (result$complete) {
        # Survey finished
        shinychat::chat_append("chat", bot_response(result$message), session = session)
        survey$cleanup()
        survey <<- NULL
      } else {
        # Continue with next question or retry
        shinychat::chat_append("chat", bot_response(result$message), session = session)
      }
    }
  })
}
