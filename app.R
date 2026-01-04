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
  completion = paste(
    "Thanks, {name}! I recorded your love for {ice_cream}! 🤓📊",
    "I hope you enjoy your next scoop soon! 🍦✨"
  )
)

# Content templates ----
content <- list(
  funfact = list(
    prompt = "Share a fun fact about {ice_cream} ice cream. Return a brief fun fact (1-2 sentences) with  no prefix and include a fun emoji (but don't use the ice cream cone or dish).",
    intro = "Oh, {ice_cream}! {content}\n\n{next_question}"
  ),
  follow_up = list(
    prompt = paste(
      "User '{name}' likes {ice_cream} ice cream because: {why_favorite}.",
      "Acknowledge their reason briefly. Then, generate one curious follow-up question",
      "about their ice cream preference based on what they said.",
      "Examples: If they mention texture, ask about their experience or if they add extra toppings.",
      "If they mentioned nostalgia, ask about memories.",
      "Return ONLY the question text with no preamble."
    )
  )
)

# Survey questions with extraction schemas ----
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
    id = "ice_cream",
    text = "Hey {name}! Let's talk ice cream. 🍦 What's your favorite flavor?",
    schema = type_object(
      ice_cream = type_string("The ice cream flavor, e.g. 'mint chocolate chip'"),
      answered_clearly = type_boolean("TRUE if they mentioned any flavor, FALSE only if completely off-topic")
    )
  ),
  list(
    id = "why_favorite",
    text = "What about {ice_cream} makes it your favorite ice cream flavor?",
    content = "funfact",
    schema = type_object(
      why_favorite = type_string("The reason why they like this preference"),
      answered_clearly = type_boolean("TRUE if they mentioned ANY positive feeling, emotion, memory, or reason related to their preference, even if very brief like 'I love it' or 'It reminds me of something'. FALSE only if completely off-topic, rude, or makes no sense")
    )
  ),
  list(
    id = "fu_favorite",
    text = NULL,
    content = "follow_up",
    schema = type_object(
      fu_favorite = type_string("The core answer to the adaptive question"),
      answered_clearly = type_boolean("TRUE if they provided ANY answer that shows engagement with the question, even if brief, somewhat vague, or unconventional. FALSE only if completely off-topic or rude")
    )
  ),
  list(
    id = "brand_shop",
    text = "Love it! Where's your go-to spot to get {ice_cream} ice cream? 👀",
    schema = type_object(
      brand_shop = type_string("Brand or shop name EXACTLY as stated by user, no inference. If vague like 'the store', extract that literally"),
      answered_clearly = type_boolean("TRUE if they named a specific brand/shop/location, FALSE if too vague (like 'the store', 'somewhere') or off-topic")
    )
  ),
  list(
    id = "when_eat",
    text = "{brand_shop} is a great choice! When do you crave {ice_cream} the most? 🤔",
    schema = type_object(
      when_eat = type_string("Brief summary of when they consume/enjoy their preference"),
      answered_clearly = type_boolean("TRUE if they described any timing, occasion, situation, or context for consuming/enjoying their preference, even if somewhat vague or unconventional. FALSE only if completely off-topic or rude")
    )
  )
)

# Setup chat ----
chat <- chat_claude(echo = "none")

# UI ----
ui <- page_fillable(
  card(
    card_header("SurveyBot"),
    chat_ui(id = "chat")
  )
)

# Server ----
server <- \(input, output, session) {
  fns$chat_survey(input, output, session, chat, questions, messages, content)
}

# Run ----
shinyApp(ui, server)
