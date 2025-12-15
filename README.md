# surveybot

surveybot demonstrates conversational data collection using [shinychat](https://posit-dev.github.io/shinychat/) and Claude. Expanding on traditional surveys and interviews, users answer questions through natural conversation while the LLM extracts structured data, generates content, and asks adaptive follow-up questions.

## Usage 🍦✨

``` r
shiny::runApp("app.R")
```

The demo collects ice cream preferences through a shinychat UI. The files are modular to make your own surveybot (package to come).

## Key Features

-   **LLM data extraction** with structured schemas and validation
-   **Adaptive questioning** based on user responses
-   **Automatic retry logic** for unclear answers
-   **SQLite storage** with session tracking (replace with any DB backend)

## Files

-   `app.R` - Main Shiny application
-   `functions.R` - Database operations and chat utilities
-   `analyze.R` - Database analysis script
