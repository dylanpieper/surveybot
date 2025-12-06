# SurveyBot: Conversational Survey Platform

A demonstration of using [shinychat](https://posit-dev.github.io/shinychat/) to create engaging survey experiences through natural conversation. This project showcases how chat-based interfaces can transform traditional surveys into interactive dialogues, making them particularly powerful for qualitative data collection.

## 🍦 Live Demo & Data

-   **Shiny App**: [Ice Cream Survey (NLP)](https://dylan-pieper.shinyapps.io/surveybot/)
-   **Data**: [Google Sheets](https://docs.google.com/spreadsheets/d/1bOW8rtww5aIBZlD93OWyeTlTNBrJbWT1bE7MpATlt5Y/edit?usp=sharing)

## Why Conversational Surveys?

Traditional surveys often feel impersonal with a disappointing UI/UX. SurveyBot demonstrates how conversational interfaces can:

-   **Increase engagement** through natural, flowing dialogue
-   **Gather richer responses** by allowing follow-up questions
-   **Reduce survey fatigue** with interactive, human-like interactions
-   **Collect qualitative insights** that structured forms often miss
-   **Adapt in real-time** based on participant responses

## Two Approaches

### 1. ✨LLM Version✨ (`surveybot-ice-cream-LLM.R`)

**Intelligent conversation with adaptive questioning**

This version uses Claude LLM to create sophisticated conversational experiences:

-   **Smart data extraction** from natural language responses using structured schemas
-   **Intelligent validation** with response quality flags based on LLM judgement
-   **Automatic retry logic** when responses need clarification (one retry per question)
-   **Adaptive follow-up questions** generated based on what users share
-   **Contextual conversation** that remembers and builds on previous answers
-   **Rich interaction** with fun facts and personalized responses

**How LLM Extraction & Validation Works:** Each question defines a schema that extracts both the answer AND validates response quality:

``` r
schema = type_object(
  ice_cream = type_string("The ice cream flavor mentioned"),
  answered_clearly = type_boolean("TRUE if they mentioned any flavor, FALSE if off-topic")
)
```

The LLM simultaneously extracts structured data and determines if the response adequately answered the question. If `answered_clearly` is FALSE, the bot asks for clarification once before moving on.

Perfect for research scenarios where you want deep, nuanced responses and can adapt the conversation flow dynamically.

### 2. ✨NLP Version✨ (`surveybot-ice-cream-NLP-google.R`)

**Deterministic processing with no LLM needed (no cost or data privacy concerns)**

This version uses traditional NLP techniques for consistent, predictable conversations:

-   **Rule-based text processing** using linguistic patterns
-   **Reliable data extraction** suitable for production environments
-   **Real-time visualization** of responses through wordclouds
-   **Cloud data storage** with automatic Google Sheets integration
-   **Multi-user analytics** dashboard

Ideal for use cases where consistency, reliability, and data privacy are required.

## Perfect for Qualitative Research

Both implementations excel at collecting rich, qualitative data that traditional surveys struggle with:

-   **Open-ended responses** feel natural in conversation
-   **Context preservation** allows for meaningful follow-ups
-   **Participant comfort** increases with familiar chat interfaces
-   **Rich storytelling** emerges from conversational prompts
-   **Emotional context** is captured alongside factual responses

## Use Cases for Conversational Surveys

-   **Customer feedback collection** with follow-up questions
-   **User experience research** gathering detailed stories
-   **Academic research** requiring nuanced responses
-   **Market research** exploring motivations and preferences
-   **Employee satisfaction** surveys with adaptive questioning
-   **Patient intake** forms in healthcare settings

Transform your data collection from static forms to engaging conversations with SurveyBot!
