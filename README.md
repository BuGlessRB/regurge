<h1>regurge</h1>

regurge is a commandline tool to interact with an LLM.
regurge.vim is the LLM interface AGI (As God Intended).

regurge has the following features:
- Connects to VertexAI using the latest @google/genai interface.
- Disables censoring.
- Answers are mostly reproducible.
- Google Search grounding enabled.
- JSON mode.

regurge.vim supports:
- Multiple conversations (one per buffer).
- Fully editable history, to mold the scope the LLM sees.
- Full power of vim available to copy/paste from/into LLM conversations.
- No pesky delimiters which cloud the answers.
- Colour-coded conversation view.
- Foldable conversation view.
- Automatic caching of past history (lowers cost).
- Supports naming the process.
- Supports providing/altering system instructions in the first fold.
- All configuration constants are tweakable in the first fold.
- Session cost accounting.

## Requirements

Node.js
Vim9

## Basic usage

Simply drop regurge in your path and load regurge.vim.
- Start the conversation using `:R [persona]`
- When exiting insert mode, it autodetects if it can send to the LLM.
- Use `\s` to send the conversation to the LLM explicitly.
- Use `\r` to reduce the conversation to only your questions.
- Use `\R` to reset the conversation to an empty question (cheaper).
- Use `\a` to abort the running response.
- Use zo to open folds.
- Use zc to close folds.

## References

Card-carrying member of the `zerodeps` movement.

Sadly, however, I find that I need a dependency on `@google/genai`
in the regurge script.
