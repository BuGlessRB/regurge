<h1>regurge</h1>

regurge is a commandline tool to interact with an LLM.
regurge.vim is the LLM interface AGI (As God Intended).

regurge has the following features:
- Connects to VertexAI.
- Disables censoring, sets model temperature to zero, answers are
  fully reproducible.
- JSON mode.

regurge.vim supports:
- Multiple conversations (one per buffer).
- Fully editable history, to mold the scope the LLM sees.
- Full power of vim available to copy/paste from/into LLM conversations.
- No pesky delimiters which cloud the answers.
- Colour-coded conversation view.
- Foldable conversation view.
- Automatic caching of past history (lowers cost).

## Requirements

Node.js
Vim9

## Basic usage

Simply drop regurge in your path and load regurge.vim.
The start the conversation using :R

## References

Card-carrying member of the `zerodeps` movement.

Sadly, however, I find that I need a dependency on `@google-cloud/vertexai`
in the regurge script.
