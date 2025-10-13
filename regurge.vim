vim9script

     # @license
    # regurge.vim: Opens a conversation buffer with an LLM through
   #               the regurge cmdline tool.
  # Copyright (c) 2025 by Stephen R. van den Berg <srb@cuci.nl>
 # License: ISC OR GPL-3.0
# Sponsored by: Cubic Circle, The Netherlands

# Start using :R [persona] or :Regurge [persona], then send using \s
# It starts in insert mode.  When you leave insert mode it autosends.
# Pressing enter jumps you down, into insert mode.
# zo opens a fold.
# zc closes a fold.
# \r reduces the buffer to only your own text.
# \R resets the conversation.
# \a aborts the running response.
# Whereas the \ represents the default <leader> key.
#
# It uses folds to hide meta information and to separate user and model
# responses.
#
# FIXME When undoing then redoing a model response, folds do not restore.
# This is a known limitation of Vim.
#
# Global variables to allow for customisation from your .vimrc:
#
# regurge_sendkey		      # Default: s
# regurge_reducekey		      # Default: r
# regurge_resetkey		      # Default: R
# regurge_abortkey		      # Default: a
# regurge_model
# regurge_project
# regurge_location
# regurge_autofold_code: number	      # More than this many lines are folded.
#
# regurge_personas: dict<dict<any>>   # User-defined personas.
# regurge_persona		      # Default persona.
# regurge_systeminstruction: list<string> # Default system instructions.
# regurge_config: dict<any>    	      # Default configuration.
#
# Colour profiles used:
# RegurgeUser
# RegurgeModel
# RegurgeMeta

# Global variable that stores total cost in $
# since start of vim: g:regurge_cost

const default_model: string = "gemini-2.5-flash-lite"
#const default_model: string = "gemini-flash-lite-latest"
const gvarprefix: string = "regurge_"
const pluginname: string = "Regurge"
const default_autofold_code: number = 8
# The following system instructions tend to result in a minimum
# of wasted output tokens.
const default_systeminstruction: list<string> =<< trim HERE
 Be exceedingly concise, blunt.
 If unsure, state doubt.
 Answer in staccato keywords.
 When suggesting changes: summarize issues,
 use unified-diff focus on functional changes
 preserving comments/whitespace.
 You are addressing a fellow senior developer/physicist.
 Answer in the prompt-language.
 No preamble, politeness, disclaimers.
HERE

const prices: dict<list<float>> = {
                # [$/M tokens input, $/M tokens output]
  "gemini-flash-lite-latest": [0.15,  0.60],
  "gemini-2.5-flash-lite":    [0.15,  0.60],
  "gemini-flash-latest":      [0.30,  2.50],
  "gemini-2.5-flash":         [0.30,  2.50],
  "gemini-pro-latest":        [2.50, 10.00],
  "gemini-2.5-pro":           [2.50, 10.00],
}

def Getgvar(tailname: string, defval: any): any
  return get(g:, gvarprefix .. tailname, defval)
enddef

const default_config: dict<any> = {
 "model": Getgvar("model", default_model),
 "systemInstruction": Getgvar("systeminstruction", default_systeminstruction),
 # The gemini 2.5 allows up to 131072 output tokens.
 # Limit runaway cost.
 "maxOutputTokens": 8192,
 "temperature": 0.1,
 "topP": 0.95,
 "topK": 1,
 "frequencyPenalty": 0.6,
 "presencePenalty": 0.3,
 "candidateCount": 1,
 "thinkingConfig": {
   "includeThoughts": false,
   "thinkingBudget": 0,
 },
 # There are more options to be included here, check regurge.
 # Putting the options here allows direct overrides over the
 # defaults already preset in regurge.
 # As an exception to the rule, you can even override the
 # model with a "model": entry in here.
}

const system_personas: dict<dict<any>> = {
  [pluginname]:
  { "config": extend(copy(Getgvar("config", default_config)), {
      # Add overriding options for this persona here.
    }),
    "project": "",    # Override default project if non-empty.
    "location": "",   # Override default location if non-empty.
  },
}

def Regurge(args: list<string> = [])
  var persona: string
  var append_content: string

  if !empty(args)
    persona = args[0]
    if len(args) > 1
      append_content = join(args[1 : ])
    endif
  else
    persona = Getgvar("persona", pluginname)
  endif

  var foundbuffer: bool

  # Check if a buffer with this persona already exists.
  for bufinfo in getbufinfo({"buflisted": 1})
    if has_key(bufinfo.variables, "regurge_persona")
     && bufinfo.variables.regurge_persona == persona
      foundbuffer = true
      execute "buffer " .. bufinfo.bufnr
      break
    endif
  endfor

  if !foundbuffer
    # Do not create/write b: (buffer local) variables before enew.
    # If no existing buffer was found, proceed with creating a new one.
    enew
    setlocal noswapfile
    setlocal noundofile
    setlocal wrap
    setlocal linebreak
    setlocal noautoindent nosmartindent nocindent
    setlocal indentkeys=
    setlocal indentexpr=
    setlocal nomodified
    setlocal modifiable
    setlocal foldmethod=manual
    setlocal buftype=nofile
    # Setting filetype should be last, since it triggers a FileType event.
    setlocal filetype=markdown

    # Define custom highlight groups for fold levels.
    # Default definitions; users can override in their vimrc.
    hi default RegurgeUser  ctermfg=Green guifg=Green
    hi default RegurgeModel ctermfg=NONE  guifg=NONE
    hi default RegurgeMeta  ctermfg=NONE  guifg=NONE

    const ourbuf: number = bufnr("%")
    b:regurge_persona = persona
    # The b:regurge_persona variable is also used as a marker to check if we
    # are looking at a Regurge buffer.
    execute printf("file [%s %d]", persona, ourbuf)

    # Default is: \s to send to LLM.
    const leader_sendkey:   string = Getgvar("sendkey",   "s")
    # Default is: \r reduce the chat to only the user input.
    const leader_reducekey: string = Getgvar("reducekey", "r")
    # Default is: \R reset the chat to system instructions only.
    const leader_resetkey:  string = Getgvar("resetkey",  "R")
    # Default is: \a abort the running response.
    const leader_abortkey:  string = Getgvar("abortkey",  "a")
    const personas: dict<dict<string>> = Getgvar("personas", {})
    const profile: dict<any> =
       has_key(personas, persona)        ? personas[persona]
     : has_key(system_personas, persona) ? system_personas[persona]
                                              : system_personas[pluginname]

    const systemconfig: dict<any> = extend(copy(profile.config),
      { "systemInstruction":
         extend(profile.config.systemInstruction[ : ],
                # Extend system instructions with persona name.
                [ printf("Your name is '%s'.", persona) ]) })

    if has_key(systemconfig, "model")
      b:model = systemconfig.model
    endif

    var configfold: list<string>
    for [key, value] in items(systemconfig)
      if type(value) == v:t_list
        final mylist: list<string> = value[ : ]
        add(configfold, key .. ":")
        for i in range(len(mylist))
          mylist[i] = substitute(mylist[i], '[`\\]', '\\&', "g")
        endfor
        mylist[0] = "`" .. mylist[0]
        mylist[-1] = mylist[-1] .. "`,"
        extend(configfold, mylist)
      else
        add(configfold, printf("%s: %s,", key,
          (type(value) == v:t_string
           && key != "model" ? value : json_encode(value))))
      endif
    endfor
    # Fill the first fold with the config and system instructions.
    append(0, configfold)
    execute ":1,$-1fold"
    execute ":1foldclose"

    # Temporarily disable 'showmode' to suppress "--- INSERT ---" message
    b:old_showmode = &showmode
    setlocal noshowmode
    feedkeys("\<C-o>i")   # Enter insert mode.

    b:helper = ["regurge", "-j"]

    def Add_flags(flag: string, varname: string, defval: string): string
      const gval: string = has_key(profile, varname)
                            && !empty(profile[varname])
                           ? profile[varname]
                           : Getgvar(varname, defval)
      if !empty(gval)
        extend(b:helper, [flag, gval])
      endif
      return gval
    enddef

    Add_flags("-L", "location", "") # Default via environment (see regurge).
    Add_flags("-P", "project", "")  # Default via environment (see regurge).

    b:job_obj = null_job        # Init it, in case the buffer is wiped now.

    autocmd BufDelete            <buffer> Cleanup(str2nr(expand("<abuf>")))
    autocmd WinEnter,SafeState   <buffer> ApplyFoldHighlighting()
    autocmd BufWinLeave          <buffer> ClearFoldHighlighting()
    autocmd InsertEnter          <buffer> DisableMagicEnter()
    autocmd InsertLeave          <buffer> AutoSend()
    autocmd CmdlineEnter         <buffer> timer_stop(get(b:, "timer_id", 0))

    def Definelkey(key: string, func: string): void
      execute printf(
               "nnoremap <buffer> <silent> <Leader>%s <cmd>call <SID>%s<CR>",
               key, func)
    enddef

    Definelkey(leader_sendkey,   "SendMessageToLLM()")
    Definelkey(leader_reducekey, "ResetChat(v:false)")
    Definelkey(leader_resetkey,  "ResetChat(v:true)")
    Definelkey(leader_abortkey,  "CancelLLMResponse()")

    redraw | echohl Normal |
     echo printf("Type then send to %s using %s%s",
                 persona, get(g:, "mapleader", "\\"), leader_sendkey)
  endif

  # Append any preset content provided.
  if !empty(append_content)
    append("$", append_content)
  endif

  # Move cursor to the end of the buffer.
  normal! G

  # If initial content was provided, send it immediately.
  if !empty(append_content)
    SendMessageToLLM()
  endif
enddef

# Returns: 0: failed 1: running 2: restarted.
def StartRegurgeProcess(ourbuf: number): number
  var job_obj: job = getbufvar(ourbuf, "job_obj")
  if job_status(job_obj) != "run"
    if job_status(job_obj) == "dead"
      echomsg printf("regurge process [%d] died, restarting it...", ourbuf)
    endif
    # Start the regurge process in JSON mode.
    job_obj = job_start(getbufvar(ourbuf, "helper"), {
     "out_cb": (channel, msg) => HandleLLMOutput(channel, msg, ourbuf),
     "err_cb": (channel, msg) => HandleLLMError(channel, msg, ourbuf),
     "close_cb": (channel) => HandleRegurgeClose(ourbuf),
    })
    setbufvar(ourbuf, "job_obj", job_obj)
    return job_status(job_obj) == "run" ? 2 : 0
  endif
  return 1
enddef

def ShowHourglass(ourbuf: number, timer_id: number): void
  # Check if the buffer still exists and is a regurgechat buffer.
  # This is important because the timer might fire after the buffer
  # has been wiped out.
  const start_time: any = getbufvar(ourbuf, "start_time", "")
  if !bufexists(ourbuf) || empty(start_time)
    timer_stop(timer_id)
    return
  endif
  if bufnr("%") == ourbuf
    redraw | echohl Normal | echo printf("Waiting for LLM ... %.0fs",
                                         reltimefloat(reltime(start_time)))
  endif
enddef

# FIXME The fold-level-dependent highlighting has window scope, so it needs
# to be toggled on and off, depending on the buffer being in view.
def ClearFoldHighlighting(): void
  const fold_match_ids: list<number> = get(w:, "regurge_fold_match_ids", [])
  # Window-local list to store match IDs for dynamic highlighting.
  for id in fold_match_ids
    matchdelete(id)
  endfor
  w:regurge_fold_match_ids = []
enddef

# Function to apply fold-level-dependent highlighting to visible lines.
def ApplyFoldHighlighting(): void
  ClearFoldHighlighting()

  final linesperlevel: list<list<number>> = [[], [], []]

  def ColourFold(group: string, level: number)
    const lines: list<number> = linesperlevel[level]
    if !empty(lines)
      add(w:regurge_fold_match_ids, matchaddpos(group, lines))
    endif
  enddef

  for lnum in range(line("w0"), line("w$"))
    add(linesperlevel[min([foldlevel(lnum), 2])], lnum)
  endfor
  ColourFold("RegurgeUser", 0)
  ColourFold("RegurgeModel", 1)
  ColourFold("RegurgeMeta", 2)
enddef

def DisableMagicEnter(): void
  if !empty(maparg("<CR>"))
    nunmap <buffer> <CR>
  endif
enddef

def AutoSend(): void
  if IsWaitingForResponse()
    return
  endif
  if b:old_showmode
    setlocal showmode
  endif
  const curline: number = line('.')
  if curline == line("$") && col(".") > 1 && col(".") + 1 == col("$")
     && foldlevel(curline) == 0 && !empty(getline(curline))
    SendMessageToLLM()
  endif
enddef

def IsWaitingForResponse(): number
  if !&modifiable
    echohl WarningMsg | echo "Please wait for the current response..."
    return 1
  endif
  return 0
enddef

def GotToInsertModeAtEnd(): void
  if IsWaitingForResponse()
    return
  endif
  DisableMagicEnter()	  # Only allow magic-enter once per cycle.
  # Go to the last line, open a new line, and enter insert mode.
  execute "normal! G"
  feedkeys("o")
enddef

def StartShowHourglass(): void
  # Make this a local variable, so that repeated ShowHourglass() calls
  # use the constant value from this closure.
  const ourbuf: number = bufnr("%")
  b:start_time = reltime()
  # Start the timer for the waiting message.
  b:timer_id = timer_start(1000,
                (id) => ShowHourglass(ourbuf, id), {"repeat": -1})
enddef

def SendMessageToLLM(): void
  if StartRegurgeProcess(bufnr("%")) == 0
    echohl ErrorMsg | echo "Connection to regurge failed, retry later."
    return
  endif
  # Function to send the current user message and entire history to regurge.
  if IsWaitingForResponse()
    return
  endif

  # Parse the entire buffer to get the complete chat history.
  var history: list<dict<any>>
  var text_lines: list<string>
  var role: string

  def Flushparts(newrole: string, lnum: number)
    if role != newrole && !empty(text_lines)
      add(history, {
       "role": role, "parts": [{"text": join(text_lines, "\n")}]})
      text_lines = []
    endif
    role = newrole
    add(text_lines, getline(lnum))
  enddef

  var pastmeta: bool
  for lnum in range(1, line("$"))
    const flevel: number = foldlevel(lnum)
    if flevel == 0
      pastmeta = false
      Flushparts("user", lnum)        # Look for markdown marker.
    elseif flevel == 1 || pastmeta || getline(lnum) =~ '^\s*```'
      pastmeta = true
      Flushparts("model", lnum)
    endif
  endfor
  Flushparts("", 0)

  if empty(history) || history[-1].role != "user"
     || empty(trim(history[-1].parts[0].text))
    return    # Nothing to send.
  endif

  if (history[0].role == "model")
    const modelinfold =
     matchstr(history[0].parts[0].text, '\s\s*model:\s*"\zs[^"]\+\ze",')
    if !empty(modelinfold)
      b:model = modelinfold
    endif
  endif

  # Disable buffer modifications while waiting for the LLM response.
  setlocal nomodifiable

  StartShowHourglass()

  b:response_start_line = 0
  # Send the JSON history to the stdin of the regurge process.
  ch_sendraw(job_getchannel(b:job_obj), json_encode(history) .. "\n")
  echohl Normal | echo "Sent message to regurge..."

  setlocal nomodified
enddef

def AppendLLMResponse(response: list<string>, metadata: list<string>,
                 active: bool): void
  b:partial_msg = ""	      # Received whole message, so clear it.
  const finalmsg = !empty(metadata)
  if (finalmsg)
    timer_stop(b:timer_id)
  endif

  # Clear the status field (only visible if the buffer is active).
  redraw | echohl Normal | echo ""

  setlocal modifiable

  const start_line: number = b:response_start_line == 0 ?
                             line("$") + 1 : b:response_start_line
  var end_meta_line: number
  const resptime: string = printf(" \"ResponseTime\": %.0f ",
                            reltimefloat(reltime(b:start_time)) * 1000)
  const ourbuf: number = bufnr("%")
  if finalmsg
    metadata[0] = printf("{%s,", resptime)
  else
    # Placeholder.
    extend(metadata, [ "{" .. resptime, "}" ])
  endif
  if b:response_start_line == 0
    append(start_line - 1, metadata)
    end_meta_line = line("$")
  else
    # Insert after the first line, but before the last line of the fold.
    append(start_line, metadata)
    # Delete the first line of the old metadata.
    deletebufline(ourbuf, start_line)
    end_meta_line = start_line + len(metadata) - 1
    # Delete the last line of the old metadata.
    deletebufline(ourbuf, end_meta_line)
  endif
  var end_line: number = line("$")
  if b:response_start_line == 0
    if !finalmsg
      add(response, "...")
    endif
    append(end_line, response)
  else
    # Insert just before the ... trailing line.
    end_line -= 1
    append(end_line, response)
    setline(end_line, [getline(end_line) .. getline(end_line + 1)])
    deletebufline(ourbuf, end_line + 1)
    if finalmsg
      # Delete ... trailer.
      deletebufline(ourbuf, line("$"))
    endif
  endif
  end_line = line("$")

  if b:response_start_line == 0
    const cmdprefix: string = ":" .. start_line

    def Dofoldop(cmdtail: string): void
      execute cmdprefix .. cmdtail
    enddef

    # Create a fold for the newly added model response.
    if start_line <= end_line
      Dofoldop(printf(",%dfold", end_line))
      Dofoldop("foldopen")
      if start_line <= end_meta_line
        Dofoldop(printf(",%dfold", end_meta_line))
        Dofoldop("foldclose")
      endif
      Dofoldop("foldopen")
    endif
  endif

  if finalmsg
    # Fold first-level markdown quoted code snippets.
    var lnum: number = end_meta_line
    var lstart: number
    while lnum < end_line
      lnum += 1
      const line: string = getline(lnum)
      if line =~ '^\s*```'
        if line =~ '^\s*```\w\+$'
          lstart = lnum
        elseif line =~ '^\s*```\s*$'
          execute printf(":%d,%dfold", lstart, lnum)
	  # Open small source snippets.
          if lnum - lstart <= Getgvar("autofold_code", default_autofold_code)
	     # Except for the Google internal search-script snippets.
	     && (lnum - lstart != 2
	         || getline(lstart + 1) !~ '^print(google_search\.search(')
            execute printf(":%dfoldopen", lstart)
          endif
        endif
      endif
    endwhile
  endif

  if active
    ApplyFoldHighlighting()
  endif
  if b:response_start_line == 0
    b:response_start_line = start_line
    # Ensure the screen updates and scrolls to the new content.
    cursor(start_line - 1, 1)
    normal! zt
    cursor(start_line, 1)
    redraw
  endif
  if finalmsg
    # Pressing a mere enter jumps to the end, new line, insert mode.
    nnoremap <buffer> <silent> <CR> <cmd>call <SID>GotToInsertModeAtEnd()<CR>
  else
    # Disable buffer modifications again while waiting for more LLM responses.
    setlocal nomodifiable
  endif
enddef

def HandleLLMOutput(curchan: channel, msg: string, ourbuf: number): void
  # Callback function for stdout from the regurge process.
  if !bufexists(ourbuf)   # guard against race conditions.
    return
  endif

  const fullmsg: string = getbufvar(ourbuf, "partial_msg", "") .. msg
  var json_parts: list<any>

  try
    json_parts = json_decode(fullmsg)
    if empty(json_parts)
      throw "E491:"
    endif
  catch /E49[1-9]:/
    setbufvar(ourbuf, "partial_msg", fullmsg)
    if StartRegurgeProcess(ourbuf) == 1
      return               # Wait for a complete msg.
    else
      echohl ErrorMsg | echo "Connection to regurge failed, retry later."
      json_parts = []
    endif
  endtry

  var model_response_text: list<string>
  var model_metadata: list<string>
  for part in json_parts
    if has_key(part, "text")
      extend(model_response_text, [part.text])
    elseif has_key(part, "usageMetadata")
      extend(model_metadata, split(part.usageMetadata, "\n", 1))
    endif
  endfor
  model_response_text = split(join(model_response_text, ""), "\n", 1)

  const original_buf: number = bufnr("%")

  if ourbuf == original_buf
    AppendLLMResponse(model_response_text, model_metadata, true)
  else
    const original_pos: list<number> = getcurpos()[1 : ]
    const noa_b: string = "noautocmd buffer "
    try
      # Switch to the target buffer to perform updates.
      execute noa_b .. ourbuf
      AppendLLMResponse(model_response_text, model_metadata, false)
    catch
    finally
      # Always try to return to the buffer the user was in.
      execute noa_b .. original_buf
      cursor(original_pos)
    endtry
  endif

  if !empty(model_metadata)
    const struct_metadata: dict<any> = json_decode(join(model_metadata))
    const model = has_key(struct_metadata, "modelVersion")
                ? struct_metadata.modelVersion : ""
    const bmodel = getbufvar(ourbuf, "model", "")
    # If we cannot find the model in our pricelist, provide the most expensive
    # guess instead.
    const modelprices: list<float> =
       has_key(prices, model)  ? prices[model]  :
       has_key(prices, bmodel) ? prices[bmodel] : [75.0, 150.0]

    def CalcTokenCost(tokenname: string, costoffset: number): float
      return has_key(struct_metadata, tokenname)
           ? struct_metadata[tokenname] * modelprices[costoffset] * 0.000001
           : 0.0
    enddef

    const cost_inc: float = CalcTokenCost("promptTokenCount",     0)
                          + CalcTokenCost("candidatesTokenCount", 1)
    const cost_old: float = get(g:, "regurge_cost", 0.0)
    g:regurge_cost = cost_old + cost_inc

    # This echo will appear in the original buffer.
    echohl Normal |
     echo printf("%s %d, ResponseTime: %d  $%.5f + $%.5f = $%.02f",
                 getbufvar(ourbuf, "regurge_persona"), ourbuf,
                 struct_metadata.ResponseTime,
		 cost_old, cost_inc, g:regurge_cost)
  endif
enddef

def HandleLLMError(curchan: channel, msg: string, ourbuf: number): void
  # Callback function for stderr from the regurge process.
  # Must be specified, otherwise vim will choke on stderr output.
  echohl ErrorMsg | echomsg printf("%s %d %s", pluginname, ourbuf, msg)
enddef

def HandleRegurgeClose(ourbuf: number): void
  timer_stop(getbufvar(ourbuf, "timer_id", 0))
  setbufvar(ourbuf, '&modifiable', 1)
  echohl ErrorMsg |
   echomsg printf("%s %d helper process died.", pluginname, ourbuf)
enddef

def ResetChat(fullreset: bool): void
  # Only perform this on Regurge buffers.
  if empty(get(b:, "regurge_persona", ""))
    return
  endif
  var lnum: number = 1
  while foldlevel(lnum) == 1
    lnum += 1                 # Preserve system instructions.
  endwhile
  const original_pos: list<number> = getcurpos()[1 : ]
  cursor(lnum, 1)
  normal! zt
  cursor(original_pos)
  # Ensure the screen updates and scrolls to the new content.
  redraw
  const ourbuf: number = bufnr("%")
  while lnum <= line("$")
    if foldlevel(lnum) != 0 || fullreset
      deletebufline(ourbuf, lnum)
    else
      lnum += 1               # Preserve user input (at most).
    endif
  endwhile
  if fullreset
    GotToInsertModeAtEnd()
  endif
enddef

def CancelLLMResponse(): void
  # Only perform this on Regurge buffers.
  if empty(get(b:, "regurge_persona", ""))
    return
  endif
  const job_obj: job = get(b:, "job_obj", null_job)
  if job_status(job_obj) == "run"
    if foldlevel(line("$")) == 0
      job_stop(job_obj)		    # Still no output.
      setlocal modifiable
    else
      ch_sendraw(job_getchannel(job_obj),
                 json_encode({ "abort": 1 }) .. "\n")
    endif
  else
    setlocal modifiable
  endif
enddef

def Cleanup(ourbuf: number)
  const job_obj: job = getbufvar(ourbuf, "job_obj", null_job)
  if job_status(job_obj) == "run"
    job_stop(job_obj)
  endif
enddef

# Define the user command to start the chat.
command! -nargs=* Regurge call Regurge(<f-args>)
# Define a shorthand alias.
command! -nargs=* R Regurge(<f-args>)
