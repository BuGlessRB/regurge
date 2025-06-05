vim9script

# Opens up a conversation buffer with an LLM through the regurge cmdline tool.
#
# Start using :R or :Regurge, then send using \s
# It starts in insert mode.  When you leave insert mode it autosends.
# Pressing enter jumps you down, into insert mode.
# zo opens a fold
# zc closes a fold
#
# Copyright (c) 2025, Stephen R. van den Berg <srb@cuci.nl>, The Netherlands
# Released into the wild under the GPL3 open source license.
#
# It uses folds to hide meta information and to separate user and model
# responses.
# FIXME Whenever you undo, then redo a model response, the folds will not come
# back.  This cannot easily be fixed, since Vim does not restore manual folds
# at the moment.

def Regurge()
  var leader_hotkey = "s"   # \s to send to LLM

  # Define custom highlight groups for fold levels.
  # These are default definitions. Users can override these.
  hi default RegurgeUser ctermfg=LightYellow guifg=LightYellow
  hi default RegurgeModel ctermbg=NONE guibg=NONE
  hi default RegurgeMeta ctermfg=Cyan guifg=Cyan

  enew
  setlocal noswapfile
  setlocal noundofile
  setlocal wrap
  setlocal linebreak
  setlocal noautoindent nosmartindent nocindent
  setlocal foldmethod=manual
  setlocal indentkeys=
  setlocal indentexpr=
  setlocal filetype=regurgechat
  setlocal buftype=nofile
  execute "file [Regurge Chat " .. bufnr("%") .. "]"
  setlocal nomodified
  setlocal modifiable

  b:job_obj = null_job      # Init it, in case the buffer is wiped right away
  # Temporarily disable 'showmode' to suppress "--- INSERT ---" message
  b:old_showmode = &showmode
  setlocal noshowmode
  feedkeys("i")   # Enter insert mode

  autocmd BufWipeout <buffer> Stop_helperprocess(str2nr(expand("<abuf>")))
  autocmd BufEnter,SafeState <buffer> Show_foldcolours()
  autocmd BufLeave <buffer> Hide_foldcolours()
  autocmd InsertEnter <buffer> UnsetMagicEnter()
  autocmd InsertLeave <buffer> AutoSend()
  # Move cursor to the end of the buffer
  normal! G
  # Define a normal mode mapping to send the message
  execute "nnoremap <buffer> <silent> <Leader>" ..
           leader_hotkey .. " <cmd>call <SID>SendtoLLM()<CR>"

  # Buffer-local list to store match IDs for dynamic highlighting.
  b:regurge_fold_match_ids = []

  redraw | echo "Type your messages, send them to the LLM using " ..
                 get(g:, "mapleader", "\\") .. "s"
enddef

# Returns: 0: failed 1: running 2: restarted
def Start_helperprocess(ourbuf: number): number
  var job_obj: job = getbufvar(ourbuf, "job_obj")
  if job_status(job_obj) != "run"
    Stop_helperprocess(ourbuf)       # Just in case
    # Start the regurge process in JSON mode
    var cmd: list<string> = ["regurge", "-j"]
    job_obj = job_start(cmd, {
     "in_io": "pipe",
     "out_io": "pipe",
     "callback": (channel, msg) => MsgfromLLM(channel, msg, ourbuf),
    })
    setbufvar(ourbuf, "job_obj", job_obj)
    return job_status(job_obj) == "run" ? 2 : 0
  endif
  return 1
enddef

def Hourglass(ourbuf: number, timer_id: number): void
  # Check if the buffer still exists and is a regurgechat buffer
  # This is important because the timer might fire after the buffer is wiped out
  if !bufexists(ourbuf)
    timer_stop(timer_id)
  endif
  var start_time: any = getbufvar(ourbuf, "start_time")
  if bufnr("%") == ourbuf
    redraw | echo "Waiting for LLM... " ..
                  printf("%.0f", reltimefloat(reltime(start_time))) .. "s"
  endif
enddef

def Hide_foldcolours(): void
  for id in b:regurge_fold_match_ids
    matchdelete(id)
  endfor
  b:regurge_fold_match_ids = []
enddef

# Function to apply fold-level-dependent highlighting to visible lines.
def Show_foldcolours(): void
  Hide_foldcolours()

  var linesperlevel: list<list<number>> = [[], [], []]

  def ColourFold(group: string, level: number)
    var lines: list<number> = linesperlevel[level]
    if (!empty(lines))
      add(b:regurge_fold_match_ids, matchaddpos(group, lines))
    endif
  enddef

  var lfirst: number = getcurpos()[1] - winline()
  for lnum in range(line("w0"), line("w$"))
    add(linesperlevel[min([foldlevel(lnum), 2])], lnum)
  endfor
  ColourFold("RegurgeUser", 0)
  ColourFold("RegurgeModel", 1)
  ColourFold("RegurgeMeta", 2)
enddef

def UnsetMagicEnter(): void
  if !empty(maparg("<CR>"))
    nunmap <buffer> <CR>
  endif
enddef

def AutoSend(): void
  if Check_waitingforLLM()
    return
  endif
  if b:old_showmode
    setlocal showmode
  endif
  var curline: number = line('.')
  if curline == line("$") && col(".") + 1 == col("$") &&
     foldlevel(curline) == 0 && !empty(getline(curline))
    SendtoLLM()
  endif
enddef

def Check_waitingforLLM(): number
  if !&modifiable
    echohl WarningMsg | echo "Please wait for the current response..."
    return 1
  endif
  return 0
enddef

def EntertoType(): void
  if Check_waitingforLLM()
    return
  endif
  UnsetMagicEnter()
  # Go to the last line, open a new line, and enter insert mode
  execute "normal! G"
  feedkeys("o")
enddef

def SendtoLLM(): void
  if Start_helperprocess(bufnr("%")) == 0
    echohl ErrorMsg | echo "Connection to regurge failed, retry later."
    return
  endif
  # Function to send the current user message and entire history to regurge
  if Check_waitingforLLM()
    return
  endif

  # Parse the entire buffer to get the complete chat history
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

  for lnum in range(1, line("$"))
    var flevel: number = foldlevel(lnum)
    if flevel == 0
      Flushparts("user", lnum)
    elseif flevel == 1
      Flushparts("model", lnum)
    endif
    # Drop foldlevels > 1
  endfor
  Flushparts("", 0)

  # Disable buffer modifications while waiting for the LLM response
  setlocal nomodifiable

  # Start the timer for the waiting message
  b:start_time = reltime()
  b:timer_id = timer_start(1000,
                function("Hourglass", [bufnr("%")]), {"repeat": -1})

  # Send the JSON history to the stdin of the regurge process
  ch_sendraw(job_getchannel(b:job_obj), json_encode(history) .. "\n")
  echohl Normal | echo "Sent message to regurge..."

  setlocal nomodified
enddef

def UpdateBuffer(response: list<string>, metadata: list<string>,
                 active: bool): void
  b:partial_msg = ""	      # Received whole message, so clear it
  # Stop the timer
  timer_stop(b:timer_id)

  # Clear the status field (only visible if the buffer is active)
  redraw | echo ""

  setlocal modifiable

  var start_line: number = line("$") + 1
  if (!empty(metadata))
    metadata[0] = "{ \"ResponseTime\": " ..
     printf("%.0f", reltimefloat(reltime(b:start_time)) * 1000) .. ","
  endif
  append(line("$"), metadata)
  var end_meta_line: number = line("$")
  append(line("$"), response)
  var end_line: number = line("$")

  var cmdprefix = ":" .. start_line

  def Dofoldop(cmdtail: string): void
    execute cmdprefix .. cmdtail
  enddef

  # Create a fold for the newly added model response
  if start_line <= end_line
    Dofoldop("," .. end_line .. "fold")
    Dofoldop("foldopen")
    if start_line <= end_meta_line
      Dofoldop("," .. end_meta_line .. "fold")
      Dofoldop("foldclose")
    endif
    Dofoldop("foldopen")
  endif

  cursor(start_line, 1)
  if active
    Show_foldcolours()
  endif
  # Ensure the screen updates and scrolls to the new content
  normal! zt
  redraw
  # Pressing a mere enter jumps to the end, new line, insert mode
  nnoremap <buffer> <silent> <CR> <cmd>call <SID>EntertoType()<CR>
enddef

def MsgfromLLM(curchan: channel, msg: string, ourbuf: number): void
  # Callback function for stdout from the regurge process
  if !bufexists(ourbuf)   # guard against race conditions
    return
  endif

  var fullmsg: string = getbufvar(ourbuf, "partial_msg", "") .. msg
  var json_parts: list<any>

  try
    json_parts = json_decode(fullmsg)
    if empty(json_parts)
      throw "Crashed?"
    endif
  catch /.*/
    setbufvar(ourbuf, "partial_msg", fullmsg)
    if Start_helperprocess(ourbuf) == 1
      return               # Wait for a complete msg
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

  var original_bufnr: number = bufnr("%")
  var original_lnum: number = line(".")
  var original_col: number = col(".")

  try
    # Switch to the target buffer to perform updates.
    execute "noautocmd buffer " .. ourbuf
    UpdateBuffer(model_response_text, model_metadata,
                 ourbuf == original_bufnr)
  finally
    # Always try to return to the buffer the user was in
    execute "noautocmd buffer " .. original_bufnr
    cursor(original_lnum, original_col)
  endtry
  
  if !empty(model_metadata)
    # This echo will appear in the original buffer
    echohl Normal | echo "Regurge, total tokens: " ..
      json_decode(join(model_metadata)).totalTokenCount
  endif
enddef

# Function to stop the regurge helper process
def Stop_helperprocess(ourbuf: number)
  var job_obj: job = getbufvar(ourbuf, "job_obj")
  if job_status(job_obj) == "run"
    job_stop(job_obj)
  endif
enddef

# Define the user command to start the chat
command! Regurge call Regurge()
command! R call Regurge()
