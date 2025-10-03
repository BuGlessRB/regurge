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
# zo opens a fold
# zc closes a fold
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
# regurge_autofold_code: number	      # More than this many lines are folded
#
# regurge_personas: dict<dict<any>>   # User-defined personas
# regurge_persona		      # Default persona
# regurge_systeminstruction: list<string> # Default system instructions
# regurge_config: dict<any>    	      # Default configuration
#
# Colour profiles used:
# RegurgeUser
# RegurgeModel
# RegurgeMeta

#const default_model: string = "gemini-2.5-flash-lite"
const default_model: string = "gemini-flash-lite-latest"
const gvarprefix: string = "regurge_"
const extname: string = "Regurge"
const default_autofold_code: number = 8
const default_systeminstruction: list<string> =<< trim HERE
 Be exceedingly concise, blunt.
 Articulate doubt if unsure.
 Answer in staccato keywords by default.
 When suggesting changes: summarise issues,
 use unified-diff focus on functional changes
 without any changes in comments/whitespace.
 You are addressing a fellow senior developer/physicist.
 Respond in the prompt-language by default.
 No preamble, politeness, compliments, apologies, disclaimers.
HERE

def Getgvar(tailname: string, def: any): any
  return get(g:, gvarprefix .. tailname, def)
enddef

const default_config: dict<any> = {
 "systemInstruction": Getgvar("systeminstruction", default_systeminstruction),
 "maxOutputTokens": 2048,
 "temperature": 0.1,
 "topP": 0.95,
 "topK": 1.0,
 "frequencyPenalty": 0.5,
 "presencePenalty": 0.3,
}

const system_personas: dict<dict<any>> = {
  [extname]:
  { "config": extend(copy(Getgvar("config", default_config)), {
      # Add config options here
    }),
    "model": "",      # Override model if non-empty
    "project": "",    # Override project if non-empty
    "location": "",   # Override location if non-empty
  },
}

def Regurge(requested_persona: string = extname)
  # Do not create/write b: (buffer local) variables before enew
  enew
  setlocal noswapfile
  setlocal noundofile
  setlocal wrap
  setlocal linebreak
  setlocal noautoindent nosmartindent nocindent
  setlocal indentkeys=
  setlocal indentexpr=
  setlocal foldmethod=manual
  setlocal buftype=nofile
  setlocal nomodified
  setlocal modifiable
  # Setting filetype should be last, since it triggers a FileType event
  setlocal filetype=markdown

  # Define custom highlight groups for fold levels.
  # Default definitions; users can override in their vimrc.
  hi default RegurgeUser  ctermfg=Green guifg=Green
  hi default RegurgeModel ctermfg=NONE  guifg=NONE
  hi default RegurgeMeta  ctermfg=NONE  guifg=NONE

  const ourbuf: number = bufnr("%")
  b:persona = empty(requested_persona) ?
                    Getgvar("persona", extname) : requested_persona
  execute printf("file [%s %d]", b:persona, ourbuf)

  b:regurge_model = default_model    # Default override

  # Default is: \s to send to LLM
  const leader_sendkey:   string = Getgvar("sendkey",   "s")
  # Default is: \r reduce the chat to only the user input
  const leader_reducekey: string = Getgvar("reducekey", "r")
  # Default is: \R reset the chat to system instructions only
  const leader_resetkey:  string = Getgvar("resetkey",  "R")
  # Default is: \a abort the running response
  const leader_abortkey:  string = Getgvar("abortkey",  "a")
  const personas: dict<dict<string>> = Getgvar("personas", {})
  const profile: dict<any> =
     has_key(personas, b:persona)        ? personas[b:persona]
   : has_key(system_personas, b:persona) ? system_personas[b:persona]
                                         : system_personas[extname]

  const systemconfig: dict<any> = extend(copy(profile.config),
    { "systemInstruction":
       extend(profile.config.systemInstruction[ : ],
	      # Extend system instructions with persona name
	      [ printf("Your name is '%s'.", b:persona) ]) })

  def Add_flags(flag: string, varname: string)
    const gval: string = has_key(profile, varname) && !empty(profile[varname])
                         ? profile[varname]
                         : Getgvar(varname, "")
    if !empty(gval)
      extend(b:helpercmd, [flag, gval])
    endif
  enddef

  def Definelkey(key: string, func: string): void
    execute printf(
             "nnoremap <buffer> <silent> <Leader>%s <cmd>call <SID>%s<CR>",
             key, func)
  enddef

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
        (type(value) == v:t_string ? value : json_encode(value))))
    endif
  endfor
  append(0, configfold)
  execute ":1,$-1fold"
  execute ":1foldclose"
  # Move cursor to the end of the buffer
  normal! G

  # Temporarily disable 'showmode' to suppress "--- INSERT ---" message
  b:old_showmode = &showmode
  setlocal noshowmode
  feedkeys("\<C-o>i")   # Enter insert mode

  b:helpercmd = ["regurge", "-j"]
  Add_flags("-M", "model")    # Default set in regurge
  Add_flags("-L", "location") # Default via environment (see regurge)
  Add_flags("-P", "project")  # Default via environment (see regurge)

  b:job_obj = null_job      # Init it, in case the buffer is wiped right away
  #Start_helperprocess(ourbuf)  # For debugging only

  autocmd BufDelete             <buffer> Cleanup(str2nr(expand("<abuf>")))
  autocmd BufWinEnter,SafeState <buffer> Show_foldcolours()
  autocmd BufWinLeave           <buffer> Hide_foldcolours()
  autocmd InsertEnter           <buffer> UnsetMagicEnter()
  autocmd InsertLeave           <buffer> AutoSend()
  Definelkey(leader_sendkey,   "SendtoLLM()")
  Definelkey(leader_reducekey, "ResetChat(v:false)")
  Definelkey(leader_resetkey,  "ResetChat(v:true)")
  Definelkey(leader_abortkey,  "AbortResponse()")

  redraw | echohl Normal |
   echo printf("Type then send to %s using %s%s",
               b:persona, get(g:, "mapleader", "\\"), leader_sendkey)
enddef

# Returns: 0: failed 1: running 2: restarted
def Start_helperprocess(ourbuf: number): number
  var job_obj: job = getbufvar(ourbuf, "job_obj")
  if job_status(job_obj) != "run"
    if job_status(job_obj) == "dead"
      echomsg printf("regurge process [%d] died, restarting it...", ourbuf)
    endif
    # Start the regurge process in JSON mode
    job_obj = job_start(getbufvar(ourbuf, "helpercmd"), {
     "out_cb": (channel, msg) => MsgfromLLM(channel, msg, ourbuf),
     "err_cb": (channel, msg) => ErrorfromLLM(channel, msg, ourbuf),
     "close_cb": (channel) => Helperclosed(ourbuf),
    })
    setbufvar(ourbuf, "job_obj", job_obj)
    return job_status(job_obj) == "run" ? 2 : 0
  endif
  return 1
enddef

def Hourglass(ourbuf: number, timer_id: number): void
  # Check if the buffer still exists and is a regurgechat buffer
  # This is important because the timer might fire after the buffer is wiped out
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
# to be toggled on and off, depending on the buffer being in view
def Hide_foldcolours(): void
  const fold_match_ids: list<number> = get(w:, "regurge_fold_match_ids", [])
  # Window-local list to store match IDs for dynamic highlighting.
  for id in fold_match_ids
    matchdelete(id)
  endfor
  w:regurge_fold_match_ids = []
enddef

# Function to apply fold-level-dependent highlighting to visible lines.
def Show_foldcolours(): void
  Hide_foldcolours()

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
  const curline: number = line('.')
  if curline == line("$") && col(".") > 1 && col(".") + 1 == col("$") &&
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
  UnsetMagicEnter()	  # Only allow magic-enter once per cycle
  # Go to the last line, open a new line, and enter insert mode
  execute "normal! G"
  feedkeys("o")
enddef

def StartHourglass(): void
  # Make this a local variable, so that repeated Hourglass() calls
  # use the constant value from this closure
  const ourbuf: number = bufnr("%")
  b:start_time = reltime()
  # Start the timer for the waiting message
  b:timer_id = timer_start(1000,
                (id) => Hourglass(ourbuf, id), {"repeat": -1})
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

  var pastmeta: bool
  for lnum in range(1, line("$"))
    const flevel: number = foldlevel(lnum)
    if flevel == 0
      pastmeta = false
      Flushparts("user", lnum)        # Look for markdown marker
    elseif flevel == 1 || pastmeta || getline(lnum) =~ '^\s*```'
      pastmeta = true
      Flushparts("model", lnum)
    endif
  endfor
  Flushparts("", 0)

  if empty(history) || history[-1].role != "user" ||
     empty(trim(history[-1].parts[0].text))
    return    # Nothing to send
  endif

  # Disable buffer modifications while waiting for the LLM response
  setlocal nomodifiable

  StartHourglass()

  b:response_start_line = 0
  # Send the JSON history to the stdin of the regurge process
  ch_sendraw(job_getchannel(b:job_obj), json_encode(history) .. "\n")
  echohl Normal | echo "Sent message to regurge..."

  setlocal nomodified
enddef

def UpdateBuffer(response: list<string>, metadata: list<string>,
                 active: bool): void
  b:partial_msg = ""	      # Received whole message, so clear it
  const finalmsg = !empty(metadata)
  if (finalmsg)
    # Stop the timer
    timer_stop(b:timer_id)
  endif

  # Clear the status field (only visible if the buffer is active)
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
    # Placeholder
    extend(metadata, [ "{" .. resptime, "}" ])
  endif
  if b:response_start_line == 0
    append(start_line - 1, metadata)
    end_meta_line = line("$")
  else
    append(start_line, metadata)
    # Delete the first line of the old metadata
    deletebufline(ourbuf, start_line)
    end_meta_line = start_line + len(metadata) - 1
    # Delete the last line of the old metadata
    deletebufline(ourbuf, end_meta_line)
  endif
  var end_line: number = line("$")
  if b:response_start_line == 0
    if !finalmsg
      add(response, "...")
    endif
    append(end_line, response)
  else
    # Insert just before the ... trailing line
    end_line -= 1
    append(end_line, response)
    setline(end_line, [getline(end_line) .. getline(end_line + 1)])
    deletebufline(ourbuf, end_line + 1)
    if finalmsg
      # Delete ... trailer
      deletebufline(ourbuf, line("$"))
    endif
  endif
  end_line = line("$")

  if b:response_start_line == 0
    const cmdprefix: string = ":" .. start_line

    def Dofoldop(cmdtail: string): void
      execute cmdprefix .. cmdtail
    enddef

    # Create a fold for the newly added model response
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
    # Fold first-level markdown quoted code snippets
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
          if lnum - lstart <= Getgvar("autofold_code", default_autofold_code)
            execute printf(":%dfoldopen", lstart)
          endif
        endif
      endif
    endwhile
  endif

  if active
    Show_foldcolours()
  endif
  if b:response_start_line == 0
    b:response_start_line = start_line
    # Ensure the screen updates and scrolls to the new content
    cursor(start_line - 1, 1)
    normal! zt
    cursor(start_line, 1)
    redraw
  endif
  if finalmsg
    # Pressing a mere enter jumps to the end, new line, insert mode
    nnoremap <buffer> <silent> <CR> <cmd>call <SID>EntertoType()<CR>
  else
    setlocal nomodifiable
  endif
enddef

def MsgfromLLM(curchan: channel, msg: string, ourbuf: number): void
  # Callback function for stdout from the regurge process
  if !bufexists(ourbuf)   # guard against race conditions
    return
  endif

  const fullmsg: string = getbufvar(ourbuf, "partial_msg", "") .. msg
  var json_parts: list<any>

  try
    json_parts = json_decode(fullmsg)
    if empty(json_parts)
      throw "E491:"
    endif
  catch /E491:/
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

  const original_buf: number = bufnr("%")

  if ourbuf == original_buf
    UpdateBuffer(model_response_text, model_metadata, true)
  else
    const original_pos: list<number> = getcurpos()[1 : ]
    const noa_b: string = "noautocmd buffer "
    try
      # Switch to the target buffer to perform updates.
      execute noa_b .. ourbuf
      UpdateBuffer(model_response_text, model_metadata, false)
    catch
    finally
      # Always try to return to the buffer the user was in
      execute noa_b .. original_buf
      cursor(original_pos)
    endtry
  endif

  if !empty(model_metadata)
    # This echo will appear in the original buffer
    echohl Normal |
     echo printf("%s %d, ResponseTime: %d",
                 getbufvar(ourbuf, "persona"), ourbuf,
                 json_decode(join(model_metadata)).ResponseTime)
  endif
enddef

def ErrorfromLLM(curchan: channel, msg: string, ourbuf: number): void
  # Callback function for stderr from the regurge process
  # Must be specified, otherwise vim will choke on stderr output
  echohl ErrorMsg | echomsg printf("%s %d %s", extname, ourbuf, msg)
enddef

def Helperclosed(ourbuf: number): void
  timer_stop(getbufvar(ourbuf, "timer_id", 0))
  setbufvar(ourbuf, '&modifiable', 1)
  echohl ErrorMsg |
   echomsg printf("%s %d helper process died.", extname, ourbuf)
enddef

def ResetChat(fullreset: bool): void
  # Only perform this on Regurge buffers
  if empty(get(b:, "regurge_model", ""))
    return
  endif
  var lnum: number = 1
  while foldlevel(lnum) == 1
    lnum += 1
  endwhile
  const original_pos: list<number> = getcurpos()[1 : ]
  cursor(lnum, 1)
  normal! zt
  cursor(original_pos)
  # Ensure the screen updates and scrolls to the new content
  redraw
  const ourbuf: number = bufnr("%")
  while lnum <= line("$")
    if foldlevel(lnum) != 0 || fullreset
      deletebufline(ourbuf, lnum)
    else
      lnum += 1
    endif
  endwhile
  if fullreset
    EntertoType()
  endif
enddef

def AbortResponse(): void
  # Only perform this on Regurge buffers
  if empty(get(b:, "regurge_model", ""))
    return
  endif
  const job_obj: job = get(b:, "job_obj", null_job)
  if job_status(job_obj) == "run"
    if foldlevel(line("$")) == 0
      job_stop(job_obj)		    # Still no output
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

# Define the user command to start the chat
command! -nargs=? Regurge call Regurge(<f-args>)
# Define a shorthand alias
command! -nargs=? R Regurge(<f-args>)
