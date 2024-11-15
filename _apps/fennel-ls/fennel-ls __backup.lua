--#!/usr/bin/env lua
package.preload["fennel"] = package.preload["fennel"] or function(...)
  -- SPDX-License-Identifier: MIT
  -- SPDX-FileCopyrightText: Calvin Rose and contributors
  package.preload["fennel.repl"] = package.preload["fennel.repl"] or function(...)
    local utils = require("fennel.utils")
    local parser = require("fennel.parser")
    local compiler = require("fennel.compiler")
    local specials = require("fennel.specials")
    local view = require("fennel.view")
    local depth = 0
    local function prompt_for(top_3f)
      if top_3f then
        return (string.rep(">", (depth + 1)) .. " ")
      else
        return (string.rep(".", (depth + 1)) .. " ")
      end
    end
    local function default_read_chunk(parser_state)
      io.write(prompt_for((0 == parser_state["stack-size"])))
      io.flush()
      local input = io.read()
      return (input and (input .. "\n"))
    end
    local function default_on_values(xs)
      io.write(table.concat(xs, "\9"))
      return io.write("\n")
    end
    local function default_on_error(errtype, err)
      local function _702_()
        local _701_0 = errtype
        if (_701_0 == "Runtime") then
          return (compiler.traceback(tostring(err), 4) .. "\n")
        else
          local _ = _701_0
          return ("%s error: %s\n"):format(errtype, tostring(err))
        end
      end
      return io.write(_702_())
    end
    local function splice_save_locals(env, lua_source, scope)
      local saves = nil
      do
        local tbl_17_ = {}
        local i_18_ = #tbl_17_
        for name in pairs(env.___replLocals___) do
          local val_19_ = ("local %s = ___replLocals___[%q]"):format((scope.manglings[name] or name), name)
          if (nil ~= val_19_) then
            i_18_ = (i_18_ + 1)
            tbl_17_[i_18_] = val_19_
          end
        end
        saves = tbl_17_
      end
      local binds = nil
      do
        local tbl_17_ = {}
        local i_18_ = #tbl_17_
        for raw, name in pairs(scope.manglings) do
          local val_19_ = nil
          if not scope.gensyms[name] then
            val_19_ = ("___replLocals___[%q] = %s"):format(raw, name)
          else
          val_19_ = nil
          end
          if (nil ~= val_19_) then
            i_18_ = (i_18_ + 1)
            tbl_17_[i_18_] = val_19_
          end
        end
        binds = tbl_17_
      end
      local gap = nil
      if lua_source:find("\n") then
        gap = "\n"
      else
        gap = " "
      end
      local function _708_()
        if next(saves) then
          return (table.concat(saves, " ") .. gap)
        else
          return ""
        end
      end
      local function _711_()
        local _709_0, _710_0 = lua_source:match("^(.*)[\n ](return .*)$")
        if ((nil ~= _709_0) and (nil ~= _710_0)) then
          local body = _709_0
          local _return = _710_0
          return (body .. gap .. table.concat(binds, " ") .. gap .. _return)
        else
          local _ = _709_0
          return lua_source
        end
      end
      return (_708_() .. _711_())
    end
    local commands = {}
    local function completer(env, scope, text, _3ffulltext, _from, _to)
      local max_items = 2000
      local seen = {}
      local matches = {}
      local input_fragment = text:gsub(".*[%s)(]+", "")
      local stop_looking_3f = false
      local function add_partials(input, tbl, prefix)
        local scope_first_3f = ((tbl == env) or (tbl == env.___replLocals___))
        local tbl_17_ = matches
        local i_18_ = #tbl_17_
        local function _713_()
          if scope_first_3f then
            return scope.manglings
          else
            return tbl
          end
        end
        for k, is_mangled in utils.allpairs(_713_()) do
          if (max_items <= #matches) then break end
          local val_19_ = nil
          do
            local lookup_k = nil
            if scope_first_3f then
              lookup_k = is_mangled
            else
              lookup_k = k
            end
            if ((type(k) == "string") and (input == k:sub(0, #input)) and not seen[k] and ((":" ~= prefix:sub(-1)) or ("function" == type(tbl[lookup_k])))) then
              seen[k] = true
              val_19_ = (prefix .. k)
            else
            val_19_ = nil
            end
          end
          if (nil ~= val_19_) then
            i_18_ = (i_18_ + 1)
            tbl_17_[i_18_] = val_19_
          end
        end
        return tbl_17_
      end
      local function descend(input, tbl, prefix, add_matches, method_3f)
        local splitter = nil
        if method_3f then
          splitter = "^([^:]+):(.*)"
        else
          splitter = "^([^.]+)%.(.*)"
        end
        local head, tail = input:match(splitter)
        local raw_head = (scope.manglings[head] or head)
        if (type(tbl[raw_head]) == "table") then
          stop_looking_3f = true
          if method_3f then
            return add_partials(tail, tbl[raw_head], (prefix .. head .. ":"))
          else
            return add_matches(tail, tbl[raw_head], (prefix .. head))
          end
        end
      end
      local function add_matches(input, tbl, prefix)
        local prefix0 = nil
        if prefix then
          prefix0 = (prefix .. ".")
        else
          prefix0 = ""
        end
        if (not input:find("%.") and input:find(":")) then
          return descend(input, tbl, prefix0, add_matches, true)
        elseif not input:find("%.") then
          return add_partials(input, tbl, prefix0)
        else
          return descend(input, tbl, prefix0, add_matches, false)
        end
      end
      do
        local _722_0 = tostring((_3ffulltext or text)):match("^%s*,([^%s()[%]]*)$")
        if (nil ~= _722_0) then
          local cmd_fragment = _722_0
          add_partials(cmd_fragment, commands, ",")
        else
          local _ = _722_0
          for _0, source in ipairs({scope.specials, scope.macros, (env.___replLocals___ or {}), env, env._G}) do
            if stop_looking_3f then break end
            add_matches(input_fragment, source)
          end
        end
      end
      return matches
    end
    local function command_3f(input)
      return input:match("^%s*,")
    end
    local function command_docs()
      local _724_
      do
        local tbl_17_ = {}
        local i_18_ = #tbl_17_
        for name, f in utils.stablepairs(commands) do
          local val_19_ = ("  ,%s - %s"):format(name, ((compiler.metadata):get(f, "fnl/docstring") or "undocumented"))
          if (nil ~= val_19_) then
            i_18_ = (i_18_ + 1)
            tbl_17_[i_18_] = val_19_
          end
        end
        _724_ = tbl_17_
      end
      return table.concat(_724_, "\n")
    end
    commands.help = function(_, _0, on_values)
      return on_values({("Welcome to Fennel.\nThis is the REPL where you can enter code to be evaluated.\nYou can also run these repl commands:\n\n" .. command_docs() .. "\n  ,return FORM - Evaluate FORM and return its value to the REPL's caller.\n  ,exit - Leave the repl.\n\nUse ,doc something to see descriptions for individual macros and special forms.\nValues from previous inputs are kept in *1, *2, and *3.\n\nFor more information about the language, see https://fennel-lang.org/reference")})
    end
    do end (compiler.metadata):set(commands.help, "fnl/docstring", "Show this message.")
    local function reload(module_name, env, on_values, on_error)
      local _726_0, _727_0 = pcall(specials["load-code"]("return require(...)", env), module_name)
      if ((_726_0 == true) and (nil ~= _727_0)) then
        local old = _727_0
        local _ = nil
        package.loaded[module_name] = nil
        _ = nil
        local new = nil
        do
          local _728_0, _729_0 = pcall(require, module_name)
          if ((_728_0 == true) and (nil ~= _729_0)) then
            local new0 = _729_0
            new = new0
          elseif (true and (nil ~= _729_0)) then
            local _0 = _728_0
            local msg = _729_0
            on_error("Repl", msg)
            new = old
          else
          new = nil
          end
        end
        specials["macro-loaded"][module_name] = nil
        if ((type(old) == "table") and (type(new) == "table")) then
          for k, v in pairs(new) do
            old[k] = v
          end
          for k in pairs(old) do
            if (nil == new[k]) then
              old[k] = nil
            end
          end
          package.loaded[module_name] = old
        end
        return on_values({"ok"})
      elseif ((_726_0 == false) and (nil ~= _727_0)) then
        local msg = _727_0
        if msg:match("loop or previous error loading module") then
          package.loaded[module_name] = nil
          return reload(module_name, env, on_values, on_error)
        elseif specials["macro-loaded"][module_name] then
          specials["macro-loaded"][module_name] = nil
          return nil
        else
          local function _734_()
            local _733_0 = msg:gsub("\n.*", "")
            return _733_0
          end
          return on_error("Runtime", _734_())
        end
      end
    end
    local function run_command(read, on_error, f)
      local _737_0, _738_0, _739_0 = pcall(read)
      if ((_737_0 == true) and (_738_0 == true) and (nil ~= _739_0)) then
        local val = _739_0
        local _740_0, _741_0 = pcall(f, val)
        if ((_740_0 == false) and (nil ~= _741_0)) then
          local msg = _741_0
          return on_error("Runtime", msg)
        end
      elseif (_737_0 == false) then
        return on_error("Parse", "Couldn't parse input.")
      end
    end
    commands.reload = function(env, read, on_values, on_error)
      local function _744_(_241)
        return reload(tostring(_241), env, on_values, on_error)
      end
      return run_command(read, on_error, _744_)
    end
    do end (compiler.metadata):set(commands.reload, "fnl/docstring", "Reload the specified module.")
    commands.reset = function(env, _, on_values)
      env.___replLocals___ = {}
      return on_values({"ok"})
    end
    do end (compiler.metadata):set(commands.reset, "fnl/docstring", "Erase all repl-local scope.")
    commands.complete = function(env, read, on_values, on_error, scope, chars)
      local function _745_()
        return on_values(completer(env, scope, table.concat(chars):gsub("^%s*,complete%s+", ""):sub(1, -2)))
      end
      return run_command(read, on_error, _745_)
    end
    do end (compiler.metadata):set(commands.complete, "fnl/docstring", "Print all possible completions for a given input symbol.")
    local function apropos_2a(pattern, tbl, prefix, seen, names)
      for name, subtbl in pairs(tbl) do
        if (("string" == type(name)) and (package ~= subtbl)) then
          local _746_0 = type(subtbl)
          if (_746_0 == "function") then
            if ((prefix .. name)):match(pattern) then
              table.insert(names, (prefix .. name))
            end
          elseif (_746_0 == "table") then
            if not seen[subtbl] then
              local _748_
              do
                seen[subtbl] = true
                _748_ = seen
              end
              apropos_2a(pattern, subtbl, (prefix .. name:gsub("%.", "/") .. "."), _748_, names)
            end
          end
        end
      end
      return names
    end
    local function apropos(pattern)
      return apropos_2a(pattern:gsub("^_G%.", ""), package.loaded, "", {}, {})
    end
    commands.apropos = function(_env, read, on_values, on_error, _scope)
      local function _752_(_241)
        return on_values(apropos(tostring(_241)))
      end
      return run_command(read, on_error, _752_)
    end
    do end (compiler.metadata):set(commands.apropos, "fnl/docstring", "Print all functions matching a pattern in all loaded modules.")
    local function apropos_follow_path(path)
      local paths = nil
      do
        local tbl_17_ = {}
        local i_18_ = #tbl_17_
        for p in path:gmatch("[^%.]+") do
          local val_19_ = p
          if (nil ~= val_19_) then
            i_18_ = (i_18_ + 1)
            tbl_17_[i_18_] = val_19_
          end
        end
        paths = tbl_17_
      end
      local tgt = package.loaded
      for _, path0 in ipairs(paths) do
        if (nil == tgt) then break end
        local _755_
        do
          local _754_0 = path0:gsub("%/", ".")
          _755_ = _754_0
        end
        tgt = tgt[_755_]
      end
      return tgt
    end
    local function apropos_doc(pattern)
      local tbl_17_ = {}
      local i_18_ = #tbl_17_
      for _, path in ipairs(apropos(".*")) do
        local val_19_ = nil
        do
          local tgt = apropos_follow_path(path)
          if ("function" == type(tgt)) then
            local _756_0 = (compiler.metadata):get(tgt, "fnl/docstring")
            if (nil ~= _756_0) then
              local docstr = _756_0
              val_19_ = (docstr:match(pattern) and path)
            else
            val_19_ = nil
            end
          else
          val_19_ = nil
          end
        end
        if (nil ~= val_19_) then
          i_18_ = (i_18_ + 1)
          tbl_17_[i_18_] = val_19_
        end
      end
      return tbl_17_
    end
    commands["apropos-doc"] = function(_env, read, on_values, on_error, _scope)
      local function _760_(_241)
        return on_values(apropos_doc(tostring(_241)))
      end
      return run_command(read, on_error, _760_)
    end
    do end (compiler.metadata):set(commands["apropos-doc"], "fnl/docstring", "Print all functions that match the pattern in their docs")
    local function apropos_show_docs(on_values, pattern)
      for _, path in ipairs(apropos(pattern)) do
        local tgt = apropos_follow_path(path)
        if (("function" == type(tgt)) and (compiler.metadata):get(tgt, "fnl/docstring")) then
          on_values({specials.doc(tgt, path)})
          on_values({})
        end
      end
      return nil
    end
    commands["apropos-show-docs"] = function(_env, read, on_values, on_error)
      local function _762_(_241)
        return apropos_show_docs(on_values, tostring(_241))
      end
      return run_command(read, on_error, _762_)
    end
    do end (compiler.metadata):set(commands["apropos-show-docs"], "fnl/docstring", "Print all documentations matching a pattern in function name")
    local function resolve(identifier, _763_0, scope)
      local _764_ = _763_0
      local env = _764_
      local ___replLocals___ = _764_["___replLocals___"]
      local e = nil
      local function _765_(_241, _242)
        return (___replLocals___[scope.unmanglings[_242]] or env[_242])
      end
      e = setmetatable({}, {__index = _765_})
      local function _766_(...)
        local _767_0, _768_0 = ...
        if ((_767_0 == true) and (nil ~= _768_0)) then
          local code = _768_0
          local function _769_(...)
            local _770_0, _771_0 = ...
            if ((_770_0 == true) and (nil ~= _771_0)) then
              local val = _771_0
              return val
            else
              local _ = _770_0
              return nil
            end
          end
          return _769_(pcall(specials["load-code"](code, e)))
        else
          local _ = _767_0
          return nil
        end
      end
      return _766_(pcall(compiler["compile-string"], tostring(identifier), {scope = scope}))
    end
    commands.find = function(env, read, on_values, on_error, scope)
      local function _774_(_241)
        local _775_0 = nil
        do
          local _776_0 = utils["sym?"](_241)
          if (nil ~= _776_0) then
            local _777_0 = resolve(_776_0, env, scope)
            if (nil ~= _777_0) then
              _775_0 = debug.getinfo(_777_0)
            else
              _775_0 = _777_0
            end
          else
            _775_0 = _776_0
          end
        end
        if ((_G.type(_775_0) == "table") and (nil ~= _775_0.linedefined) and (nil ~= _775_0.short_src) and (nil ~= _775_0.source) and (_775_0.what == "Lua")) then
          local line = _775_0.linedefined
          local src = _775_0.short_src
          local source = _775_0.source
          local fnlsrc = nil
          do
            local _780_0 = compiler.sourcemap
            if (nil ~= _780_0) then
              _780_0 = _780_0[source]
            end
            if (nil ~= _780_0) then
              _780_0 = _780_0[line]
            end
            if (nil ~= _780_0) then
              _780_0 = _780_0[2]
            end
            fnlsrc = _780_0
          end
          return on_values({string.format("%s:%s", src, (fnlsrc or line))})
        elseif (_775_0 == nil) then
          return on_error("Repl", "Unknown value")
        else
          local _ = _775_0
          return on_error("Repl", "No source info")
        end
      end
      return run_command(read, on_error, _774_)
    end
    do end (compiler.metadata):set(commands.find, "fnl/docstring", "Print the filename and line number for a given function")
    commands.doc = function(env, read, on_values, on_error, scope)
      local function _785_(_241)
        local name = tostring(_241)
        local path = (utils["multi-sym?"](name) or {name})
        local ok_3f, target = nil, nil
        local function _786_()
          return (scope.specials[name] or utils["get-in"](scope.macros, path) or resolve(name, env, scope))
        end
        ok_3f, target = pcall(_786_)
        if ok_3f then
          return on_values({specials.doc(target, name)})
        else
          return on_error("Repl", ("Could not find " .. name .. " for docs."))
        end
      end
      return run_command(read, on_error, _785_)
    end
    do end (compiler.metadata):set(commands.doc, "fnl/docstring", "Print the docstring and arglist for a function, macro, or special form.")
    commands.compile = function(_, read, on_values, on_error, _0, _1, opts)
      local function _788_(_241)
        local _789_0, _790_0 = pcall(compiler.compile, _241, opts)
        if ((_789_0 == true) and (nil ~= _790_0)) then
          local result = _790_0
          return on_values({result})
        elseif (true and (nil ~= _790_0)) then
          local _2 = _789_0
          local msg = _790_0
          return on_error("Repl", ("Error compiling expression: " .. msg))
        end
      end
      return run_command(read, on_error, _788_)
    end
    do end (compiler.metadata):set(commands.compile, "fnl/docstring", "compiles the expression into lua and prints the result.")
    local function load_plugin_commands(plugins)
      for i = #(plugins or {}), 1, -1 do
        for name, f in pairs(plugins[i]) do
          local _792_0 = name:match("^repl%-command%-(.*)")
          if (nil ~= _792_0) then
            local cmd_name = _792_0
            commands[cmd_name] = f
          end
        end
      end
      return nil
    end
    local function run_command_loop(input, read, loop, env, on_values, on_error, scope, chars, opts)
      local command_name = input:match(",([^%s/]+)")
      do
        local _794_0 = commands[command_name]
        if (nil ~= _794_0) then
          local command = _794_0
          command(env, read, on_values, on_error, scope, chars, opts)
        else
          local _ = _794_0
          if ((command_name ~= "exit") and (command_name ~= "return")) then
            on_values({"Unknown command", command_name})
          end
        end
      end
      if ("exit" ~= command_name) then
        return loop((command_name == "return"))
      end
    end
    local function try_readline_21(opts, ok, readline)
      if ok then
        if readline.set_readline_name then
          readline.set_readline_name("fennel")
        end
        readline.set_options({histfile = "", keeplines = 1000})
        opts.readChunk = function(parser_state)
          local prompt = nil
          if (0 < parser_state["stack-size"]) then
            prompt = ".. "
          else
            prompt = ">> "
          end
          local str = readline.readline(prompt)
          if str then
            return (str .. "\n")
          end
        end
        local completer0 = nil
        opts.registerCompleter = function(repl_completer)
          completer0 = repl_completer
          return nil
        end
        local function repl_completer(text, from, to)
          if completer0 then
            readline.set_completion_append_character("")
            return completer0(text:sub(from, to), text, from, to)
          else
            return {}
          end
        end
        readline.set_complete_function(repl_completer)
        return readline
      end
    end
    local function should_use_readline_3f(opts)
      return (("dumb" ~= os.getenv("TERM")) and not opts.readChunk and not opts.registerCompleter)
    end
    local function repl(_3foptions)
      local old_root_options = utils.root.options
      local _803_ = utils.copy(_3foptions)
      local opts = _803_
      local _3ffennelrc = _803_["fennelrc"]
      local _ = nil
      opts.fennelrc = nil
      _ = nil
      local readline = (should_use_readline_3f(opts) and try_readline_21(opts, pcall(require, "readline")))
      local _0 = nil
      if _3ffennelrc then
        _0 = _3ffennelrc()
      else
      _0 = nil
      end
      local env = specials["wrap-env"]((opts.env or rawget(_G, "_ENV") or _G))
      local callbacks = {["view-opts"] = (opts["view-opts"] or {depth = 4}), env = env, onError = (opts.onError or default_on_error), onValues = (opts.onValues or default_on_values), pp = (opts.pp or view), readChunk = (opts.readChunk or default_read_chunk)}
      local save_locals_3f = (opts.saveLocals ~= false)
      local byte_stream, clear_stream = nil, nil
      local function _805_(_241)
        return callbacks.readChunk(_241)
      end
      byte_stream, clear_stream = parser.granulate(_805_)
      local chars = {}
      local read, reset = nil, nil
      local function _806_(parser_state)
        local b = byte_stream(parser_state)
        if b then
          table.insert(chars, string.char(b))
        end
        return b
      end
      read, reset = parser.parser(_806_)
      depth = (depth + 1)
      if opts.message then
        callbacks.onValues({opts.message})
      end
      env.___repl___ = callbacks
      opts.env, opts.scope = env, compiler["make-scope"]()
      opts.useMetadata = (opts.useMetadata ~= false)
      if (opts.allowedGlobals == nil) then
        opts.allowedGlobals = specials["current-global-names"](env)
      end
      if opts.init then
        opts.init(opts, depth)
      end
      if opts.registerCompleter then
        local function _812_()
          local _811_0 = opts.scope
          local function _813_(...)
            return completer(env, _811_0, ...)
          end
          return _813_
        end
        opts.registerCompleter(_812_())
      end
      load_plugin_commands(opts.plugins)
      if save_locals_3f then
        local function newindex(t, k, v)
          if opts.scope.manglings[k] then
            return rawset(t, k, v)
          end
        end
        env.___replLocals___ = setmetatable({}, {__newindex = newindex})
      end
      local function print_values(...)
        local vals = {...}
        local out = {}
        local pp = callbacks.pp
        env._, env.__ = vals[1], vals
        for i = 1, select("#", ...) do
          table.insert(out, pp(vals[i], callbacks["view-opts"]))
        end
        return callbacks.onValues(out)
      end
      local function save_value(...)
        env.___replLocals___["*3"] = env.___replLocals___["*2"]
        env.___replLocals___["*2"] = env.___replLocals___["*1"]
        env.___replLocals___["*1"] = ...
        return ...
      end
      opts.scope.manglings["*1"], opts.scope.unmanglings._1 = "_1", "*1"
      opts.scope.manglings["*2"], opts.scope.unmanglings._2 = "_2", "*2"
      opts.scope.manglings["*3"], opts.scope.unmanglings._3 = "_3", "*3"
      local function loop(exit_next_3f)
        for k in pairs(chars) do
          chars[k] = nil
        end
        reset()
        local ok, parser_not_eof_3f, form = pcall(read)
        local src_string = table.concat(chars)
        local readline_not_eof_3f = (not readline or (src_string ~= "(null)"))
        local not_eof_3f = (readline_not_eof_3f and parser_not_eof_3f)
        if not ok then
          callbacks.onError("Parse", not_eof_3f)
          clear_stream()
          return loop()
        elseif command_3f(src_string) then
          return run_command_loop(src_string, read, loop, env, callbacks.onValues, callbacks.onError, opts.scope, chars, opts)
        else
          if not_eof_3f then
            local function _817_(...)
              local _818_0, _819_0 = ...
              if ((_818_0 == true) and (nil ~= _819_0)) then
                local src = _819_0
                local function _820_(...)
                  local _821_0, _822_0 = ...
                  if ((_821_0 == true) and (nil ~= _822_0)) then
                    local chunk = _822_0
                    local function _823_()
                      return print_values(save_value(chunk()))
                    end
                    local function _824_(...)
                      return callbacks.onError("Runtime", ...)
                    end
                    return xpcall(_823_, _824_)
                  elseif ((_821_0 == false) and (nil ~= _822_0)) then
                    local msg = _822_0
                    clear_stream()
                    return callbacks.onError("Compile", msg)
                  end
                end
                local function _827_(...)
                  local src0 = nil
                  if save_locals_3f then
                    src0 = splice_save_locals(env, src, opts.scope)
                  else
                    src0 = src
                  end
                  return pcall(specials["load-code"], src0, env)
                end
                return _820_(_827_(...))
              elseif ((_818_0 == false) and (nil ~= _819_0)) then
                local msg = _819_0
                clear_stream()
                return callbacks.onError("Compile", msg)
              end
            end
            local function _829_()
              opts["source"] = src_string
              return opts
            end
            _817_(pcall(compiler.compile, form, _829_()))
            utils.root.options = old_root_options
            if exit_next_3f then
              return env.___replLocals___["*1"]
            else
              return loop()
            end
          end
        end
      end
      local value = loop()
      depth = (depth - 1)
      if readline then
        readline.save_history()
      end
      if opts.exit then
        opts.exit(opts, depth)
      end
      return value
    end
    local function _835_(overrides, _3fopts)
      return repl(utils.copy(_3fopts, utils.copy(overrides)))
    end
    return setmetatable({}, {__call = _835_, __index = {repl = repl}})
  end
  package.preload["fennel.specials"] = package.preload["fennel.specials"] or function(...)
    local utils = require("fennel.utils")
    local view = require("fennel.view")
    local parser = require("fennel.parser")
    local compiler = require("fennel.compiler")
    local unpack = (table.unpack or _G.unpack)
    local SPECIALS = compiler.scopes.global.specials
    local function str1(x)
      return tostring(x[1])
    end
    local function wrap_env(env)
      local function _475_(_, key)
        if utils["string?"](key) then
          return env[compiler["global-unmangling"](key)]
        else
          return env[key]
        end
      end
      local function _477_(_, key, value)
        if utils["string?"](key) then
          env[compiler["global-unmangling"](key)] = value
          return nil
        else
          env[key] = value
          return nil
        end
      end
      local function _479_()
        local _480_
        do
          local tbl_14_ = {}
          for k, v in utils.stablepairs(env) do
            local k_15_, v_16_ = nil, nil
            local _481_
            if utils["string?"](k) then
              _481_ = compiler["global-unmangling"](k)
            else
              _481_ = k
            end
            k_15_, v_16_ = _481_, v
            if ((k_15_ ~= nil) and (v_16_ ~= nil)) then
              tbl_14_[k_15_] = v_16_
            end
          end
          _480_ = tbl_14_
        end
        return next, _480_, nil
      end
      return setmetatable({}, {__index = _475_, __newindex = _477_, __pairs = _479_})
    end
    local function fennel_module_name()
      return (utils.root.options.moduleName or "fennel")
    end
    local function current_global_names(_3fenv)
      local mt = nil
      do
        local _484_0 = getmetatable(_3fenv)
        if ((_G.type(_484_0) == "table") and (nil ~= _484_0.__pairs)) then
          local mtpairs = _484_0.__pairs
          local tbl_14_ = {}
          for k, v in mtpairs(_3fenv) do
            local k_15_, v_16_ = k, v
            if ((k_15_ ~= nil) and (v_16_ ~= nil)) then
              tbl_14_[k_15_] = v_16_
            end
          end
          mt = tbl_14_
        elseif (_484_0 == nil) then
          mt = (_3fenv or _G)
        else
        mt = nil
        end
      end
      local function _487_()
        local tbl_17_ = {}
        local i_18_ = #tbl_17_
        for k in utils.stablepairs(mt) do
          local val_19_ = compiler["global-unmangling"](k)
          if (nil ~= val_19_) then
            i_18_ = (i_18_ + 1)
            tbl_17_[i_18_] = val_19_
          end
        end
        return tbl_17_
      end
      return (mt and _487_())
    end
    local function load_code(code, _3fenv, _3ffilename)
      local env = (_3fenv or rawget(_G, "_ENV") or _G)
      local _489_0, _490_0 = rawget(_G, "setfenv"), rawget(_G, "loadstring")
      if ((nil ~= _489_0) and (nil ~= _490_0)) then
        local setfenv = _489_0
        local loadstring = _490_0
        local f = assert(loadstring(code, _3ffilename))
        setfenv(f, env)
        return f
      else
        local _ = _489_0
        return assert(load(code, _3ffilename, "t", env))
      end
    end
    local function v__3edocstring(tgt)
      return (((compiler.metadata):get(tgt, "fnl/docstring") or "#<undocumented>")):gsub("\n$", ""):gsub("\n", "\n  ")
    end
    local function doc_2a(tgt, name)
      assert(("string" == type(name)), "name must be a string")
      if not tgt then
        return (name .. " not found")
      else
        local function _493_()
          local _492_0 = getmetatable(tgt)
          if ((_G.type(_492_0) == "table") and true) then
            local __call = _492_0.__call
            return ("function" == type(__call))
          end
        end
        if ((type(tgt) == "function") or _493_()) then
          local elts = {name, unpack(((compiler.metadata):get(tgt, "fnl/arglist") or {"#<unknown-arguments>"}))}
          return string.format("(%s)\n  %s", table.concat(elts, " "), v__3edocstring(tgt))
        else
          return string.format("%s\n  %s", name, v__3edocstring(tgt))
        end
      end
    end
    local function doc_special(name, arglist, docstring, body_form_3f)
      compiler.metadata[SPECIALS[name]] = {["fnl/arglist"] = arglist, ["fnl/body-form?"] = body_form_3f, ["fnl/docstring"] = docstring}
      return nil
    end
    local function compile_do(ast, scope, parent, _3fstart)
      local start = (_3fstart or 2)
      local len = #ast
      local sub_scope = compiler["make-scope"](scope)
      for i = start, len do
        compiler.compile1(ast[i], sub_scope, parent, {nval = 0})
      end
      return nil
    end
    SPECIALS["do"] = function(ast, scope, parent, opts, _3fstart, _3fchunk, _3fsub_scope, _3fpre_syms)
      local start = (_3fstart or 2)
      local sub_scope = (_3fsub_scope or compiler["make-scope"](scope))
      local chunk = (_3fchunk or {})
      local len = #ast
      local retexprs = {returned = true}
      utils.hook("pre-do", ast, sub_scope)
      local function compile_body(outer_target, outer_tail, outer_retexprs)
        for i = start, len do
          local subopts = {nval = (((i ~= len) and 0) or opts.nval), tail = (((i == len) and outer_tail) or nil), target = (((i == len) and outer_target) or nil)}
          local _ = utils["propagate-options"](opts, subopts)
          local subexprs = compiler.compile1(ast[i], sub_scope, chunk, subopts)
          if (i ~= len) then
            compiler["keep-side-effects"](subexprs, parent, nil, ast[i])
          end
        end
        compiler.emit(parent, chunk, ast)
        compiler.emit(parent, "end", ast)
        utils.hook("do", ast, sub_scope)
        return (outer_retexprs or retexprs)
      end
      if (opts.target or (opts.nval == 0) or opts.tail) then
        compiler.emit(parent, "do", ast)
        return compile_body(opts.target, opts.tail)
      elseif opts.nval then
        local syms = {}
        for i = 1, opts.nval do
          local s = ((_3fpre_syms and _3fpre_syms[i]) or compiler.gensym(scope))
          syms[i] = s
          retexprs[i] = utils.expr(s, "sym")
        end
        local outer_target = table.concat(syms, ", ")
        compiler.emit(parent, string.format("local %s", outer_target), ast)
        compiler.emit(parent, "do", ast)
        return compile_body(outer_target, opts.tail)
      else
        local fname = compiler.gensym(scope)
        local fargs = nil
        if scope.vararg then
          fargs = "..."
        else
          fargs = ""
        end
        compiler.emit(parent, string.format("local function %s(%s)", fname, fargs), ast)
        return compile_body(nil, true, utils.expr((fname .. "(" .. fargs .. ")"), "statement"))
      end
    end
    doc_special("do", {"..."}, "Evaluate multiple forms; return last value.", true)
    local function iter_args(ast)
      local ast0, len, i = ast, #ast, 1
      local function _499_()
        i = (1 + i)
        while ((i == len) and utils["call-of?"](ast0[i], "values")) do
          ast0 = ast0[i]
          len = #ast0
          i = 2
        end
        return ast0[i], (nil == ast0[(i + 1)])
      end
      return _499_
    end
    SPECIALS.values = function(ast, scope, parent)
      local exprs = {}
      for subast, last_3f in iter_args(ast) do
        local subexprs = compiler.compile1(subast, scope, parent, {nval = (not last_3f and 1)})
        table.insert(exprs, subexprs[1])
        if last_3f then
          for j = 2, #subexprs do
            table.insert(exprs, subexprs[j])
          end
        end
      end
      return exprs
    end
    doc_special("values", {"..."}, "Return multiple values from a function. Must be in tail position.")
    local function __3estack(stack, tbl)
      for k, v in pairs(tbl) do
        table.insert(stack, k)
        table.insert(stack, v)
      end
      return stack
    end
    local function literal_3f(val)
      local res = true
      if utils["list?"](val) then
        res = false
      elseif utils["table?"](val) then
        local stack = __3estack({}, val)
        for _, elt in ipairs(stack) do
          if not res then break end
          if utils["list?"](elt) then
            res = false
          elseif utils["table?"](elt) then
            __3estack(stack, elt)
          end
        end
      end
      return res
    end
    local function compile_value(v)
      local opts = {nval = 1, tail = false}
      local scope = compiler["make-scope"]()
      local chunk = {}
      local _503_ = compiler.compile1(v, scope, chunk, opts)
      local _504_ = _503_[1]
      local v0 = _504_[1]
      return v0
    end
    local function insert_meta(meta, k, v)
      local view_opts = {["escape-newlines?"] = true, ["line-length"] = math.huge, ["one-line?"] = true}
      compiler.assert((type(k) == "string"), ("expected string keys in metadata table, got: %s"):format(view(k, view_opts)))
      compiler.assert(literal_3f(v), ("expected literal value in metadata table, got: %s %s"):format(view(k, view_opts), view(v, view_opts)))
      table.insert(meta, view(k))
      local function _505_()
        if ("string" == type(v)) then
          return view(v, view_opts)
        else
          return compile_value(v)
        end
      end
      table.insert(meta, _505_())
      return meta
    end
    local function insert_arglist(meta, arg_list)
      local opts = {["escape-newlines?"] = true, ["line-length"] = math.huge, ["one-line?"] = true}
      local view_args = nil
      do
        local tbl_17_ = {}
        local i_18_ = #tbl_17_
        for _, arg in ipairs(arg_list) do
          local val_19_ = view(view(arg, opts))
          if (nil ~= val_19_) then
            i_18_ = (i_18_ + 1)
            tbl_17_[i_18_] = val_19_
          end
        end
        view_args = tbl_17_
      end
      table.insert(meta, "\"fnl/arglist\"")
      table.insert(meta, ("{" .. table.concat(view_args, ", ") .. "}"))
      return meta
    end
    local function set_fn_metadata(f_metadata, parent, fn_name)
      if utils.root.options.useMetadata then
        local meta_fields = {}
        for k, v in utils.stablepairs(f_metadata) do
          if (k == "fnl/arglist") then
            insert_arglist(meta_fields, v)
          else
            insert_meta(meta_fields, k, v)
          end
        end
        local meta_str = ("require(\"%s\").metadata"):format(fennel_module_name())
        return compiler.emit(parent, ("pcall(function() %s:setall(%s, %s) end)"):format(meta_str, fn_name, table.concat(meta_fields, ", ")))
      end
    end
    local function get_fn_name(ast, scope, fn_name, multi)
      if (fn_name and (fn_name[1] ~= "nil")) then
        local _509_
        if not multi then
          _509_ = compiler["declare-local"](fn_name, scope, ast)
        else
          _509_ = compiler["symbol-to-expression"](fn_name, scope)[1]
        end
        return _509_, not multi, 3
      else
        return nil, true, 2
      end
    end
    local function compile_named_fn(ast, f_scope, f_chunk, parent, index, fn_name, local_3f, arg_name_list, f_metadata)
      utils.hook("pre-fn", ast, f_scope)
      for i = (index + 1), #ast do
        compiler.compile1(ast[i], f_scope, f_chunk, {nval = (((i ~= #ast) and 0) or nil), tail = (i == #ast)})
      end
      local _512_
      if local_3f then
        _512_ = "local function %s(%s)"
      else
        _512_ = "%s = function(%s)"
      end
      compiler.emit(parent, string.format(_512_, fn_name, table.concat(arg_name_list, ", ")), ast)
      compiler.emit(parent, f_chunk, ast)
      compiler.emit(parent, "end", ast)
      set_fn_metadata(f_metadata, parent, fn_name)
      utils.hook("fn", ast, f_scope)
      return utils.expr(fn_name, "sym")
    end
    local function compile_anonymous_fn(ast, f_scope, f_chunk, parent, index, arg_name_list, f_metadata, scope)
      local fn_name = compiler.gensym(scope)
      return compile_named_fn(ast, f_scope, f_chunk, parent, index, fn_name, true, arg_name_list, f_metadata)
    end
    local function maybe_metadata(ast, pred, handler, mt, index)
      local index_2a = (index + 1)
      local index_2a_before_ast_end_3f = (index_2a < #ast)
      local expr = ast[index_2a]
      if (index_2a_before_ast_end_3f and pred(expr)) then
        return handler(mt, expr), index_2a
      else
        return mt, index
      end
    end
    local function get_function_metadata(ast, arg_list, index)
      local function _515_(_241, _242)
        local tbl_14_ = _241
        for k, v in pairs(_242) do
          local k_15_, v_16_ = k, v
          if ((k_15_ ~= nil) and (v_16_ ~= nil)) then
            tbl_14_[k_15_] = v_16_
          end
        end
        return tbl_14_
      end
      local function _517_(_241, _242)
        _241["fnl/docstring"] = _242
        return _241
      end
      return maybe_metadata(ast, utils["kv-table?"], _515_, maybe_metadata(ast, utils["string?"], _517_, {["fnl/arglist"] = arg_list}, index))
    end
    SPECIALS.fn = function(ast, scope, parent, opts)
      local f_scope = nil
      do
        local _518_0 = compiler["make-scope"](scope)
        _518_0["vararg"] = false
        f_scope = _518_0
      end
      local f_chunk = {}
      local fn_sym = utils["sym?"](ast[2])
      local multi = (fn_sym and utils["multi-sym?"](fn_sym[1]))
      local fn_name, local_3f, index = get_fn_name(ast, scope, fn_sym, multi, opts)
      local arg_list = compiler.assert(utils["table?"](ast[index]), "expected parameters table", ast)
      compiler.assert((not multi or not multi["multi-sym-method-call"]), ("unexpected multi symbol " .. tostring(fn_name)), fn_sym)
      if (multi and not scope.symmeta[multi[1]] and not compiler["global-allowed?"](multi[1])) then
        compiler.assert(nil, ("expected local table " .. multi[1]), ast[2])
      end
      local function destructure_arg(arg)
        local raw = utils.sym(compiler.gensym(scope))
        local declared = compiler["declare-local"](raw, f_scope, ast)
        compiler.destructure(arg, raw, ast, f_scope, f_chunk, {declaration = true, nomulti = true, symtype = "arg"})
        return declared
      end
      local function destructure_amp(i)
        compiler.assert((i == (#arg_list - 1)), "expected rest argument before last parameter", arg_list[(i + 1)], arg_list)
        f_scope.vararg = true
        compiler.destructure(arg_list[#arg_list], {utils.varg()}, ast, f_scope, f_chunk, {declaration = true, nomulti = true, symtype = "arg"})
        return "..."
      end
      local function get_arg_name(arg, i)
        if f_scope.vararg then
          return nil
        elseif utils["varg?"](arg) then
          compiler.assert((arg == arg_list[#arg_list]), "expected vararg as last parameter", ast)
          f_scope.vararg = true
          return "..."
        elseif utils["sym?"](arg, "&") then
          return destructure_amp(i)
        elseif (utils["sym?"](arg) and (tostring(arg) ~= "nil") and not utils["multi-sym?"](tostring(arg))) then
          return compiler["declare-local"](arg, f_scope, ast)
        elseif utils["table?"](arg) then
          return destructure_arg(arg)
        else
          return compiler.assert(false, ("expected symbol for function parameter: %s"):format(tostring(arg)), ast[index])
        end
      end
      local arg_name_list = nil
      do
        local tbl_17_ = {}
        local i_18_ = #tbl_17_
        for i, a in ipairs(arg_list) do
          local val_19_ = get_arg_name(a, i)
          if (nil ~= val_19_) then
            i_18_ = (i_18_ + 1)
            tbl_17_[i_18_] = val_19_
          end
        end
        arg_name_list = tbl_17_
      end
      local f_metadata, index0 = get_function_metadata(ast, arg_list, index)
      if fn_name then
        return compile_named_fn(ast, f_scope, f_chunk, parent, index0, fn_name, local_3f, arg_name_list, f_metadata)
      else
        return compile_anonymous_fn(ast, f_scope, f_chunk, parent, index0, arg_name_list, f_metadata, scope)
      end
    end
    doc_special("fn", {"name?", "args", "docstring?", "..."}, "Function syntax. May optionally include a name and docstring or a metadata table.\nIf a name is provided, the function will be bound in the current scope.\nWhen called with the wrong number of args, excess args will be discarded\nand lacking args will be nil, use lambda for arity-checked functions.", true)
    SPECIALS.lua = function(ast, _, parent)
      compiler.assert(((#ast == 2) or (#ast == 3)), "expected 1 or 2 arguments", ast)
      local _524_
      do
        local _523_0 = utils["sym?"](ast[2])
        if (nil ~= _523_0) then
          _524_ = tostring(_523_0)
        else
          _524_ = _523_0
        end
      end
      if ("nil" ~= _524_) then
        table.insert(parent, {ast = ast, leaf = tostring(ast[2])})
      end
      local _528_
      do
        local _527_0 = utils["sym?"](ast[3])
        if (nil ~= _527_0) then
          _528_ = tostring(_527_0)
        else
          _528_ = _527_0
        end
      end
      if ("nil" ~= _528_) then
        return tostring(ast[3])
      end
    end
    local function dot(ast, scope, parent)
      compiler.assert((1 < #ast), "expected table argument", ast)
      local len = #ast
      local lhs_node = compiler.macroexpand(ast[2], scope)
      local _531_ = compiler.compile1(lhs_node, scope, parent, {nval = 1})
      local lhs = _531_[1]
      if (len == 2) then
        return tostring(lhs)
      else
        local indices = {}
        for i = 3, len do
          local index = ast[i]
          if (utils["string?"](index) and utils["valid-lua-identifier?"](index)) then
            table.insert(indices, ("." .. index))
          else
            local _532_ = compiler.compile1(index, scope, parent, {nval = 1})
            local index0 = _532_[1]
            table.insert(indices, ("[" .. tostring(index0) .. "]"))
          end
        end
        if (not (utils["sym?"](lhs_node) or utils["list?"](lhs_node)) or ("nil" == tostring(lhs_node))) then
          return ("(" .. tostring(lhs) .. ")" .. table.concat(indices))
        else
          return (tostring(lhs) .. table.concat(indices))
        end
      end
    end
    SPECIALS["."] = dot
    doc_special(".", {"tbl", "key1", "..."}, "Look up key1 in tbl table. If more args are provided, do a nested lookup.")
    SPECIALS.global = function(ast, scope, parent)
      compiler.assert((#ast == 3), "expected name and value", ast)
      compiler.destructure(ast[2], ast[3], ast, scope, parent, {forceglobal = true, nomulti = true, symtype = "global"})
      return nil
    end
    doc_special("global", {"name", "val"}, "Set name as a global with val. Deprecated.")
    SPECIALS.set = function(ast, scope, parent)
      compiler.assert((#ast == 3), "expected name and value", ast)
      compiler.destructure(ast[2], ast[3], ast, scope, parent, {noundef = true, symtype = "set"})
      return nil
    end
    doc_special("set", {"name", "val"}, "Set a local variable to a new value. Only works on locals using var.")
    local function set_forcibly_21_2a(ast, scope, parent)
      compiler.assert((#ast == 3), "expected name and value", ast)
      compiler.destructure(ast[2], ast[3], ast, scope, parent, {forceset = true, symtype = "set"})
      return nil
    end
    SPECIALS["set-forcibly!"] = set_forcibly_21_2a
    local function local_2a(ast, scope, parent, opts)
      compiler.assert(((0 == opts.nval) or opts.tail), "can't introduce local here", ast)
      compiler.assert((#ast == 3), "expected name and value", ast)
      compiler.destructure(ast[2], ast[3], ast, scope, parent, {declaration = true, nomulti = true, symtype = "local"})
      return nil
    end
    SPECIALS["local"] = local_2a
    doc_special("local", {"name", "val"}, "Introduce new top-level immutable local.")
    SPECIALS.var = function(ast, scope, parent, opts)
      compiler.assert(((0 == opts.nval) or opts.tail), "can't introduce var here", ast)
      compiler.assert((#ast == 3), "expected name and value", ast)
      compiler.destructure(ast[2], ast[3], ast, scope, parent, {declaration = true, isvar = true, nomulti = true, symtype = "var"})
      return nil
    end
    doc_special("var", {"name", "val"}, "Introduce new mutable local.")
    local function kv_3f(t)
      local _536_
      do
        local tbl_17_ = {}
        local i_18_ = #tbl_17_
        for k in pairs(t) do
          local val_19_ = nil
          if ("number" ~= type(k)) then
            val_19_ = k
          else
          val_19_ = nil
          end
          if (nil ~= val_19_) then
            i_18_ = (i_18_ + 1)
            tbl_17_[i_18_] = val_19_
          end
        end
        _536_ = tbl_17_
      end
      return _536_[1]
    end
    SPECIALS.let = function(_539_0, scope, parent, opts)
      local _540_ = _539_0
      local _ = _540_[1]
      local bindings = _540_[2]
      local ast = _540_
      compiler.assert((utils["table?"](bindings) and not kv_3f(bindings)), "expected binding sequence", (bindings or ast[1]))
      compiler.assert(((#bindings % 2) == 0), "expected even number of name/value bindings", bindings)
      compiler.assert((3 <= #ast), "expected body expression", ast[1])
      local pre_syms = nil
      do
        local tbl_17_ = {}
        local i_18_ = #tbl_17_
        for _0 = 1, (opts.nval or 0) do
          local val_19_ = compiler.gensym(scope)
          if (nil ~= val_19_) then
            i_18_ = (i_18_ + 1)
            tbl_17_[i_18_] = val_19_
          end
        end
        pre_syms = tbl_17_
      end
      local sub_scope = compiler["make-scope"](scope)
      local sub_chunk = {}
      for i = 1, #bindings, 2 do
        compiler.destructure(bindings[i], bindings[(i + 1)], ast, sub_scope, sub_chunk, {declaration = true, nomulti = true, symtype = "let"})
      end
      return SPECIALS["do"](ast, scope, parent, opts, 3, sub_chunk, sub_scope, pre_syms)
    end
    doc_special("let", {"[name1 val1 ... nameN valN]", "..."}, "Introduces a new scope in which a given set of local bindings are used.", true)
    local function get_prev_line(parent)
      if ("table" == type(parent)) then
        return get_prev_line((parent.leaf or parent[#parent]))
      else
        return (parent or "")
      end
    end
    local function needs_separator_3f(root, prev_line)
      return (root:match("^%(") and prev_line and not prev_line:find(" end$"))
    end
    SPECIALS.tset = function(ast, scope, parent)
      compiler.assert((3 < #ast), "expected table, key, and value arguments", ast)
      compiler.assert(((type(ast[2]) ~= "boolean") and (type(ast[2]) ~= "number")), "cannot set field of literal value", ast)
      local root = str1(compiler.compile1(ast[2], scope, parent, {nval = 1}))
      local root0 = nil
      if root:match("^[.{\"]") then
        root0 = string.format("(%s)", root)
      else
        root0 = root
      end
      local keys = nil
      do
        local tbl_17_ = {}
        local i_18_ = #tbl_17_
        for i = 3, (#ast - 1) do
          local val_19_ = str1(compiler.compile1(ast[i], scope, parent, {nval = 1}))
          if (nil ~= val_19_) then
            i_18_ = (i_18_ + 1)
            tbl_17_[i_18_] = val_19_
          end
        end
        keys = tbl_17_
      end
      local value = str1(compiler.compile1(ast[#ast], scope, parent, {nval = 1}))
      local fmtstr = nil
      if needs_separator_3f(root0, get_prev_line(parent)) then
        fmtstr = "do end %s[%s] = %s"
      else
        fmtstr = "%s[%s] = %s"
      end
      return compiler.emit(parent, fmtstr:format(root0, table.concat(keys, "]["), value), ast)
    end
    doc_special("tset", {"tbl", "key1", "...", "keyN", "val"}, "Set the value of a table field. Deprecated in favor of set.")
    local function calculate_if_target(scope, opts)
      if not (opts.tail or opts.target or opts.nval) then
        return "iife", true, nil
      elseif (opts.nval and (opts.nval ~= 0) and not opts.target) then
        local accum = {}
        local target_exprs = {}
        for i = 1, opts.nval do
          local s = compiler.gensym(scope)
          accum[i] = s
          target_exprs[i] = utils.expr(s, "sym")
        end
        return "target", opts.tail, table.concat(accum, ", "), target_exprs
      else
        return "none", opts.tail, opts.target
      end
    end
    local function if_2a(ast, scope, parent, opts)
      compiler.assert((2 < #ast), "expected condition and body", ast)
      if ((1 == (#ast % 2)) and (ast[(#ast - 1)] == true)) then
        table.remove(ast, (#ast - 1))
      end
      if (1 == (#ast % 2)) then
        table.insert(ast, utils.sym("nil"))
      end
      if (#ast == 2) then
        return SPECIALS["do"](utils.list(utils.sym("do"), ast[2]), scope, parent, opts)
      else
        local do_scope = compiler["make-scope"](scope)
        local branches = {}
        local wrapper, inner_tail, inner_target, target_exprs = calculate_if_target(scope, opts)
        local body_opts = {nval = opts.nval, tail = inner_tail, target = inner_target}
        local function compile_body(i)
          local chunk = {}
          local cscope = compiler["make-scope"](do_scope)
          compiler["keep-side-effects"](compiler.compile1(ast[i], cscope, chunk, body_opts), chunk, nil, ast[i])
          return {chunk = chunk, scope = cscope}
        end
        for i = 2, (#ast - 1), 2 do
          local condchunk = {}
          local _549_ = compiler.compile1(ast[i], do_scope, condchunk, {nval = 1})
          local cond = _549_[1]
          local branch = compile_body((i + 1))
          branch.cond = cond
          branch.condchunk = condchunk
          branch.nested = ((i ~= 2) and (next(condchunk, nil) == nil))
          table.insert(branches, branch)
        end
        local else_branch = compile_body(#ast)
        local s = compiler.gensym(scope)
        local buffer = {}
        local last_buffer = buffer
        for i = 1, #branches do
          local branch = branches[i]
          local fstr = nil
          if not branch.nested then
            fstr = "if %s then"
          else
            fstr = "elseif %s then"
          end
          local cond = tostring(branch.cond)
          local cond_line = fstr:format(cond)
          if branch.nested then
            compiler.emit(last_buffer, branch.condchunk, ast)
          else
            for _, v in ipairs(branch.condchunk) do
              compiler.emit(last_buffer, v, ast)
            end
          end
          compiler.emit(last_buffer, cond_line, ast)
          compiler.emit(last_buffer, branch.chunk, ast)
          if (i == #branches) then
            compiler.emit(last_buffer, "else", ast)
            compiler.emit(last_buffer, else_branch.chunk, ast)
            compiler.emit(last_buffer, "end", ast)
          elseif not branches[(i + 1)].nested then
            local next_buffer = {}
            compiler.emit(last_buffer, "else", ast)
            compiler.emit(last_buffer, next_buffer, ast)
            compiler.emit(last_buffer, "end", ast)
            last_buffer = next_buffer
          end
        end
        if (wrapper == "iife") then
          local iifeargs = ((scope.vararg and "...") or "")
          compiler.emit(parent, ("local function %s(%s)"):format(tostring(s), iifeargs), ast)
          compiler.emit(parent, buffer, ast)
          compiler.emit(parent, "end", ast)
          return utils.expr(("%s(%s)"):format(tostring(s), iifeargs), "statement")
        elseif (wrapper == "none") then
          for i = 1, #buffer do
            compiler.emit(parent, buffer[i], ast)
          end
          return {returned = true}
        else
          compiler.emit(parent, ("local %s"):format(inner_target), ast)
          for i = 1, #buffer do
            compiler.emit(parent, buffer[i], ast)
          end
          return target_exprs
        end
      end
    end
    SPECIALS["if"] = if_2a
    doc_special("if", {"cond1", "body1", "...", "condN", "bodyN"}, "Conditional form.\nTakes any number of condition/body pairs and evaluates the first body where\nthe condition evaluates to truthy. Similar to cond in other lisps.")
    local function clause_3f(v)
      return (utils["string?"](v) or (utils["sym?"](v) and not utils["multi-sym?"](v) and tostring(v):match("^&(.+)")))
    end
    local function remove_until_condition(bindings, ast)
      local _until = nil
      for i = (#bindings - 1), 3, -1 do
        local _555_0 = clause_3f(bindings[i])
        if ((_555_0 == false) or (_555_0 == nil)) then
        elseif (nil ~= _555_0) then
          local clause = _555_0
          compiler.assert(((clause == "until") and not _until), ("unexpected iterator clause: " .. clause), ast)
          table.remove(bindings, i)
          _until = table.remove(bindings, i)
        end
      end
      return _until
    end
    local function compile_until(_3fcondition, scope, chunk)
      if _3fcondition then
        local _557_ = compiler.compile1(_3fcondition, scope, chunk, {nval = 1})
        local condition_lua = _557_[1]
        return compiler.emit(chunk, ("if %s then break end"):format(tostring(condition_lua)), utils.expr(_3fcondition, "expression"))
      end
    end
    local function iterator_bindings(ast)
      local bindings = utils.copy(ast)
      local _3funtil = remove_until_condition(bindings, ast)
      local iter = table.remove(bindings)
      local bindings0 = nil
      if (1 == #bindings) then
        bindings0 = (utils["list?"](bindings[1]) or bindings)
      else
        for _, b in ipairs(bindings) do
          if utils["list?"](b) then
            utils.warn("unexpected parens in iterator", b)
          end
        end
        bindings0 = bindings
      end
      return bindings0, iter, _3funtil
    end
    SPECIALS.each = function(ast, scope, parent)
      compiler.assert((3 <= #ast), "expected body expression", ast[1])
      compiler.assert(utils["table?"](ast[2]), "expected binding table", ast)
      local sub_scope = compiler["make-scope"](scope)
      local binding, iter, _3funtil_condition = iterator_bindings(ast[2])
      local destructures = {}
      local deferred_scope_changes = {manglings = {}, symmeta = {}}
      utils.hook("pre-each", ast, sub_scope, binding, iter, _3funtil_condition)
      local function destructure_binding(v)
        if utils["sym?"](v) then
          return compiler["declare-local"](v, sub_scope, ast, nil, deferred_scope_changes)
        else
          local raw = utils.sym(compiler.gensym(sub_scope))
          destructures[raw] = v
          return compiler["declare-local"](raw, sub_scope, ast)
        end
      end
      local bind_vars = nil
      do
        local tbl_17_ = {}
        local i_18_ = #tbl_17_
        for _, b in ipairs(binding) do
          local val_19_ = destructure_binding(b)
          if (nil ~= val_19_) then
            i_18_ = (i_18_ + 1)
            tbl_17_[i_18_] = val_19_
          end
        end
        bind_vars = tbl_17_
      end
      local vals = compiler.compile1(iter, scope, parent)
      local val_names = nil
      do
        local tbl_17_ = {}
        local i_18_ = #tbl_17_
        for _, v in ipairs(vals) do
          local val_19_ = tostring(v)
          if (nil ~= val_19_) then
            i_18_ = (i_18_ + 1)
            tbl_17_[i_18_] = val_19_
          end
        end
        val_names = tbl_17_
      end
      local chunk = {}
      compiler.assert(bind_vars[1], "expected binding and iterator", ast)
      compiler.emit(parent, ("for %s in %s do"):format(table.concat(bind_vars, ", "), table.concat(val_names, ", ")), ast)
      for raw, args in utils.stablepairs(destructures) do
        compiler.destructure(args, raw, ast, sub_scope, chunk, {declaration = true, nomulti = true, symtype = "each"})
      end
      compiler["apply-deferred-scope-changes"](sub_scope, deferred_scope_changes, ast)
      compile_until(_3funtil_condition, sub_scope, chunk)
      compile_do(ast, sub_scope, chunk, 3)
      compiler.emit(parent, chunk, ast)
      return compiler.emit(parent, "end", ast)
    end
    doc_special("each", {"[key value (iterator)]", "..."}, "Runs the body once for each set of values provided by the given iterator.\nMost commonly used with ipairs for sequential tables or pairs for  undefined\norder, but can be used with any iterator.", true)
    local function while_2a(ast, scope, parent)
      local len1 = #parent
      local condition = compiler.compile1(ast[2], scope, parent, {nval = 1})[1]
      local len2 = #parent
      local sub_chunk = {}
      if (len1 ~= len2) then
        for i = (len1 + 1), len2 do
          table.insert(sub_chunk, parent[i])
          parent[i] = nil
        end
        compiler.emit(parent, "while true do", ast)
        compiler.emit(sub_chunk, ("if not %s then break end"):format(condition[1]), ast)
      else
        compiler.emit(parent, ("while " .. tostring(condition) .. " do"), ast)
      end
      compile_do(ast, compiler["make-scope"](scope), sub_chunk, 3)
      compiler.emit(parent, sub_chunk, ast)
      return compiler.emit(parent, "end", ast)
    end
    SPECIALS["while"] = while_2a
    doc_special("while", {"condition", "..."}, "The classic while loop. Evaluates body until a condition is non-truthy.", true)
    local function for_2a(ast, scope, parent)
      compiler.assert(utils["table?"](ast[2]), "expected binding table", ast)
      local ranges = setmetatable(utils.copy(ast[2]), getmetatable(ast[2]))
      local until_condition = remove_until_condition(ranges, ast)
      local binding_sym = table.remove(ranges, 1)
      local sub_scope = compiler["make-scope"](scope)
      local range_args = {}
      local chunk = {}
      compiler.assert(utils["sym?"](binding_sym), ("unable to bind %s %s"):format(type(binding_sym), tostring(binding_sym)), ast[2])
      compiler.assert((3 <= #ast), "expected body expression", ast[1])
      compiler.assert((#ranges <= 3), "unexpected arguments", ranges)
      compiler.assert((1 < #ranges), "expected range to include start and stop", ranges)
      utils.hook("pre-for", ast, sub_scope, binding_sym)
      for i = 1, math.min(#ranges, 3) do
        range_args[i] = str1(compiler.compile1(ranges[i], scope, parent, {nval = 1}))
      end
      compiler.emit(parent, ("for %s = %s do"):format(compiler["declare-local"](binding_sym, sub_scope, ast), table.concat(range_args, ", ")), ast)
      compile_until(until_condition, sub_scope, chunk)
      compile_do(ast, sub_scope, chunk, 3)
      compiler.emit(parent, chunk, ast)
      return compiler.emit(parent, "end", ast)
    end
    SPECIALS["for"] = for_2a
    doc_special("for", {"[index start stop step?]", "..."}, "Numeric loop construct.\nEvaluates body once for each value between start and stop (inclusive).", true)
    local function method_special_type(ast)
      if (utils["string?"](ast[3]) and utils["valid-lua-identifier?"](ast[3])) then
        return "native"
      elseif utils["sym?"](ast[2]) then
        return "nonnative"
      else
        return "binding"
      end
    end
    local function native_method_call(ast, _scope, _parent, target, args)
      local _566_ = ast
      local _ = _566_[1]
      local _0 = _566_[2]
      local method_string = _566_[3]
      local call_string = nil
      if ((target.type == "literal") or (target.type == "varg") or ((target.type == "expression") and not (target[1]):match("[%)%]]$") and not (target[1]):match("%.[%a_][%w_]*$"))) then
        call_string = "(%s):%s(%s)"
      else
        call_string = "%s:%s(%s)"
      end
      return utils.expr(string.format(call_string, tostring(target), method_string, table.concat(args, ", ")), "statement")
    end
    local function nonnative_method_call(ast, scope, parent, target, args)
      local method_string = str1(compiler.compile1(ast[3], scope, parent, {nval = 1}))
      local args0 = {tostring(target), unpack(args)}
      return utils.expr(string.format("%s[%s](%s)", tostring(target), method_string, table.concat(args0, ", ")), "statement")
    end
    local function binding_method_call(ast, scope, parent, target, args)
      local method_string = str1(compiler.compile1(ast[3], scope, parent, {nval = 1}))
      local target_local = compiler.gensym(scope, "tgt")
      local args0 = {target_local, unpack(args)}
      compiler.emit(parent, string.format("local %s = %s", target_local, tostring(target)))
      return utils.expr(string.format("(%s)[%s](%s)", target_local, method_string, table.concat(args0, ", ")), "statement")
    end
    local function method_call(ast, scope, parent)
      compiler.assert((2 < #ast), "expected at least 2 arguments", ast)
      local _568_ = compiler.compile1(ast[2], scope, parent, {nval = 1})
      local target = _568_[1]
      local args = {}
      for i = 4, #ast do
        local subexprs = nil
        local _569_
        if (i ~= #ast) then
          _569_ = 1
        else
        _569_ = nil
        end
        subexprs = compiler.compile1(ast[i], scope, parent, {nval = _569_})
        local tbl_17_ = args
        local i_18_ = #tbl_17_
        for _, subexpr in ipairs(subexprs) do
          local val_19_ = tostring(subexpr)
          if (nil ~= val_19_) then
            i_18_ = (i_18_ + 1)
            tbl_17_[i_18_] = val_19_
          end
        end
      end
      local _572_0 = method_special_type(ast)
      if (_572_0 == "native") then
        return native_method_call(ast, scope, parent, target, args)
      elseif (_572_0 == "nonnative") then
        return nonnative_method_call(ast, scope, parent, target, args)
      elseif (_572_0 == "binding") then
        return binding_method_call(ast, scope, parent, target, args)
      end
    end
    SPECIALS[":"] = method_call
    doc_special(":", {"tbl", "method-name", "..."}, "Call the named method on tbl with the provided args.\nMethod name doesn't have to be known at compile-time; if it is, use\n(tbl:method-name ...) instead.")
    SPECIALS.comment = function(ast, _, parent)
      local c = nil
      local _574_
      do
        local tbl_17_ = {}
        local i_18_ = #tbl_17_
        for i, elt in ipairs(ast) do
          local val_19_ = nil
          if (i ~= 1) then
            val_19_ = view(elt, {["one-line?"] = true})
          else
          val_19_ = nil
          end
          if (nil ~= val_19_) then
            i_18_ = (i_18_ + 1)
            tbl_17_[i_18_] = val_19_
          end
        end
        _574_ = tbl_17_
      end
      c = table.concat(_574_, " "):gsub("%]%]", "]\\]")
      return compiler.emit(parent, ("--[[ " .. c .. " ]]"), ast)
    end
    doc_special("comment", {"..."}, "Comment which will be emitted in Lua output.", true)
    local function hashfn_max_used(f_scope, i, max)
      local max0 = nil
      if f_scope.symmeta[("$" .. i)].used then
        max0 = i
      else
        max0 = max
      end
      if (i < 9) then
        return hashfn_max_used(f_scope, (i + 1), max0)
      else
        return max0
      end
    end
    SPECIALS.hashfn = function(ast, scope, parent)
      compiler.assert((#ast == 2), "expected one argument", ast)
      local f_scope = nil
      do
        local _579_0 = compiler["make-scope"](scope)
        _579_0["vararg"] = false
        _579_0["hashfn"] = true
        f_scope = _579_0
      end
      local f_chunk = {}
      local name = compiler.gensym(scope)
      local symbol = utils.sym(name)
      local args = {}
      compiler["declare-local"](symbol, scope, ast)
      for i = 1, 9 do
        args[i] = compiler["declare-local"](utils.sym(("$" .. i)), f_scope, ast)
      end
      local function walker(idx, node, _3fparent_node)
        if utils["sym?"](node, "$...") then
          f_scope.vararg = true
          if _3fparent_node then
            _3fparent_node[idx] = utils.varg()
            return nil
          else
            return utils.varg()
          end
        else
          return ((utils["list?"](node) and (not _3fparent_node or not utils["sym?"](node[1], "hashfn"))) or utils["table?"](node))
        end
      end
      utils["walk-tree"](ast, walker)
      compiler.compile1(ast[2], f_scope, f_chunk, {tail = true})
      local max_used = hashfn_max_used(f_scope, 1, 0)
      if f_scope.vararg then
        compiler.assert((max_used == 0), "$ and $... in hashfn are mutually exclusive", ast)
      end
      local arg_str = nil
      if f_scope.vararg then
        arg_str = tostring(utils.varg())
      else
        arg_str = table.concat(args, ", ", 1, max_used)
      end
      compiler.emit(parent, string.format("local function %s(%s)", name, arg_str), ast)
      compiler.emit(parent, f_chunk, ast)
      compiler.emit(parent, "end", ast)
      return utils.expr(name, "sym")
    end
    doc_special("hashfn", {"..."}, "Function literal shorthand; args are either $... OR $1, $2, etc.")
    local function comparator_special_type(ast)
      if (3 == #ast) then
        return "native"
      elseif utils["every?"]({unpack(ast, 3, (#ast - 1))}, utils["idempotent-expr?"]) then
        return "idempotent"
      else
        return "binding"
      end
    end
    local function short_circuit_safe_3f(x, scope)
      if (("table" ~= type(x)) or utils["sym?"](x) or utils["varg?"](x)) then
        return true
      elseif utils["table?"](x) then
        local ok = true
        for k, v in pairs(x) do
          if not ok then break end
          ok = (short_circuit_safe_3f(v, scope) and short_circuit_safe_3f(k, scope))
        end
        return ok
      elseif utils["list?"](x) then
        if utils["sym?"](x[1]) then
          local _585_0 = str1(x)
          if ((_585_0 == "fn") or (_585_0 == "hashfn") or (_585_0 == "let") or (_585_0 == "local") or (_585_0 == "var") or (_585_0 == "set") or (_585_0 == "tset") or (_585_0 == "if") or (_585_0 == "each") or (_585_0 == "for") or (_585_0 == "while") or (_585_0 == "do") or (_585_0 == "lua") or (_585_0 == "global")) then
            return false
          elseif (((_585_0 == "<") or (_585_0 == ">") or (_585_0 == "<=") or (_585_0 == ">=") or (_585_0 == "=") or (_585_0 == "not=") or (_585_0 == "~=")) and (comparator_special_type(x) == "binding")) then
            return false
          else
            local function _586_()
              return (1 ~= x[2])
            end
            if ((_585_0 == "pick-values") and _586_()) then
              return false
            else
              local function _587_()
                local call = _585_0
                return scope.macros[call]
              end
              if ((nil ~= _585_0) and _587_()) then
                local call = _585_0
                return false
              else
                local function _588_()
                  return (method_special_type(x) == "binding")
                end
                if ((_585_0 == ":") and _588_()) then
                  return false
                else
                  local _ = _585_0
                  local ok = true
                  for i = 2, #x do
                    if not ok then break end
                    ok = short_circuit_safe_3f(x[i], scope)
                  end
                  return ok
                end
              end
            end
          end
        else
          local ok = true
          for _, v in ipairs(x) do
            if not ok then break end
            ok = short_circuit_safe_3f(v, scope)
          end
          return ok
        end
      end
    end
    local function operator_special_result(ast, zero_arity, unary_prefix, padded_op, operands)
      local _592_0 = #operands
      if (_592_0 == 0) then
        if zero_arity then
          return utils.expr(zero_arity, "literal")
        else
          return compiler.assert(false, "Expected more than 0 arguments", ast)
        end
      elseif (_592_0 == 1) then
        if unary_prefix then
          return ("(" .. unary_prefix .. padded_op .. operands[1] .. ")")
        else
          return operands[1]
        end
      else
        local _ = _592_0
        return ("(" .. table.concat(operands, padded_op) .. ")")
      end
    end
    local function emit_short_circuit_if(ast, scope, parent, name, subast, accumulator, expr_string, setter)
      if (accumulator ~= expr_string) then
        compiler.emit(parent, string.format(setter, accumulator, expr_string), ast)
      end
      local function _597_()
        if (name == "and") then
          return accumulator
        else
          return ("not " .. accumulator)
        end
      end
      compiler.emit(parent, ("if %s then"):format(_597_()), subast)
      do
        local chunk = {}
        compiler.compile1(subast, scope, chunk, {nval = 1, target = accumulator})
        compiler.emit(parent, chunk)
      end
      return compiler.emit(parent, "end")
    end
    local function operator_special(name, zero_arity, unary_prefix, ast, scope, parent)
      compiler.assert(not ((#ast == 2) and utils["varg?"](ast[2])), "tried to use vararg with operator", ast)
      local padded_op = (" " .. name .. " ")
      local operands, accumulator = {}
      if utils["call-of?"](ast[#ast], "values") then
        utils.warn("multiple values in operators are deprecated", ast)
      end
      for subast in iter_args(ast) do
        if ((nil ~= next(operands)) and ((name == "or") or (name == "and")) and not short_circuit_safe_3f(subast, scope)) then
          local expr_string = table.concat(operands, padded_op)
          local setter = nil
          if accumulator then
            setter = "%s = %s"
          else
            setter = "local %s = %s"
          end
          if not accumulator then
            accumulator = compiler.gensym(scope, name)
          end
          emit_short_circuit_if(ast, scope, parent, name, subast, accumulator, expr_string, setter)
          operands = {accumulator}
        else
          table.insert(operands, str1(compiler.compile1(subast, scope, parent, {nval = 1})))
        end
      end
      return operator_special_result(ast, zero_arity, unary_prefix, padded_op, operands)
    end
    local function define_arithmetic_special(name, zero_arity, unary_prefix, _3flua_name)
      local _603_
      do
        local _602_0 = (_3flua_name or name)
        local function _604_(...)
          return operator_special(_602_0, zero_arity, unary_prefix, ...)
        end
        _603_ = _604_
      end
      SPECIALS[name] = _603_
      return doc_special(name, {"a", "b", "..."}, "Arithmetic operator; works the same as Lua but accepts more arguments.")
    end
    define_arithmetic_special("+", "0", "0")
    define_arithmetic_special("..", "''")
    define_arithmetic_special("^")
    define_arithmetic_special("-", nil, "")
    define_arithmetic_special("*", "1", "1")
    define_arithmetic_special("%")
    define_arithmetic_special("/", nil, "1")
    define_arithmetic_special("//", nil, "1")
    SPECIALS["or"] = function(ast, scope, parent)
      return operator_special("or", "false", nil, ast, scope, parent)
    end
    SPECIALS["and"] = function(ast, scope, parent)
      return operator_special("and", "true", nil, ast, scope, parent)
    end
    doc_special("and", {"a", "b", "..."}, "Boolean operator; works the same as Lua but accepts more arguments.")
    doc_special("or", {"a", "b", "..."}, "Boolean operator; works the same as Lua but accepts more arguments.")
    local function bitop_special(native_name, lib_name, zero_arity, unary_prefix, ast, scope, parent)
      if (#ast == 1) then
        return compiler.assert(zero_arity, "Expected more than 0 arguments.", ast)
      else
        local len = #ast
        local operands = {}
        local padded_native_name = (" " .. native_name .. " ")
        local prefixed_lib_name = ("bit." .. lib_name)
        for i = 2, len do
          local subexprs = nil
          local _605_
          if (i ~= len) then
            _605_ = 1
          else
          _605_ = nil
          end
          subexprs = compiler.compile1(ast[i], scope, parent, {nval = _605_})
          local tbl_17_ = operands
          local i_18_ = #tbl_17_
          for _, s in ipairs(subexprs) do
            local val_19_ = tostring(s)
            if (nil ~= val_19_) then
              i_18_ = (i_18_ + 1)
              tbl_17_[i_18_] = val_19_
            end
          end
        end
        if (#operands == 1) then
          if utils.root.options.useBitLib then
            return (prefixed_lib_name .. "(" .. unary_prefix .. ", " .. operands[1] .. ")")
          else
            return ("(" .. unary_prefix .. padded_native_name .. operands[1] .. ")")
          end
        else
          if utils.root.options.useBitLib then
            return (prefixed_lib_name .. "(" .. table.concat(operands, ", ") .. ")")
          else
            return ("(" .. table.concat(operands, padded_native_name) .. ")")
          end
        end
      end
    end
    local function define_bitop_special(name, zero_arity, unary_prefix, native)
      local function _612_(...)
        return bitop_special(native, name, zero_arity, unary_prefix, ...)
      end
      SPECIALS[name] = _612_
      return nil
    end
    define_bitop_special("lshift", nil, "1", "<<")
    define_bitop_special("rshift", nil, "1", ">>")
    define_bitop_special("band", "-1", "-1", "&")
    define_bitop_special("bor", "0", "0", "|")
    define_bitop_special("bxor", "0", "0", "~")
    doc_special("lshift", {"x", "n"}, "Bitwise logical left shift of x by n bits.\nOnly works in Lua 5.3+ or LuaJIT with the --use-bit-lib flag.")
    doc_special("rshift", {"x", "n"}, "Bitwise logical right shift of x by n bits.\nOnly works in Lua 5.3+ or LuaJIT with the --use-bit-lib flag.")
    doc_special("band", {"x1", "x2", "..."}, "Bitwise AND of any number of arguments.\nOnly works in Lua 5.3+ or LuaJIT with the --use-bit-lib flag.")
    doc_special("bor", {"x1", "x2", "..."}, "Bitwise OR of any number of arguments.\nOnly works in Lua 5.3+ or LuaJIT with the --use-bit-lib flag.")
    doc_special("bxor", {"x1", "x2", "..."}, "Bitwise XOR of any number of arguments.\nOnly works in Lua 5.3+ or LuaJIT with the --use-bit-lib flag.")
    SPECIALS.bnot = function(ast, scope, parent)
      compiler.assert((#ast == 2), "expected one argument", ast)
      local _613_ = compiler.compile1(ast[2], scope, parent, {nval = 1})
      local value = _613_[1]
      if utils.root.options.useBitLib then
        return ("bit.bnot(" .. tostring(value) .. ")")
      else
        return ("~(" .. tostring(value) .. ")")
      end
    end
    doc_special("bnot", {"x"}, "Bitwise negation; only works in Lua 5.3+ or LuaJIT with the --use-bit-lib flag.")
    doc_special("..", {"a", "b", "..."}, "String concatenation operator; works the same as Lua but accepts more arguments.")
    local function native_comparator(op, _615_0, scope, parent)
      local _616_ = _615_0
      local _ = _616_[1]
      local lhs_ast = _616_[2]
      local rhs_ast = _616_[3]
      local _617_ = compiler.compile1(lhs_ast, scope, parent, {nval = 1})
      local lhs = _617_[1]
      local _618_ = compiler.compile1(rhs_ast, scope, parent, {nval = 1})
      local rhs = _618_[1]
      return string.format("(%s %s %s)", tostring(lhs), op, tostring(rhs))
    end
    local function idempotent_comparator(op, chain_op, ast, scope, parent)
      local vals = nil
      do
        local tbl_17_ = {}
        local i_18_ = #tbl_17_
        for i = 2, #ast do
          local val_19_ = str1(compiler.compile1(ast[i], scope, parent, {nval = 1}))
          if (nil ~= val_19_) then
            i_18_ = (i_18_ + 1)
            tbl_17_[i_18_] = val_19_
          end
        end
        vals = tbl_17_
      end
      local comparisons = nil
      do
        local tbl_17_ = {}
        local i_18_ = #tbl_17_
        for i = 1, (#vals - 1) do
          local val_19_ = string.format("(%s %s %s)", vals[i], op, vals[(i + 1)])
          if (nil ~= val_19_) then
            i_18_ = (i_18_ + 1)
            tbl_17_[i_18_] = val_19_
          end
        end
        comparisons = tbl_17_
      end
      local chain = string.format(" %s ", (chain_op or "and"))
      return ("(" .. table.concat(comparisons, chain) .. ")")
    end
    local function binding_comparator(op, chain_op, ast, scope, parent)
      local binding_left = {}
      local binding_right = {}
      local vals = {}
      local chain = string.format(" %s ", (chain_op or "and"))
      for i = 2, #ast do
        local compiled = str1(compiler.compile1(ast[i], scope, parent, {nval = 1}))
        if (utils["idempotent-expr?"](ast[i]) or (i == 2) or (i == #ast)) then
          table.insert(vals, compiled)
        else
          local my_sym = compiler.gensym(scope)
          table.insert(binding_left, my_sym)
          table.insert(binding_right, compiled)
          table.insert(vals, my_sym)
        end
      end
      compiler.emit(parent, string.format("local %s = %s", table.concat(binding_left, ", "), table.concat(binding_right, ", "), ast))
      local _622_
      do
        local tbl_17_ = {}
        local i_18_ = #tbl_17_
        for i = 1, (#vals - 1) do
          local val_19_ = string.format("(%s %s %s)", vals[i], op, vals[(i + 1)])
          if (nil ~= val_19_) then
            i_18_ = (i_18_ + 1)
            tbl_17_[i_18_] = val_19_
          end
        end
        _622_ = tbl_17_
      end
      return ("(" .. table.concat(_622_, chain) .. ")")
    end
    local function define_comparator_special(name, _3flua_op, _3fchain_op)
      do
        local op = (_3flua_op or name)
        local function opfn(ast, scope, parent)
          compiler.assert((2 < #ast), "expected at least two arguments", ast)
          local _624_0 = comparator_special_type(ast)
          if (_624_0 == "native") then
            return native_comparator(op, ast, scope, parent)
          elseif (_624_0 == "idempotent") then
            return idempotent_comparator(op, _3fchain_op, ast, scope, parent)
          elseif (_624_0 == "binding") then
            return binding_comparator(op, _3fchain_op, ast, scope, parent)
          else
            local _ = _624_0
            return error("internal compiler error. please report this to the fennel devs.")
          end
        end
        SPECIALS[name] = opfn
      end
      return doc_special(name, {"a", "b", "..."}, "Comparison operator; works the same as Lua but accepts more arguments.")
    end
    define_comparator_special(">")
    define_comparator_special("<")
    define_comparator_special(">=")
    define_comparator_special("<=")
    define_comparator_special("=", "==")
    define_comparator_special("not=", "~=", "or")
    local function define_unary_special(op, _3frealop)
      local function opfn(ast, scope, parent)
        compiler.assert((#ast == 2), "expected one argument", ast)
        local tail = compiler.compile1(ast[2], scope, parent, {nval = 1})
        return ((_3frealop or op) .. str1(tail))
      end
      SPECIALS[op] = opfn
      return nil
    end
    define_unary_special("not", "not ")
    doc_special("not", {"x"}, "Logical operator; works the same as Lua.")
    define_unary_special("length", "#")
    doc_special("length", {"x"}, "Returns the length of a table or string.")
    SPECIALS["~="] = SPECIALS["not="]
    SPECIALS["#"] = SPECIALS.length
    SPECIALS.quote = function(ast, scope, parent)
      compiler.assert((#ast == 2), "expected one argument", ast)
      local runtime, this_scope = true, scope
      while this_scope do
        this_scope = this_scope.parent
        if (this_scope == compiler.scopes.compiler) then
          runtime = false
        end
      end
      return compiler["do-quote"](ast[2], scope, parent, runtime)
    end
    doc_special("quote", {"x"}, "Quasiquote the following form. Only works in macro/compiler scope.")
    local macro_loaded = {}
    local function safe_getmetatable(tbl)
      local mt = getmetatable(tbl)
      assert((mt ~= getmetatable("")), "Illegal metatable access!")
      return mt
    end
    local safe_require = nil
    local function safe_compiler_env()
      local _628_
      do
        local _627_0 = rawget(_G, "utf8")
        if (nil ~= _627_0) then
          _628_ = utils.copy(_627_0)
        else
          _628_ = _627_0
        end
      end
      return {_VERSION = _VERSION, assert = assert, bit = rawget(_G, "bit"), error = error, getmetatable = safe_getmetatable, ipairs = ipairs, math = utils.copy(math), next = next, pairs = utils.stablepairs, pcall = pcall, print = print, rawequal = rawequal, rawget = rawget, rawlen = rawget(_G, "rawlen"), rawset = rawset, require = safe_require, select = select, setmetatable = setmetatable, string = utils.copy(string), table = utils.copy(table), tonumber = tonumber, tostring = tostring, type = type, utf8 = _628_, xpcall = xpcall}
    end
    local function combined_mt_pairs(env)
      local combined = {}
      local _630_ = getmetatable(env)
      local __index = _630_["__index"]
      if ("table" == type(__index)) then
        for k, v in pairs(__index) do
          combined[k] = v
        end
      end
      for k, v in next, env, nil do
        combined[k] = v
      end
      return next, combined, nil
    end
    local function make_compiler_env(ast, scope, parent, _3fopts)
      local provided = nil
      do
        local _632_0 = (_3fopts or utils.root.options)
        if ((_G.type(_632_0) == "table") and (_632_0["compiler-env"] == "strict")) then
          provided = safe_compiler_env()
        elseif ((_G.type(_632_0) == "table") and (nil ~= _632_0.compilerEnv)) then
          local compilerEnv = _632_0.compilerEnv
          provided = compilerEnv
        elseif ((_G.type(_632_0) == "table") and (nil ~= _632_0["compiler-env"])) then
          local compiler_env = _632_0["compiler-env"]
          provided = compiler_env
        else
          local _ = _632_0
          provided = safe_compiler_env()
        end
      end
      local env = nil
      local function _634_()
        return compiler.scopes.macro
      end
      local function _635_(symbol)
        compiler.assert(compiler.scopes.macro, "must call from macro", ast)
        return compiler.scopes.macro.manglings[tostring(symbol)]
      end
      local function _636_(base)
        return utils.sym(compiler.gensym((compiler.scopes.macro or scope), base))
      end
      local function _637_(form)
        compiler.assert(compiler.scopes.macro, "must call from macro", ast)
        return compiler.macroexpand(form, compiler.scopes.macro)
      end
      env = {["assert-compile"] = compiler.assert, ["ast-source"] = utils["ast-source"], ["comment?"] = utils["comment?"], ["fennel-module-name"] = fennel_module_name, ["get-scope"] = _634_, ["in-scope?"] = _635_, ["list?"] = utils["list?"], ["macro-loaded"] = macro_loaded, ["multi-sym?"] = utils["multi-sym?"], ["sequence?"] = utils["sequence?"], ["sym?"] = utils["sym?"], ["table?"] = utils["table?"], ["varg?"] = utils["varg?"], _AST = ast, _CHUNK = parent, _IS_COMPILER = true, _SCOPE = scope, _SPECIALS = compiler.scopes.global.specials, _VARARG = utils.varg(), comment = utils.comment, gensym = _636_, list = utils.list, macroexpand = _637_, sequence = utils.sequence, sym = utils.sym, unpack = unpack, version = utils.version, view = view}
      env._G = env
      return setmetatable(env, {__index = provided, __newindex = provided, __pairs = combined_mt_pairs})
    end
    local function _638_(...)
      local tbl_17_ = {}
      local i_18_ = #tbl_17_
      for c in string.gmatch((package.config or ""), "([^\n]+)") do
        local val_19_ = c
        if (nil ~= val_19_) then
          i_18_ = (i_18_ + 1)
          tbl_17_[i_18_] = val_19_
        end
      end
      return tbl_17_
    end
    local _640_ = _638_(...)
    local dirsep = _640_[1]
    local pathsep = _640_[2]
    local pathmark = _640_[3]
    local pkg_config = {dirsep = (dirsep or "/"), pathmark = (pathmark or "?"), pathsep = (pathsep or ";")}
    local function escapepat(str)
      return string.gsub(str, "[^%w]", "%%%1")
    end
    local function search_module(modulename, _3fpathstring)
      local pathsepesc = escapepat(pkg_config.pathsep)
      local pattern = ("([^%s]*)%s"):format(pathsepesc, pathsepesc)
      local no_dot_module = modulename:gsub("%.", pkg_config.dirsep)
      local fullpath = ((_3fpathstring or utils["fennel-module"].path) .. pkg_config.pathsep)
      local function try_path(path)
        local filename = path:gsub(escapepat(pkg_config.pathmark), no_dot_module)
        local filename2 = path:gsub(escapepat(pkg_config.pathmark), modulename)
        local _641_0 = (io.open(filename) or io.open(filename2))
        if (nil ~= _641_0) then
          local file = _641_0
          file:close()
          return filename
        else
          local _ = _641_0
          return nil, ("no file '" .. filename .. "'")
        end
      end
      local function find_in_path(start, _3ftried_paths)
        local _643_0 = fullpath:match(pattern, start)
        if (nil ~= _643_0) then
          local path = _643_0
          local _644_0, _645_0 = try_path(path)
          if (nil ~= _644_0) then
            local filename = _644_0
            return filename
          elseif ((_644_0 == nil) and (nil ~= _645_0)) then
            local error = _645_0
            local function _647_()
              local _646_0 = (_3ftried_paths or {})
              table.insert(_646_0, error)
              return _646_0
            end
            return find_in_path((start + #path + 1), _647_())
          end
        else
          local _ = _643_0
          local function _649_()
            local tried_paths = table.concat((_3ftried_paths or {}), "\n\9")
            if (_VERSION < "Lua 5.4") then
              return ("\n\9" .. tried_paths)
            else
              return tried_paths
            end
          end
          return nil, _649_()
        end
      end
      return find_in_path(1)
    end
    local function make_searcher(_3foptions)
      local function _652_(module_name)
        local opts = utils.copy(utils.root.options)
        for k, v in pairs((_3foptions or {})) do
          opts[k] = v
        end
        opts["module-name"] = module_name
        local _653_0, _654_0 = search_module(module_name)
        if (nil ~= _653_0) then
          local filename = _653_0
          local function _655_(...)
            return utils["fennel-module"].dofile(filename, opts, ...)
          end
          return _655_, filename
        elseif ((_653_0 == nil) and (nil ~= _654_0)) then
          local error = _654_0
          return error
        end
      end
      return _652_
    end
    local function dofile_with_searcher(fennel_macro_searcher, filename, opts, ...)
      local searchers = (package.loaders or package.searchers or {})
      local _ = table.insert(searchers, 1, fennel_macro_searcher)
      local m = utils["fennel-module"].dofile(filename, opts, ...)
      table.remove(searchers, 1)
      return m
    end
    local function fennel_macro_searcher(module_name)
      local opts = nil
      do
        local _657_0 = utils.copy(utils.root.options)
        _657_0["module-name"] = module_name
        _657_0["env"] = "_COMPILER"
        _657_0["requireAsInclude"] = false
        _657_0["allowedGlobals"] = nil
        opts = _657_0
      end
      local _658_0 = search_module(module_name, utils["fennel-module"]["macro-path"])
      if (nil ~= _658_0) then
        local filename = _658_0
        local _659_
        if (opts["compiler-env"] == _G) then
          local function _660_(...)
            return dofile_with_searcher(fennel_macro_searcher, filename, opts, ...)
          end
          _659_ = _660_
        else
          local function _661_(...)
            return utils["fennel-module"].dofile(filename, opts, ...)
          end
          _659_ = _661_
        end
        return _659_, filename
      end
    end
    local function lua_macro_searcher(module_name)
      local _664_0 = search_module(module_name, package.path)
      if (nil ~= _664_0) then
        local filename = _664_0
        local code = nil
        do
          local f = io.open(filename)
          local function close_handlers_10_(ok_11_, ...)
            f:close()
            if ok_11_ then
              return ...
            else
              return error(..., 0)
            end
          end
          local function _666_()
            return assert(f:read("*a"))
          end
          code = close_handlers_10_(_G.xpcall(_666_, (package.loaded.fennel or debug).traceback))
        end
        local chunk = load_code(code, make_compiler_env(), filename)
        return chunk, filename
      end
    end
    local macro_searchers = {fennel_macro_searcher, lua_macro_searcher}
    local function search_macro_module(modname, n)
      local _668_0 = macro_searchers[n]
      if (nil ~= _668_0) then
        local f = _668_0
        local _669_0, _670_0 = f(modname)
        if ((nil ~= _669_0) and true) then
          local loader = _669_0
          local _3ffilename = _670_0
          return loader, _3ffilename
        else
          local _ = _669_0
          return search_macro_module(modname, (n + 1))
        end
      end
    end
    local function sandbox_fennel_module(modname)
      if ((modname == "fennel.macros") or (package and package.loaded and ("table" == type(package.loaded[modname])) and (package.loaded[modname].metadata == compiler.metadata))) then
        local function _673_(_, ...)
          return (compiler.metadata):setall(...)
        end
        return {metadata = {setall = _673_}, view = view}
      end
    end
    local function _675_(modname)
      local function _676_()
        local loader, filename = search_macro_module(modname, 1)
        compiler.assert(loader, (modname .. " module not found."))
        macro_loaded[modname] = loader(modname, filename)
        return macro_loaded[modname]
      end
      return (macro_loaded[modname] or sandbox_fennel_module(modname) or _676_())
    end
    safe_require = _675_
    local function add_macros(macros_2a, ast, scope)
      compiler.assert(utils["table?"](macros_2a), "expected macros to be table", ast)
      for k, v in pairs(macros_2a) do
        compiler.assert((type(v) == "function"), "expected each macro to be function", ast)
        compiler["check-binding-valid"](utils.sym(k), scope, ast, {["macro?"] = true})
        scope.macros[k] = v
      end
      return nil
    end
    local function resolve_module_name(_677_0, _scope, _parent, opts)
      local _678_ = _677_0
      local second = _678_[2]
      local filename = _678_["filename"]
      local filename0 = (filename or (utils["table?"](second) and second.filename))
      local module_name = utils.root.options["module-name"]
      local modexpr = compiler.compile(second, opts)
      local modname_chunk = load_code(modexpr)
      return modname_chunk(module_name, filename0)
    end
    SPECIALS["require-macros"] = function(ast, scope, parent, _3freal_ast)
      compiler.assert((#ast == 2), "Expected one module name argument", (_3freal_ast or ast))
      local modname = resolve_module_name(ast, scope, parent, {})
      compiler.assert(utils["string?"](modname), "module name must compile to string", (_3freal_ast or ast))
      if not macro_loaded[modname] then
        local loader, filename = search_macro_module(modname, 1)
        compiler.assert(loader, (modname .. " module not found."), ast)
        macro_loaded[modname] = compiler.assert(utils["table?"](loader(modname, filename)), "expected macros to be table", (_3freal_ast or ast))
      end
      if ("import-macros" == str1(ast)) then
        return macro_loaded[modname]
      else
        return add_macros(macro_loaded[modname], ast, scope)
      end
    end
    doc_special("require-macros", {"macro-module-name"}, "Load given module and use its contents as macro definitions in current scope.\nDeprecated.")
    local function emit_included_fennel(src, path, opts, sub_chunk)
      local subscope = compiler["make-scope"](utils.root.scope.parent)
      local forms = {}
      if utils.root.options.requireAsInclude then
        subscope.specials.require = compiler["require-include"]
      end
      for _, val in parser.parser(parser["string-stream"](src), path) do
        table.insert(forms, val)
      end
      for i = 1, #forms do
        local subopts = nil
        if (i == #forms) then
          subopts = {tail = true}
        else
          subopts = {nval = 0}
        end
        utils["propagate-options"](opts, subopts)
        compiler.compile1(forms[i], subscope, sub_chunk, subopts)
      end
      return nil
    end
    local function include_path(ast, opts, path, mod, fennel_3f)
      utils.root.scope.includes[mod] = "fnl/loading"
      local src = nil
      do
        local f = assert(io.open(path))
        local function close_handlers_10_(ok_11_, ...)
          f:close()
          if ok_11_ then
            return ...
          else
            return error(..., 0)
          end
        end
        local function _684_()
          return assert(f:read("*all")):gsub("[\13\n]*$", "")
        end
        src = close_handlers_10_(_G.xpcall(_684_, (package.loaded.fennel or debug).traceback))
      end
      local ret = utils.expr(("require(\"" .. mod .. "\")"), "statement")
      local target = ("package.preload[%q]"):format(mod)
      local preload_str = (target .. " = " .. target .. " or function(...)")
      local temp_chunk, sub_chunk = {}, {}
      compiler.emit(temp_chunk, preload_str, ast)
      compiler.emit(temp_chunk, sub_chunk)
      compiler.emit(temp_chunk, "end", ast)
      for _, v in ipairs(temp_chunk) do
        table.insert(utils.root.chunk, v)
      end
      if fennel_3f then
        emit_included_fennel(src, path, opts, sub_chunk)
      else
        compiler.emit(sub_chunk, src, ast)
      end
      utils.root.scope.includes[mod] = ret
      return ret
    end
    local function include_circular_fallback(mod, modexpr, fallback, ast)
      if (utils.root.scope.includes[mod] == "fnl/loading") then
        compiler.assert(fallback, "circular include detected", ast)
        return fallback(modexpr)
      end
    end
    SPECIALS.include = function(ast, scope, parent, opts)
      compiler.assert((#ast == 2), "expected one argument", ast)
      local modexpr = nil
      do
        local _687_0, _688_0 = pcall(resolve_module_name, ast, scope, parent, opts)
        if ((_687_0 == true) and (nil ~= _688_0)) then
          local modname = _688_0
          modexpr = utils.expr(string.format("%q", modname), "literal")
        else
          local _ = _687_0
          modexpr = compiler.compile1(ast[2], scope, parent, {nval = 1})[1]
        end
      end
      if ((modexpr.type ~= "literal") or ((modexpr[1]):byte() ~= 34)) then
        if opts.fallback then
          return opts.fallback(modexpr)
        else
          return compiler.assert(false, "module name must be string literal", ast)
        end
      else
        local mod = load_code(("return " .. modexpr[1]))()
        local oldmod = utils.root.options["module-name"]
        local _ = nil
        utils.root.options["module-name"] = mod
        _ = nil
        local res = nil
        local function _692_()
          local _691_0 = search_module(mod)
          if (nil ~= _691_0) then
            local fennel_path = _691_0
            return include_path(ast, opts, fennel_path, mod, true)
          else
            local _0 = _691_0
            local lua_path = search_module(mod, package.path)
            if lua_path then
              return include_path(ast, opts, lua_path, mod, false)
            elseif opts.fallback then
              return opts.fallback(modexpr)
            else
              return compiler.assert(false, ("module not found " .. mod), ast)
            end
          end
        end
        res = ((utils["member?"](mod, (utils.root.options.skipInclude or {})) and opts.fallback(modexpr, true)) or include_circular_fallback(mod, modexpr, opts.fallback, ast) or utils.root.scope.includes[mod] or _692_())
        utils.root.options["module-name"] = oldmod
        return res
      end
    end
    doc_special("include", {"module-name-literal"}, "Like require but load the target module during compilation and embed it in the\nLua output. The module must be a string literal and resolvable at compile time.")
    local function eval_compiler_2a(ast, scope, parent)
      local env = make_compiler_env(ast, scope, parent)
      local opts = utils.copy(utils.root.options)
      opts.scope = compiler["make-scope"](compiler.scopes.compiler)
      opts.allowedGlobals = current_global_names(env)
      return assert(load_code(compiler.compile(ast, opts), wrap_env(env)))(opts["module-name"], ast.filename)
    end
    SPECIALS.macros = function(ast, scope, parent)
      compiler.assert((#ast == 2), "Expected one table argument", ast)
      local macro_tbl = eval_compiler_2a(ast[2], scope, parent)
      compiler.assert(utils["table?"](macro_tbl), "Expected one table argument", ast)
      return add_macros(macro_tbl, ast, scope)
    end
    doc_special("macros", {"{:macro-name-1 (fn [...] ...) ... :macro-name-N macro-body-N}"}, "Define all functions in the given table as macros local to the current scope.")
    SPECIALS["tail!"] = function(ast, scope, parent, opts)
      compiler.assert((#ast == 2), "Expected one argument", ast)
      local call = utils["list?"](compiler.macroexpand(ast[2], scope))
      local callee = tostring((call and utils["sym?"](call[1])))
      compiler.assert((call and not scope.specials[callee]), "Expected a function call as argument", ast)
      compiler.assert(opts.tail, "Must be in tail position", ast)
      return compiler.compile1(call, scope, parent, opts)
    end
    doc_special("tail!", {"body"}, "Assert that the body being called is in tail position.")
    SPECIALS["pick-values"] = function(ast, scope, parent)
      local n = ast[2]
      local vals = utils.list(utils.sym("values"), unpack(ast, 3))
      compiler.assert((("number" == type(n)) and (0 <= n) and (n == math.floor(n))), ("Expected n to be an integer >= 0, got " .. tostring(n)))
      if (1 == n) then
        local _696_ = compiler.compile1(vals, scope, parent, {nval = 1})
        local _697_ = _696_[1]
        local expr = _697_[1]
        return {("(" .. expr .. ")")}
      elseif (0 == n) then
        for i = 3, #ast do
          compiler["keep-side-effects"](compiler.compile1(ast[i], scope, parent, {nval = 0}), parent, nil, ast[i])
        end
        return {}
      else
        local syms = nil
        do
          local tbl_17_ = utils.list()
          local i_18_ = #tbl_17_
          for _ = 1, n do
            local val_19_ = utils.sym(compiler.gensym(scope, "pv"))
            if (nil ~= val_19_) then
              i_18_ = (i_18_ + 1)
              tbl_17_[i_18_] = val_19_
            end
          end
          syms = tbl_17_
        end
        compiler.destructure(syms, vals, ast, scope, parent, {declaration = true, nomulti = true, noundef = true, symtype = "pv"})
        return syms
      end
    end
    doc_special("pick-values", {"n", "..."}, "Evaluate to exactly n values.\n\nFor example,\n  (pick-values 2 ...)\nexpands to\n  (let [(_0_ _1_) ...]\n    (values _0_ _1_))")
    SPECIALS["eval-compiler"] = function(ast, scope, parent)
      local old_first = ast[1]
      ast[1] = utils.sym("do")
      local val = eval_compiler_2a(ast, scope, parent)
      ast[1] = old_first
      return val
    end
    doc_special("eval-compiler", {"..."}, "Evaluate the body at compile-time. Use the macro system instead if possible.", true)
    SPECIALS.unquote = function(ast)
      return compiler.assert(false, "tried to use unquote outside quote", ast)
    end
    doc_special("unquote", {"..."}, "Evaluate the argument even if it's in a quoted form.")
    return {["current-global-names"] = current_global_names, ["get-function-metadata"] = get_function_metadata, ["load-code"] = load_code, ["macro-loaded"] = macro_loaded, ["macro-searchers"] = macro_searchers, ["make-compiler-env"] = make_compiler_env, ["make-searcher"] = make_searcher, ["search-module"] = search_module, ["wrap-env"] = wrap_env, doc = doc_2a}
  end
  package.preload["fennel.compiler"] = package.preload["fennel.compiler"] or function(...)
    local utils = require("fennel.utils")
    local parser = require("fennel.parser")
    local friend = require("fennel.friend")
    local view = require("fennel.view")
    local unpack = (table.unpack or _G.unpack)
    local scopes = {compiler = nil, global = nil, macro = nil}
    local function make_scope(_3fparent)
      local parent = (_3fparent or scopes.global)
      local _275_
      if parent then
        _275_ = ((parent.depth or 0) + 1)
      else
        _275_ = 0
      end
      return {["gensym-base"] = setmetatable({}, {__index = (parent and parent["gensym-base"])}), autogensyms = setmetatable({}, {__index = (parent and parent.autogensyms)}), depth = _275_, gensyms = setmetatable({}, {__index = (parent and parent.gensyms)}), hashfn = (parent and parent.hashfn), includes = setmetatable({}, {__index = (parent and parent.includes)}), macros = setmetatable({}, {__index = (parent and parent.macros)}), manglings = setmetatable({}, {__index = (parent and parent.manglings)}), parent = parent, refedglobals = {}, specials = setmetatable({}, {__index = (parent and parent.specials)}), symmeta = setmetatable({}, {__index = (parent and parent.symmeta)}), unmanglings = setmetatable({}, {__index = (parent and parent.unmanglings)}), vararg = (parent and parent.vararg)}
    end
    local function assert_msg(ast, msg)
      local ast_tbl = nil
      if ("table" == type(ast)) then
        ast_tbl = ast
      else
        ast_tbl = {}
      end
      local m = getmetatable(ast)
      local filename = ((m and m.filename) or ast_tbl.filename or "unknown")
      local line = ((m and m.line) or ast_tbl.line or "?")
      local col = ((m and m.col) or ast_tbl.col or "?")
      local target = tostring((utils["sym?"](ast_tbl[1]) or ast_tbl[1] or "()"))
      return string.format("%s:%s:%s: Compile error in '%s': %s", filename, line, col, target, msg)
    end
    local function assert_compile(condition, msg, ast, _3ffallback_ast)
      if not condition then
        local _278_ = (utils.root.options or {})
        local error_pinpoint = _278_["error-pinpoint"]
        local source = _278_["source"]
        local unfriendly = _278_["unfriendly"]
        local ast0 = nil
        if next(utils["ast-source"](ast)) then
          ast0 = ast
        else
          ast0 = (_3ffallback_ast or {})
        end
        if (nil == utils.hook("assert-compile", condition, msg, ast0, utils.root.reset)) then
          utils.root.reset()
          if unfriendly then
            error(assert_msg(ast0, msg), 0)
          else
            friend["assert-compile"](condition, msg, ast0, source, {["error-pinpoint"] = error_pinpoint})
          end
        end
      end
      return condition
    end
    scopes.global = make_scope()
    scopes.global.vararg = true
    scopes.compiler = make_scope(scopes.global)
    scopes.macro = scopes.global
    local function serialize_string(str)
      local function _283_(_241)
        return ("\\" .. _241:byte())
      end
      return string.gsub(string.gsub(string.gsub(string.format("%q", str), "\\\n", "\\n"), "\\9", "\\t"), "[\128-\255]", _283_)
    end
    local function global_mangling(str)
      if utils["valid-lua-identifier?"](str) then
        return str
      else
        local function _284_(_241)
          return string.format("_%02x", _241:byte())
        end
        return ("__fnl_global__" .. str:gsub("[^%w]", _284_))
      end
    end
    local function global_unmangling(identifier)
      local _286_0 = string.match(identifier, "^__fnl_global__(.*)$")
      if (nil ~= _286_0) then
        local rest = _286_0
        local _287_0 = nil
        local function _288_(_241)
          return string.char(tonumber(_241:sub(2), 16))
        end
        _287_0 = string.gsub(rest, "_[%da-f][%da-f]", _288_)
        return _287_0
      else
        local _ = _286_0
        return identifier
      end
    end
    local function global_allowed_3f(name)
      local allowed = nil
      do
        local _290_0 = utils.root.options
        if (nil ~= _290_0) then
          _290_0 = _290_0.allowedGlobals
        end
        allowed = _290_0
      end
      return (not allowed or utils["member?"](name, allowed))
    end
    local function unique_mangling(original, mangling, scope, append)
      if scope.unmanglings[mangling] then
        return unique_mangling(original, (original .. append), scope, (append + 1))
      else
        return mangling
      end
    end
    local function apply_deferred_scope_changes(scope, deferred_scope_changes, ast)
      for raw, mangled in pairs(deferred_scope_changes.manglings) do
        assert_compile(not scope.refedglobals[mangled], ("use of global " .. raw .. " is aliased by a local"), ast)
        scope.manglings[raw] = mangled
      end
      for raw, symmeta in pairs(deferred_scope_changes.symmeta) do
        scope.symmeta[raw] = symmeta
      end
      return nil
    end
    local function combine_parts(parts, scope)
      local ret = (scope.manglings[parts[1]] or global_mangling(parts[1]))
      for i = 2, #parts do
        if utils["valid-lua-identifier?"](parts[i]) then
          if (parts["multi-sym-method-call"] and (i == #parts)) then
            ret = (ret .. ":" .. parts[i])
          else
            ret = (ret .. "." .. parts[i])
          end
        else
          ret = (ret .. "[" .. serialize_string(parts[i]) .. "]")
        end
      end
      return ret
    end
    local function root_scope(scope)
      return ((utils.root and utils.root.scope) or (scope.parent and root_scope(scope.parent)) or scope)
    end
    local function next_append(root_scope_2a)
      root_scope_2a["gensym-append"] = ((root_scope_2a["gensym-append"] or 0) + 1)
      return ("_" .. root_scope_2a["gensym-append"] .. "_")
    end
    local function gensym(scope, _3fbase, _3fsuffix)
      local root_scope_2a = root_scope(scope)
      local mangling = ((_3fbase or "") .. next_append(root_scope_2a) .. (_3fsuffix or ""))
      while scope.unmanglings[mangling] do
        mangling = ((_3fbase or "") .. next_append(root_scope_2a) .. (_3fsuffix or ""))
      end
      if (_3fbase and (0 < #_3fbase)) then
        scope["gensym-base"][mangling] = _3fbase
      end
      scope.gensyms[mangling] = true
      return mangling
    end
    local function combine_auto_gensym(parts, first)
      parts[1] = first
      local last = table.remove(parts)
      local last2 = table.remove(parts)
      local last_joiner = ((parts["multi-sym-method-call"] and ":") or ".")
      table.insert(parts, (last2 .. last_joiner .. last))
      return table.concat(parts, ".")
    end
    local function autogensym(base, scope)
      local _296_0 = utils["multi-sym?"](base)
      if (nil ~= _296_0) then
        local parts = _296_0
        return combine_auto_gensym(parts, autogensym(parts[1], scope))
      else
        local _ = _296_0
        local function _297_()
          local mangling = gensym(scope, base:sub(1, -2), "auto")
          scope.autogensyms[base] = mangling
          return mangling
        end
        return (scope.autogensyms[base] or _297_())
      end
    end
    local function check_binding_valid(symbol, scope, ast, _3fopts)
      local name = tostring(symbol)
      local macro_3f = nil
      do
        local _299_0 = _3fopts
        if (nil ~= _299_0) then
          _299_0 = _299_0["macro?"]
        end
        macro_3f = _299_0
      end
      assert_compile(("&" ~= name:match("[&.:]")), "invalid character: &", symbol)
      assert_compile(not name:find("^%."), "invalid character: .", symbol)
      assert_compile(not (scope.specials[name] or (not macro_3f and scope.macros[name])), ("local %s was overshadowed by a special form or macro"):format(name), ast)
      return assert_compile(not utils["quoted?"](symbol), string.format("macro tried to bind %s without gensym", name), symbol)
    end
    local function declare_local(symbol, scope, ast, _3fvar_3f, _3fdeferred_scope_changes)
      check_binding_valid(symbol, scope, ast)
      assert_compile(not utils["multi-sym?"](symbol), ("unexpected multi symbol " .. tostring(symbol)), ast)
      local str = tostring(symbol)
      local raw = nil
      if (utils["lua-keyword?"](str) or str:match("^%d")) then
        raw = ("_" .. str)
      else
        raw = str
      end
      local mangling = nil
      local function _302_(_241)
        return string.format("_%02x", _241:byte())
      end
      mangling = string.gsub(string.gsub(raw, "-", "_"), "[^%w_]", _302_)
      local unique = unique_mangling(mangling, mangling, scope, 0)
      scope.unmanglings[unique] = (scope["gensym-base"][str] or str)
      do
        local target = (_3fdeferred_scope_changes or scope)
        target.manglings[str] = unique
        target.symmeta[str] = {symbol = symbol, var = _3fvar_3f}
      end
      return unique
    end
    local function hashfn_arg_name(name, multi_sym_parts, scope)
      if not scope.hashfn then
        return nil
      elseif (name == "$") then
        return "$1"
      elseif multi_sym_parts then
        if (multi_sym_parts and (multi_sym_parts[1] == "$")) then
          multi_sym_parts[1] = "$1"
        end
        return table.concat(multi_sym_parts, ".")
      end
    end
    local function symbol_to_expression(symbol, scope, _3freference_3f)
      utils.hook("symbol-to-expression", symbol, scope, _3freference_3f)
      local name = symbol[1]
      local multi_sym_parts = utils["multi-sym?"](name)
      local name0 = (hashfn_arg_name(name, multi_sym_parts, scope) or name)
      local parts = (multi_sym_parts or {name0})
      local etype = (((1 < #parts) and "expression") or "sym")
      local local_3f = scope.manglings[parts[1]]
      if (local_3f and scope.symmeta[parts[1]]) then
        scope.symmeta[parts[1]]["used"] = true
        symbol.referent = scope.symmeta[parts[1]].symbol
      end
      assert_compile(not scope.macros[parts[1]], "tried to reference a macro without calling it", symbol)
      assert_compile((not scope.specials[parts[1]] or ("require" == parts[1])), "tried to reference a special form without calling it", symbol)
      assert_compile((not _3freference_3f or local_3f or ("_ENV" == parts[1]) or global_allowed_3f(parts[1])), ("unknown identifier: " .. tostring(parts[1])), symbol)
      local function _307_()
        local _306_0 = utils.root.options
        if (nil ~= _306_0) then
          _306_0 = _306_0.allowedGlobals
        end
        return _306_0
      end
      if (_307_() and not local_3f and scope.parent) then
        scope.parent.refedglobals[parts[1]] = true
      end
      return utils.expr(combine_parts(parts, scope), etype)
    end
    local function emit(chunk, out, _3fast)
      if (type(out) == "table") then
        return table.insert(chunk, out)
      else
        return table.insert(chunk, {ast = _3fast, leaf = out})
      end
    end
    local function peephole(chunk)
      if chunk.leaf then
        return chunk
      elseif ((3 <= #chunk) and (chunk[(#chunk - 2)].leaf == "do") and not chunk[(#chunk - 1)].leaf and (chunk[#chunk].leaf == "end")) then
        local kid = peephole(chunk[(#chunk - 1)])
        local new_chunk = {ast = chunk.ast}
        for i = 1, (#chunk - 3) do
          table.insert(new_chunk, peephole(chunk[i]))
        end
        for i = 1, #kid do
          table.insert(new_chunk, kid[i])
        end
        return new_chunk
      else
        local tbl_17_ = {}
        local i_18_ = #tbl_17_
        for _, x in ipairs(chunk) do
          local val_19_ = peephole(x)
          if (nil ~= val_19_) then
            i_18_ = (i_18_ + 1)
            tbl_17_[i_18_] = val_19_
          end
        end
        return tbl_17_
      end
    end
    local function flatten_chunk_correlated(main_chunk, options)
      local function flatten(chunk, out, last_line, file)
        local last_line0 = last_line
        if chunk.leaf then
          out[last_line0] = ((out[last_line0] or "") .. " " .. chunk.leaf)
        else
          for _, subchunk in ipairs(chunk) do
            if (subchunk.leaf or next(subchunk)) then
              local source = utils["ast-source"](subchunk.ast)
              if (file == source.filename) then
                last_line0 = math.max(last_line0, (source.line or 0))
              end
              last_line0 = flatten(subchunk, out, last_line0, file)
            end
          end
        end
        return last_line0
      end
      local out = {}
      local last = flatten(main_chunk, out, 1, options.filename)
      for i = 1, last do
        if (out[i] == nil) then
          out[i] = ""
        end
      end
      return table.concat(out, "\n")
    end
    local function flatten_chunk(file_sourcemap, chunk, tab, depth)
      if chunk.leaf then
        local _317_ = utils["ast-source"](chunk.ast)
        local endline = _317_["endline"]
        local filename = _317_["filename"]
        local line = _317_["line"]
        if ("end" == chunk.leaf) then
          table.insert(file_sourcemap, {filename, (endline or line)})
        else
          table.insert(file_sourcemap, {filename, line})
        end
        return chunk.leaf
      else
        local tab0 = nil
        do
          local _319_0 = tab
          if (_319_0 == true) then
            tab0 = "  "
          elseif (_319_0 == false) then
            tab0 = ""
          elseif (nil ~= _319_0) then
            local tab1 = _319_0
            tab0 = tab1
          elseif (_319_0 == nil) then
            tab0 = ""
          else
          tab0 = nil
          end
        end
        local _321_
        do
          local tbl_17_ = {}
          local i_18_ = #tbl_17_
          for _, c in ipairs(chunk) do
            local val_19_ = nil
            if (c.leaf or next(c)) then
              local sub = flatten_chunk(file_sourcemap, c, tab0, (depth + 1))
              if (0 < depth) then
                val_19_ = (tab0 .. sub:gsub("\n", ("\n" .. tab0)))
              else
                val_19_ = sub
              end
            else
            val_19_ = nil
            end
            if (nil ~= val_19_) then
              i_18_ = (i_18_ + 1)
              tbl_17_[i_18_] = val_19_
            end
          end
          _321_ = tbl_17_
        end
        return table.concat(_321_, "\n")
      end
    end
    local sourcemap = {}
    local function make_short_src(source)
      local source0 = source:gsub("\n", " ")
      if (#source0 <= 49) then
        return ("[fennel \"" .. source0 .. "\"]")
      else
        return ("[fennel \"" .. source0:sub(1, 46) .. "...\"]")
      end
    end
    local function flatten(chunk, options)
      local chunk0 = peephole(chunk)
      local indent = (options.indent or "  ")
      if options.correlate then
        return flatten_chunk_correlated(chunk0, options), {}
      else
        local file_sourcemap = {}
        local src = flatten_chunk(file_sourcemap, chunk0, indent, 0)
        file_sourcemap.short_src = (options.filename or make_short_src((options.source or src)))
        if options.filename then
          file_sourcemap.key = ("@" .. options.filename)
        else
          file_sourcemap.key = src
        end
        sourcemap[file_sourcemap.key] = file_sourcemap
        return src, file_sourcemap
      end
    end
    local function make_metadata()
      local function _329_(self, tgt, _3fkey)
        if self[tgt] then
          if (nil ~= _3fkey) then
            return self[tgt][_3fkey]
          else
            return self[tgt]
          end
        end
      end
      local function _332_(self, tgt, key, value)
        self[tgt] = (self[tgt] or {})
        self[tgt][key] = value
        return tgt
      end
      local function _333_(self, tgt, ...)
        local kv_len = select("#", ...)
        local kvs = {...}
        if ((kv_len % 2) ~= 0) then
          error("metadata:setall() expected even number of k/v pairs")
        end
        self[tgt] = (self[tgt] or {})
        for i = 1, kv_len, 2 do
          self[tgt][kvs[i]] = kvs[(i + 1)]
        end
        return tgt
      end
      return setmetatable({}, {__index = {get = _329_, set = _332_, setall = _333_}, __mode = "k"})
    end
    local function exprs1(exprs)
      local _335_
      do
        local tbl_17_ = {}
        local i_18_ = #tbl_17_
        for _, e in ipairs(exprs) do
          local val_19_ = tostring(e)
          if (nil ~= val_19_) then
            i_18_ = (i_18_ + 1)
            tbl_17_[i_18_] = val_19_
          end
        end
        _335_ = tbl_17_
      end
      return table.concat(_335_, ", ")
    end
    local function keep_side_effects(exprs, chunk, _3fstart, ast)
      for j = (_3fstart or 1), #exprs do
        local subexp = exprs[j]
        if ((subexp.type == "expression") and (subexp[1] ~= "nil")) then
          emit(chunk, ("do local _ = %s end"):format(tostring(subexp)), ast)
        elseif (subexp.type == "statement") then
          local code = tostring(subexp)
          local disambiguated = nil
          if (code:byte() == 40) then
            disambiguated = ("do end " .. code)
          else
            disambiguated = code
          end
          emit(chunk, disambiguated, ast)
        end
      end
      return nil
    end
    local function handle_compile_opts(exprs, parent, opts, ast)
      if opts.nval then
        local n = opts.nval
        local len = #exprs
        if (n ~= len) then
          if (n < len) then
            keep_side_effects(exprs, parent, (n + 1), ast)
            for i = (n + 1), len do
              exprs[i] = nil
            end
          else
            for i = (#exprs + 1), n do
              exprs[i] = utils.expr("nil", "literal")
            end
          end
        end
      end
      if opts.tail then
        emit(parent, string.format("return %s", exprs1(exprs)), ast)
      end
      if opts.target then
        local result = exprs1(exprs)
        local function _343_()
          if (result == "") then
            return "nil"
          else
            return result
          end
        end
        emit(parent, string.format("%s = %s", opts.target, _343_()), ast)
      end
      if (opts.tail or opts.target) then
        return {returned = true}
      else
        exprs["returned"] = true
        return exprs
      end
    end
    local function find_macro(ast, scope)
      local macro_2a = nil
      do
        local _346_0 = utils["sym?"](ast[1])
        if (_346_0 ~= nil) then
          local _347_0 = tostring(_346_0)
          if (_347_0 ~= nil) then
            macro_2a = scope.macros[_347_0]
          else
            macro_2a = _347_0
          end
        else
          macro_2a = _346_0
        end
      end
      local multi_sym_parts = utils["multi-sym?"](ast[1])
      if (not macro_2a and multi_sym_parts) then
        local nested_macro = utils["get-in"](scope.macros, multi_sym_parts)
        assert_compile((not scope.macros[multi_sym_parts[1]] or (type(nested_macro) == "function")), "macro not found in imported macro module", ast)
        return nested_macro
      else
        return macro_2a
      end
    end
    local function propagate_trace_info(_351_0, _index, node)
      local _352_ = _351_0
      local byteend = _352_["byteend"]
      local bytestart = _352_["bytestart"]
      local filename = _352_["filename"]
      local line = _352_["line"]
      do
        local src = utils["ast-source"](node)
        if (("table" == type(node)) and (filename ~= src.filename)) then
          src.filename, src.line, src["from-macro?"] = filename, line, true
          src.bytestart, src.byteend = bytestart, byteend
        end
      end
      return ("table" == type(node))
    end
    local function quote_literal_nils(index, node, parent)
      if (parent and utils["list?"](parent)) then
        for i = 1, utils.maxn(parent) do
          local _354_0 = parent[i]
          if (_354_0 == nil) then
            parent[i] = utils.sym("nil")
          end
        end
      end
      return index, node, parent
    end
    local function built_in_3f(m)
      local found_3f = false
      for _, f in pairs(scopes.global.macros) do
        if found_3f then break end
        found_3f = (f == m)
      end
      return found_3f
    end
    local function macroexpand_2a(ast, scope, _3fonce)
      local _357_0 = nil
      if utils["list?"](ast) then
        _357_0 = find_macro(ast, scope)
      else
      _357_0 = nil
      end
      if (_357_0 == false) then
        return ast
      elseif (nil ~= _357_0) then
        local macro_2a = _357_0
        local old_scope = scopes.macro
        local _ = nil
        scopes.macro = scope
        _ = nil
        local ok, transformed = nil, nil
        local function _359_()
          return macro_2a(unpack(ast, 2))
        end
        local function _360_()
          if built_in_3f(macro_2a) then
            return tostring
          else
            return debug.traceback
          end
        end
        ok, transformed = xpcall(_359_, _360_())
        local function _361_(...)
          return propagate_trace_info(ast, quote_literal_nils(...))
        end
        utils["walk-tree"](transformed, _361_)
        scopes.macro = old_scope
        assert_compile(ok, transformed, ast)
        utils.hook("macroexpand", ast, transformed, scope)
        if (_3fonce or not transformed) then
          return transformed
        else
          return macroexpand_2a(transformed, scope)
        end
      else
        local _ = _357_0
        return ast
      end
    end
    local function compile_special(ast, scope, parent, opts, special)
      local exprs = (special(ast, scope, parent, opts) or utils.expr("nil", "literal"))
      local exprs0 = nil
      if ("table" ~= type(exprs)) then
        exprs0 = utils.expr(exprs, "expression")
      else
        exprs0 = exprs
      end
      local exprs2 = nil
      if utils["expr?"](exprs0) then
        exprs2 = {exprs0}
      else
        exprs2 = exprs0
      end
      if not exprs2.returned then
        return handle_compile_opts(exprs2, parent, opts, ast)
      elseif (opts.tail or opts.target) then
        return {returned = true}
      else
        return exprs2
      end
    end
    local function callable_3f(_367_0, ctype, callee)
      local _368_ = _367_0
      local call_ast = _368_[1]
      if ("literal" == ctype) then
        return ("\"" == string.sub(callee, 1, 1))
      else
        return (utils["sym?"](call_ast) or utils["list?"](call_ast))
      end
    end
    local function compile_function_call(ast, scope, parent, opts, compile1, len)
      local _370_ = compile1(ast[1], scope, parent, {nval = 1})[1]
      local callee = _370_[1]
      local ctype = _370_["type"]
      local fargs = {}
      assert_compile(callable_3f(ast, ctype, callee), ("cannot call literal value " .. tostring(ast[1])), ast)
      for i = 2, len do
        local subexprs = nil
        local _371_
        if (i ~= len) then
          _371_ = 1
        else
        _371_ = nil
        end
        subexprs = compile1(ast[i], scope, parent, {nval = _371_})
        table.insert(fargs, subexprs[1])
        if (i == len) then
          for j = 2, #subexprs do
            table.insert(fargs, subexprs[j])
          end
        else
          keep_side_effects(subexprs, parent, 2, ast[i])
        end
      end
      local pat = nil
      if ("literal" == ctype) then
        pat = "(%s)(%s)"
      else
        pat = "%s(%s)"
      end
      local call = string.format(pat, tostring(callee), exprs1(fargs))
      return handle_compile_opts({utils.expr(call, "statement")}, parent, opts, ast)
    end
    local function compile_call(ast, scope, parent, opts, compile1)
      utils.hook("call", ast, scope)
      local len = #ast
      local first = ast[1]
      local multi_sym_parts = utils["multi-sym?"](first)
      local special = (utils["sym?"](first) and scope.specials[tostring(first)])
      assert_compile((0 < len), "expected a function, macro, or special to call", ast)
      if special then
        return compile_special(ast, scope, parent, opts, special)
      elseif (multi_sym_parts and multi_sym_parts["multi-sym-method-call"]) then
        local table_with_method = table.concat({unpack(multi_sym_parts, 1, (#multi_sym_parts - 1))}, ".")
        local method_to_call = multi_sym_parts[#multi_sym_parts]
        local new_ast = utils.list(utils.sym(":", ast), utils.sym(table_with_method, ast), method_to_call, select(2, unpack(ast)))
        return compile1(new_ast, scope, parent, opts)
      else
        return compile_function_call(ast, scope, parent, opts, compile1, len)
      end
    end
    local function compile_varg(ast, scope, parent, opts)
      local _376_
      if scope.hashfn then
        _376_ = "use $... in hashfn"
      else
        _376_ = "unexpected vararg"
      end
      assert_compile(scope.vararg, _376_, ast)
      return handle_compile_opts({utils.expr("...", "varg")}, parent, opts, ast)
    end
    local function compile_sym(ast, scope, parent, opts)
      local multi_sym_parts = utils["multi-sym?"](ast)
      assert_compile(not (multi_sym_parts and multi_sym_parts["multi-sym-method-call"]), "multisym method calls may only be in call position", ast)
      local e = nil
      if (ast[1] == "nil") then
        e = utils.expr("nil", "literal")
      else
        e = symbol_to_expression(ast, scope, true)
      end
      return handle_compile_opts({e}, parent, opts, ast)
    end
    local view_opts = nil
    do
      local nan = tostring((0 / 0))
      local _379_
      if (45 == nan:byte()) then
        _379_ = "(0/0)"
      else
        _379_ = "(- (0/0))"
      end
      local _381_
      if (45 == nan:byte()) then
        _381_ = "(- (0/0))"
      else
        _381_ = "(0/0)"
      end
      view_opts = {["negative-infinity"] = "(-1/0)", ["negative-nan"] = _379_, infinity = "(1/0)", nan = _381_}
    end
    local function compile_scalar(ast, _scope, parent, opts)
      local compiled = nil
      do
        local _383_0 = type(ast)
        if (_383_0 == "nil") then
          compiled = "nil"
        elseif (_383_0 == "boolean") then
          compiled = tostring(ast)
        elseif (_383_0 == "string") then
          compiled = serialize_string(ast)
        elseif (_383_0 == "number") then
          compiled = view(ast, view_opts)
        else
        compiled = nil
        end
      end
      return handle_compile_opts({utils.expr(compiled, "literal")}, parent, opts)
    end
    local function compile_table(ast, scope, parent, opts, compile1)
      local function escape_key(k)
        if ((type(k) == "string") and utils["valid-lua-identifier?"](k)) then
          return k
        else
          local _385_ = compile1(k, scope, parent, {nval = 1})
          local compiled = _385_[1]
          return ("[" .. tostring(compiled) .. "]")
        end
      end
      local keys = {}
      local buffer = nil
      do
        local tbl_17_ = {}
        local i_18_ = #tbl_17_
        for i, elem in ipairs(ast) do
          local val_19_ = nil
          do
            local nval = ((nil ~= ast[(i + 1)]) and 1)
            keys[i] = true
            val_19_ = exprs1(compile1(elem, scope, parent, {nval = nval}))
          end
          if (nil ~= val_19_) then
            i_18_ = (i_18_ + 1)
            tbl_17_[i_18_] = val_19_
          end
        end
        buffer = tbl_17_
      end
      do
        local tbl_17_ = buffer
        local i_18_ = #tbl_17_
        for k in utils.stablepairs(ast) do
          local val_19_ = nil
          if not keys[k] then
            local _388_ = compile1(ast[k], scope, parent, {nval = 1})
            local v = _388_[1]
            val_19_ = string.format("%s = %s", escape_key(k), tostring(v))
          else
          val_19_ = nil
          end
          if (nil ~= val_19_) then
            i_18_ = (i_18_ + 1)
            tbl_17_[i_18_] = val_19_
          end
        end
      end
      return handle_compile_opts({utils.expr(("{" .. table.concat(buffer, ", ") .. "}"), "expression")}, parent, opts, ast)
    end
    local function compile1(ast, scope, parent, _3fopts)
      local opts = (_3fopts or {})
      local ast0 = macroexpand_2a(ast, scope)
      if utils["list?"](ast0) then
        return compile_call(ast0, scope, parent, opts, compile1)
      elseif utils["varg?"](ast0) then
        return compile_varg(ast0, scope, parent, opts)
      elseif utils["sym?"](ast0) then
        return compile_sym(ast0, scope, parent, opts)
      elseif (type(ast0) == "table") then
        return compile_table(ast0, scope, parent, opts, compile1)
      elseif ((type(ast0) == "nil") or (type(ast0) == "boolean") or (type(ast0) == "number") or (type(ast0) == "string")) then
        return compile_scalar(ast0, scope, parent, opts)
      else
        return assert_compile(false, ("could not compile value of type " .. type(ast0)), ast0)
      end
    end
    local function destructure(to, from, ast, scope, parent, opts)
      local opts0 = (opts or {})
      local _392_ = opts0
      local declaration = _392_["declaration"]
      local forceglobal = _392_["forceglobal"]
      local forceset = _392_["forceset"]
      local isvar = _392_["isvar"]
      local symtype = _392_["symtype"]
      local symtype0 = ("_" .. (symtype or "dst"))
      local setter = nil
      if declaration then
        setter = "local %s = %s"
      else
        setter = "%s = %s"
      end
      local deferred_scope_changes = {manglings = {}, symmeta = {}}
      local function getname(symbol, ast0)
        local raw = symbol[1]
        assert_compile(not (opts0.nomulti and utils["multi-sym?"](raw)), ("unexpected multi symbol " .. raw), ast0)
        if declaration then
          return declare_local(symbol, scope, symbol, isvar, deferred_scope_changes)
        else
          local parts = (utils["multi-sym?"](raw) or {raw})
          local _394_ = parts
          local first = _394_[1]
          local meta = scope.symmeta[first]
          assert_compile(not raw:find(":"), "cannot set method sym", symbol)
          if ((#parts == 1) and not forceset) then
            assert_compile(not (forceglobal and meta), string.format("global %s conflicts with local", tostring(symbol)), symbol)
            assert_compile(not (meta and not meta.var), ("expected var " .. raw), symbol)
          end
          assert_compile((meta or not opts0.noundef or (scope.hashfn and ("$" == first)) or global_allowed_3f(first)), ("expected local " .. first), symbol)
          if forceglobal then
            assert_compile(not scope.symmeta[scope.unmanglings[raw]], ("global " .. raw .. " conflicts with local"), symbol)
            scope.manglings[raw] = global_mangling(raw)
            scope.unmanglings[global_mangling(raw)] = raw
            local _397_
            do
              local _396_0 = utils.root.options
              if (nil ~= _396_0) then
                _396_0 = _396_0.allowedGlobals
              end
              _397_ = _396_0
            end
            if _397_ then
              local _400_
              do
                local _399_0 = utils.root.options
                if (nil ~= _399_0) then
                  _399_0 = _399_0.allowedGlobals
                end
                _400_ = _399_0
              end
              table.insert(_400_, raw)
            end
          end
          return symbol_to_expression(symbol, scope)[1]
        end
      end
      local function compile_top_target(lvalues)
        local inits = nil
        do
          local tbl_17_ = {}
          local i_18_ = #tbl_17_
          for _, l in ipairs(lvalues) do
            local val_19_ = nil
            if scope.manglings[l] then
              val_19_ = l
            else
              val_19_ = "nil"
            end
            if (nil ~= val_19_) then
              i_18_ = (i_18_ + 1)
              tbl_17_[i_18_] = val_19_
            end
          end
          inits = tbl_17_
        end
        local init = table.concat(inits, ", ")
        local lvalue = table.concat(lvalues, ", ")
        local plast = parent[#parent]
        local plen = #parent
        local ret = compile1(from, scope, parent, {target = lvalue})
        if declaration then
          for pi = plen, #parent do
            if (parent[pi] == plast) then
              plen = pi
            end
          end
          if ((#parent == (plen + 1)) and parent[#parent].leaf) then
            parent[#parent]["leaf"] = ("local " .. parent[#parent].leaf)
          elseif (init == "nil") then
            table.insert(parent, (plen + 1), {ast = ast, leaf = ("local " .. lvalue)})
          else
            table.insert(parent, (plen + 1), {ast = ast, leaf = ("local " .. lvalue .. " = " .. init)})
          end
        end
        return ret
      end
      local function destructure_sym(left, rightexprs, up1, top_3f)
        local lname = getname(left, up1)
        check_binding_valid(left, scope, left)
        if top_3f then
          return compile_top_target({lname})
        else
          return emit(parent, setter:format(lname, exprs1(rightexprs)), left)
        end
      end
      local function dynamic_set_target(_411_0)
        local _412_ = _411_0
        local _ = _412_[1]
        local target = _412_[2]
        local keys = {(table.unpack or unpack)(_412_, 3)}
        assert_compile(utils["sym?"](target), "dynamic set needs symbol target", ast)
        assert_compile(next(keys), "dynamic set needs at least one key", ast)
        local keys0 = nil
        do
          local tbl_17_ = {}
          local i_18_ = #tbl_17_
          for _0, k in ipairs(keys) do
            local val_19_ = tostring(compile1(k, scope, parent, {nval = 1})[1])
            if (nil ~= val_19_) then
              i_18_ = (i_18_ + 1)
              tbl_17_[i_18_] = val_19_
            end
          end
          keys0 = tbl_17_
        end
        return string.format("%s[%s]", tostring(symbol_to_expression(target, scope, true)), table.concat(keys0, "]["))
      end
      local function destructure_values(left, rightexprs, up1, destructure1, top_3f)
        local left_names, tables = {}, {}
        for i, name in ipairs(left) do
          if utils["sym?"](name) then
            table.insert(left_names, getname(name, up1))
          elseif utils["call-of?"](name, ".") then
            table.insert(left_names, dynamic_set_target(name))
          else
            local symname = gensym(scope, symtype0)
            table.insert(left_names, symname)
            tables[i] = {name, utils.expr(symname, "sym")}
          end
        end
        assert_compile(left[1], "must provide at least one value", left)
        if top_3f then
          compile_top_target(left_names)
        elseif utils["expr?"](rightexprs) then
          emit(parent, setter:format(table.concat(left_names, ","), exprs1(rightexprs)), left)
        else
          local names = table.concat(left_names, ",")
          local target = nil
          if declaration then
            target = ("local " .. names)
          else
            target = names
          end
          emit(parent, compile1(rightexprs, scope, parent, {target = target}), left)
        end
        for _, pair in utils.stablepairs(tables) do
          destructure1(pair[1], {pair[2]}, left)
        end
        return nil
      end
      local unpack_fn = "function (t, k, e)\n                        local mt = getmetatable(t)\n                        if 'table' == type(mt) and mt.__fennelrest then\n                          return mt.__fennelrest(t, k)\n                        elseif e then\n                          local rest = {}\n                          for k, v in pairs(t) do\n                            if not e[k] then rest[k] = v end\n                          end\n                          return rest\n                        else\n                          return {(table.unpack or unpack)(t, k)}\n                        end\n                      end"
      local function destructure_kv_rest(s, v, left, excluded_keys, destructure1)
        local exclude_str = nil
        local _417_
        do
          local tbl_17_ = {}
          local i_18_ = #tbl_17_
          for _, k in ipairs(excluded_keys) do
            local val_19_ = string.format("[%s] = true", serialize_string(k))
            if (nil ~= val_19_) then
              i_18_ = (i_18_ + 1)
              tbl_17_[i_18_] = val_19_
            end
          end
          _417_ = tbl_17_
        end
        exclude_str = table.concat(_417_, ", ")
        local subexpr = utils.expr(string.format(string.gsub(("(" .. unpack_fn .. ")(%s, %s, {%s})"), "\n%s*", " "), s, tostring(v), exclude_str), "expression")
        return destructure1(v, {subexpr}, left)
      end
      local function destructure_rest(s, k, left, destructure1)
        local unpack_str = ("(" .. unpack_fn .. ")(%s, %s)")
        local formatted = string.format(string.gsub(unpack_str, "\n%s*", " "), s, k)
        local subexpr = utils.expr(formatted, "expression")
        local function _419_()
          local next_symbol = left[(k + 2)]
          return ((nil == next_symbol) or utils["sym?"](next_symbol, "&as"))
        end
        assert_compile((utils["sequence?"](left) and _419_()), "expected rest argument before last parameter", left)
        return destructure1(left[(k + 1)], {subexpr}, left)
      end
      local function optimize_table_destructure_3f(left, right)
        local function _420_()
          local all = next(left)
          for _, d in ipairs(left) do
            if not all then break end
            all = ((utils["sym?"](d) and not tostring(d):find("^&")) or (utils["list?"](d) and utils["sym?"](d[1], ".")))
          end
          return all
        end
        return (utils["sequence?"](left) and utils["sequence?"](right) and _420_())
      end
      local function destructure_table(left, rightexprs, top_3f, destructure1, up1)
        if optimize_table_destructure_3f(left, rightexprs) then
          return destructure_values(utils.list(unpack(left)), utils.list(utils.sym("values"), unpack(rightexprs)), up1, destructure1)
        else
          local right = nil
          do
            local _421_0 = nil
            if top_3f then
              _421_0 = exprs1(compile1(from, scope, parent))
            else
              _421_0 = exprs1(rightexprs)
            end
            if (_421_0 == "") then
              right = "nil"
            elseif (nil ~= _421_0) then
              local right0 = _421_0
              right = right0
            else
            right = nil
            end
          end
          local s = nil
          if utils["sym?"](rightexprs) then
            s = right
          else
            s = gensym(scope, symtype0)
          end
          local excluded_keys = {}
          if not utils["sym?"](rightexprs) then
            emit(parent, string.format("local %s = %s", s, right), left)
          end
          for k, v in utils.stablepairs(left) do
            if not (("number" == type(k)) and tostring(left[(k - 1)]):find("^&")) then
              if (utils["sym?"](k) and (tostring(k) == "&")) then
                destructure_kv_rest(s, v, left, excluded_keys, destructure1)
              elseif (utils["sym?"](v) and (tostring(v) == "&")) then
                destructure_rest(s, k, left, destructure1)
              elseif (utils["sym?"](k) and (tostring(k) == "&as")) then
                destructure_sym(v, {utils.expr(tostring(s))}, left)
              elseif (utils["sequence?"](left) and (tostring(v) == "&as")) then
                local _, next_sym, trailing = select(k, unpack(left))
                assert_compile((nil == trailing), "expected &as argument before last parameter", left)
                destructure_sym(next_sym, {utils.expr(tostring(s))}, left)
              else
                local key = nil
                if (type(k) == "string") then
                  key = serialize_string(k)
                else
                  key = k
                end
                local subexpr = utils.expr(("%s[%s]"):format(s, key), "expression")
                if (type(k) == "string") then
                  table.insert(excluded_keys, k)
                end
                destructure1(v, subexpr, left)
              end
            end
          end
          return nil
        end
      end
      local function destructure1(left, rightexprs, up1, top_3f)
        if (utils["sym?"](left) and (left[1] ~= "nil")) then
          destructure_sym(left, rightexprs, up1, top_3f)
        elseif utils["table?"](left) then
          destructure_table(left, rightexprs, top_3f, destructure1, up1)
        elseif utils["call-of?"](left, ".") then
          destructure_values({left}, rightexprs, up1, destructure1)
        elseif utils["list?"](left) then
          assert_compile(top_3f, "can't nest multi-value destructuring", left)
          destructure_values(left, rightexprs, up1, destructure1, true)
        else
          assert_compile(false, string.format("unable to bind %s %s", type(left), tostring(left)), (((type(up1[2]) == "table") and up1[2]) or up1))
        end
        return (top_3f and {returned = true})
      end
      local ret = destructure1(to, from, ast, true)
      utils.hook("destructure", from, to, scope, opts0)
      apply_deferred_scope_changes(scope, deferred_scope_changes, ast)
      return ret
    end
    local function require_include(ast, scope, parent, opts)
      opts.fallback = function(e, no_warn)
        if not no_warn then
          utils.warn(("include module not found, falling back to require: %s"):format(tostring(e)), ast)
        end
        return utils.expr(string.format("require(%s)", tostring(e)), "statement")
      end
      return scopes.global.specials.include(ast, scope, parent, opts)
    end
    local function compile_asts(asts, options)
      local opts = utils.copy(options)
      local scope = (opts.scope or make_scope(scopes.global))
      local chunk = {}
      if opts.requireAsInclude then
        scope.specials.require = require_include
      end
      if opts.assertAsRepl then
        scope.macros.assert = scope.macros["assert-repl"]
      end
      local _435_ = utils.root
      _435_["set-reset"](_435_)
      utils.root.chunk, utils.root.scope, utils.root.options = chunk, scope, opts
      for i = 1, #asts do
        local exprs = compile1(asts[i], scope, chunk, {nval = (((i < #asts) and 0) or nil), tail = (i == #asts)})
        keep_side_effects(exprs, chunk, nil, asts[i])
        if (i == #asts) then
          utils.hook("chunk", asts[i], scope)
        end
      end
      utils.root.reset()
      return flatten(chunk, opts)
    end
    local function compile_stream(stream, _3fopts)
      local opts = (_3fopts or {})
      local asts = nil
      do
        local tbl_17_ = {}
        local i_18_ = #tbl_17_
        for _, ast in parser.parser(stream, opts.filename, opts) do
          local val_19_ = ast
          if (nil ~= val_19_) then
            i_18_ = (i_18_ + 1)
            tbl_17_[i_18_] = val_19_
          end
        end
        asts = tbl_17_
      end
      return compile_asts(asts, opts)
    end
    local function compile_string(str, _3fopts)
      return compile_stream(parser["string-stream"](str, _3fopts), _3fopts)
    end
    local function compile(from, _3fopts)
      local _438_0 = type(from)
      if (_438_0 == "userdata") then
        local function _439_()
          local _440_0 = from:read(1)
          if (nil ~= _440_0) then
            return _440_0:byte()
          else
            return _440_0
          end
        end
        return compile_stream(_439_, _3fopts)
      elseif (_438_0 == "function") then
        return compile_stream(from, _3fopts)
      else
        local _ = _438_0
        return compile_asts({from}, _3fopts)
      end
    end
    local function traceback_frame(info)
      if ((info.what == "C") and info.name) then
        return string.format("\9[C]: in function '%s'", info.name)
      elseif (info.what == "C") then
        return "\9[C]: in ?"
      else
        local remap = sourcemap[info.source]
        if (remap and remap[info.currentline]) then
          if ((remap[info.currentline][1] or "unknown") ~= "unknown") then
            info.short_src = sourcemap[("@" .. remap[info.currentline][1])].short_src
          else
            info.short_src = remap.short_src
          end
          info.currentline = (remap[info.currentline][2] or -1)
        end
        if (info.what == "Lua") then
          local function _445_()
            if info.name then
              return ("'" .. info.name .. "'")
            else
              return "?"
            end
          end
          return string.format("\9%s:%d: in function %s", info.short_src, info.currentline, _445_())
        elseif (info.short_src == "(tail call)") then
          return "  (tail call)"
        else
          return string.format("\9%s:%d: in main chunk", info.short_src, info.currentline)
        end
      end
    end
    local lua_getinfo = debug.getinfo
    local function traceback(_3fmsg, _3fstart)
      local _448_0 = type(_3fmsg)
      if ((_448_0 == "nil") or (_448_0 == "string")) then
        local msg = (_3fmsg or "")
        if ((msg:find("^%g+:%d+:%d+ Compile error:.*") or msg:find("^%g+:%d+:%d+ Parse error:.*")) and not utils["debug-on?"]("trace")) then
          return msg
        else
          local lines = {}
          if (msg:find("^%g+:%d+:%d+ Compile error:") or msg:find("^%g+:%d+:%d+ Parse error:")) then
            table.insert(lines, msg)
          else
            local newmsg = msg:gsub("^[^:]*:%d+:%s+", "runtime error: ")
            table.insert(lines, newmsg)
          end
          table.insert(lines, "stack traceback:")
          local done_3f, level = false, (_3fstart or 2)
          while not done_3f do
            do
              local _450_0 = lua_getinfo(level, "Sln")
              if (_450_0 == nil) then
                done_3f = true
              elseif (nil ~= _450_0) then
                local info = _450_0
                table.insert(lines, traceback_frame(info))
              end
            end
            level = (level + 1)
          end
          return table.concat(lines, "\n")
        end
      else
        local _ = _448_0
        return _3fmsg
      end
    end
    local function getinfo(thread_or_level, ...)
      local thread_or_level0 = nil
      if ("number" == type(thread_or_level)) then
        thread_or_level0 = (1 + thread_or_level)
      else
        thread_or_level0 = thread_or_level
      end
      local info = lua_getinfo(thread_or_level0, ...)
      local mapped = (info and sourcemap[info.source])
      if mapped then
        for _, key in ipairs({"currentline", "linedefined", "lastlinedefined"}) do
          local mapped_value = nil
          do
            local _455_0 = mapped
            if (nil ~= _455_0) then
              _455_0 = _455_0[info[key]]
            end
            if (nil ~= _455_0) then
              _455_0 = _455_0[2]
            end
            mapped_value = _455_0
          end
          if (info[key] and mapped_value) then
            info[key] = mapped_value
          end
        end
        if info.activelines then
          local tbl_14_ = {}
          for line in pairs(info.activelines) do
            local k_15_, v_16_ = mapped[line][2], true
            if ((k_15_ ~= nil) and (v_16_ ~= nil)) then
              tbl_14_[k_15_] = v_16_
            end
          end
          info.activelines = tbl_14_
        end
        if (info.what == "Lua") then
          info.what = "Fennel"
        end
      end
      return info
    end
    local function mixed_concat(t, joiner)
      local seen = {}
      local ret, s = "", ""
      for k, v in ipairs(t) do
        table.insert(seen, k)
        ret = (ret .. s .. v)
        s = joiner
      end
      for k, v in utils.stablepairs(t) do
        if not seen[k] then
          ret = (ret .. s .. "[" .. k .. "]" .. "=" .. v)
          s = joiner
        end
      end
      return ret
    end
    local function do_quote(form, scope, parent, runtime_3f)
      local function quote_all(form0, discard_non_numbers)
        local tbl_14_ = {}
        for k, v in utils.stablepairs(form0) do
          local k_15_, v_16_ = nil, nil
          if (type(k) == "number") then
            k_15_, v_16_ = k, do_quote(v, scope, parent, runtime_3f)
          elseif not discard_non_numbers then
            k_15_, v_16_ = do_quote(k, scope, parent, runtime_3f), do_quote(v, scope, parent, runtime_3f)
          else
          k_15_, v_16_ = nil
          end
          if ((k_15_ ~= nil) and (v_16_ ~= nil)) then
            tbl_14_[k_15_] = v_16_
          end
        end
        return tbl_14_
      end
      if utils["varg?"](form) then
        assert_compile(not runtime_3f, "quoted ... may only be used at compile time", form)
        return "_VARARG"
      elseif utils["sym?"](form) then
        local filename = nil
        if form.filename then
          filename = string.format("%q", form.filename)
        else
          filename = "nil"
        end
        local symstr = tostring(form)
        assert_compile(not runtime_3f, "symbols may only be used at compile time", form)
        if (symstr:find("#$") or symstr:find("#[:.]")) then
          return string.format("_G.sym('%s', {filename=%s, line=%s})", autogensym(symstr, scope), filename, (form.line or "nil"))
        else
          return string.format("_G.sym('%s', {quoted=true, filename=%s, line=%s})", symstr, filename, (form.line or "nil"))
        end
      elseif utils["call-of?"](form, "unquote") then
        local res = unpack(compile1(form[2], scope, parent))
        return res[1]
      elseif utils["list?"](form) then
        local mapped = quote_all(form, true)
        local filename = nil
        if form.filename then
          filename = string.format("%q", form.filename)
        else
          filename = "nil"
        end
        assert_compile(not runtime_3f, "lists may only be used at compile time", form)
        return string.format(("setmetatable({filename=%s, line=%s, bytestart=%s, %s}" .. ", getmetatable(_G.list()))"), filename, (form.line or "nil"), (form.bytestart or "nil"), mixed_concat(mapped, ", "))
      elseif utils["sequence?"](form) then
        local mapped = quote_all(form)
        local source = getmetatable(form)
        local filename = nil
        if source.filename then
          filename = string.format("%q", source.filename)
        else
          filename = "nil"
        end
        local _470_
        if source then
          _470_ = source.line
        else
          _470_ = "nil"
        end
        return string.format("setmetatable({%s}, {filename=%s, line=%s, sequence=%s})", mixed_concat(mapped, ", "), filename, _470_, "(getmetatable(_G.sequence()))['sequence']")
      elseif (type(form) == "table") then
        local mapped = quote_all(form)
        local source = getmetatable(form)
        local filename = nil
        if source.filename then
          filename = string.format("%q", source.filename)
        else
          filename = "nil"
        end
        local function _473_()
          if source then
            return source.line
          else
            return "nil"
          end
        end
        return string.format("setmetatable({%s}, {filename=%s, line=%s})", mixed_concat(mapped, ", "), filename, _473_())
      elseif (type(form) == "string") then
        return serialize_string(form)
      else
        return tostring(form)
      end
    end
    return {["apply-deferred-scope-changes"] = apply_deferred_scope_changes, ["check-binding-valid"] = check_binding_valid, ["compile-stream"] = compile_stream, ["compile-string"] = compile_string, ["declare-local"] = declare_local, ["do-quote"] = do_quote, ["global-allowed?"] = global_allowed_3f, ["global-mangling"] = global_mangling, ["global-unmangling"] = global_unmangling, ["keep-side-effects"] = keep_side_effects, ["make-scope"] = make_scope, ["require-include"] = require_include, ["symbol-to-expression"] = symbol_to_expression, assert = assert_compile, autogensym = autogensym, compile = compile, compile1 = compile1, destructure = destructure, emit = emit, gensym = gensym, getinfo = getinfo, macroexpand = macroexpand_2a, metadata = make_metadata(), scopes = scopes, sourcemap = sourcemap, traceback = traceback}
  end
  package.preload["fennel.friend"] = package.preload["fennel.friend"] or function(...)
    local utils = require("fennel.utils")
    local utf8_ok_3f, utf8 = pcall(require, "utf8")
    local suggestions = {["$ and $... in hashfn are mutually exclusive"] = {"modifying the hashfn so it only contains $... or $, $1, $2, $3, etc"}, ["can't introduce (.*) here"] = {"declaring the local at the top-level"}, ["can't start multisym segment with a digit"] = {"removing the digit", "adding a non-digit before the digit"}, ["cannot call literal value"] = {"checking for typos", "checking for a missing function name", "making sure to use prefix operators, not infix"}, ["could not compile value of type "] = {"debugging the macro you're calling to return a list or table"}, ["could not read number (.*)"] = {"removing the non-digit character", "beginning the identifier with a non-digit if it is not meant to be a number"}, ["expected a function.* to call"] = {"removing the empty parentheses", "using square brackets if you want an empty table"}, ["expected at least one pattern/body pair"] = {"adding a pattern and a body to execute when the pattern matches"}, ["expected binding and iterator"] = {"making sure you haven't omitted a local name or iterator"}, ["expected binding sequence"] = {"placing a table here in square brackets containing identifiers to bind"}, ["expected body expression"] = {"putting some code in the body of this form after the bindings"}, ["expected each macro to be function"] = {"ensuring that the value for each key in your macros table contains a function", "avoid defining nested macro tables"}, ["expected even number of name/value bindings"] = {"finding where the identifier or value is missing"}, ["expected even number of pattern/body pairs"] = {"checking that every pattern has a body to go with it", "adding _ before the final body"}, ["expected even number of values in table literal"] = {"removing a key", "adding a value"}, ["expected local"] = {"looking for a typo", "looking for a local which is used out of its scope"}, ["expected macros to be table"] = {"ensuring your macro definitions return a table"}, ["expected parameters"] = {"adding function parameters as a list of identifiers in brackets"}, ["expected range to include start and stop"] = {"adding missing arguments"}, ["expected rest argument before last parameter"] = {"moving & to right before the final identifier when destructuring"}, ["expected symbol for function parameter: (.*)"] = {"changing %s to an identifier instead of a literal value"}, ["expected var (.*)"] = {"declaring %s using var instead of let/local", "introducing a new local instead of changing the value of %s"}, ["expected vararg as last parameter"] = {"moving the \"...\" to the end of the parameter list"}, ["expected whitespace before opening delimiter"] = {"adding whitespace"}, ["global (.*) conflicts with local"] = {"renaming local %s"}, ["invalid character: (.)"] = {"deleting or replacing %s", "avoiding reserved characters like \", \\, ', ~, ;, @, `, and comma"}, ["local (.*) was overshadowed by a special form or macro"] = {"renaming local %s"}, ["macro not found in macro module"] = {"checking the keys of the imported macro module's returned table"}, ["macro tried to bind (.*) without gensym"] = {"changing to %s# when introducing identifiers inside macros"}, ["malformed multisym"] = {"ensuring each period or colon is not followed by another period or colon"}, ["may only be used at compile time"] = {"moving this to inside a macro if you need to manipulate symbols/lists", "using square brackets instead of parens to construct a table"}, ["method must be last component"] = {"using a period instead of a colon for field access", "removing segments after the colon", "making the method call, then looking up the field on the result"}, ["mismatched closing delimiter (.), expected (.)"] = {"replacing %s with %s", "deleting %s", "adding matching opening delimiter earlier"}, ["missing subject"] = {"adding an item to operate on"}, ["multisym method calls may only be in call position"] = {"using a period instead of a colon to reference a table's fields", "putting parens around this"}, ["tried to reference a macro without calling it"] = {"renaming the macro so as not to conflict with locals"}, ["tried to reference a special form without calling it"] = {"making sure to use prefix operators, not infix", "wrapping the special in a function if you need it to be first class"}, ["tried to use unquote outside quote"] = {"moving the form to inside a quoted form", "removing the comma"}, ["tried to use vararg with operator"] = {"accumulating over the operands"}, ["unable to bind (.*)"] = {"replacing the %s with an identifier"}, ["unexpected arguments"] = {"removing an argument", "checking for typos"}, ["unexpected closing delimiter (.)"] = {"deleting %s", "adding matching opening delimiter earlier"}, ["unexpected iterator clause"] = {"removing an argument", "checking for typos"}, ["unexpected multi symbol (.*)"] = {"removing periods or colons from %s"}, ["unexpected vararg"] = {"putting \"...\" at the end of the fn parameters if the vararg was intended"}, ["unknown identifier: (.*)"] = {"looking to see if there's a typo", "using the _G table instead, eg. _G.%s if you really want a global", "moving this code to somewhere that %s is in scope", "binding %s as a local in the scope of this code"}, ["unused local (.*)"] = {"renaming the local to _%s if it is meant to be unused", "fixing a typo so %s is used", "disabling the linter which checks for unused locals"}, ["use of global (.*) is aliased by a local"] = {"renaming local %s", "refer to the global using _G.%s instead of directly"}}
    local unpack = (table.unpack or _G.unpack)
    local function suggest(msg)
      local s = nil
      for pat, sug in pairs(suggestions) do
        if s then break end
        local matches = {msg:match(pat)}
        if next(matches) then
          local tbl_17_ = {}
          local i_18_ = #tbl_17_
          for _, s0 in ipairs(sug) do
            local val_19_ = s0:format(unpack(matches))
            if (nil ~= val_19_) then
              i_18_ = (i_18_ + 1)
              tbl_17_[i_18_] = val_19_
            end
          end
          s = tbl_17_
        else
        s = nil
        end
      end
      return s
    end
    local function read_line(filename, line, _3fsource)
      if _3fsource then
        local matcher = string.gmatch((_3fsource .. "\n"), "(.-)(\13?\n)")
        for _ = 2, line do
          matcher()
        end
        return matcher()
      else
        local f = assert(_G.io.open(filename))
        local function close_handlers_10_(ok_11_, ...)
          f:close()
          if ok_11_ then
            return ...
          else
            return error(..., 0)
          end
        end
        local function _190_()
          for _ = 2, line do
            f:read()
          end
          return f:read()
        end
        return close_handlers_10_(_G.xpcall(_190_, (package.loaded.fennel or debug).traceback))
      end
    end
    local function sub(str, start, _end)
      if ((_end < start) or (#str < start)) then
        return ""
      elseif utf8_ok_3f then
        return string.sub(str, utf8.offset(str, start), ((utf8.offset(str, (_end + 1)) or (utf8.len(str) + 1)) - 1))
      else
        return string.sub(str, start, math.min(_end, str:len()))
      end
    end
    local function highlight_line(codeline, col, _3fendcol, opts)
      if ((opts and (false == opts["error-pinpoint"])) or (os and os.getenv and os.getenv("NO_COLOR"))) then
        return codeline
      else
        local _193_ = (opts or {})
        local error_pinpoint = _193_["error-pinpoint"]
        local endcol = (_3fendcol or col)
        local eol = nil
        if utf8_ok_3f then
          eol = utf8.len(codeline)
        else
          eol = string.len(codeline)
        end
        local _195_ = (error_pinpoint or {"\27[7m", "\27[0m"})
        local open = _195_[1]
        local close = _195_[2]
        return (sub(codeline, 1, col) .. open .. sub(codeline, (col + 1), (endcol + 1)) .. close .. sub(codeline, (endcol + 2), eol))
      end
    end
    local function friendly_msg(msg, _197_0, source, opts)
      local _198_ = _197_0
      local col = _198_["col"]
      local endcol = _198_["endcol"]
      local endline = _198_["endline"]
      local filename = _198_["filename"]
      local line = _198_["line"]
      local ok, codeline = pcall(read_line, filename, line, source)
      local endcol0 = nil
      if (ok and codeline and (line ~= endline)) then
        endcol0 = #codeline
      else
        endcol0 = endcol
      end
      local out = {msg, ""}
      if (ok and codeline) then
        if col then
          table.insert(out, highlight_line(codeline, col, endcol0, opts))
        else
          table.insert(out, codeline)
        end
      end
      for _, suggestion in ipairs((suggest(msg) or {})) do
        table.insert(out, ("* Try %s."):format(suggestion))
      end
      return table.concat(out, "\n")
    end
    local function assert_compile(condition, msg, ast, source, opts)
      if not condition then
        local _202_ = utils["ast-source"](ast)
        local col = _202_["col"]
        local filename = _202_["filename"]
        local line = _202_["line"]
        error(friendly_msg(("%s:%s:%s: Compile error: %s"):format((filename or "unknown"), (line or "?"), (col or "?"), msg), utils["ast-source"](ast), source, opts), 0)
      end
      return condition
    end
    local function parse_error(msg, filename, line, col, source, opts)
      return error(friendly_msg(("%s:%s:%s: Parse error: %s"):format(filename, line, col, msg), {col = col, filename = filename, line = line}, source, opts), 0)
    end
    return {["assert-compile"] = assert_compile, ["parse-error"] = parse_error}
  end
  package.preload["fennel.parser"] = package.preload["fennel.parser"] or function(...)
    local utils = require("fennel.utils")
    local friend = require("fennel.friend")
    local unpack = (table.unpack or _G.unpack)
    local function granulate(getchunk)
      local c, index, done_3f = "", 1, false
      local function _204_(parser_state)
        if not done_3f then
          if (index <= #c) then
            local b = c:byte(index)
            index = (index + 1)
            return b
          else
            local _205_0 = getchunk(parser_state)
            local function _206_()
              local char = _205_0
              return (char ~= "")
            end
            if ((nil ~= _205_0) and _206_()) then
              local char = _205_0
              c = char
              index = 2
              return c:byte()
            else
              local _ = _205_0
              done_3f = true
              return nil
            end
          end
        end
      end
      local function _210_()
        c = ""
        return nil
      end
      return _204_, _210_
    end
    local function string_stream(str, _3foptions)
      local str0 = str:gsub("^#!", ";;")
      if _3foptions then
        _3foptions.source = str0
      end
      local index = 1
      local function _212_()
        local r = str0:byte(index)
        index = (index + 1)
        return r
      end
      return _212_
    end
    local delims = {[123] = 125, [125] = true, [40] = 41, [41] = true, [91] = 93, [93] = true}
    local function sym_char_3f(b)
      local b0 = nil
      if ("number" == type(b)) then
        b0 = b
      else
        b0 = string.byte(b)
      end
      return ((32 < b0) and not delims[b0] and (b0 ~= 127) and (b0 ~= 34) and (b0 ~= 39) and (b0 ~= 126) and (b0 ~= 59) and (b0 ~= 44) and (b0 ~= 64) and (b0 ~= 96))
    end
    local prefixes = {[35] = "hashfn", [39] = "quote", [44] = "unquote", [96] = "quote"}
    local nan, negative_nan = nil, nil
    if (45 == string.byte(tostring((0 / 0)))) then
      nan, negative_nan = ( - (0 / 0)), (0 / 0)
    else
      nan, negative_nan = (0 / 0), ( - (0 / 0))
    end
    local function char_starter_3f(b)
      return (((1 < b) and (b < 127)) or ((192 < b) and (b < 247)))
    end
    local function parser_fn(getbyte, filename, _215_0)
      local _216_ = _215_0
      local options = _216_
      local comments = _216_["comments"]
      local source = _216_["source"]
      local unfriendly = _216_["unfriendly"]
      local stack = {}
      local line, byteindex, col, prev_col, lastb = 1, 0, 0, 0, nil
      local function ungetb(ub)
        if char_starter_3f(ub) then
          col = (col - 1)
        end
        if (ub == 10) then
          line, col = (line - 1), prev_col
        end
        byteindex = (byteindex - 1)
        lastb = ub
        return nil
      end
      local function getb()
        local r = nil
        if lastb then
          r, lastb = lastb, nil
        else
          r = getbyte({["stack-size"] = #stack})
        end
        if r then
          byteindex = (byteindex + 1)
        end
        if (r and char_starter_3f(r)) then
          col = (col + 1)
        end
        if (r == 10) then
          line, col, prev_col = (line + 1), 0, col
        end
        return r
      end
      local function whitespace_3f(b)
        local function _224_()
          local _223_0 = options.whitespace
          if (nil ~= _223_0) then
            _223_0 = _223_0[b]
          end
          return _223_0
        end
        return ((b == 32) or ((9 <= b) and (b <= 13)) or _224_())
      end
      local function parse_error(msg, _3fcol_adjust)
        local col0 = (col + (_3fcol_adjust or -1))
        if (nil == utils["hook-opts"]("parse-error", options, msg, filename, (line or "?"), col0, source, utils.root.reset)) then
          utils.root.reset()
          if unfriendly then
            return error(string.format("%s:%s:%s: Parse error: %s", filename, (line or "?"), col0, msg), 0)
          else
            return friend["parse-error"](msg, filename, (line or "?"), col0, source, options)
          end
        end
      end
      local function parse_stream()
        local whitespace_since_dispatch, done_3f, retval = true
        local function set_source_fields(source0)
          source0.byteend, source0.endcol, source0.endline = byteindex, (col - 1), line
          return nil
        end
        local function dispatch(v, _3fsource, _3fraw)
          whitespace_since_dispatch = false
          local v0 = nil
          do
            local _228_0 = utils["hook-opts"]("parse-form", options, v, _3fsource, _3fraw, stack)
            if (nil ~= _228_0) then
              local hookv = _228_0
              v0 = hookv
            else
              local _ = _228_0
              v0 = v
            end
          end
          local _230_0 = stack[#stack]
          if (_230_0 == nil) then
            retval, done_3f = v0, true
            return nil
          elseif ((_G.type(_230_0) == "table") and (nil ~= _230_0.prefix)) then
            local prefix = _230_0.prefix
            local source0 = nil
            do
              local _231_0 = table.remove(stack)
              set_source_fields(_231_0)
              source0 = _231_0
            end
            local list = utils.list(utils.sym(prefix, source0), v0)
            return dispatch(utils.copy(source0, list))
          elseif (nil ~= _230_0) then
            local top = _230_0
            return table.insert(top, v0)
          end
        end
        local function badend()
          local closers = nil
          do
            local tbl_17_ = {}
            local i_18_ = #tbl_17_
            for _, _233_0 in ipairs(stack) do
              local _234_ = _233_0
              local closer = _234_["closer"]
              local val_19_ = closer
              if (nil ~= val_19_) then
                i_18_ = (i_18_ + 1)
                tbl_17_[i_18_] = val_19_
              end
            end
            closers = tbl_17_
          end
          local _236_
          if (#stack == 1) then
            _236_ = ""
          else
            _236_ = "s"
          end
          return parse_error(string.format("expected closing delimiter%s %s", _236_, string.char(unpack(closers))), 0)
        end
        local function skip_whitespace(b, close_table)
          if (b and whitespace_3f(b)) then
            whitespace_since_dispatch = true
            return skip_whitespace(getb(), close_table)
          elseif (not b and next(stack)) then
            badend()
            for i = #stack, 2, -1 do
              close_table(stack[i].closer)
            end
            return stack[1].closer
          else
            return b
          end
        end
        local function parse_comment(b, contents)
          if (b and (10 ~= b)) then
            local function _239_()
              table.insert(contents, string.char(b))
              return contents
            end
            return parse_comment(getb(), _239_())
          elseif comments then
            ungetb(10)
            return dispatch(utils.comment(table.concat(contents), {filename = filename, line = line}))
          end
        end
        local function open_table(b)
          if not whitespace_since_dispatch then
            parse_error(("expected whitespace before opening delimiter " .. string.char(b)))
          end
          return table.insert(stack, {bytestart = byteindex, closer = delims[b], col = (col - 1), filename = filename, line = line})
        end
        local function close_list(list)
          return dispatch(setmetatable(list, getmetatable(utils.list())))
        end
        local function close_sequence(tbl)
          local mt = getmetatable(utils.sequence())
          for k, v in pairs(tbl) do
            if ("number" ~= type(k)) then
              mt[k] = v
              tbl[k] = nil
            end
          end
          return dispatch(setmetatable(tbl, mt))
        end
        local function add_comment_at(comments0, index, node)
          local _243_0 = comments0[index]
          if (nil ~= _243_0) then
            local existing = _243_0
            return table.insert(existing, node)
          else
            local _ = _243_0
            comments0[index] = {node}
            return nil
          end
        end
        local function next_noncomment(tbl, i)
          if utils["comment?"](tbl[i]) then
            return next_noncomment(tbl, (i + 1))
          elseif utils["sym?"](tbl[i], ":") then
            return tostring(tbl[(i + 1)])
          else
            return tbl[i]
          end
        end
        local function extract_comments(tbl)
          local comments0 = {keys = {}, last = {}, values = {}}
          while utils["comment?"](tbl[#tbl]) do
            table.insert(comments0.last, 1, table.remove(tbl))
          end
          local last_key_3f = false
          for i, node in ipairs(tbl) do
            if not utils["comment?"](node) then
              last_key_3f = not last_key_3f
            elseif last_key_3f then
              add_comment_at(comments0.values, next_noncomment(tbl, i), node)
            else
              add_comment_at(comments0.keys, next_noncomment(tbl, i), node)
            end
          end
          for i = #tbl, 1, -1 do
            if utils["comment?"](tbl[i]) then
              table.remove(tbl, i)
            end
          end
          return comments0
        end
        local function close_curly_table(tbl)
          local comments0 = extract_comments(tbl)
          local keys = {}
          local val = {}
          if ((#tbl % 2) ~= 0) then
            byteindex = (byteindex - 1)
            parse_error("expected even number of values in table literal")
          end
          setmetatable(val, tbl)
          for i = 1, #tbl, 2 do
            if ((tostring(tbl[i]) == ":") and utils["sym?"](tbl[(i + 1)]) and utils["sym?"](tbl[i])) then
              tbl[i] = tostring(tbl[(i + 1)])
            end
            val[tbl[i]] = tbl[(i + 1)]
            table.insert(keys, tbl[i])
          end
          tbl.comments = comments0
          tbl.keys = keys
          return dispatch(val)
        end
        local function close_table(b)
          local top = table.remove(stack)
          if (top == nil) then
            parse_error(("unexpected closing delimiter " .. string.char(b)))
          end
          if (top.closer and (top.closer ~= b)) then
            parse_error(("mismatched closing delimiter " .. string.char(b) .. ", expected " .. string.char(top.closer)))
          end
          set_source_fields(top)
          if (b == 41) then
            return close_list(top)
          elseif (b == 93) then
            return close_sequence(top)
          else
            return close_curly_table(top)
          end
        end
        local function parse_string_loop(chars, b, state)
          if b then
            table.insert(chars, string.char(b))
          end
          local state0 = nil
          do
            local _254_0 = {state, b}
            if ((_G.type(_254_0) == "table") and (_254_0[1] == "base") and (_254_0[2] == 92)) then
              state0 = "backslash"
            elseif ((_G.type(_254_0) == "table") and (_254_0[1] == "base") and (_254_0[2] == 34)) then
              state0 = "done"
            elseif ((_G.type(_254_0) == "table") and (_254_0[1] == "backslash") and (_254_0[2] == 10)) then
              table.remove(chars, (#chars - 1))
              state0 = "base"
            else
              local _ = _254_0
              state0 = "base"
            end
          end
          if (b and (state0 ~= "done")) then
            return parse_string_loop(chars, getb(), state0)
          else
            return b
          end
        end
        local function escape_char(c)
          return ({[10] = "\\n", [11] = "\\v", [12] = "\\f", [13] = "\\r", [7] = "\\a", [8] = "\\b", [9] = "\\t"})[c:byte()]
        end
        local function parse_string(source0)
          if not whitespace_since_dispatch then
            utils.warn("expected whitespace before string", nil, filename, line)
          end
          table.insert(stack, {closer = 34})
          local chars = {"\""}
          if not parse_string_loop(chars, getb(), "base") then
            badend()
          end
          table.remove(stack)
          local raw = table.concat(chars)
          local formatted = raw:gsub("[\7-\13]", escape_char)
          local _259_0 = (rawget(_G, "loadstring") or load)(("return " .. formatted))
          if (nil ~= _259_0) then
            local load_fn = _259_0
            return dispatch(load_fn(), source0, raw)
          elseif (_259_0 == nil) then
            return parse_error(("Invalid string: " .. raw))
          end
        end
        local function parse_prefix(b)
          table.insert(stack, {bytestart = byteindex, col = (col - 1), filename = filename, line = line, prefix = prefixes[b]})
          local nextb = getb()
          local trailing_whitespace_3f = (whitespace_3f(nextb) or (true == delims[nextb]))
          if (trailing_whitespace_3f and (b ~= 35)) then
            parse_error("invalid whitespace after quoting prefix")
          end
          ungetb(nextb)
          if (trailing_whitespace_3f and (b == 35)) then
            local source0 = table.remove(stack)
            set_source_fields(source0)
            return dispatch(utils.sym("#", source0))
          end
        end
        local function parse_sym_loop(chars, b)
          if (b and sym_char_3f(b)) then
            table.insert(chars, string.char(b))
            return parse_sym_loop(chars, getb())
          else
            if b then
              ungetb(b)
            end
            return chars
          end
        end
        local function parse_number(rawstr, source0)
          local trimmed = (not rawstr:find("^_") and rawstr:gsub("_", ""))
          if ((trimmed == "nan") or (trimmed == "-nan")) then
            return false
          elseif rawstr:match("^%d") then
            dispatch((tonumber(trimmed) or parse_error(("could not read number \"" .. rawstr .. "\""))), source0, rawstr)
            return true
          else
            local _265_0 = tonumber(trimmed)
            if (nil ~= _265_0) then
              local x = _265_0
              dispatch(x, source0, rawstr)
              return true
            else
              local _ = _265_0
              return false
            end
          end
        end
        local function check_malformed_sym(rawstr)
          local function col_adjust(pat)
            return (rawstr:find(pat) - utils.len(rawstr) - 1)
          end
          if (rawstr:match("^~") and (rawstr ~= "~=")) then
            parse_error("invalid character: ~")
          elseif (rawstr:match("[%.:][%.:]") and (rawstr ~= "..") and (rawstr ~= "$...")) then
            parse_error(("malformed multisym: " .. rawstr), col_adjust("[%.:][%.:]"))
          elseif ((rawstr ~= ":") and rawstr:match(":$")) then
            parse_error(("malformed multisym: " .. rawstr), col_adjust(":$"))
          elseif rawstr:match(":.+[%.:]") then
            parse_error(("method must be last component of multisym: " .. rawstr), col_adjust(":.+[%.:]"))
          end
          if not whitespace_since_dispatch then
            utils.warn("expected whitespace before token", nil, filename, line)
          end
          return rawstr
        end
        local function parse_sym(b)
          local source0 = {bytestart = byteindex, col = (col - 1), filename = filename, line = line}
          local rawstr = table.concat(parse_sym_loop({string.char(b)}, getb()))
          set_source_fields(source0)
          if (rawstr == "true") then
            return dispatch(true, source0)
          elseif (rawstr == "false") then
            return dispatch(false, source0)
          elseif (rawstr == "...") then
            return dispatch(utils.varg(source0))
          elseif (rawstr == ".inf") then
            return dispatch((1 / 0), source0, rawstr)
          elseif (rawstr == "-.inf") then
            return dispatch((-1 / 0), source0, rawstr)
          elseif (rawstr == ".nan") then
            return dispatch(nan, source0, rawstr)
          elseif (rawstr == "-.nan") then
            return dispatch(negative_nan, source0, rawstr)
          elseif rawstr:match("^:.+$") then
            return dispatch(rawstr:sub(2), source0, rawstr)
          elseif not parse_number(rawstr, source0) then
            return dispatch(utils.sym(check_malformed_sym(rawstr), source0))
          end
        end
        local function parse_loop(b)
          if not b then
          elseif (b == 59) then
            parse_comment(getb(), {";"})
          elseif (type(delims[b]) == "number") then
            open_table(b)
          elseif delims[b] then
            close_table(b)
          elseif (b == 34) then
            parse_string({bytestart = byteindex, col = col, filename = filename, line = line})
          elseif prefixes[b] then
            parse_prefix(b)
          elseif (sym_char_3f(b) or (b == string.byte("~"))) then
            parse_sym(b)
          elseif not utils["hook-opts"]("illegal-char", options, b, getb, ungetb, dispatch) then
            parse_error(("invalid character: " .. string.char(b)))
          end
          if not b then
            return nil
          elseif done_3f then
            return true, retval
          else
            return parse_loop(skip_whitespace(getb(), close_table))
          end
        end
        return parse_loop(skip_whitespace(getb(), close_table))
      end
      local function _273_()
        stack, line, byteindex, col, lastb = {}, 1, 0, 0, ((lastb ~= 10) and lastb)
        return nil
      end
      return parse_stream, _273_
    end
    local function parser(stream_or_string, _3ffilename, _3foptions)
      local filename = (_3ffilename or "unknown")
      local options = (_3foptions or utils.root.options or {})
      assert(("string" == type(filename)), "expected filename as second argument to parser")
      if ("string" == type(stream_or_string)) then
        return parser_fn(string_stream(stream_or_string, options), filename, options)
      else
        return parser_fn(stream_or_string, filename, options)
      end
    end
    return {["string-stream"] = string_stream, ["sym-char?"] = sym_char_3f, granulate = granulate, parser = parser}
  end
  local utils = nil
  package.preload["fennel.view"] = package.preload["fennel.view"] or function(...)
    local type_order = {["function"] = 5, boolean = 2, number = 1, string = 3, table = 4, thread = 7, userdata = 6}
    local default_opts = {["detect-cycles?"] = true, ["empty-as-sequence?"] = false, ["escape-newlines?"] = false, ["line-length"] = 80, ["max-sparse-gap"] = 10, ["metamethod?"] = true, ["one-line?"] = false, ["prefer-colon?"] = false, ["utf8?"] = true, depth = 128}
    local lua_pairs = pairs
    local lua_ipairs = ipairs
    local function pairs(t)
      local _1_0 = getmetatable(t)
      if ((_G.type(_1_0) == "table") and (nil ~= _1_0.__pairs)) then
        local p = _1_0.__pairs
        return p(t)
      else
        local _ = _1_0
        return lua_pairs(t)
      end
    end
    local function ipairs(t)
      local _3_0 = getmetatable(t)
      if ((_G.type(_3_0) == "table") and (nil ~= _3_0.__ipairs)) then
        local i = _3_0.__ipairs
        return i(t)
      else
        local _ = _3_0
        return lua_ipairs(t)
      end
    end
    local function length_2a(t)
      local _5_0 = getmetatable(t)
      if ((_G.type(_5_0) == "table") and (nil ~= _5_0.__len)) then
        local l = _5_0.__len
        return l(t)
      else
        local _ = _5_0
        return #t
      end
    end
    local function get_default(key)
      local _7_0 = default_opts[key]
      if (_7_0 == nil) then
        return error(("option '%s' doesn't have a default value, use the :after key to set it"):format(tostring(key)))
      elseif (nil ~= _7_0) then
        local v = _7_0
        return v
      end
    end
    local function getopt(options, key)
      local _9_0 = options[key]
      if ((_G.type(_9_0) == "table") and (nil ~= _9_0.once)) then
        local val_2a = _9_0.once
        return val_2a
      else
        local _3fval = _9_0
        return _3fval
      end
    end
    local function normalize_opts(options)
      local tbl_14_ = {}
      for k, v in pairs(options) do
        local k_15_, v_16_ = nil, nil
        local function _12_()
          local _11_0 = v
          if ((_G.type(_11_0) == "table") and (nil ~= _11_0.after)) then
            local val = _11_0.after
            return val
          else
            local function _13_()
              return v.once
            end
            if ((_G.type(_11_0) == "table") and _13_()) then
              return get_default(k)
            else
              local _ = _11_0
              return v
            end
          end
        end
        k_15_, v_16_ = k, _12_()
        if ((k_15_ ~= nil) and (v_16_ ~= nil)) then
          tbl_14_[k_15_] = v_16_
        end
      end
      return tbl_14_
    end
    local function sort_keys(_16_0, _18_0)
      local _17_ = _16_0
      local a = _17_[1]
      local _19_ = _18_0
      local b = _19_[1]
      local ta = type(a)
      local tb = type(b)
      if ((ta == tb) and ((ta == "string") or (ta == "number"))) then
        return (a < b)
      else
        local dta = type_order[ta]
        local dtb = type_order[tb]
        if (dta and dtb) then
          return (dta < dtb)
        elseif dta then
          return true
        elseif dtb then
          return false
        else
          return (ta < tb)
        end
      end
    end
    local function max_index_gap(kv)
      local gap = 0
      if (0 < length_2a(kv)) then
        local i = 0
        for _, _22_0 in ipairs(kv) do
          local _23_ = _22_0
          local k = _23_[1]
          if (gap < (k - i)) then
            gap = (k - i)
          end
          i = k
        end
      end
      return gap
    end
    local function fill_gaps(kv)
      local missing_indexes = {}
      local i = 0
      for _, _26_0 in ipairs(kv) do
        local _27_ = _26_0
        local j = _27_[1]
        i = (i + 1)
        while (i < j) do
          table.insert(missing_indexes, i)
          i = (i + 1)
        end
      end
      for _, k in ipairs(missing_indexes) do
        table.insert(kv, k, {k})
      end
      return nil
    end
    local function table_kv_pairs(t, options)
      local assoc_3f = false
      local kv = {}
      local insert = table.insert
      for k, v in pairs(t) do
        if ((type(k) ~= "number") or (k < 1)) then
          assoc_3f = true
        end
        insert(kv, {k, v})
      end
      table.sort(kv, sort_keys)
      if not assoc_3f then
        if (options["max-sparse-gap"] < max_index_gap(kv)) then
          assoc_3f = true
        else
          fill_gaps(kv)
        end
      end
      if (length_2a(kv) == 0) then
        return kv, "empty"
      else
        local function _31_()
          if assoc_3f then
            return "table"
          else
            return "seq"
          end
        end
        return kv, _31_()
      end
    end
    local function count_table_appearances(t, appearances)
      if (type(t) == "table") then
        if not appearances[t] then
          appearances[t] = 1
          for k, v in pairs(t) do
            count_table_appearances(k, appearances)
            count_table_appearances(v, appearances)
          end
        else
          appearances[t] = ((appearances[t] or 0) + 1)
        end
      end
      return appearances
    end
    local function save_table(t, seen)
      local seen0 = (seen or {len = 0})
      local id = (seen0.len + 1)
      if not seen0[t] then
        seen0[t] = id
        seen0.len = id
      end
      return seen0
    end
    local function detect_cycle(t, seen)
      if ("table" == type(t)) then
        seen[t] = true
        local res = nil
        for k, v in pairs(t) do
          if res then break end
          res = (seen[k] or detect_cycle(k, seen) or seen[v] or detect_cycle(v, seen))
        end
        return res
      end
    end
    local function visible_cycle_3f(t, options)
      return (getopt(options, "detect-cycles?") and detect_cycle(t, {}) and save_table(t, options.seen) and (1 < (options.appearances[t] or 0)))
    end
    local function table_indent(indent, id)
      local opener_length = nil
      if id then
        opener_length = (length_2a(tostring(id)) + 2)
      else
        opener_length = 1
      end
      return (indent + opener_length)
    end
    local pp = nil
    local function concat_table_lines(elements, options, multiline_3f, indent, table_type, prefix, last_comment_3f)
      local indent_str = ("\n" .. string.rep(" ", indent))
      local open = nil
      local function _38_()
        if ("seq" == table_type) then
          return "["
        else
          return "{"
        end
      end
      open = ((prefix or "") .. _38_())
      local close = nil
      if ("seq" == table_type) then
        close = "]"
      else
        close = "}"
      end
      local oneline = (open .. table.concat(elements, " ") .. close)
      if (not getopt(options, "one-line?") and (multiline_3f or (options["line-length"] < (indent + length_2a(oneline))) or last_comment_3f)) then
        local function _40_()
          if last_comment_3f then
            return indent_str
          else
            return ""
          end
        end
        return (open .. table.concat(elements, indent_str) .. _40_() .. close)
      else
        return oneline
      end
    end
    local function utf8_len(x)
      local n = 0
      for _ in string.gmatch(x, "[%z\1-\127\192-\247]") do
        n = (n + 1)
      end
      return n
    end
    local function comment_3f(x)
      if ("table" == type(x)) then
        local fst = x[1]
        return (("string" == type(fst)) and (nil ~= fst:find("^;")))
      else
        return false
      end
    end
    local function pp_associative(t, kv, options, indent)
      local multiline_3f = false
      local id = options.seen[t]
      if (options.depth <= options.level) then
        return "{...}"
      elseif (id and getopt(options, "detect-cycles?")) then
        return ("@" .. id .. "{...}")
      else
        local visible_cycle_3f0 = visible_cycle_3f(t, options)
        local id0 = (visible_cycle_3f0 and options.seen[t])
        local indent0 = table_indent(indent, id0)
        local slength = nil
        if getopt(options, "utf8?") then
          slength = utf8_len
        else
          local function _43_(_241)
            return #_241
          end
          slength = _43_
        end
        local prefix = nil
        if visible_cycle_3f0 then
          prefix = ("@" .. id0)
        else
          prefix = ""
        end
        local items = nil
        do
          local options0 = normalize_opts(options)
          local tbl_17_ = {}
          local i_18_ = #tbl_17_
          for _, _46_0 in ipairs(kv) do
            local _47_ = _46_0
            local k = _47_[1]
            local v = _47_[2]
            local val_19_ = nil
            do
              local k0 = pp(k, options0, (indent0 + 1), true)
              local v0 = pp(v, options0, (indent0 + slength(k0) + 1))
              multiline_3f = (multiline_3f or k0:find("\n") or v0:find("\n"))
              val_19_ = (k0 .. " " .. v0)
            end
            if (nil ~= val_19_) then
              i_18_ = (i_18_ + 1)
              tbl_17_[i_18_] = val_19_
            end
          end
          items = tbl_17_
        end
        return concat_table_lines(items, options, multiline_3f, indent0, "table", prefix, false)
      end
    end
    local function pp_sequence(t, kv, options, indent)
      local multiline_3f = false
      local id = options.seen[t]
      if (options.depth <= options.level) then
        return "[...]"
      elseif (id and getopt(options, "detect-cycles?")) then
        return ("@" .. id .. "[...]")
      else
        local visible_cycle_3f0 = visible_cycle_3f(t, options)
        local id0 = (visible_cycle_3f0 and options.seen[t])
        local indent0 = table_indent(indent, id0)
        local prefix = nil
        if visible_cycle_3f0 then
          prefix = ("@" .. id0)
        else
          prefix = ""
        end
        local last_comment_3f = comment_3f(t[#t])
        local items = nil
        do
          local options0 = normalize_opts(options)
          local tbl_17_ = {}
          local i_18_ = #tbl_17_
          for _, _51_0 in ipairs(kv) do
            local _52_ = _51_0
            local _0 = _52_[1]
            local v = _52_[2]
            local val_19_ = nil
            do
              local v0 = pp(v, options0, indent0)
              multiline_3f = (multiline_3f or v0:find("\n") or v0:find("^;"))
              val_19_ = v0
            end
            if (nil ~= val_19_) then
              i_18_ = (i_18_ + 1)
              tbl_17_[i_18_] = val_19_
            end
          end
          items = tbl_17_
        end
        return concat_table_lines(items, options, multiline_3f, indent0, "seq", prefix, last_comment_3f)
      end
    end
    local function concat_lines(lines, options, indent, force_multi_line_3f)
      if (length_2a(lines) == 0) then
        if getopt(options, "empty-as-sequence?") then
          return "[]"
        else
          return "{}"
        end
      else
        local oneline = nil
        local _56_
        do
          local tbl_17_ = {}
          local i_18_ = #tbl_17_
          for _, line in ipairs(lines) do
            local val_19_ = line:gsub("^%s+", "")
            if (nil ~= val_19_) then
              i_18_ = (i_18_ + 1)
              tbl_17_[i_18_] = val_19_
            end
          end
          _56_ = tbl_17_
        end
        oneline = table.concat(_56_, " ")
        if (not getopt(options, "one-line?") and (force_multi_line_3f or oneline:find("\n") or (options["line-length"] < (indent + length_2a(oneline))))) then
          return table.concat(lines, ("\n" .. string.rep(" ", indent)))
        else
          return oneline
        end
      end
    end
    local function pp_metamethod(t, metamethod, options, indent)
      if (options.depth <= options.level) then
        if getopt(options, "empty-as-sequence?") then
          return "[...]"
        else
          return "{...}"
        end
      else
        local _ = nil
        local function _61_(_241)
          return visible_cycle_3f(_241, options)
        end
        options["visible-cycle?"] = _61_
        _ = nil
        local lines, force_multi_line_3f = nil, nil
        do
          local options0 = normalize_opts(options)
          lines, force_multi_line_3f = metamethod(t, pp, options0, indent)
        end
        options["visible-cycle?"] = nil
        local _62_0 = type(lines)
        if (_62_0 == "string") then
          return lines
        elseif (_62_0 == "table") then
          return concat_lines(lines, options, indent, force_multi_line_3f)
        else
          local _0 = _62_0
          return error("__fennelview metamethod must return a table of lines")
        end
      end
    end
    local function pp_table(x, options, indent)
      options.level = (options.level + 1)
      local x0 = nil
      do
        local _65_0 = nil
        if getopt(options, "metamethod?") then
          local _66_0 = x
          if (nil ~= _66_0) then
            local _67_0 = getmetatable(_66_0)
            if (nil ~= _67_0) then
              _65_0 = _67_0.__fennelview
            else
              _65_0 = _67_0
            end
          else
            _65_0 = _66_0
          end
        else
        _65_0 = nil
        end
        if (nil ~= _65_0) then
          local metamethod = _65_0
          x0 = pp_metamethod(x, metamethod, options, indent)
        else
          local _ = _65_0
          local _71_0, _72_0 = table_kv_pairs(x, options)
          if (true and (_72_0 == "empty")) then
            local _0 = _71_0
            if getopt(options, "empty-as-sequence?") then
              x0 = "[]"
            else
              x0 = "{}"
            end
          elseif ((nil ~= _71_0) and (_72_0 == "table")) then
            local kv = _71_0
            x0 = pp_associative(x, kv, options, indent)
          elseif ((nil ~= _71_0) and (_72_0 == "seq")) then
            local kv = _71_0
            x0 = pp_sequence(x, kv, options, indent)
          else
          x0 = nil
          end
        end
      end
      options.level = (options.level - 1)
      return x0
    end
    local function exponential_notation(n, fallback)
      local s = nil
      for i = 0, 308 do
        if s then break end
        local s0 = string.format(("%." .. i .. "e"), n)
        if (n == tonumber(s0)) then
          local exp = s0:match("e%+?(%d+)$")
          if (exp and (14 < tonumber(exp))) then
            s = s0
          else
            s = fallback
          end
        else
        s = nil
        end
      end
      return s
    end
    local inf_str = tostring((1 / 0))
    local neg_inf_str = tostring((-1 / 0))
    local function number__3estring(n, options)
      local val = nil
      if (n ~= n) then
        if (45 == string.byte(tostring(n))) then
          val = (options["negative-nan"] or "-.nan")
        else
          val = (options.nan or ".nan")
        end
      elseif (math.floor(n) == n) then
        local s1 = string.format("%.f", n)
        if (s1 == inf_str) then
          val = (options.infinity or ".inf")
        elseif (s1 == neg_inf_str) then
          val = (options["negative-infinity"] or "-.inf")
        elseif (s1 == tostring(n)) then
          val = s1
        else
          val = (exponential_notation(n, s1) or s1)
        end
      else
        val = tostring(n)
      end
      local _81_0 = string.gsub(val, ",", ".")
      return _81_0
    end
    local function colon_string_3f(s)
      return s:find("^[-%w?^_!$%&*+./|<=>]+$")
    end
    local utf8_inits = {{["max-byte"] = 127, ["max-code"] = 127, ["min-byte"] = 0, ["min-code"] = 0, len = 1}, {["max-byte"] = 223, ["max-code"] = 2047, ["min-byte"] = 192, ["min-code"] = 128, len = 2}, {["max-byte"] = 239, ["max-code"] = 65535, ["min-byte"] = 224, ["min-code"] = 2048, len = 3}, {["max-byte"] = 247, ["max-code"] = 1114111, ["min-byte"] = 240, ["min-code"] = 65536, len = 4}}
    local function default_byte_escape(byte, _options)
      return ("\\%03d"):format(byte)
    end
    local function utf8_escape(str, options)
      local function validate_utf8(str0, index)
        local inits = utf8_inits
        local byte = string.byte(str0, index)
        local init = nil
        do
          local ret = nil
          for _, init0 in ipairs(inits) do
            if ret then break end
            ret = (byte and (function(_82_,_83_,_84_) return (_82_ <= _83_) and (_83_ <= _84_) end)(init0["min-byte"],byte,init0["max-byte"]) and init0)
          end
          init = ret
        end
        local code = nil
        local function _85_()
          local code0 = nil
          if init then
            code0 = (byte - init["min-byte"])
          else
            code0 = nil
          end
          for i = (index + 1), (index + init.len + -1) do
            local byte0 = string.byte(str0, i)
            code0 = (byte0 and code0 and ((128 <= byte0) and (byte0 <= 191)) and ((code0 * 64) + (byte0 - 128)))
          end
          return code0
        end
        code = (init and _85_())
        if (code and (function(_87_,_88_,_89_) return (_87_ <= _88_) and (_88_ <= _89_) end)(init["min-code"],code,init["max-code"]) and not ((55296 <= code) and (code <= 57343))) then
          return init.len
        end
      end
      local index = 1
      local output = {}
      local byte_escape = (getopt(options, "byte-escape") or default_byte_escape)
      while (index <= #str) do
        local nexti = (string.find(str, "[\128-\255]", index) or (#str + 1))
        local len = validate_utf8(str, nexti)
        table.insert(output, string.sub(str, index, (nexti + (len or 0) + -1)))
        if (not len and (nexti <= #str)) then
          table.insert(output, byte_escape(str:byte(nexti), options))
        end
        if len then
          index = (nexti + len)
        else
          index = (nexti + 1)
        end
      end
      return table.concat(output)
    end
    local function pp_string(str, options, indent)
      local len = length_2a(str)
      local esc_newline_3f = ((len < 2) or (getopt(options, "escape-newlines?") and (len < (options["line-length"] - indent))))
      local byte_escape = (getopt(options, "byte-escape") or default_byte_escape)
      local escs = nil
      local _93_
      if esc_newline_3f then
        _93_ = "\\n"
      else
        _93_ = "\n"
      end
      local function _95_(_241, _242)
        return byte_escape(_242:byte(), options)
      end
      escs = setmetatable({["\""] = "\\\"", ["\11"] = "\\v", ["\12"] = "\\f", ["\13"] = "\\r", ["\7"] = "\\a", ["\8"] = "\\b", ["\9"] = "\\t", ["\\"] = "\\\\", ["\n"] = _93_}, {__index = _95_})
      local str0 = ("\"" .. str:gsub("[%c\\\"]", escs) .. "\"")
      if getopt(options, "utf8?") then
        return utf8_escape(str0, options)
      else
        return str0
      end
    end
    local function make_options(t, options)
      local defaults = nil
      do
        local tbl_14_ = {}
        for k, v in pairs(default_opts) do
          local k_15_, v_16_ = k, v
          if ((k_15_ ~= nil) and (v_16_ ~= nil)) then
            tbl_14_[k_15_] = v_16_
          end
        end
        defaults = tbl_14_
      end
      local overrides = {appearances = count_table_appearances(t, {}), level = 0, seen = {len = 0}}
      for k, v in pairs((options or {})) do
        defaults[k] = v
      end
      for k, v in pairs(overrides) do
        defaults[k] = v
      end
      return defaults
    end
    local function _98_(x, options, indent, colon_3f)
      local indent0 = (indent or 0)
      local options0 = (options or make_options(x))
      local x0 = nil
      if options0.preprocess then
        x0 = options0.preprocess(x, options0)
      else
        x0 = x
      end
      local tv = type(x0)
      local function _101_()
        local _100_0 = getmetatable(x0)
        if ((_G.type(_100_0) == "table") and true) then
          local __fennelview = _100_0.__fennelview
          return __fennelview
        end
      end
      if ((tv == "table") or ((tv == "userdata") and _101_())) then
        return pp_table(x0, options0, indent0)
      elseif (tv == "number") then
        return number__3estring(x0, options0)
      else
        local function _103_()
          if (colon_3f ~= nil) then
            return colon_3f
          elseif ("function" == type(options0["prefer-colon?"])) then
            return options0["prefer-colon?"](x0)
          else
            return getopt(options0, "prefer-colon?")
          end
        end
        if ((tv == "string") and colon_string_3f(x0) and _103_()) then
          return (":" .. x0)
        elseif (tv == "string") then
          return pp_string(x0, options0, indent0)
        elseif ((tv == "boolean") or (tv == "nil")) then
          return tostring(x0)
        else
          return ("#<" .. tostring(x0) .. ">")
        end
      end
    end
    pp = _98_
    local function _view(x, _3foptions)
      return pp(x, make_options(x, _3foptions), 0)
    end
    return _view
  end
  package.preload["fennel.utils"] = package.preload["fennel.utils"] or function(...)
    local view = require("fennel.view")
    local version = "1.5.1"
    local function luajit_vm_3f()
      return ((nil ~= _G.jit) and (type(_G.jit) == "table") and (nil ~= _G.jit.on) and (nil ~= _G.jit.off) and (type(_G.jit.version_num) == "number"))
    end
    local function luajit_vm_version()
      local jit_os = nil
      if (_G.jit.os == "OSX") then
        jit_os = "macOS"
      else
        jit_os = _G.jit.os
      end
      return (_G.jit.version .. " " .. jit_os .. "/" .. _G.jit.arch)
    end
    local function fengari_vm_3f()
      return ((nil ~= _G.fengari) and (type(_G.fengari) == "table") and (nil ~= _G.fengari.VERSION) and (type(_G.fengari.VERSION_NUM) == "number"))
    end
    local function fengari_vm_version()
      return (_G.fengari.RELEASE .. " (" .. _VERSION .. ")")
    end
    local function lua_vm_version()
      if luajit_vm_3f() then
        return luajit_vm_version()
      elseif fengari_vm_3f() then
        return fengari_vm_version()
      else
        return ("PUC " .. _VERSION)
      end
    end
    local function runtime_version(_3fas_table)
      if _3fas_table then
        return {fennel = version, lua = lua_vm_version()}
      else
        return ("Fennel " .. version .. " on " .. lua_vm_version())
      end
    end
    local len = nil
    do
      local _108_0, _109_0 = pcall(require, "utf8")
      if ((_108_0 == true) and (nil ~= _109_0)) then
        local utf8 = _109_0
        len = utf8.len
      else
        local _ = _108_0
        len = string.len
      end
    end
    local kv_order = {boolean = 2, number = 1, string = 3, table = 4}
    local function kv_compare(a, b)
      local _111_0, _112_0 = type(a), type(b)
      if (((_111_0 == "number") and (_112_0 == "number")) or ((_111_0 == "string") and (_112_0 == "string"))) then
        return (a < b)
      else
        local function _113_()
          local a_t = _111_0
          local b_t = _112_0
          return (a_t ~= b_t)
        end
        if (((nil ~= _111_0) and (nil ~= _112_0)) and _113_()) then
          local a_t = _111_0
          local b_t = _112_0
          return ((kv_order[a_t] or 5) < (kv_order[b_t] or 5))
        else
          local _ = _111_0
          return (tostring(a) < tostring(b))
        end
      end
    end
    local function add_stable_keys(succ, prev_key, src, _3fpred)
      local first = prev_key
      local last = nil
      do
        local prev = prev_key
        for _, k in ipairs(src) do
          if ((prev == k) or (succ[k] ~= nil) or (_3fpred and not _3fpred(k))) then
            prev = prev
          else
            if (first == nil) then
              first = k
              prev = k
            elseif (prev ~= nil) then
              succ[prev] = k
              prev = k
            else
              prev = k
            end
          end
        end
        last = prev
      end
      return succ, last, first
    end
    local function stablepairs(t)
      local mt_keys = nil
      do
        local _117_0 = getmetatable(t)
        if (nil ~= _117_0) then
          _117_0 = _117_0.keys
        end
        mt_keys = _117_0
      end
      local succ, prev, first_mt = nil, nil, nil
      local function _119_(_241)
        return t[_241]
      end
      succ, prev, first_mt = add_stable_keys({}, nil, (mt_keys or {}), _119_)
      local pairs_keys = nil
      do
        local _120_0 = nil
        do
          local tbl_17_ = {}
          local i_18_ = #tbl_17_
          for k in pairs(t) do
            local val_19_ = k
            if (nil ~= val_19_) then
              i_18_ = (i_18_ + 1)
              tbl_17_[i_18_] = val_19_
            end
          end
          _120_0 = tbl_17_
        end
        table.sort(_120_0, kv_compare)
        pairs_keys = _120_0
      end
      local succ0, _, first_after_mt = add_stable_keys(succ, prev, pairs_keys)
      local first = nil
      if (first_mt == nil) then
        first = first_after_mt
      else
        first = first_mt
      end
      local function stablenext(tbl, key)
        local _123_0 = nil
        if (key == nil) then
          _123_0 = first
        else
          _123_0 = succ0[key]
        end
        if (nil ~= _123_0) then
          local next_key = _123_0
          local _125_0 = tbl[next_key]
          if (_125_0 ~= nil) then
            return next_key, _125_0
          else
            return _125_0
          end
        end
      end
      return stablenext, t, nil
    end
    local function get_in(tbl, path)
      if (nil ~= path[1]) then
        local t = tbl
        for _, k in ipairs(path) do
          if (nil == t) then break end
          if (type(t) == "table") then
            t = t[k]
          else
          t = nil
          end
        end
        return t
      end
    end
    local function copy(_3ffrom, _3fto)
      local tbl_14_ = (_3fto or {})
      for k, v in pairs((_3ffrom or {})) do
        local k_15_, v_16_ = k, v
        if ((k_15_ ~= nil) and (v_16_ ~= nil)) then
          tbl_14_[k_15_] = v_16_
        end
      end
      return tbl_14_
    end
    local function member_3f(x, tbl, _3fn)
      local _131_0 = tbl[(_3fn or 1)]
      if (_131_0 == x) then
        return true
      elseif (_131_0 == nil) then
        return nil
      else
        local _ = _131_0
        return member_3f(x, tbl, ((_3fn or 1) + 1))
      end
    end
    local function maxn(tbl)
      local max = 0
      for k in pairs(tbl) do
        if ("number" == type(k)) then
          max = math.max(max, k)
        else
          max = max
        end
      end
      return max
    end
    local function every_3f(t, predicate)
      local result = true
      for _, item in ipairs(t) do
        if not result then break end
        result = predicate(item)
      end
      return result
    end
    local function allpairs(tbl)
      assert((type(tbl) == "table"), "allpairs expects a table")
      local t = tbl
      local seen = {}
      local function allpairs_next(_, state)
        local next_state, value = next(t, state)
        if seen[next_state] then
          return allpairs_next(nil, next_state)
        elseif next_state then
          seen[next_state] = true
          return next_state, value
        else
          local _134_0 = getmetatable(t)
          if ((_G.type(_134_0) == "table") and true) then
            local __index = _134_0.__index
            if ("table" == type(__index)) then
              t = __index
              return allpairs_next(t)
            end
          end
        end
      end
      return allpairs_next
    end
    local function deref(self)
      return self[1]
    end
    local function list__3estring(self, _3fview, _3foptions, _3findent)
      local viewed = nil
      do
        local tbl_17_ = {}
        local i_18_ = #tbl_17_
        for i = 1, maxn(self) do
          local val_19_ = nil
          if _3fview then
            val_19_ = _3fview(self[i], _3foptions, _3findent)
          else
            val_19_ = view(self[i])
          end
          if (nil ~= val_19_) then
            i_18_ = (i_18_ + 1)
            tbl_17_[i_18_] = val_19_
          end
        end
        viewed = tbl_17_
      end
      return ("(" .. table.concat(viewed, " ") .. ")")
    end
    local function comment_view(c)
      return c, true
    end
    local function sym_3d(a, b)
      return ((deref(a) == deref(b)) and (getmetatable(a) == getmetatable(b)))
    end
    local function sym_3c(a, b)
      return (a[1] < tostring(b))
    end
    local symbol_mt = {"SYMBOL", __eq = sym_3d, __fennelview = deref, __lt = sym_3c, __tostring = deref}
    local expr_mt = nil
    local function _140_(x)
      return tostring(deref(x))
    end
    expr_mt = {"EXPR", __tostring = _140_}
    local list_mt = {"LIST", __fennelview = list__3estring, __tostring = list__3estring}
    local comment_mt = {"COMMENT", __eq = sym_3d, __fennelview = comment_view, __lt = sym_3c, __tostring = deref}
    local sequence_marker = {"SEQUENCE"}
    local varg_mt = {"VARARG", __fennelview = deref, __tostring = deref}
    local getenv = nil
    local function _141_()
      return nil
    end
    getenv = ((os and os.getenv) or _141_)
    local function debug_on_3f(flag)
      local level = (getenv("FENNEL_DEBUG") or "")
      return ((level == "all") or level:find(flag))
    end
    local function list(...)
      return setmetatable({...}, list_mt)
    end
    local function sym(str, _3fsource)
      local _142_
      do
        local tbl_14_ = {str}
        for k, v in pairs((_3fsource or {})) do
          local k_15_, v_16_ = nil, nil
          if (type(k) == "string") then
            k_15_, v_16_ = k, v
          else
          k_15_, v_16_ = nil
          end
          if ((k_15_ ~= nil) and (v_16_ ~= nil)) then
            tbl_14_[k_15_] = v_16_
          end
        end
        _142_ = tbl_14_
      end
      return setmetatable(_142_, symbol_mt)
    end
    local function sequence(...)
      local function _145_(seq, view0, inspector, indent)
        local opts = nil
        do
          inspector["empty-as-sequence?"] = {after = inspector["empty-as-sequence?"], once = true}
          inspector["metamethod?"] = {after = inspector["metamethod?"], once = false}
          opts = inspector
        end
        return view0(seq, opts, indent)
      end
      return setmetatable({...}, {__fennelview = _145_, sequence = sequence_marker})
    end
    local function expr(strcode, etype)
      return setmetatable({strcode, type = etype}, expr_mt)
    end
    local function comment_2a(contents, _3fsource)
      local _146_ = (_3fsource or {})
      local filename = _146_["filename"]
      local line = _146_["line"]
      return setmetatable({contents, filename = filename, line = line}, comment_mt)
    end
    local function varg(_3fsource)
      local _147_
      do
        local tbl_14_ = {"..."}
        for k, v in pairs((_3fsource or {})) do
          local k_15_, v_16_ = nil, nil
          if (type(k) == "string") then
            k_15_, v_16_ = k, v
          else
          k_15_, v_16_ = nil
          end
          if ((k_15_ ~= nil) and (v_16_ ~= nil)) then
            tbl_14_[k_15_] = v_16_
          end
        end
        _147_ = tbl_14_
      end
      return setmetatable(_147_, varg_mt)
    end
    local function expr_3f(x)
      return ((type(x) == "table") and (getmetatable(x) == expr_mt) and x)
    end
    local function varg_3f(x)
      return ((type(x) == "table") and (getmetatable(x) == varg_mt) and x)
    end
    local function list_3f(x)
      return ((type(x) == "table") and (getmetatable(x) == list_mt) and x)
    end
    local function sym_3f(x, _3fname)
      return ((type(x) == "table") and (getmetatable(x) == symbol_mt) and ((nil == _3fname) or (x[1] == _3fname)) and x)
    end
    local function sequence_3f(x)
      local mt = ((type(x) == "table") and getmetatable(x))
      return (mt and (mt.sequence == sequence_marker) and x)
    end
    local function comment_3f(x)
      return ((type(x) == "table") and (getmetatable(x) == comment_mt) and x)
    end
    local function table_3f(x)
      return ((type(x) == "table") and not varg_3f(x) and (getmetatable(x) ~= list_mt) and (getmetatable(x) ~= symbol_mt) and not comment_3f(x) and x)
    end
    local function kv_table_3f(t)
      if table_3f(t) then
        local nxt, t0, k = pairs(t)
        local len0 = #t0
        local next_state = nil
        if (0 == len0) then
          next_state = k
        else
          next_state = len0
        end
        return ((nil ~= nxt(t0, next_state)) and t0)
      end
    end
    local function string_3f(x)
      if (type(x) == "string") then
        return x
      else
        return false
      end
    end
    local function multi_sym_3f(str)
      if sym_3f(str) then
        return multi_sym_3f(tostring(str))
      elseif (type(str) ~= "string") then
        return false
      else
        local function _153_()
          local parts = {}
          for part in str:gmatch("[^%.%:]+[%.%:]?") do
            local last_char = part:sub(-1)
            if (last_char == ":") then
              parts["multi-sym-method-call"] = true
            end
            if ((last_char == ":") or (last_char == ".")) then
              parts[(#parts + 1)] = part:sub(1, -2)
            else
              parts[(#parts + 1)] = part
            end
          end
          return (next(parts) and parts)
        end
        return ((str:match("%.") or str:match(":")) and not str:match("%.%.") and (str:byte() ~= string.byte(".")) and (str:byte() ~= string.byte(":")) and (str:byte(-1) ~= string.byte(".")) and (str:byte(-1) ~= string.byte(":")) and _153_())
      end
    end
    local function call_of_3f(ast, callee)
      return (list_3f(ast) and sym_3f(ast[1], callee))
    end
    local function quoted_3f(symbol)
      return symbol.quoted
    end
    local function idempotent_expr_3f(x)
      local t = type(x)
      return ((t == "string") or (t == "number") or (t == "boolean") or (sym_3f(x) and not multi_sym_3f(x)))
    end
    local function walk_tree(root, f, _3fcustom_iterator)
      local function walk(iterfn, parent, idx, node)
        if (f(idx, node, parent) and not sym_3f(node)) then
          for k, v in iterfn(node) do
            walk(iterfn, node, k, v)
          end
          return nil
        end
      end
      walk((_3fcustom_iterator or pairs), nil, nil, root)
      return root
    end
    local root = nil
    local function _158_()
    end
    root = {chunk = nil, options = nil, reset = _158_, scope = nil}
    root["set-reset"] = function(_159_0)
      local _160_ = _159_0
      local chunk = _160_["chunk"]
      local options = _160_["options"]
      local reset = _160_["reset"]
      local scope = _160_["scope"]
      root.reset = function()
        root.chunk, root.scope, root.options, root.reset = chunk, scope, options, reset
        return nil
      end
      return root.reset
    end
    local lua_keywords = {["and"] = true, ["break"] = true, ["do"] = true, ["else"] = true, ["elseif"] = true, ["end"] = true, ["false"] = true, ["for"] = true, ["function"] = true, ["goto"] = true, ["if"] = true, ["in"] = true, ["local"] = true, ["nil"] = true, ["not"] = true, ["or"] = true, ["repeat"] = true, ["return"] = true, ["then"] = true, ["true"] = true, ["until"] = true, ["while"] = true}
    local function lua_keyword_3f(str)
      local function _162_()
        local _161_0 = root.options
        if (nil ~= _161_0) then
          _161_0 = _161_0.keywords
        end
        if (nil ~= _161_0) then
          _161_0 = _161_0[str]
        end
        return _161_0
      end
      return (lua_keywords[str] or _162_())
    end
    local function valid_lua_identifier_3f(str)
      return (str:match("^[%a_][%w_]*$") and not lua_keyword_3f(str))
    end
    local propagated_options = {"allowedGlobals", "indent", "correlate", "useMetadata", "env", "compiler-env", "compilerEnv"}
    local function propagate_options(options, subopts)
      local tbl_14_ = subopts
      for _, name in ipairs(propagated_options) do
        local k_15_, v_16_ = name, options[name]
        if ((k_15_ ~= nil) and (v_16_ ~= nil)) then
          tbl_14_[k_15_] = v_16_
        end
      end
      return tbl_14_
    end
    local function ast_source(ast)
      if (table_3f(ast) or sequence_3f(ast)) then
        return (getmetatable(ast) or {})
      elseif ("table" == type(ast)) then
        return ast
      else
        return {}
      end
    end
    local function warn(msg, _3fast, _3ffilename, _3fline)
      local _167_0 = nil
      do
        local _168_0 = root.options
        if (nil ~= _168_0) then
          _168_0 = _168_0.warn
        end
        _167_0 = _168_0
      end
      if (nil ~= _167_0) then
        local opt_warn = _167_0
        return opt_warn(msg, _3fast, _3ffilename, _3fline)
      else
        local _ = _167_0
        if (_G.io and _G.io.stderr) then
          local loc = nil
          do
            local _170_0 = ast_source(_3fast)
            if ((_G.type(_170_0) == "table") and (nil ~= _170_0.filename) and (nil ~= _170_0.line)) then
              local filename = _170_0.filename
              local line = _170_0.line
              loc = (filename .. ":" .. line .. ": ")
            else
              local _0 = _170_0
              if (_3ffilename and _3fline) then
                loc = (_3ffilename .. ":" .. _3fline .. ": ")
              else
                loc = ""
              end
            end
          end
          return (_G.io.stderr):write(("--WARNING: %s%s\n"):format(loc, msg))
        end
      end
    end
    local warned = {}
    local function check_plugin_version(_175_0)
      local _176_ = _175_0
      local plugin = _176_
      local name = _176_["name"]
      local versions = _176_["versions"]
      if (not member_3f(version:gsub("-dev", ""), (versions or {})) and not (string_3f(versions) and version:find(versions)) and not warned[plugin]) then
        warned[plugin] = true
        return warn(string.format("plugin %s does not support Fennel version %s", (name or "unknown"), version))
      end
    end
    local function hook_opts(event, _3foptions, ...)
      local plugins = nil
      local function _179_(...)
        local _178_0 = _3foptions
        if (nil ~= _178_0) then
          _178_0 = _178_0.plugins
        end
        return _178_0
      end
      local function _182_(...)
        local _181_0 = root.options
        if (nil ~= _181_0) then
          _181_0 = _181_0.plugins
        end
        return _181_0
      end
      plugins = (_179_(...) or _182_(...))
      if plugins then
        local result = nil
        for _, plugin in ipairs(plugins) do
          if (nil ~= result) then break end
          check_plugin_version(plugin)
          local _184_0 = plugin[event]
          if (nil ~= _184_0) then
            local f = _184_0
            result = f(...)
          else
          result = nil
          end
        end
        return result
      end
    end
    local function hook(event, ...)
      return hook_opts(event, root.options, ...)
    end
    return {["ast-source"] = ast_source, ["call-of?"] = call_of_3f, ["comment?"] = comment_3f, ["debug-on?"] = debug_on_3f, ["every?"] = every_3f, ["expr?"] = expr_3f, ["fennel-module"] = nil, ["get-in"] = get_in, ["hook-opts"] = hook_opts, ["idempotent-expr?"] = idempotent_expr_3f, ["kv-table?"] = kv_table_3f, ["list?"] = list_3f, ["lua-keyword?"] = lua_keyword_3f, ["macro-path"] = table.concat({"./?.fnl", "./?/init-macros.fnl", "./?/init.fnl", getenv("FENNEL_MACRO_PATH")}, ";"), ["member?"] = member_3f, ["multi-sym?"] = multi_sym_3f, ["propagate-options"] = propagate_options, ["quoted?"] = quoted_3f, ["runtime-version"] = runtime_version, ["sequence?"] = sequence_3f, ["string?"] = string_3f, ["sym?"] = sym_3f, ["table?"] = table_3f, ["valid-lua-identifier?"] = valid_lua_identifier_3f, ["varg?"] = varg_3f, ["walk-tree"] = walk_tree, allpairs = allpairs, comment = comment_2a, copy = copy, expr = expr, hook = hook, len = len, list = list, maxn = maxn, path = table.concat({"./?.fnl", "./?/init.fnl", getenv("FENNEL_PATH")}, ";"), root = root, sequence = sequence, stablepairs = stablepairs, sym = sym, varg = varg, version = version, warn = warn}
  end
  utils = require("fennel.utils")
  local parser = require("fennel.parser")
  local compiler = require("fennel.compiler")
  local specials = require("fennel.specials")
  local repl = require("fennel.repl")
  local view = require("fennel.view")
  local function eval_env(env, opts)
    if (env == "_COMPILER") then
      local env0 = specials["make-compiler-env"](nil, compiler.scopes.compiler, {}, opts)
      if (opts.allowedGlobals == nil) then
        opts.allowedGlobals = specials["current-global-names"](env0)
      end
      return specials["wrap-env"](env0)
    else
      return (env and specials["wrap-env"](env))
    end
  end
  local function eval_opts(options, str)
    local opts = utils.copy(options)
    if (opts.allowedGlobals == nil) then
      opts.allowedGlobals = specials["current-global-names"](opts.env)
    end
    if (not opts.filename and not opts.source) then
      opts.source = str
    end
    if (opts.env == "_COMPILER") then
      opts.scope = compiler["make-scope"](compiler.scopes.compiler)
    end
    return opts
  end
  local function eval(str, _3foptions, ...)
    local opts = eval_opts(_3foptions, str)
    local env = eval_env(opts.env, opts)
    local lua_source = compiler["compile-string"](str, opts)
    local loader = nil
    local function _841_(...)
      if opts.filename then
        return ("@" .. opts.filename)
      else
        return str
      end
    end
    loader = specials["load-code"](lua_source, env, _841_(...))
    opts.filename = nil
    return loader(...)
  end
  local function dofile_2a(filename, _3foptions, ...)
    local opts = utils.copy(_3foptions)
    local f = assert(io.open(filename, "rb"))
    local source = assert(f:read("*all"), ("Could not read " .. filename))
    f:close()
    opts.filename = filename
    return eval(source, opts, ...)
  end
  local function syntax()
    local body_3f = {"when", "with-open", "collect", "icollect", "fcollect", "lambda", "\206\187", "macro", "match", "match-try", "case", "case-try", "accumulate", "faccumulate", "doto"}
    local binding_3f = {"collect", "icollect", "fcollect", "each", "for", "let", "with-open", "accumulate", "faccumulate"}
    local define_3f = {"fn", "lambda", "\206\187", "var", "local", "macro", "macros", "global"}
    local deprecated = {"~=", "#", "global", "require-macros", "pick-args"}
    local out = {}
    for k, v in pairs(compiler.scopes.global.specials) do
      local metadata = (compiler.metadata[v] or {})
      out[k] = {["binding-form?"] = utils["member?"](k, binding_3f), ["body-form?"] = metadata["fnl/body-form?"], ["define?"] = utils["member?"](k, define_3f), ["deprecated?"] = utils["member?"](k, deprecated), ["special?"] = true}
    end
    for k in pairs(compiler.scopes.global.macros) do
      out[k] = {["binding-form?"] = utils["member?"](k, binding_3f), ["body-form?"] = utils["member?"](k, body_3f), ["define?"] = utils["member?"](k, define_3f), ["macro?"] = true}
    end
    for k, v in pairs(_G) do
      local _842_0 = type(v)
      if (_842_0 == "function") then
        out[k] = {["function?"] = true, ["global?"] = true}
      elseif (_842_0 == "table") then
        if not k:find("^_") then
          for k2, v2 in pairs(v) do
            if ("function" == type(v2)) then
              out[(k .. "." .. k2)] = {["function?"] = true, ["global?"] = true}
            end
          end
          out[k] = {["global?"] = true}
        end
      end
    end
    return out
  end
  local mod = {["ast-source"] = utils["ast-source"], ["comment?"] = utils["comment?"], ["compile-stream"] = compiler["compile-stream"], ["compile-string"] = compiler["compile-string"], ["list?"] = utils["list?"], ["load-code"] = specials["load-code"], ["macro-loaded"] = specials["macro-loaded"], ["macro-path"] = utils["macro-path"], ["macro-searchers"] = specials["macro-searchers"], ["make-searcher"] = specials["make-searcher"], ["multi-sym?"] = utils["multi-sym?"], ["runtime-version"] = utils["runtime-version"], ["search-module"] = specials["search-module"], ["sequence?"] = utils["sequence?"], ["string-stream"] = parser["string-stream"], ["sym-char?"] = parser["sym-char?"], ["sym?"] = utils["sym?"], ["table?"] = utils["table?"], ["varg?"] = utils["varg?"], comment = utils.comment, compile = compiler.compile, compile1 = compiler.compile1, compileStream = compiler["compile-stream"], compileString = compiler["compile-string"], doc = specials.doc, dofile = dofile_2a, eval = eval, gensym = compiler.gensym, getinfo = compiler.getinfo, granulate = parser.granulate, list = utils.list, loadCode = specials["load-code"], macroLoaded = specials["macro-loaded"], macroPath = utils["macro-path"], macroSearchers = specials["macro-searchers"], makeSearcher = specials["make-searcher"], make_searcher = specials["make-searcher"], mangle = compiler["global-mangling"], metadata = compiler.metadata, parser = parser.parser, path = utils.path, repl = repl, runtimeVersion = utils["runtime-version"], scope = compiler["make-scope"], searchModule = specials["search-module"], searcher = specials["make-searcher"](), sequence = utils.sequence, stringStream = parser["string-stream"], sym = utils.sym, syntax = syntax, traceback = compiler.traceback, unmangle = compiler["global-unmangling"], varg = utils.varg, version = utils.version, view = view}
  mod.install = function(_3fopts)
    table.insert((package.searchers or package.loaders), specials["make-searcher"](_3fopts))
    return mod
  end
  utils["fennel-module"] = mod
  do
    local module_name = "fennel.macros"
    local _ = nil
    local function _846_()
      return mod
    end
    package.preload[module_name] = _846_
    _ = nil
    local env = nil
    do
      local _847_0 = specials["make-compiler-env"](nil, compiler.scopes.compiler, {})
      _847_0["utils"] = utils
      _847_0["fennel"] = mod
      _847_0["get-function-metadata"] = specials["get-function-metadata"]
      env = _847_0
    end
    local built_ins = eval([===[;; fennel-ls: macro-file
    
    ;; These macros are awkward because their definition cannot rely on the any
    ;; built-in macros, only special forms. (no when, no icollect, etc)
    
    (fn copy [t]
      (let [out []]
        (each [_ v (ipairs t)] (table.insert out v))
        (setmetatable out (getmetatable t))))
    
    (fn ->* [val ...]
      "Thread-first macro.
    Take the first value and splice it into the second form as its first argument.
    The value of the second form is spliced into the first arg of the third, etc."
      (var x val)
      (each [_ e (ipairs [...])]
        (let [elt (if (list? e) (copy e) (list e))]
          (table.insert elt 2 x)
          (set x elt)))
      x)
    
    (fn ->>* [val ...]
      "Thread-last macro.
    Same as ->, except splices the value into the last position of each form
    rather than the first."
      (var x val)
      (each [_ e (ipairs [...])]
        (let [elt (if (list? e) (copy e) (list e))]
          (table.insert elt x)
          (set x elt)))
      x)
    
    (fn -?>* [val ?e ...]
      "Nil-safe thread-first macro.
    Same as -> except will short-circuit with nil when it encounters a nil value."
      (if (= nil ?e)
          val
          (not (utils.idempotent-expr? val))
          ;; try again, but with an eval-safe val
          `(let [tmp# ,val]
            (-?> tmp# ,?e ,...))
          (let [call (if (list? ?e) (copy ?e) (list ?e))]
            (table.insert call 2 val)
            `(if (not= nil ,val)
                 ,(-?>* call ...)))))
    
    (fn -?>>* [val ?e ...]
      "Nil-safe thread-last macro.
    Same as ->> except will short-circuit with nil when it encounters a nil value."
      (if (= nil ?e)
          val
          (not (utils.idempotent-expr? val))
          ;; try again, but with an eval-safe val
          `(let [tmp# ,val]
            (-?>> tmp# ,?e ,...))
          (let [call (if (list? ?e) (copy ?e) (list ?e))]
            (table.insert call val)
            `(if (not= ,val nil)
                 ,(-?>>* call ...)))))
    
    (fn ?dot [tbl ...]
      "Nil-safe table look up.
    Same as . (dot), except will short-circuit with nil when it encounters
    a nil value in any of subsequent keys."
      (let [head (gensym :t)
            lookups `(do
                       (var ,head ,tbl)
                       ,head)]
        (each [i k (ipairs [...])]
          ;; Kinda gnarly to reassign in place like this, but it emits the best lua.
          ;; With this impl, it emits a flat, concise, and readable set of ifs
          (table.insert lookups (+ i 2)
                        `(if (not= nil ,head) (set ,head (. ,head ,k)))))
        lookups))
    
    (fn doto* [val ...]
      "Evaluate val and splice it into the first argument of subsequent forms."
      (assert (not= val nil) "missing subject")
      (if (not (utils.idempotent-expr? val))
        `(let [tmp# ,val]
           (doto tmp# ,...))
        (let [form `(do)]
          (each [_ elt (ipairs [...])]
            (let [elt (if (list? elt) (copy elt) (list elt))]
              (table.insert elt 2 val)
              (table.insert form elt)))
          (table.insert form val)
          form)))
    
    (fn when* [condition body1 ...]
      "Evaluate body for side-effects only when condition is truthy."
      (assert body1 "expected body")
      `(if ,condition
           (do
             ,body1
             ,...)))
    
    (fn with-open* [closable-bindings ...]
      "Like `let`, but invokes (v:close) on each binding after evaluating the body.
    The body is evaluated inside `xpcall` so that bound values will be closed upon
    encountering an error before propagating it."
      (let [bodyfn `(fn []
                      ,...)
            closer `(fn close-handlers# [ok# ...]
                      (if ok# ... (error ... 0)))
            traceback `(. (or (. package.loaded ,(fennel-module-name)) _G.debug {})
                          :traceback)]
        (for [i 1 (length closable-bindings) 2]
          (assert (sym? (. closable-bindings i))
                  "with-open only allows symbols in bindings")
          (table.insert closer 4 `(: ,(. closable-bindings i) :close)))
        `(let ,closable-bindings
           ,closer
           (close-handlers# (_G.xpcall ,bodyfn ,traceback)))))
    
    (fn extract-into [iter-tbl]
      (var (into iter-out found?) (values [] (copy iter-tbl)))
      (for [i (length iter-tbl) 2 -1]
        (let [item (. iter-tbl i)]
          (if (or (sym? item "&into") (= :into item))
              (do
                (assert (not found?) "expected only one &into clause")
                (set found? true)
                (set into (. iter-tbl (+ i 1)))
                (table.remove iter-out i)
                (table.remove iter-out i)))))
      (assert (or (not found?) (sym? into) (table? into) (list? into))
              "expected table, function call, or symbol in &into clause")
      (values into iter-out found?))
    
    (fn collect* [iter-tbl key-expr value-expr ...]
      "Return a table made by running an iterator and evaluating an expression that
    returns key-value pairs to be inserted sequentially into the table.  This can
    be thought of as a table comprehension. The body should provide two expressions
    (used as key and value) or nil, which causes it to be omitted.
    
    For example,
      (collect [k v (pairs {:apple \"red\" :orange \"orange\"})]
        (values v k))
    returns
      {:red \"apple\" :orange \"orange\"}
    
    Supports an &into clause after the iterator to put results in an existing table.
    Supports early termination with an &until clause."
      (assert (and (sequence? iter-tbl) (<= 2 (length iter-tbl)))
              "expected iterator binding table")
      (assert (not= nil key-expr) "expected key and value expression")
      (assert (= nil ...)
              "expected 1 or 2 body expressions; wrap multiple expressions with do")
      (assert (or value-expr (list? key-expr)) "need key and value")
      (let [kv-expr (if (= nil value-expr) key-expr `(values ,key-expr ,value-expr))
            (into iter) (extract-into iter-tbl)]
        `(let [tbl# ,into]
           (each ,iter
             (let [(k# v#) ,kv-expr]
               (if (and (not= k# nil) (not= v# nil))
                 (tset tbl# k# v#))))
           tbl#)))
    
    (fn seq-collect [how iter-tbl value-expr ...]
      "Common part between icollect and fcollect for producing sequential tables.
    
    Iteration code only differs in using the for or each keyword, the rest
    of the generated code is identical."
      (assert (not= nil value-expr) "expected table value expression")
      (assert (= nil ...)
              "expected exactly one body expression. Wrap multiple expressions in do")
      (let [(into iter has-into?) (extract-into iter-tbl)]
        (if has-into?
            `(let [tbl# ,into]
               (,how ,iter (let [val# ,value-expr]
                             (table.insert tbl# val#)))
               tbl#)
            ;; believe it or not, using a var here has a pretty good performance
            ;; boost: https://p.hagelb.org/icollect-performance.html
            ;; but it doesn't always work with &into clauses, so skip if that's used
            `(let [tbl# []]
               (var i# 0)
               (,how ,iter
                     (let [val# ,value-expr]
                       (when (not= nil val#)
                         (set i# (+ i# 1))
                         (tset tbl# i# val#))))
               tbl#))))
    
    (fn icollect* [iter-tbl value-expr ...]
      "Return a sequential table made by running an iterator and evaluating an
    expression that returns values to be inserted sequentially into the table.
    This can be thought of as a table comprehension. If the body evaluates to nil
    that element is omitted.
    
    For example,
      (icollect [_ v (ipairs [1 2 3 4 5])]
        (when (not= v 3)
          (* v v)))
    returns
      [1 4 16 25]
    
    Supports an &into clause after the iterator to put results in an existing table.
    Supports early termination with an &until clause."
      (assert (and (sequence? iter-tbl) (<= 2 (length iter-tbl)))
              "expected iterator binding table")
      (seq-collect 'each iter-tbl value-expr ...))
    
    (fn fcollect* [iter-tbl value-expr ...]
      "Return a sequential table made by advancing a range as specified by
    for, and evaluating an expression that returns values to be inserted
    sequentially into the table.  This can be thought of as a range
    comprehension. If the body evaluates to nil that element is omitted.
    
    For example,
      (fcollect [i 1 10 2]
        (when (not= i 3)
          (* i i)))
    returns
      [1 25 49 81]
    
    Supports an &into clause after the range to put results in an existing table.
    Supports early termination with an &until clause."
      (assert (and (sequence? iter-tbl) (< 2 (length iter-tbl)))
              "expected range binding table")
      (seq-collect 'for iter-tbl value-expr ...))
    
    (fn accumulate-impl [for? iter-tbl body ...]
      (assert (and (sequence? iter-tbl) (<= 4 (length iter-tbl)))
              "expected initial value and iterator binding table")
      (assert (not= nil body) "expected body expression")
      (assert (= nil ...)
              "expected exactly one body expression. Wrap multiple expressions with do")
      (let [[accum-var accum-init] iter-tbl
            iter (sym (if for? "for" "each"))] ; accumulate or faccumulate?
        `(do
           (var ,accum-var ,accum-init)
           (,iter ,[(unpack iter-tbl 3)]
                  (set ,accum-var ,body))
           ,(if (list? accum-var)
              (list (sym :values) (unpack accum-var))
              accum-var))))
    
    (fn accumulate* [iter-tbl body ...]
      "Accumulation macro.
    
    It takes a binding table and an expression as its arguments.  In the binding
    table, the first form starts out bound to the second value, which is an initial
    accumulator. The rest are an iterator binding table in the format `each` takes.
    
    It runs through the iterator in each step of which the given expression is
    evaluated, and the accumulator is set to the value of the expression. It
    eventually returns the final value of the accumulator.
    
    For example,
      (accumulate [total 0
                   _ n (pairs {:apple 2 :orange 3})]
        (+ total n))
    returns 5"
      (accumulate-impl false iter-tbl body ...))
    
    (fn faccumulate* [iter-tbl body ...]
      "Identical to accumulate, but after the accumulator the binding table is the
    same as `for` instead of `each`. Like collect to fcollect, will iterate over a
    numerical range like `for` rather than an iterator."
      (accumulate-impl true iter-tbl body ...))
    
    (fn partial* [f ...]
      "Return a function with all arguments partially applied to f."
      (assert f "expected a function to partially apply")
      (let [bindings []
            args []]
        (each [_ arg (ipairs [...])]
          (if (utils.idempotent-expr? arg)
            (table.insert args arg)
            (let [name (gensym)]
              (table.insert bindings name)
              (table.insert bindings arg)
              (table.insert args name))))
        (let [body (list f (unpack args))]
          (table.insert body _VARARG)
          ;; only use the extra let if we need double-eval protection
          (if (= nil (. bindings 1))
              `(fn [,_VARARG] ,body)
              `(let ,bindings
                 (fn [,_VARARG] ,body))))))
    
    (fn pick-args* [n f]
      "Create a function of arity n that applies its arguments to f. Deprecated."
      (if (and _G.io _G.io.stderr)
          (_G.io.stderr:write
           "-- WARNING: pick-args is deprecated and will be removed in the future.\n"))
      (let [bindings []]
        (for [i 1 n] (tset bindings i (gensym)))
        `(fn ,bindings (,f ,(unpack bindings)))))
    
    (fn lambda* [...]
      "Function literal with nil-checked arguments.
    Like `fn`, but will throw an exception if a declared argument is passed in as
    nil, unless that argument's name begins with a question mark."
      (let [args [...]
            args-len (length args)
            has-internal-name? (sym? (. args 1))
            arglist (if has-internal-name? (. args 2) (. args 1))
            metadata-position (if has-internal-name? 3 2)
            (_ check-position) (get-function-metadata [:lambda ...] arglist
                                                      metadata-position)
            empty-body? (< args-len check-position)]
        (fn check! [a]
          (if (table? a)
              (each [_ a (pairs a)] (check! a))
              (let [as (tostring a)]
                (and (not (as:find "^?")) (not= as "&") (not (as:find "^_"))
                     (not= as "...") (not= as "&as")))
              (table.insert args check-position
                            `(_G.assert (not= nil ,a)
                                        ,(: "Missing argument %s on %s:%s" :format
                                            (tostring a)
                                            (or a.filename :unknown)
                                            (or a.line "?"))))))
    
        (assert (= :table (type arglist)) "expected arg list")
        (each [_ a (ipairs arglist)] (check! a))
        (if empty-body? (table.insert args (sym :nil)))
        `(fn ,(unpack args))))
    
    (fn macro* [name ...]
      "Define a single macro."
      (assert (sym? name) "expected symbol for macro name")
      (local args [...])
      `(macros {,(tostring name) (fn ,(unpack args))}))
    
    (fn macrodebug* [form return?]
      "Print the resulting form after performing macroexpansion.
    With a second argument, returns expanded form as a string instead of printing."
      (let [handle (if return? `do `print)]
        ;; TODO: Provide a helpful compiler error in the unlikely edge case of an
        ;; infinite AST instead of the current "silently expand until max depth"
        `(,handle ,(view (macroexpand form _SCOPE) {:detect-cycles? false}))))
    
    (fn import-macros* [binding1 module-name1 ...]
      "Bind a table of macros from each macro module according to a binding form.
    Each binding form can be either a symbol or a k/v destructuring table.
    Example:
      (import-macros mymacros                 :my-macros    ; bind to symbol
                     {:macro1 alias : macro2} :proj.macros) ; import by name"
      (assert (and binding1 module-name1 (= 0 (% (select "#" ...) 2)))
              "expected even number of binding/modulename pairs")
      (for [i 1 (select "#" binding1 module-name1 ...) 2]
        ;; delegate the actual loading of the macros to the require-macros
        ;; special which already knows how to set up the compiler env and stuff.
        ;; this is weird because require-macros is deprecated but it works.
        (let [(binding modname) (select i binding1 module-name1 ...)
              scope (get-scope)
              ;; if the module-name is an expression (and not just a string) we
              ;; patch our expression to have the correct source filename so
              ;; require-macros can pass it down when resolving the module-name.
              expr `(import-macros ,modname)
              filename (if (list? modname) (. modname 1 :filename) :unknown)
              _ (tset expr :filename filename)
              macros* (_SPECIALS.require-macros expr scope {} binding)]
          (if (sym? binding)
              ;; bind whole table of macros to table bound to symbol
              (tset scope.macros (. binding 1) macros*)
              ;; 1-level table destructuring for importing individual macros
              (table? binding)
              (each [macro-name [import-key] (pairs binding)]
                (assert (= :function (type (. macros* macro-name)))
                        (.. "macro " macro-name " not found in module "
                            (tostring modname)))
                (tset scope.macros import-key (. macros* macro-name))))))
      nil)
    
    (fn assert-repl* [condition ...]
      "Enter into a debug REPL  and print the message when condition is false/nil.
    Works as a drop-in replacement for Lua's `assert`.
    REPL `,return` command returns values to assert in place to continue execution."
      {:fnl/arglist [condition ?message ...]}
      (fn add-locals [{: symmeta : parent} locals]
        (each [name (pairs symmeta)]
          (tset locals name (sym name)))
        (if parent (add-locals parent locals) locals))
      `(let [unpack# (or table.unpack _G.unpack)
             pack# (or table.pack #(doto [$...] (tset :n (select :# $...))))
             ;; need to pack/unpack input args to account for (assert (foo)),
             ;; because assert returns *all* arguments upon success
             vals# (pack# ,condition ,...)
             condition# (. vals# 1)
             message# (or (. vals# 2) "assertion failed, entering repl.")]
         (if (not condition#)
             (let [opts# {:assert-repl? true}
                   fennel# (require ,(fennel-module-name))
                   locals# ,(add-locals (get-scope) [])]
               (set opts#.message (fennel#.traceback message#))
               (set opts#.env (collect [k# v# (pairs _G) &into locals#]
                                (if (= nil (. locals# k#)) (values k# v#))))
               (_G.assert (fennel#.repl opts#)))
             (values (unpack# vals# 1 vals#.n)))))
    
    {:-> ->*
     :->> ->>*
     :-?> -?>*
     :-?>> -?>>*
     :?. ?dot
     :doto doto*
     :when when*
     :with-open with-open*
     :collect collect*
     :icollect icollect*
     :fcollect fcollect*
     :accumulate accumulate*
     :faccumulate faccumulate*
     :partial partial*
     :lambda lambda*
     :λ lambda*
     :pick-args pick-args*
     :macro macro*
     :macrodebug macrodebug*
     :import-macros import-macros*
     :assert-repl assert-repl*}
    ]===], {env = env, filename = "src/fennel/macros.fnl", moduleName = module_name, scope = compiler.scopes.compiler, useMetadata = true})
    local _0 = nil
    for k, v in pairs(built_ins) do
      compiler.scopes.global.macros[k] = v
    end
    _0 = nil
    local match_macros = eval([===[;; fennel-ls: macro-file
    
    ;;; Pattern matching
    ;; This is separated out so we can use the "core" macros during the
    ;; implementation of pattern matching.
    
    (fn copy [t] (collect [k v (pairs t)] k v))
    
    (fn double-eval-safe? [x type]
      (or (= :number type) (= :string type) (= :boolean type)
          (and (sym? x) (not (multi-sym? x)))))
    
    (fn with [opts k]
      (doto (copy opts) (tset k true)))
    
    (fn without [opts k]
      (doto (copy opts) (tset k nil)))
    
    (fn case-values [vals pattern unifications case-pattern opts]
      (let [condition `(and)
            bindings []]
        (each [i pat (ipairs pattern)]
          (let [(subcondition subbindings) (case-pattern [(. vals i)] pat
                                                          unifications (without opts :multival?))]
            (table.insert condition subcondition)
            (icollect [_ b (ipairs subbindings) &into bindings] b)))
        (values condition bindings)))
    
    (fn case-table [val pattern unifications case-pattern opts ?top]
      (let [condition (if (= :table ?top) `(and) `(and (= (_G.type ,val) :table)))
            bindings []]
        (each [k pat (pairs pattern)]
          (if (sym? pat :&)
              (let [rest-pat (. pattern (+ k 1))
                    rest-val `(select ,k ((or table.unpack _G.unpack) ,val))
                    subcondition (case-table `(pick-values 1 ,rest-val)
                                              rest-pat unifications case-pattern
                                              (without opts :multival?))]
                (if (not (sym? rest-pat))
                    (table.insert condition subcondition))
                (assert (= nil (. pattern (+ k 2)))
                        "expected & rest argument before last parameter")
                (table.insert bindings rest-pat)
                (table.insert bindings [rest-val]))
              (sym? k :&as)
              (do
                (table.insert bindings pat)
                (table.insert bindings val))
              (and (= :number (type k)) (sym? pat :&as))
              (do
                (assert (= nil (. pattern (+ k 2)))
                        "expected &as argument before last parameter")
                (table.insert bindings (. pattern (+ k 1)))
                (table.insert bindings val))
              ;; don't process the pattern right after &/&as; already got it
              (or (not= :number (type k)) (and (not (sym? (. pattern (- k 1)) :&as))
                                               (not (sym? (. pattern (- k 1)) :&))))
              (let [subval `(. ,val ,k)
                    (subcondition subbindings) (case-pattern [subval] pat
                                                              unifications
                                                              (without opts :multival?))]
                (table.insert condition subcondition)
                (icollect [_ b (ipairs subbindings) &into bindings] b))))
        (values condition bindings)))
    
    (fn case-guard [vals condition guards unifications case-pattern opts]
      (if (. guards 1)
        (let [(pcondition bindings) (case-pattern vals condition unifications opts)
              condition `(and ,(unpack guards))]
           (values `(and ,pcondition
                         (let ,bindings
                           ,condition)) bindings))
        (case-pattern vals condition unifications opts)))
    
    (fn symbols-in-pattern [pattern]
      "gives the set of symbols inside a pattern"
      (if (list? pattern)
          (if (or (sym? (. pattern 1) :where)
                  (sym? (. pattern 1) :=))
              (symbols-in-pattern (. pattern 2))
              (sym? (. pattern 2) :?)
              (symbols-in-pattern (. pattern 1))
              (let [result {}]
                (each [_ child-pattern (ipairs pattern)]
                  (collect [name symbol (pairs (symbols-in-pattern child-pattern)) &into result]
                    name symbol))
                result))
          (sym? pattern)
          (if (and (not (sym? pattern :or))
                   (not (sym? pattern :nil)))
              {(tostring pattern) pattern}
              {})
          (= (type pattern) :table)
          (let [result {}]
            (each [key-pattern value-pattern (pairs pattern)]
              (collect [name symbol (pairs (symbols-in-pattern key-pattern)) &into result]
                name symbol)
              (collect [name symbol (pairs (symbols-in-pattern value-pattern)) &into result]
                name symbol))
            result)
          {}))
    
    (fn symbols-in-every-pattern [pattern-list infer-unification?]
      "gives a list of symbols that are present in every pattern in the list"
      (let [?symbols (accumulate [?symbols nil
                                  _ pattern (ipairs pattern-list)]
                       (let [in-pattern (symbols-in-pattern pattern)]
                         (if ?symbols
                           (do
                             (each [name (pairs ?symbols)]
                               (when (not (. in-pattern name))
                                 (tset ?symbols name nil)))
                             ?symbols)
                           in-pattern)))]
        (icollect [_ symbol (pairs (or ?symbols {}))]
          (if (not (and infer-unification?
                        (in-scope? symbol)))
            symbol))))
    
    (fn case-or [vals pattern guards unifications case-pattern opts]
      (let [pattern [(unpack pattern 2)]
            bindings (symbols-in-every-pattern pattern opts.infer-unification?)]
        (if (= nil (. bindings 1))
            ;; no bindings special case generates simple code
            (let [condition (icollect [_ subpattern (ipairs pattern) &into `(or)]
                              (case-pattern vals subpattern unifications opts))]
              (values (if (. guards 1)
                          `(and ,condition ,(unpack guards))
                          condition)
                      []))
          ;; case with bindings is handled specially, and returns three values instead of two
          (let [matched? (gensym :matched?)
                bindings-mangled (icollect [_ binding (ipairs bindings)]
                                   (gensym (tostring binding)))
                pre-bindings `(if)]
            (each [_ subpattern (ipairs pattern)]
              (let [(subcondition subbindings) (case-guard vals subpattern guards {} case-pattern opts)]
                (table.insert pre-bindings subcondition)
                (table.insert pre-bindings `(let ,subbindings
                                              (values true ,(unpack bindings))))))
            (values matched?
                    [`(,(unpack bindings)) `(values ,(unpack bindings-mangled))]
                    [`(,matched? ,(unpack bindings-mangled)) pre-bindings])))))
    
    (fn case-pattern [vals pattern unifications opts ?top]
      "Take the AST of values and a single pattern and returns a condition
    to determine if it matches as well as a list of bindings to
    introduce for the duration of the body if it does match."
    
      ;; This function returns the following values (multival):
      ;; a "condition", which is an expression that determines whether the
      ;;   pattern should match,
      ;; a "bindings", which bind all of the symbols used in a pattern
      ;; an optional "pre-bindings", which is a list of bindings that happen
      ;;   before the condition and bindings are evaluated. These should only
      ;;   come from a (case-or). In this case there should be no recursion:
      ;;   the call stack should be case-condition > case-pattern > case-or
      ;;
      ;; Here are the expected flags in the opts table:
      ;;   :infer-unification? boolean - if the pattern should guess when to unify  (ie, match -> true, case -> false)
      ;;   :multival? boolean - if the pattern can contain multivals  (in order to disallow patterns like [(1 2)])
      ;;   :in-where? boolean - if the pattern is surrounded by (where)  (where opts into more pattern features)
      ;;   :legacy-guard-allowed? boolean - if the pattern should allow `(a ? b) patterns
    
      ;; we have to assume we're matching against multiple values here until we
      ;; know we're either in a multi-valued clause (in which case we know the #
      ;; of vals) or we're not, in which case we only care about the first one.
      (let [[val] vals]
        (if (and (sym? pattern)
                 (or (sym? pattern :nil)
                     (and opts.infer-unification?
                          (in-scope? pattern)
                          (not (sym? pattern :_)))
                     (and opts.infer-unification?
                          (multi-sym? pattern)
                          (in-scope? (. (multi-sym? pattern) 1)))))
            (values `(= ,val ,pattern) [])
            ;; unify a local we've seen already
            (and (sym? pattern) (. unifications (tostring pattern)))
            (values `(= ,(. unifications (tostring pattern)) ,val) [])
            ;; bind a fresh local
            (sym? pattern)
            (let [wildcard? (: (tostring pattern) :find "^_")]
              (if (not wildcard?) (tset unifications (tostring pattern) val))
              (values (if (or wildcard? (string.find (tostring pattern) "^?")) true
                          `(not= ,(sym :nil) ,val)) [pattern val]))
            ;; opt-in unify with (=)
            (and (list? pattern)
                 (sym? (. pattern 1) :=)
                 (sym? (. pattern 2)))
            (let [bind (. pattern 2)]
              (assert-compile (= 2 (length pattern)) "(=) should take only one argument" pattern)
              (assert-compile (not opts.infer-unification?) "(=) cannot be used inside of match" pattern)
              (assert-compile opts.in-where? "(=) must be used in (where) patterns" pattern)
              (assert-compile (and (sym? bind) (not (sym? bind :nil)) "= has to bind to a symbol" bind))
              (values `(= ,val ,bind) []))
            ;; where-or clause
            (and (list? pattern) (sym? (. pattern 1) :where) (list? (. pattern 2)) (sym? (. pattern 2 1) :or))
            (do
              (assert-compile ?top "can't nest (where) pattern" pattern)
              (case-or vals (. pattern 2) [(unpack pattern 3)] unifications case-pattern (with opts :in-where?)))
            ;; where clause
            (and (list? pattern) (sym? (. pattern 1) :where))
            (do
              (assert-compile ?top "can't nest (where) pattern" pattern)
              (case-guard vals (. pattern 2) [(unpack pattern 3)] unifications case-pattern (with opts :in-where?)))
            ;; or clause (not allowed on its own)
            (and (list? pattern) (sym? (. pattern 1) :or))
            (do
              (assert-compile ?top "can't nest (or) pattern" pattern)
              ;; This assertion can be removed to make patterns more permissive
              (assert-compile false "(or) must be used in (where) patterns" pattern)
              (case-or vals pattern [] unifications case-pattern opts))
            ;; guard clause
            (and (list? pattern) (sym? (. pattern 2) :?))
            (do
              (assert-compile opts.legacy-guard-allowed? "legacy guard clause not supported in case" pattern)
              (case-guard vals (. pattern 1) [(unpack pattern 3)] unifications case-pattern opts))
            ;; multi-valued patterns (represented as lists)
            (list? pattern)
            (do
              (assert-compile opts.multival? "can't nest multi-value destructuring" pattern)
              (case-values vals pattern unifications case-pattern opts))
            ;; table patterns
            (= (type pattern) :table)
            (case-table val pattern unifications case-pattern opts ?top)
            ;; literal value
            (values `(= ,val ,pattern) []))))
    
    (fn add-pre-bindings [out pre-bindings]
      "Decide when to switch from the current `if` AST to a new one"
      (if pre-bindings
          ;; `out` no longer needs to grow.
          ;; Instead, a new tail `if` AST is introduced, which is where the rest of
          ;; the clauses will get appended. This way, all future clauses have the
          ;; pre-bindings in scope.
          (let [tail `(if)]
            (table.insert out true)
            (table.insert out `(let ,pre-bindings ,tail))
            tail)
          ;; otherwise, keep growing the current `if` AST.
          out))
    
    (fn case-condition [vals clauses match? top-table?]
      "Construct the actual `if` AST for the given match values and clauses."
      ;; root is the original `if` AST.
      ;; out is the `if` AST that is currently being grown.
      (let [root `(if)]
        (faccumulate [out root
                      i 1 (length clauses) 2]
          (let [pattern (. clauses i)
                body (. clauses (+ i 1))
                (condition bindings pre-bindings) (case-pattern vals pattern {}
                                                                {:multival? true
                                                                 :infer-unification? match?
                                                                 :legacy-guard-allowed? match?}
                                                                (if top-table? :table true))
                out (add-pre-bindings out pre-bindings)]
            ;; grow the `if` AST by one extra condition
            (table.insert out condition)
            (table.insert out `(let ,bindings ,body))
            out))
        root))
    
    (fn count-case-multival [pattern]
      "Identify the amount of multival values that a pattern requires."
      (if (and (list? pattern) (sym? (. pattern 2) :?))
          (count-case-multival (. pattern 1))
          (and (list? pattern) (sym? (. pattern 1) :where))
          (count-case-multival (. pattern 2))
          (and (list? pattern) (sym? (. pattern 1) :or))
          (accumulate [longest 0
                       _ child-pattern (ipairs pattern)]
            (math.max longest (count-case-multival child-pattern)))
          (list? pattern)
          (length pattern)
          1))
    
    (fn case-count-syms [clauses]
      "Find the length of the largest multi-valued clause"
      (let [patterns (fcollect [i 1 (length clauses) 2]
                       (. clauses i))]
        (accumulate [longest 0
                     _ pattern (ipairs patterns)]
          (math.max longest (count-case-multival pattern)))))
    
    (fn maybe-optimize-table [val clauses]
      (if (faccumulate [all (sequence? val) i 1 (length clauses) 2 &until (not all)]
            (and (sequence? (. clauses i))
                 (accumulate [all2 (next (. clauses i))
                              _ d (ipairs (. clauses i)) &until (not all2)]
                   (and all2 (or (not (sym? d)) (not (: (tostring d) :find "^&")))))))
          (values `(values ,(unpack val))
                  (fcollect [i 1 (length clauses)]
                    (if (= 1 (% i 2))
                        (list (unpack (. clauses i)))
                        (. clauses i))))
          (values val clauses)))
    
    (fn case-impl [match? init-val ...]
      "The shared implementation of case and match."
      (assert (not= init-val nil) "missing subject")
      (assert (= 0 (math.fmod (select :# ...) 2))
              "expected even number of pattern/body pairs")
      (assert (not= 0 (select :# ...))
              "expected at least one pattern/body pair")
      (let [(val clauses) (maybe-optimize-table init-val [...])
            vals-count (case-count-syms clauses)
            skips-multiple-eval-protection? (and (= vals-count 1) (double-eval-safe? val))]
        (if skips-multiple-eval-protection?
          (case-condition (list val) clauses match? (table? init-val))
          ;; protect against multiple evaluation of the value, bind against as
          ;; many values as we ever match against in the clauses.
          (let [vals (fcollect [_ 1 vals-count &into (list)] (gensym))]
            (list `let [vals val] (case-condition vals clauses match? (table? init-val)))))))
    
    (fn case* [val ...]
      "Perform pattern matching on val. See reference for details.
    
    Syntax:
    
    (case data-expression
      pattern body
      (where pattern guards*) body
      (where (or pattern patterns*) guards*) body)"
      (case-impl false val ...))
    
    (fn match* [val ...]
      "Perform pattern matching on val, automatically unifying on variables in
    local scope. See reference for details.
    
    Syntax:
    
    (match data-expression
      pattern body
      (where pattern guards*) body
      (where (or pattern patterns*) guards*) body)"
      (case-impl true val ...))
    
    (fn case-try-step [how expr else pattern body ...]
      (if (= nil pattern body)
          expr
          ;; unlike regular match, we can't know how many values the value
          ;; might evaluate to, so we have to capture them all in ... via IIFE
          ;; to avoid double-evaluation.
          `((fn [...]
              (,how ...
                ,pattern ,(case-try-step how body else ...)
                ,(unpack else)))
            ,expr)))
    
    (fn case-try-impl [how expr pattern body ...]
      (let [clauses [pattern body ...]
            last (. clauses (length clauses))
            catch (if (sym? (and (= :table (type last)) (. last 1)) :catch)
                     (let [[_ & e] (table.remove clauses)] e) ; remove `catch sym
                     [`_# `...])]
        (assert (= 0 (math.fmod (length clauses) 2))
                "expected every pattern to have a body")
        (assert (= 0 (math.fmod (length catch) 2))
                "expected every catch pattern to have a body")
        (case-try-step how expr catch (unpack clauses))))
    
    (fn case-try* [expr pattern body ...]
      "Perform chained pattern matching for a sequence of steps which might fail.
    
    The values from the initial expression are matched against the first pattern.
    If they match, the first body is evaluated and its values are matched against
    the second pattern, etc.
    
    If there is a (catch pat1 body1 pat2 body2 ...) form at the end, any mismatch
    from the steps will be tried against these patterns in sequence as a fallback
    just like a normal match. If there is no catch, the mismatched values will be
    returned as the value of the entire expression."
      (case-try-impl `case expr pattern body ...))
    
    (fn match-try* [expr pattern body ...]
      "Perform chained pattern matching for a sequence of steps which might fail.
    
    The values from the initial expression are matched against the first pattern.
    If they match, the first body is evaluated and its values are matched against
    the second pattern, etc.
    
    If there is a (catch pat1 body1 pat2 body2 ...) form at the end, any mismatch
    from the steps will be tried against these patterns in sequence as a fallback
    just like a normal match. If there is no catch, the mismatched values will be
    returned as the value of the entire expression."
      (case-try-impl `match expr pattern body ...))
    
    {:case case*
     :case-try case-try*
     :match match*
     :match-try match-try*}
    ]===], {allowedGlobals = false, env = env, filename = "src/fennel/match.fnl", moduleName = module_name, scope = compiler.scopes.compiler, useMetadata = true})
    for k, v in pairs(match_macros) do
      compiler.scopes.global.macros[k] = v
    end
    package.preload[module_name] = nil
  end
  return mod
end
require("fennel")
local dispatch
package.preload["fennel-ls.dispatch"] = package.preload["fennel-ls.dispatch"] or function(...)
  local handlers = require("fennel-ls.handlers")
  local message = require("fennel-ls.message")
  local function handle_request(server, send, id, method, _3fparams)
    _G.assert((nil ~= method), "Missing argument method on src/fennel-ls\\dispatch.fnl:13")
    _G.assert((nil ~= id), "Missing argument id on src/fennel-ls\\dispatch.fnl:13")
    _G.assert((nil ~= send), "Missing argument send on src/fennel-ls\\dispatch.fnl:13")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\dispatch.fnl:13")
    local _438_ = handlers.requests[method]
    if (nil ~= _438_) then
      local callback = _438_
      local _439_, _440_ = callback(server, send, _3fparams)
      if ((_439_ == nil) and (nil ~= _440_)) then
        local err = _440_
        return send(message["create-error"]("InternalError", err, id))
      else
        local _3fresponse = _439_
        return send(message["create-response"](id, _3fresponse))
      end
    elseif (_438_ == nil) then
      return send(message["create-error"]("MethodNotFound", ("\"" .. method .. "\" is not in the request-handlers table"), id))
    else
      return nil
    end
  end
  local function handle_response(_server, _send, _id, _result)
    return nil
  end
  local function handle_bad_response(_server, _send, _id, err)
    _G.assert((nil ~= err), "Missing argument err on src/fennel-ls\\dispatch.fnl:32")
    return error(("Client sent fennel-ls an error: " .. err.code))
  end
  local function handle_notification(server, send, method, _3fparams)
    _G.assert((nil ~= method), "Missing argument method on src/fennel-ls\\dispatch.fnl:36")
    _G.assert((nil ~= send), "Missing argument send on src/fennel-ls\\dispatch.fnl:36")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\dispatch.fnl:36")
    local _443_ = handlers.notifications[method]
    if (nil ~= _443_) then
      local callback = _443_
      return callback(server, send, _3fparams)
    else
      return nil
    end
  end
  local function handle(server, send, msg)
    _G.assert((nil ~= msg), "Missing argument msg on src/fennel-ls\\dispatch.fnl:42")
    _G.assert((nil ~= send), "Missing argument send on src/fennel-ls\\dispatch.fnl:42")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\dispatch.fnl:42")
    local _445_, _446_ = msg, type(msg)
    if ((_G.type(_445_) == "table") and (_445_.jsonrpc == "2.0") and (nil ~= _445_.id) and (nil ~= _445_.method) and true) then
      local id = _445_.id
      local method = _445_.method
      local _3fparams = _445_.params
      return handle_request(server, send, id, method, _3fparams)
    elseif ((_G.type(_445_) == "table") and (_445_.jsonrpc == "2.0") and (nil ~= _445_.method) and true) then
      local method = _445_.method
      local _3fparams = _445_.params
      return handle_notification(server, send, method, _3fparams)
    elseif ((_G.type(_445_) == "table") and (_445_.jsonrpc == "2.0") and (nil ~= _445_.id) and (nil ~= _445_.result)) then
      local id = _445_.id
      local result = _445_.result
      return handle_response(server, send, id, result)
    elseif ((_G.type(_445_) == "table") and (_445_.jsonrpc == "2.0") and (nil ~= _445_.id) and (nil ~= _445_.error)) then
      local id = _445_.id
      local err = _445_.error
      return handle_bad_response(server, send, id, err)
    elseif ((nil ~= _445_) and (_446_ == "string")) then
      local str = _445_
      return send(message["create-error"]("ParseError", str))
    else
      local _ = _445_
      return send(message["create-error"]("BadMessage", nil, msg.id))
    end
  end
  local function handle_2a(server, msg)
    _G.assert((nil ~= msg), "Missing argument msg on src/fennel-ls\\dispatch.fnl:65")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\dispatch.fnl:65")
    local out = {}
    local function _448_(...)
      return table.insert(out, ...)
    end
    handle(server, _448_, msg)
    return out
  end
  return {handle = handle, ["handle*"] = handle_2a}
end
package.preload["fennel-ls.handlers"] = package.preload["fennel-ls.handlers"] or function(...)
  local lint = require("fennel-ls.lint")
  local message = require("fennel-ls.message")
  local files = require("fennel-ls.files")
  local config = require("fennel-ls.config")
  local analyzer = require("fennel-ls.analyzer")
  local formatter = require("fennel-ls.formatter")
  local utils = require("fennel-ls.utils")
  local docs = require("fennel-ls.docs")
  local fennel = require("fennel")
  local requests = {}
  local notifications = {}
  requests.initialize = function(server, _send, params)
    _G.assert((nil ~= params), "Missing argument params on src/fennel-ls\\handlers.fnl:21")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\handlers.fnl:21")
    config.initialize(server, params)
    local capabilities = {positionEncoding = server["position-encoding"], textDocumentSync = {openClose = true, change = 2}, completionProvider = {triggerCharacters = {"(", "[", "{", ".", ":", "\""}, completionItem = {labelDetailsSupport = false}, resolveProvider = false, workDoneProgress = false}, hoverProvider = {workDoneProgress = false}, definitionProvider = {workDoneProgress = false}, referencesProvider = {workDoneProgress = false}, codeActionProvider = {workDoneProgress = false}, renameProvider = {workDoneProgress = false}}
    return {capabilities = capabilities, serverInfo = {name = "fennel-ls", version = "0.1.0"}}
  end
  requests["textDocument/definition"] = function(server, _send, _344_)
    local position = _344_["position"]
    local _arg_345_ = _344_["textDocument"]
    local uri = _arg_345_["uri"]
    _G.assert((nil ~= uri), "Missing argument uri on src/fennel-ls\\handlers.fnl:72")
    _G.assert((nil ~= position), "Missing argument position on src/fennel-ls\\handlers.fnl:72")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\handlers.fnl:72")
    local file = files["get-by-uri"](server, uri)
    local byte = utils["position->byte"](file.text, position, server["position-encoding"])
    local function _346_(...)
      local _347_, _348_ = ...
      if ((nil ~= _347_) and ((_G.type(_348_) == "table") and (nil ~= _348_[1]))) then
        local symbol = _347_
        local parent = _348_[1]
        local function _349_(...)
          local _350_ = ...
          if (nil ~= _350_) then
            local result = _350_
            if result.file then
              return message["range-and-uri"](server, result.file, (result.binding or result.definition))
            else
              return nil
            end
          else
            local _ = _350_
            return nil
          end
        end
        local function _353_(...)
          if file["require-calls"][parent] then
            return analyzer["search-ast"](server, file, parent, {}, {["stop-early?"] = true})
          else
            return analyzer["search-main"](server, file, symbol, {["stop-early?"] = true}, {byte = byte})
          end
        end
        return _349_(_353_(...))
      else
        local _ = _347_
        return nil
      end
    end
    return _346_(analyzer["find-symbol"](file.ast, byte))
  end
  requests["textDocument/references"] = function(server, _send, _355_)
    local position = _355_["position"]
    local _arg_356_ = _355_["textDocument"]
    local uri = _arg_356_["uri"]
    local _arg_357_ = _355_["context"]
    local _3finclude_declaration_3f = _arg_357_["includeDeclaration"]
    _G.assert((nil ~= uri), "Missing argument uri on src/fennel-ls\\handlers.fnl:89")
    _G.assert((nil ~= position), "Missing argument position on src/fennel-ls\\handlers.fnl:88")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\handlers.fnl:88")
    local file = files["get-by-uri"](server, uri)
    local byte = utils["position->byte"](file.text, position, server["position-encoding"])
    local function _358_(...)
      local _359_ = ...
      if (nil ~= _359_) then
        local symbol = _359_
        local function _360_(...)
          local _361_ = ...
          local and_362_ = (nil ~= _361_)
          if and_362_ then
            local definition = _361_
            and_362_ = (definition["referenced-by"] ~= nil)
          end
          if and_362_ then
            local definition = _361_
            local result
            do
              local tbl_21_auto = {}
              local i_22_auto = 0
              for _, _364_ in ipairs(definition["referenced-by"]) do
                local symbol0 = _364_["symbol"]
                local val_23_auto = message["range-and-uri"](server, definition.file, symbol0)
                if (nil ~= val_23_auto) then
                  i_22_auto = (i_22_auto + 1)
                  tbl_21_auto[i_22_auto] = val_23_auto
                else
                end
              end
              result = tbl_21_auto
            end
            if _3finclude_declaration_3f then
              table.insert(result, message["range-and-uri"](server, definition.file, definition.binding))
            else
            end
            return result
          else
            local _ = _361_
            return nil
          end
        end
        return _360_(analyzer["find-nearest-definition"](server, file, symbol, byte))
      else
        local _ = _359_
        return nil
      end
    end
    return _358_(analyzer["find-symbol"](file.ast, byte))
  end
  requests["textDocument/hover"] = function(server, _send, _369_)
    local position = _369_["position"]
    local _arg_370_ = _369_["textDocument"]
    local uri = _arg_370_["uri"]
    _G.assert((nil ~= uri), "Missing argument uri on src/fennel-ls\\handlers.fnl:107")
    _G.assert((nil ~= position), "Missing argument position on src/fennel-ls\\handlers.fnl:107")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\handlers.fnl:107")
    local file = files["get-by-uri"](server, uri)
    local byte = utils["position->byte"](file.text, position, server["position-encoding"])
    local function _371_(...)
      local _372_ = ...
      if (nil ~= _372_) then
        local symbol = _372_
        local function _373_(...)
          local _374_ = ...
          if (nil ~= _374_) then
            local result = _374_
            return {contents = formatter["hover-format"](result), range = message["ast->range"](server, file, symbol)}
          else
            local _ = _374_
            return nil
          end
        end
        return _373_(analyzer["search-main"](server, file, symbol, {}, {byte = byte}))
      else
        local _ = _372_
        return nil
      end
    end
    return _371_(analyzer["find-symbol"](file.ast, byte))
  end
  local function collect_scope(scope, typ, callback, _3ftarget)
    _G.assert((nil ~= callback), "Missing argument callback on src/fennel-ls\\handlers.fnl:118")
    _G.assert((nil ~= typ), "Missing argument typ on src/fennel-ls\\handlers.fnl:118")
    _G.assert((nil ~= scope), "Missing argument scope on src/fennel-ls\\handlers.fnl:118")
    local result = (_3ftarget or {})
    local scope0 = scope
    while scope0 do
      do
        local tbl_19_auto = result
        for i, v in pairs(scope0[typ]) do
          local val_20_auto = callback(i, v)
          table.insert(tbl_19_auto, val_20_auto)
        end
      end
      scope0 = scope0.parent
    end
    return result
  end
  local kinds = {Text = 1, Method = 2, Function = 3, Constructor = 4, Field = 5, Variable = 6, Class = 7, Interface = 8, Module = 9, Property = 10, Unit = 11, Value = 12, Enum = 13, Keyword = 14, Snippet = 15, Color = 16, File = 17, Reference = 18, Folder = 19, EnumMember = 20, Constant = 21, Struct = 22, Event = 23, Operator = 24, TypeParameter = 25}
  local function make_completion_item(server, file, name, scope)
    _G.assert((nil ~= scope), "Missing argument scope on src/fennel-ls\\handlers.fnl:134")
    _G.assert((nil ~= name), "Missing argument name on src/fennel-ls\\handlers.fnl:134")
    _G.assert((nil ~= file), "Missing argument file on src/fennel-ls\\handlers.fnl:134")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\handlers.fnl:134")
    local _377_ = analyzer["search-name-and-scope"](server, file, name, scope)
    if (nil ~= _377_) then
      local def = _377_
      return formatter["completion-item-format"](name, def)
    else
      local _ = _377_
      return {label = name}
    end
  end
  local function scope_completion(server, file, _byte, _3fsymbol, parents)
    _G.assert((nil ~= parents), "Missing argument parents on src/fennel-ls\\handlers.fnl:139")
    _G.assert((nil ~= file), "Missing argument file on src/fennel-ls\\handlers.fnl:139")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\handlers.fnl:139")
    local scope
    local _379_
    do
      local result = nil
      for _, parent in ipairs(parents) do
        if result then break end
        result = file.scopes[parent]
      end
      _379_ = result
    end
    scope = (_379_ or file.scope)
    local _3fparent = parents[1]
    local result = {}
    local in_call_position_3f = (fennel["list?"](_3fparent) and (_3fsymbol == _3fparent[1]))
    local function _380_(_241)
      local tmp_9_auto = make_completion_item(server, file, _241, scope)
      tmp_9_auto["kind"] = kinds.Variable
      return tmp_9_auto
    end
    collect_scope(scope, "manglings", _380_, result)
    if in_call_position_3f then
      local function _381_(_241)
        local tmp_9_auto = make_completion_item(server, file, _241, scope)
        tmp_9_auto["kind"] = kinds.Keyword
        return tmp_9_auto
      end
      collect_scope(scope, "macros", _381_, result)
      local function _382_(_241)
        local tmp_9_auto = make_completion_item(server, file, _241, scope)
        tmp_9_auto["kind"] = kinds.Operator
        return tmp_9_auto
      end
      collect_scope(scope, "specials", _382_, result)
    else
    end
    local tbl_19_auto = result
    for _, k in ipairs(file["allowed-globals"]) do
      local val_20_auto = make_completion_item(server, file, k, scope)
      table.insert(tbl_19_auto, val_20_auto)
    end
    return tbl_19_auto
  end
  local function field_completion(server, file, symbol, split)
    _G.assert((nil ~= split), "Missing argument split on src/fennel-ls\\handlers.fnl:157")
    _G.assert((nil ~= symbol), "Missing argument symbol on src/fennel-ls\\handlers.fnl:157")
    _G.assert((nil ~= file), "Missing argument file on src/fennel-ls\\handlers.fnl:157")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\handlers.fnl:157")
    local stack
    do
      local tbl_21_auto = {}
      local i_22_auto = 0
      for i = (#split - 1), 2, -1 do
        local val_23_auto = split[i]
        if (nil ~= val_23_auto) then
          i_22_auto = (i_22_auto + 1)
          tbl_21_auto[i_22_auto] = val_23_auto
        else
        end
      end
      stack = tbl_21_auto
    end
    local last_found_binding = {}
    local result = analyzer["search-main"](server, file, symbol, {["save-last-binding"] = last_found_binding}, {stack = stack})
    if ((_G.type(result) == "table") and (nil ~= result.definition) and (nil ~= result.file)) then
      local definition = result.definition
      local file0 = result.file
      local _385_, _386_ = definition, type(definition)
      if (true and (_386_ == "string")) then
        local _str = _385_
        local tbl_21_auto = {}
        local i_22_auto = 0
        for label, info in pairs(docs["get-global"](server, "string").fields) do
          local val_23_auto = formatter["completion-item-format"](label, info)
          if (nil ~= val_23_auto) then
            i_22_auto = (i_22_auto + 1)
            tbl_21_auto[i_22_auto] = val_23_auto
          else
          end
        end
        return tbl_21_auto
      elseif ((nil ~= _385_) and (_386_ == "table")) then
        local tbl = _385_
        local keys = {}
        do
          local tbl_19_auto = keys
          for label, _ in pairs(tbl) do
            local val_20_auto = label
            table.insert(tbl_19_auto, val_20_auto)
          end
        end
        local _389_
        do
          local t_388_ = last_found_binding
          if (nil ~= t_388_) then
            t_388_ = t_388_[1]
          else
          end
          if (nil ~= t_388_) then
            t_388_ = t_388_.fields
          else
          end
          _389_ = t_388_
        end
        if _389_ then
          local tbl_19_auto = keys
          for label, _ in pairs(last_found_binding[1].fields) do
            local val_20_auto = label
            table.insert(tbl_19_auto, val_20_auto)
          end
        else
        end
        local tbl_21_auto = {}
        local i_22_auto = 0
        for _, label in pairs(keys) do
          local val_23_auto
          if (type(label) == "string") then
            local _393_ = analyzer["search-ast"](server, file0, tbl, {label}, {})
            if (nil ~= _393_) then
              local def = _393_
              val_23_auto = formatter["completion-item-format"](label, def)
            else
              local _0 = _393_
              val_23_auto = {label = label, kind = kinds.Field}
            end
          else
            val_23_auto = nil
          end
          if (nil ~= val_23_auto) then
            i_22_auto = (i_22_auto + 1)
            tbl_21_auto[i_22_auto] = val_23_auto
          else
          end
        end
        return tbl_21_auto
      else
        return nil
      end
    elseif ((_G.type(result) == "table") and (nil ~= result.metadata) and (nil ~= result.fields)) then
      local metadata = result.metadata
      local fields = result.fields
      local _metadata = metadata
      local tbl_21_auto = {}
      local i_22_auto = 0
      for label, info in pairs(fields) do
        local val_23_auto = formatter["completion-item-format"](label, info)
        if (nil ~= val_23_auto) then
          i_22_auto = (i_22_auto + 1)
          tbl_21_auto[i_22_auto] = val_23_auto
        else
        end
      end
      return tbl_21_auto
    else
      local _ = result
      return nil
    end
  end
  requests["textDocument/completion"] = function(server, _send, _400_)
    local position = _400_["position"]
    local _arg_401_ = _400_["textDocument"]
    local uri = _arg_401_["uri"]
    _G.assert((nil ~= uri), "Missing argument uri on src/fennel-ls\\handlers.fnl:186")
    _G.assert((nil ~= position), "Missing argument position on src/fennel-ls\\handlers.fnl:186")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\handlers.fnl:186")
    local file = files["get-by-uri"](server, uri)
    local byte = utils["position->byte"](file.text, position, server["position-encoding"])
    local _3fsymbol, parents = analyzer["find-symbol"](file.ast, byte)
    local _402_
    if (nil ~= _3fsymbol) then
      _402_ = utils["multi-sym-split"](_3fsymbol)
    else
      _402_ = nil
    end
    if ((_402_ == nil) or ((_G.type(_402_) == "table") and true and (_402_[2] == nil))) then
      local input_range
      if _3fsymbol then
        input_range = message["multisym->range"](server, file, _3fsymbol, -1)
      else
        input_range = {start = position, ["end"] = position}
      end
      local _3fcompletions = scope_completion(server, file, byte, _3fsymbol, parents)
      if _3fcompletions then
        local _3fcompletions0
        local function _405_(_241)
          return _241.label
        end
        _3fcompletions0 = utils["uniq-by"](_3fcompletions, _405_)
        for _, completion in ipairs(_3fcompletions0) do
          completion.textEdit = {newText = completion.label, range = input_range}
        end
        return _3fcompletions0
      else
        return nil
      end
    elseif ((_G.type(_402_) == "table") and true and true) then
      local _a = _402_[1]
      local _b = _402_[2]
      local split = _402_
      local input_range = message["multisym->range"](server, file, _3fsymbol, -1)
      local _3fcompletions = field_completion(server, file, _3fsymbol, split)
      if _3fcompletions then
        if server.EGLOT_COMPLETION_QUIRK_MODE then
          local prefix = string.gsub(tostring(_3fsymbol), "[^.:]*$", "")
          for _, completion in ipairs(_3fcompletions) do
            completion.filterText = (prefix .. completion.label)
            completion.insertText = (prefix .. completion.label)
          end
        else
          for _, completion in ipairs(_3fcompletions) do
            completion.textEdit = {newText = completion.label, range = input_range}
          end
        end
      else
      end
      return _3fcompletions
    else
      return nil
    end
  end
  requests["textDocument/rename"] = function(server, _send, _410_)
    local position = _410_["position"]
    local _arg_411_ = _410_["textDocument"]
    local uri = _arg_411_["uri"]
    local new_name = _410_["newName"]
    _G.assert((nil ~= new_name), "Missing argument new-name on src/fennel-ls\\handlers.fnl:218")
    _G.assert((nil ~= uri), "Missing argument uri on src/fennel-ls\\handlers.fnl:218")
    _G.assert((nil ~= position), "Missing argument position on src/fennel-ls\\handlers.fnl:218")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\handlers.fnl:218")
    local file = files["get-by-uri"](server, uri)
    local byte = utils["position->byte"](file.text, position, server["position-encoding"])
    local function _412_(...)
      local _413_ = ...
      if (nil ~= _413_) then
        local symbol = _413_
        local function _414_(...)
          local _415_ = ...
          local and_416_ = (nil ~= _415_)
          if and_416_ then
            local definition = _415_
            and_416_ = (definition["referenced-by"] ~= nil)
          end
          if and_416_ then
            local definition = _415_
            local usages
            do
              local tbl_19_auto = {{range = message["multisym->range"](server, definition.file, definition.binding, 1), newText = new_name}}
              for _, _418_ in ipairs(definition["referenced-by"]) do
                local symbol0 = _418_["symbol"]
                local val_20_auto
                if (file.lexical[symbol0] and not rawequal(symbol0, definition.binding)) then
                  val_20_auto = {newText = new_name, range = message["multisym->range"](server, definition.file, symbol0, 1)}
                else
                  val_20_auto = nil
                end
                table.insert(tbl_19_auto, val_20_auto)
              end
              usages = tbl_19_auto
            end
            local function _420_(_241, _242)
              return (utils["position->byte"](definition.file.text, _241.range.start, "utf-8") > utils["position->byte"](definition.file.text, _242.range.start, "utf-8"))
            end
            table.sort(usages, _420_)
            local prev = {}
            local usages_dedup
            do
              local tbl_21_auto = {}
              local i_22_auto = 0
              for _, edit in ipairs(usages) do
                local val_23_auto
                if ((edit.range.start.line ~= prev.line) or (edit.range.start.character ~= prev.character)) then
                  prev = edit.range.start
                  val_23_auto = edit
                else
                  val_23_auto = nil
                end
                if (nil ~= val_23_auto) then
                  i_22_auto = (i_22_auto + 1)
                  tbl_21_auto[i_22_auto] = val_23_auto
                else
                end
              end
              usages_dedup = tbl_21_auto
            end
            return {changes = {[definition.file.uri] = usages_dedup}}
          else
            local _ = _415_
            return nil
          end
        end
        return _414_(analyzer["find-nearest-definition"](server, file, symbol, symbol.bytestart))
      else
        local _ = _413_
        return nil
      end
    end
    return _412_(analyzer["find-symbol"](file.ast, byte))
  end
  local function pos_3c_3d(pos_1, pos_2)
    return ((pos_1.line < pos_2.line) or ((pos_1.line == pos_2.line) and (pos_1.character <= pos_2.character)))
  end
  local function overlap_3f(range_1, range_2)
    return (pos_3c_3d(range_1.start, range_2["end"]) and pos_3c_3d(range_2.start, range_1["end"]))
  end
  requests["textDocument/codeAction"] = function(server, _send, _425_)
    local range = _425_["range"]
    local _arg_426_ = _425_["textDocument"]
    local uri = _arg_426_["uri"]
    _G.assert((nil ~= uri), "Missing argument uri on src/fennel-ls\\handlers.fnl:256")
    _G.assert((nil ~= range), "Missing argument range on src/fennel-ls\\handlers.fnl:256")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\handlers.fnl:256")
    local file = files["get-by-uri"](server, uri)
    local tbl_21_auto = {}
    local i_22_auto = 0
    for _, diagnostic in ipairs(file.diagnostics) do
      local val_23_auto
      if (overlap_3f(diagnostic.range, range) and diagnostic.quickfix) then
        val_23_auto = {title = diagnostic.codeDescription, edit = {changes = {[uri] = diagnostic.quickfix()}}}
      else
        val_23_auto = nil
      end
      if (nil ~= val_23_auto) then
        i_22_auto = (i_22_auto + 1)
        tbl_21_auto[i_22_auto] = val_23_auto
      else
      end
    end
    return tbl_21_auto
  end
  notifications["textDocument/didChange"] = function(server, send, _429_)
    local contentChanges = _429_["contentChanges"]
    local _arg_430_ = _429_["textDocument"]
    local uri = _arg_430_["uri"]
    _G.assert((nil ~= uri), "Missing argument uri on src/fennel-ls\\handlers.fnl:264")
    _G.assert((nil ~= contentChanges), "Missing argument contentChanges on src/fennel-ls\\handlers.fnl:264")
    _G.assert((nil ~= send), "Missing argument send on src/fennel-ls\\handlers.fnl:264")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\handlers.fnl:264")
    local file = files["get-by-uri"](server, uri)
    files["set-uri-contents"](server, uri, utils["apply-changes"](file.text, contentChanges, server["position-encoding"]))
    lint.check(server, file)
    return send(message.diagnostics(file))
  end
  notifications["textDocument/didOpen"] = function(server, send, _431_)
    local _arg_432_ = _431_["textDocument"]
    local text = _arg_432_["text"]
    local uri = _arg_432_["uri"]
    _G.assert((nil ~= uri), "Missing argument uri on src/fennel-ls\\handlers.fnl:270")
    _G.assert((nil ~= text), "Missing argument text on src/fennel-ls\\handlers.fnl:270")
    _G.assert((nil ~= send), "Missing argument send on src/fennel-ls\\handlers.fnl:270")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\handlers.fnl:270")
    local file = files["set-uri-contents"](server, uri, text)
    lint.check(server, file)
    send(message.diagnostics(file))
    file["open?"] = true
    return nil
  end
  notifications["textDocument/didSave"] = function(server, _send, _433_)
    local _arg_434_ = _433_["textDocument"]
    local uri = _arg_434_["uri"]
    _G.assert((nil ~= uri), "Missing argument uri on src/fennel-ls\\handlers.fnl:276")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\handlers.fnl:276")
    if utils.endswith(uri, "flsproject.fnl") then
      config.reload(server)
    else
    end
    fennel["macro-loaded"] = {}
    return nil
  end
  notifications["textDocument/didClose"] = function(server, _send, _436_)
    local _arg_437_ = _436_["textDocument"]
    local uri = _arg_437_["uri"]
    _G.assert((nil ~= uri), "Missing argument uri on src/fennel-ls\\handlers.fnl:283")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\handlers.fnl:283")
    local file = files["get-by-uri"](server, uri)
    file["open?"] = false
    fennel["macro-loaded"] = {}
    return files["flush-uri"](server, uri)
  end
  requests.shutdown = function(_server, _send)
    return nil
  end
  notifications.exit = function(_server)
    return os.exit(0)
  end
  return {requests = requests, notifications = notifications}
end
package.preload["fennel-ls.lint"] = package.preload["fennel-ls.lint"] or function(...)
  local _local_1_ = require("fennel")
  local sym_3f = _local_1_["sym?"]
  local list_3f = _local_1_["list?"]
  local view = _local_1_["view"]
  local analyzer = require("fennel-ls.analyzer")
  local message = require("fennel-ls.message")
  local utils = require("fennel-ls.utils")
  local _local_249_ = require("fennel.compiler")
  local _local_250_ = _local_249_["scopes"]
  local _local_251_ = _local_250_["global"]
  local specials = _local_251_["specials"]
  local dkjson = require("dkjson")
  local diagnostic_mt
  local function _252_(self, state)
    return dkjson.encode(self.self, state)
  end
  local function _253_(_241, _242)
    return _241.self[_242]
  end
  diagnostic_mt = {__tojson = _252_, __index = _253_}
  local function diagnostic(self, quickfix)
    return setmetatable({self = self, quickfix = quickfix}, diagnostic_mt)
  end
  local ops = {["+"] = 1, ["-"] = 1, ["*"] = 1, ["/"] = 1, ["//"] = 1, ["%"] = 1, ["^"] = 1, [">"] = 1, ["<"] = 1, [">="] = 1, ["<="] = 1, ["="] = 1, ["not="] = 1, [".."] = 1, ["."] = 1, ["and"] = 1, ["or"] = 1, band = 1, bor = 1, bxor = 1, bnot = 1, lshift = 1, rshift = 1}
  local function special_3f(item)
    return (sym_3f(item) and specials[tostring(item)] and item)
  end
  local function op_3f(item)
    return (sym_3f(item) and ops[tostring(item)] and item)
  end
  local function unused_definition(server, file, symbol, definition)
    _G.assert((nil ~= definition), "Missing argument definition on src/fennel-ls\\lint.fnl:28")
    _G.assert((nil ~= symbol), "Missing argument symbol on src/fennel-ls\\lint.fnl:28")
    _G.assert((nil ~= file), "Missing argument file on src/fennel-ls\\lint.fnl:28")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\lint.fnl:28")
    local or_254_ = ("_" == tostring(symbol):sub(1, 1))
    if not or_254_ then
      local reference = false
      for _, ref in ipairs(definition["referenced-by"]) do
        if reference then break end
        reference = ((ref["ref-type"] == "read") or (ref["ref-type"] == "mutate"))
      end
      or_254_ = reference
    end
    if not or_254_ then
      local function _255_()
        return {{range = message["ast->range"](server, file, symbol), newText = ("_" .. tostring(symbol))}}
      end
      return diagnostic({range = message["ast->range"](server, file, symbol), message = ("unused definition: " .. tostring(symbol)), severity = message.severity.WARN, code = 301, codeDescription = "unused-definition"}, _255_)
    else
      return nil
    end
  end
  local function module_field_helper(server, file, symbol, _3fast, stack)
    local opts = {}
    local item = analyzer["search-ast"](server, file, _3fast, stack, opts)
    if (not item and opts["searched-through-require-with-stack-size-1"] and not opts["searched-through-require-indeterminate"]) then
      return {range = message["ast->range"](server, file, symbol), message = ("unknown field: " .. tostring(symbol)), severity = message.severity.WARN, code = 302, codeDescription = "unknown-module-field"}
    else
      return nil
    end
  end
  local function unknown_module_field(server, file)
    _G.assert((nil ~= file), "Missing argument file on src/fennel-ls\\lint.fnl:58")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\lint.fnl:58")
    do
      local tbl_19_auto = file.diagnostics
      for symbol in pairs(file.references) do
        local val_20_auto
        if utils["multi-sym-split"](symbol)[2] then
          val_20_auto = module_field_helper(server, file, symbol, symbol, {})
        else
          val_20_auto = nil
        end
        table.insert(tbl_19_auto, val_20_auto)
      end
    end
    local tbl_19_auto = file.diagnostics
    for symbol, binding in pairs(file.definitions) do
      local val_20_auto
      if binding.keys then
        local function _259_()
          local tbl_21_auto = {}
          local i_22_auto = 0
          for i = #binding.keys, 1, -1 do
            local val_23_auto = binding.keys[i]
            if (nil ~= val_23_auto) then
              i_22_auto = (i_22_auto + 1)
              tbl_21_auto[i_22_auto] = val_23_auto
            else
            end
          end
          return tbl_21_auto
        end
        val_20_auto = module_field_helper(server, file, symbol, binding.definition, _259_())
      else
        val_20_auto = nil
      end
      table.insert(tbl_19_auto, val_20_auto)
    end
    return tbl_19_auto
  end
  local function unnecessary_method(server, file, colon, call)
    _G.assert((nil ~= call), "Missing argument call on src/fennel-ls\\lint.fnl:69")
    _G.assert((nil ~= colon), "Missing argument colon on src/fennel-ls\\lint.fnl:69")
    _G.assert((nil ~= file), "Missing argument file on src/fennel-ls\\lint.fnl:69")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\lint.fnl:69")
    if (sym_3f(colon, ":") and sym_3f(call[2]) and file.lexical[call]) then
      local method = call[3]
      if (("string" == type(method)) and not method:find("^[0-9]") and not method:find("[^!$%*+-/0-9<=>?A-Z\\^_a-z|\128-\255]")) then
        return {range = message["ast->range"](server, file, call), message = ("unnecessary : call: use (" .. tostring(call[2]) .. ":" .. method .. ")"), severity = message.severity.WARN, code = 303, codeDescription = "unnecessary-method"}
      else
        return nil
      end
    else
      return nil
    end
  end
  local function bad_unpack(server, file, op, call)
    _G.assert((nil ~= call), "Missing argument call on src/fennel-ls\\lint.fnl:84")
    _G.assert((nil ~= op), "Missing argument op on src/fennel-ls\\lint.fnl:84")
    _G.assert((nil ~= file), "Missing argument file on src/fennel-ls\\lint.fnl:84")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\lint.fnl:84")
    local last_item = call[#call]
    if (op_3f(op) and list_3f(last_item) and (sym_3f(last_item[1], "unpack") or sym_3f(last_item[1], "_G.unpack") or sym_3f(last_item[1], "table.unpack")) and file.lexical[last_item] and file.lexical[call]) then
      local _264_
      if sym_3f(op, "..") then
        local unpackme = view(last_item[2])
        _264_ = (" Use (table.concat " .. unpackme .. ") instead of (.. (unpack " .. unpackme .. "))")
      else
        _264_ = (" Use a loop when you have a dynamic number of arguments to (" .. tostring(op) .. ")")
      end
      local function _268_()
        if ((#call == 2) and (#call[2] == 2) and sym_3f(op, "..")) then
          local function _267_()
            return {{range = message["ast->range"](server, file, call), newText = ("(table.concat " .. view(call[2][2]) .. ")")}}
          end
          return _267_
        else
          return nil
        end
      end
      return diagnostic({range = message["ast->range"](server, file, last_item), message = ("faulty unpack call: " .. tostring(op) .. " isn't variadic at runtime." .. _264_), severity = message.severity.WARN, code = 304, codeDescription = "bad-unpack"}, _268_())
    else
      return nil
    end
  end
  local function var_never_set(server, file, symbol, definition)
    _G.assert((nil ~= definition), "Missing argument definition on src/fennel-ls\\lint.fnl:111")
    _G.assert((nil ~= symbol), "Missing argument symbol on src/fennel-ls\\lint.fnl:111")
    _G.assert((nil ~= file), "Missing argument file on src/fennel-ls\\lint.fnl:111")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\lint.fnl:111")
    if (definition["var?"] and not definition["var-set"] and file.lexical[symbol]) then
      return {range = message["ast->range"](server, file, symbol), message = ("var is never set: " .. tostring(symbol) .. " Consider using (local) instead of (var)"), severity = message.severity.WARN, code = 305, codeDescription = "var-never-set"}
    else
      return nil
    end
  end
  local op_identity_value = {["+"] = 0, ["*"] = 1, ["and"] = true, band = -1, bor = 0, [".."] = "", ["or"] = false}
  local function op_with_no_arguments(server, file, op, call)
    _G.assert((nil ~= call), "Missing argument call on src/fennel-ls\\lint.fnl:120")
    _G.assert((nil ~= op), "Missing argument op on src/fennel-ls\\lint.fnl:120")
    _G.assert((nil ~= file), "Missing argument file on src/fennel-ls\\lint.fnl:120")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\lint.fnl:120")
    local identity = op_identity_value[tostring(op)]
    if (op_3f(op) and not call[2] and file.lexical[call] and (nil ~= identity)) then
      local function _271_()
        return {{range = message["ast->range"](server, file, call), newText = view(identity)}}
      end
      return diagnostic({range = message["ast->range"](server, file, call), message = ("write " .. view(identity) .. " instead of (" .. tostring(op) .. ")"), severity = message.severity.WARN, code = 306, codeDescription = "op-with-no-arguments"}, _271_)
    else
      return nil
    end
  end
  local function multival_in_middle_of_call(server, file, fun, call, arg, index)
    _G.assert((nil ~= index), "Missing argument index on src/fennel-ls\\lint.fnl:136")
    _G.assert((nil ~= arg), "Missing argument arg on src/fennel-ls\\lint.fnl:136")
    _G.assert((nil ~= call), "Missing argument call on src/fennel-ls\\lint.fnl:136")
    _G.assert((nil ~= fun), "Missing argument fun on src/fennel-ls\\lint.fnl:136")
    _G.assert((nil ~= file), "Missing argument file on src/fennel-ls\\lint.fnl:136")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\lint.fnl:136")
    if (not (special_3f(fun) and not op_3f(fun)) and (index ~= #call) and list_3f(arg) and (sym_3f(arg[1], "values") or sym_3f(arg[1], "unpack") or sym_3f(arg[1], "_G.unpack") or sym_3f(arg[1], "table.unpack"))) then
      return {range = message["ast->range"](server, file, arg), message = ("bad " .. tostring(arg[1]) .. " call: only the first value of the multival will be used"), severity = message.severity.WARN, code = 307, codeDescription = "bad-unpack"}
    else
      return nil
    end
  end
  local function check(server, file)
    _G.assert((nil ~= file), "Missing argument file on src/fennel-ls\\lint.fnl:153")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\lint.fnl:153")
    local lints = server.configuration.lints
    local diagnostics = file.diagnostics
    for symbol, definition in pairs(file.definitions) do
      if lints["unused-definition"] then
        table.insert(diagnostics, unused_definition(server, file, symbol, definition))
      else
      end
      if lints["var-never-set"] then
        table.insert(diagnostics, var_never_set(server, file, symbol, definition))
      else
      end
    end
    for _276_ in pairs(file.calls) do
      local head = _276_[1]
      local call = _276_
      if head then
        if lints["bad-unpack"] then
          table.insert(diagnostics, bad_unpack(server, file, head, call))
        else
        end
        if lints["unnecessary-method"] then
          table.insert(diagnostics, unnecessary_method(server, file, head, call))
        else
        end
        if lints["op-with-no-arguments"] then
          table.insert(diagnostics, op_with_no_arguments(server, file, head, call))
        else
        end
        for index = 2, #call do
          local arg = call[index]
          if lints["multival-in-middle-of-call"] then
            table.insert(diagnostics, multival_in_middle_of_call(server, file, head, call, arg, index))
          else
          end
        end
      else
      end
    end
    if lints["unknown-module-field"] then
      return unknown_module_field(server, file)
    else
      return nil
    end
  end
  return {check = check}
end
package.preload["fennel-ls.analyzer"] = package.preload["fennel-ls.analyzer"] or function(...)
  local _local_2_ = require("fennel")
  local sym_3f = _local_2_["sym?"]
  local list_3f = _local_2_["list?"]
  local sequence_3f = _local_2_["sequence?"]
  local varg_3f = _local_2_["varg?"]
  local utils = require("fennel-ls.utils")
  local files = require("fennel-ls.files")
  local docs = require("fennel-ls.docs")
  local get_ast_info = utils["get-ast-info"]
  local search_multival = nil
  local function stack_add_keys_21(stack, _3fkeys)
    _G.assert((nil ~= stack), "Missing argument stack on src/fennel-ls\\analyzer.fnl:50")
    if _3fkeys then
      local tbl_19_auto = stack
      for i = #_3fkeys, 1, -1 do
        local val_20_auto = _3fkeys[i]
        table.insert(tbl_19_auto, val_20_auto)
      end
    else
    end
    return stack
  end
  local function stack_add_split_21(stack, split)
    _G.assert((nil ~= split), "Missing argument split on src/fennel-ls\\analyzer.fnl:57")
    _G.assert((nil ~= stack), "Missing argument stack on src/fennel-ls\\analyzer.fnl:57")
    do
      local tbl_19_auto = stack
      for i = #split, 2, -1 do
        local val_20_auto = split[i]
        table.insert(tbl_19_auto, val_20_auto)
      end
    end
    return stack
  end
  local function stack_add_multisym_21(stack, symbol)
    _G.assert((nil ~= symbol), "Missing argument symbol on src/fennel-ls\\analyzer.fnl:63")
    _G.assert((nil ~= stack), "Missing argument stack on src/fennel-ls\\analyzer.fnl:63")
    return stack_add_split_21(stack, utils["multi-sym-split"](symbol))
  end
  local function search_document(server, document, stack, opts)
    _G.assert((nil ~= opts), "Missing argument opts on src/fennel-ls\\analyzer.fnl:67")
    _G.assert((nil ~= stack), "Missing argument stack on src/fennel-ls\\analyzer.fnl:67")
    _G.assert((nil ~= document), "Missing argument document on src/fennel-ls\\analyzer.fnl:67")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\analyzer.fnl:67")
    local function _190_()
      local t_189_ = document
      if (nil ~= t_189_) then
        t_189_ = t_189_.binding
      else
      end
      return t_189_
    end
    if (tostring(_190_()) ~= "_G") then
      opts["searched-through-require-with-stack-size-1"] = true
    else
    end
    if (0 == #stack) then
      return document
    elseif (document.fields and document.fields[stack[#stack]]) then
      return search_document(server, document.fields[table.remove(stack)], stack, opts)
    elseif not document.fields then
      opts["searched-through-require-indeterminate"] = true
      return nil
    else
      return nil
    end
  end
  local function search_val(server, file, _3fast, stack, opts)
    _G.assert((nil ~= opts), "Missing argument opts on src/fennel-ls\\analyzer.fnl:79")
    _G.assert((nil ~= stack), "Missing argument stack on src/fennel-ls\\analyzer.fnl:79")
    _G.assert((nil ~= file), "Missing argument file on src/fennel-ls\\analyzer.fnl:79")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\analyzer.fnl:79")
    return search_multival(server, file, _3fast, stack, 1, opts)
  end
  local function search_assignment(server, file, assignment, stack, opts)
    _G.assert((nil ~= opts), "Missing argument opts on src/fennel-ls\\analyzer.fnl:83")
    _G.assert((nil ~= stack), "Missing argument stack on src/fennel-ls\\analyzer.fnl:83")
    _G.assert((nil ~= assignment), "Missing argument assignment on src/fennel-ls\\analyzer.fnl:83")
    _G.assert((nil ~= file), "Missing argument file on src/fennel-ls\\analyzer.fnl:83")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\analyzer.fnl:83")
    local _let_194_ = assignment["target"]
    local _ = _let_194_["binding"]
    local _3fdefinition = _let_194_["definition"]
    local _3fkeys = _let_194_["keys"]
    local _3fmultival = _let_194_["multival"]
    local _3ffields = _let_194_["fields"]
    if ((0 == #stack) and opts["save-last-binding"]) then
      opts["save-last-binding"][1] = assignment.target
    else
    end
    if ((0 == #stack) and opts["stop-early?"]) then
      return assignment.target
    else
      local and_196_ = (0 ~= #stack)
      if and_196_ then
        local t_197_ = _3ffields
        if (nil ~= t_197_) then
          t_197_ = t_197_[stack[#stack]]
        else
        end
        and_196_ = t_197_
      end
      if and_196_ then
        return search_assignment(server, file, {target = _3ffields[table.remove(stack)]}, stack, opts)
      else
        return search_multival(server, file, _3fdefinition, stack_add_keys_21(stack, _3fkeys), (_3fmultival or 1), opts)
      end
    end
  end
  local function search_reference(server, file, ref, stack, opts)
    _G.assert((nil ~= opts), "Missing argument opts on src/fennel-ls\\analyzer.fnl:99")
    _G.assert((nil ~= stack), "Missing argument stack on src/fennel-ls\\analyzer.fnl:99")
    _G.assert((nil ~= ref), "Missing argument ref on src/fennel-ls\\analyzer.fnl:99")
    _G.assert((nil ~= file), "Missing argument file on src/fennel-ls\\analyzer.fnl:99")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\analyzer.fnl:99")
    if ref.target.metadata then
      return search_document(server, ref.target, stack, opts)
    elseif ref.target.binding then
      return search_assignment(server, file, ref, stack, opts)
    else
      return nil
    end
  end
  local function search_symbol(server, file, symbol, stack, opts)
    _G.assert((nil ~= opts), "Missing argument opts on src/fennel-ls\\analyzer.fnl:105")
    _G.assert((nil ~= stack), "Missing argument stack on src/fennel-ls\\analyzer.fnl:105")
    _G.assert((nil ~= symbol), "Missing argument symbol on src/fennel-ls\\analyzer.fnl:105")
    _G.assert((nil ~= file), "Missing argument file on src/fennel-ls\\analyzer.fnl:105")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\analyzer.fnl:105")
    if (tostring(symbol) == "nil") then
      if (0 == #stack) then
        return {definition = symbol, file = file}
      else
        return nil
      end
    else
      if file.references[symbol] then
        return search_reference(server, file, file.references[symbol], stack_add_multisym_21(stack, symbol), opts)
      else
        return nil
      end
    end
  end
  local function search_table(server, file, tbl, stack, opts)
    _G.assert((nil ~= opts), "Missing argument opts on src/fennel-ls\\analyzer.fnl:113")
    _G.assert((nil ~= stack), "Missing argument stack on src/fennel-ls\\analyzer.fnl:113")
    _G.assert((nil ~= tbl), "Missing argument tbl on src/fennel-ls\\analyzer.fnl:113")
    _G.assert((nil ~= file), "Missing argument file on src/fennel-ls\\analyzer.fnl:113")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\analyzer.fnl:113")
    if tbl[stack[#stack]] then
      return search_val(server, file, tbl[table.remove(stack)], stack, opts)
    else
      return nil
    end
  end
  local function search_list(server, file, call, stack, multival, opts)
    _G.assert((nil ~= opts), "Missing argument opts on src/fennel-ls\\analyzer.fnl:118")
    _G.assert((nil ~= multival), "Missing argument multival on src/fennel-ls\\analyzer.fnl:118")
    _G.assert((nil ~= stack), "Missing argument stack on src/fennel-ls\\analyzer.fnl:118")
    _G.assert((nil ~= call), "Missing argument call on src/fennel-ls\\analyzer.fnl:118")
    _G.assert((nil ~= file), "Missing argument file on src/fennel-ls\\analyzer.fnl:118")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\analyzer.fnl:118")
    local head = call[1]
    if sym_3f(head) then
      local _205_ = tostring(head)
      if ((_205_ == "do") or (_205_ == "let")) then
        return search_multival(server, file, call[#call], stack, multival, opts)
      elseif (_205_ == "values") then
        local len = (#call - 1)
        if (multival < len) then
          return search_val(server, file, call[(1 + multival)], stack, opts)
        else
          return search_multival(server, file, call[(len + 1)], stack, (multival + ( - len) + 1), opts)
        end
      elseif ((_205_ == "require") or (_205_ == "include")) then
        local mod = call[2]
        if (multival == 1) then
          if ("string" == type(mod)) then
            local newfile = files["get-by-module"](server, mod)
            if newfile then
              local newitem = newfile.ast[#newfile.ast]
              if (#stack == 1) then
                opts["searched-through-require-with-stack-size-1"] = true
              else
              end
              return search_val(server, newfile, newitem, stack, opts)
            else
              return nil
            end
          else
            return nil
          end
        else
          return nil
        end
      elseif (_205_ == ".") then
        if (multival == 1) then
          local _ = call[1]
          local rest = (function (t, k, e) local mt = getmetatable(t) if 'table' == type(mt) and mt.__fennelrest then return mt.__fennelrest(t, k) elseif e then local rest = {} for k, v in pairs(t) do if not e[k] then rest[k] = v end end return rest else return {(table.unpack or unpack)(t, k)} end end)(call, 2)
          return search_val(server, file, call[2], stack_add_split_21(stack, rest), opts)
        else
          return nil
        end
      elseif (_205_ == "setmetatable") then
        return search_val(server, file, call[2], stack, opts)
      elseif ((_205_ == "fn") or (_205_ == "lambda") or (_205_ == "\206\187")) then
        if ((multival == 1) and (0 == #stack)) then
          return {definition = call, file = file}
        else
          return nil
        end
      else
        local _ = _205_
        if ((multival == 1) and (0 == #stack)) then
          return {definition = call, file = file}
        else
          return nil
        end
      end
    else
      return nil
    end
  end
  local function _216_(server, file, _3fast, stack, multival, opts)
    _G.assert((nil ~= opts), "Missing argument opts on src/fennel-ls\\analyzer.fnl:157")
    _G.assert((nil ~= multival), "Missing argument multival on src/fennel-ls\\analyzer.fnl:157")
    _G.assert((nil ~= stack), "Missing argument stack on src/fennel-ls\\analyzer.fnl:157")
    _G.assert((nil ~= file), "Missing argument file on src/fennel-ls\\analyzer.fnl:157")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\analyzer.fnl:157")
    local ast = _3fast
    if list_3f(ast) then
      return search_list(server, file, ast, stack, multival, opts)
    elseif varg_3f(ast) then
      return nil
    elseif (1 == multival) then
      if sym_3f(ast) then
        return search_symbol(server, file, ast, stack, opts)
      elseif (0 == #stack) then
        return {definition = ast, file = file}
      elseif ("table" == type(ast)) then
        return search_table(server, file, ast, stack, opts)
      elseif ("string" == type(ast)) then
        return search_document(server, docs["get-global"](server, "string"), stack, opts)
      else
        return nil
      end
    else
      return nil
    end
  end
  search_multival = _216_
  local _local_219_ = require("fennel.compiler")
  local METADATA = _local_219_["metadata"]
  local function search_main(server, file, symbol, opts, initialization_opts)
    _G.assert((nil ~= initialization_opts), "Missing argument initialization-opts on src/fennel-ls\\analyzer.fnl:171")
    _G.assert((nil ~= opts), "Missing argument opts on src/fennel-ls\\analyzer.fnl:171")
    _G.assert((nil ~= symbol), "Missing argument symbol on src/fennel-ls\\analyzer.fnl:171")
    _G.assert((nil ~= file), "Missing argument file on src/fennel-ls\\analyzer.fnl:171")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\analyzer.fnl:171")
    assert((type(initialization_opts) == "table"))
    if sym_3f(symbol) then
      local stack
      if initialization_opts.stack then
        stack = initialization_opts.stack
      else
        local _3fbyte = initialization_opts.byte
        local split
        local function _220_()
          if _3fbyte then
            return (_3fbyte - symbol.bytestart)
          else
            return nil
          end
        end
        split = utils["multi-sym-split"](symbol, _220_())
        stack = stack_add_split_21({}, split)
      end
      local _222_ = docs["get-builtin"](server, utils["multi-sym-base"](symbol))
      if (nil ~= _222_) then
        local document = _222_
        return search_document(server, document, stack, opts)
      else
        local _ = _222_
        local _223_ = file.references[symbol]
        if (nil ~= _223_) then
          local ref = _223_
          return search_reference(server, file, ref, stack, opts)
        else
          local _0 = _223_
          local _224_ = file.definitions[symbol]
          if (nil ~= _224_) then
            local def = _224_
            return search_multival(server, file, def.definition, stack_add_keys_21(stack, def.keys), (def.multival or 1), opts)
          else
            local _1 = _224_
            local _225_ = file["macro-refs"][symbol]
            if (nil ~= _225_) then
              local ref = _225_
              return {binding = symbol, metadata = METADATA[ref]}
            else
              return nil
            end
          end
        end
      end
    else
      return nil
    end
  end
  local function find_local_definition(file, name, _3fscope)
    _G.assert((nil ~= name), "Missing argument name on src/fennel-ls\\analyzer.fnl:194")
    _G.assert((nil ~= file), "Missing argument file on src/fennel-ls\\analyzer.fnl:194")
    if _3fscope then
      local _231_ = file["definitions-by-scope"][_3fscope][name]
      if (nil ~= _231_) then
        local definition = _231_
        return definition
      else
        local _ = _231_
        return find_local_definition(file, name, _3fscope.parent)
      end
    else
      return nil
    end
  end
  local function search_name_and_scope(server, file, name, scope, _3fopts)
    _G.assert((nil ~= scope), "Missing argument scope on src/fennel-ls\\analyzer.fnl:200")
    _G.assert((nil ~= name), "Missing argument name on src/fennel-ls\\analyzer.fnl:200")
    _G.assert((nil ~= file), "Missing argument file on src/fennel-ls\\analyzer.fnl:200")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\analyzer.fnl:200")
    assert((type(name) == "string"))
    local split = utils["multi-sym-split"](name)
    local stack = stack_add_split_21({}, split)
    local opts = (_3fopts or {})
    local _234_ = docs["get-builtin"](server, split[1])
    if (nil ~= _234_) then
      local metadata = _234_
      return search_document(server, metadata, stack, opts)
    else
      local _ = _234_
      local _235_ = docs["get-global"](server, split[1])
      if (nil ~= _235_) then
        local metadata = _235_
        return search_document(server, metadata, stack, opts)
      else
        local _0 = _235_
        local _236_ = find_local_definition(file, name, scope)
        if (nil ~= _236_) then
          local def = _236_
          return search_val(server, file, def.definition, stack_add_keys_21(stack, def.keys), opts)
        else
          return nil
        end
      end
    end
  end
  local function _past_3f(_3fast, byte)
    _G.assert((nil ~= byte), "Missing argument byte on src/fennel-ls\\analyzer.fnl:213")
    return ((type(_3fast) == "table") and get_ast_info(_3fast, "bytestart") and (byte < get_ast_info(_3fast, "bytestart")) and false)
  end
  local function contains_3f(_3fast, byte)
    _G.assert((nil ~= byte), "Missing argument byte on src/fennel-ls\\analyzer.fnl:220")
    return ((type(_3fast) == "table") and get_ast_info(_3fast, "bytestart") and get_ast_info(_3fast, "byteend") and ((get_ast_info(_3fast, "bytestart") <= byte) and (byte <= (1 + utils["get-ast-info"](_3fast, "byteend")))))
  end
  local function _does_not_contain_3f(_3fast, byte)
    _G.assert((nil ~= byte), "Missing argument byte on src/fennel-ls\\analyzer.fnl:229")
    return ((type(_3fast) == "table") and get_ast_info(_3fast, "bytestart") and get_ast_info(_3fast, "byteend") and not ((get_ast_info(_3fast, "bytestart") <= byte) and (byte <= (1 + get_ast_info(_3fast, "byteend")))))
  end
  local function find_symbol(ast, byte)
    _G.assert((nil ~= byte), "Missing argument byte on src/fennel-ls\\analyzer.fnl:239")
    _G.assert((nil ~= ast), "Missing argument ast on src/fennel-ls\\analyzer.fnl:239")
    local parents = {ast}
    local function recurse(ast0)
      _G.assert((nil ~= ast0), "Missing argument ast on src/fennel-ls\\analyzer.fnl:242")
      if sym_3f(ast0) then
        return ast0, parents
      else
        table.insert(parents, ast0)
        if (sequence_3f(ast0) or list_3f(ast0)) then
          local result, _parent = nil
          for _, child in ipairs(ast0) do
            if result then break end
            if contains_3f(child, byte) then
              result, _parent = recurse(child, byte)
            else
              result, _parent = nil
            end
          end
          return result, _parent
        elseif (not sym_3f(ast0) and not varg_3f(ast0)) then
          local result, _parent = nil
          for key, value in pairs(ast0) do
            if result then break end
            if contains_3f(key, byte) then
              result, _parent = recurse(key, byte)
            elseif contains_3f(value, byte) then
              result, _parent = recurse(value, byte)
            else
              result, _parent = nil
            end
          end
          return result, _parent
        else
          return nil
        end
      end
    end
    local _244_
    do
      local result = nil
      for _, top_level_form in ipairs(ast) do
        if result then break end
        if contains_3f(top_level_form, byte) then
          result = recurse(top_level_form, byte)
        else
          result = nil
        end
      end
      _244_ = result
    end
    local function _246_()
      local tbl_21_auto = {}
      local i_22_auto = 0
      for i = 1, #parents do
        local val_23_auto = parents[(#parents - i - -1)]
        if (nil ~= val_23_auto) then
          i_22_auto = (i_22_auto + 1)
          tbl_21_auto[i_22_auto] = val_23_auto
        else
        end
      end
      return tbl_21_auto
    end
    return _244_, _246_()
  end
  local function find_nearest_definition(server, file, symbol, _3fbyte)
    _G.assert((nil ~= symbol), "Missing argument symbol on src/fennel-ls\\analyzer.fnl:270")
    _G.assert((nil ~= file), "Missing argument file on src/fennel-ls\\analyzer.fnl:270")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\analyzer.fnl:270")
    if file.definitions[symbol] then
      return file.definitions[symbol]
    else
      return search_main(server, file, symbol, {["stop-early?"] = true}, {byte = _3fbyte})
    end
  end
  return {["find-symbol"] = find_symbol, ["find-nearest-definition"] = find_nearest_definition, ["search-main"] = search_main, ["search-name-and-scope"] = search_name_and_scope, ["search-ast"] = search_val}
end
package.preload["fennel-ls.utils"] = package.preload["fennel-ls.utils"] or function(...)
  local fennel = require("fennel")
  local function next_line(str, _3ffrom)
    _G.assert((nil ~= str), "Missing argument str on src/fennel-ls\\utils.fnl:8")
    local from = (_3ffrom or 1)
    local _3_ = str:find("[\13\n]", from)
    if (nil ~= _3_) then
      local i = _3_
      return (i + #str:match("\13?\n?", i))
    elseif (_3_ == nil) then
      return nil
    else
      return nil
    end
  end
  local function next_lines(str, nlines, _3ffrom)
    _G.assert((nil ~= nlines), "Missing argument nlines on src/fennel-ls\\utils.fnl:15")
    _G.assert((nil ~= str), "Missing argument str on src/fennel-ls\\utils.fnl:15")
    local from = (_3ffrom or 1)
    for _ = 1, nlines do
      from = next_line(str, from)
    end
    return from
  end
  local function utf(byte)
    if ((0 <= byte) and (byte <= 128)) then
      return 1, 1
    elseif ((192 <= byte) and (byte <= 223)) then
      return 2, 1
    elseif ((224 <= byte) and (byte <= 239)) then
      return 3, 1
    elseif ((240 <= byte) and (byte <= 247)) then
      return 4, 2
    else
      return error("utf8-error")
    end
  end
  local function byte__3eunit16(str, _3fbyte)
    local unit8 = math.min(#str, _3fbyte)
    local o8 = 0
    local o16 = 0
    while (o8 < unit8) do
      local a8, a16 = utf(str:byte((1 + o8)))
      o8 = (o8 + a8)
      o16 = (o16 + a16)
    end
    if (o8 == unit8) then
      return o16
    else
      return error("utf8-error")
    end
  end
  local function unit16__3ebyte(str, unit16)
    local o8 = 0
    local o16 = 0
    while (o16 < unit16) do
      local a8, a16 = utf(str:byte((1 + o8)))
      o8 = (o8 + a8)
      o16 = (o16 + a16)
    end
    if (o16 == unit16) then
      return o8
    else
      return error("utf8-error")
    end
  end
  local function pos__3eposition(str, line, character, encoding)
    _G.assert((nil ~= encoding), "Missing argument encoding on src/fennel-ls\\utils.fnl:61")
    _G.assert((nil ~= character), "Missing argument character on src/fennel-ls\\utils.fnl:61")
    _G.assert((nil ~= line), "Missing argument line on src/fennel-ls\\utils.fnl:61")
    _G.assert((nil ~= str), "Missing argument str on src/fennel-ls\\utils.fnl:61")
    if (encoding == "utf-8") then
      return {line = line, character = character}
    elseif (encoding == "utf-16") then
      local pos = next_lines(str, line)
      return {line = line, character = byte__3eunit16(str:sub(pos), character)}
    else
      local _ = encoding
      return error(("unknown encoding: " .. encoding))
    end
  end
  local function byte__3eposition(str, byte, encoding)
    _G.assert((nil ~= encoding), "Missing argument encoding on src/fennel-ls\\utils.fnl:69")
    _G.assert((nil ~= byte), "Missing argument byte on src/fennel-ls\\utils.fnl:69")
    _G.assert((nil ~= str), "Missing argument str on src/fennel-ls\\utils.fnl:69")
    local line = 0
    local pos = 1
    while true do
      local _9_
      do
        local npos = next_line(str, pos)
        if (npos and (npos <= byte)) then
          pos = npos
          line = (line + 1)
          _9_ = true
        else
          _9_ = nil
        end
      end
      if not _9_ then break end
    end
    if (encoding == "utf-8") then
      return {line = line, character = (byte - pos)}
    elseif (encoding == "utf-16") then
      return {line = line, character = byte__3eunit16(str:sub(pos), (byte - pos))}
    else
      local _ = encoding
      return error(("unknown encoding: " .. encoding))
    end
  end
  local function position__3ebyte(str, _12_, encoding)
    local line = _12_["line"]
    local character = _12_["character"]
    _G.assert((nil ~= encoding), "Missing argument encoding on src/fennel-ls\\utils.fnl:83")
    _G.assert((nil ~= character), "Missing argument character on src/fennel-ls\\utils.fnl:83")
    _G.assert((nil ~= line), "Missing argument line on src/fennel-ls\\utils.fnl:83")
    _G.assert((nil ~= str), "Missing argument str on src/fennel-ls\\utils.fnl:83")
    local pos = next_lines(str, line)
    assert(pos, "bad-pos")
    if (encoding == "utf-8") then
      return (pos + character)
    elseif (encoding == "utf-16") then
      return (pos + unit16__3ebyte(str:sub(pos), character))
    else
      local _ = encoding
      return error(("unknown encoding: " .. encoding))
    end
  end
  local function startswith(str, pre)
    _G.assert((nil ~= pre), "Missing argument pre on src/fennel-ls\\utils.fnl:92")
    _G.assert((nil ~= str), "Missing argument str on src/fennel-ls\\utils.fnl:92")
    local len = #pre
    return (str:sub(1, len) == pre)
  end
  local function endswith(str, post)
    _G.assert((nil ~= post), "Missing argument post on src/fennel-ls\\utils.fnl:96")
    _G.assert((nil ~= str), "Missing argument str on src/fennel-ls\\utils.fnl:96")
    local len = #post
    return ((post == "") or (post == str:sub(( - len))))
  end
  local function uri__3epath(uri)
    _G.assert((nil ~= uri), "Missing argument uri on src/fennel-ls\\utils.fnl:101")
    local prefix = "file://"
    if not startswith(uri, prefix) then
      error(("encountered URI " .. fennel.view(uri) .. " that does not start with \"file://\""))
    else
    end
    return string.sub(uri, (#prefix + 1))
  end
  local function path__3euri(path)
    _G.assert((nil ~= path), "Missing argument path on src/fennel-ls\\utils.fnl:108")
    return ("file://" .. path)
  end
  local function replace(text, start_position, end_position, replacement, encoding)
    _G.assert((nil ~= encoding), "Missing argument encoding on src/fennel-ls\\utils.fnl:112")
    _G.assert((nil ~= replacement), "Missing argument replacement on src/fennel-ls\\utils.fnl:112")
    _G.assert((nil ~= end_position), "Missing argument end-position on src/fennel-ls\\utils.fnl:112")
    _G.assert((nil ~= start_position), "Missing argument start-position on src/fennel-ls\\utils.fnl:112")
    _G.assert((nil ~= text), "Missing argument text on src/fennel-ls\\utils.fnl:112")
    local start = position__3ebyte(text, start_position, encoding)
    local _end = position__3ebyte(text, end_position, encoding)
    return (text:sub(1, (start - 1)) .. replacement .. text:sub(_end))
  end
  local function apply_changes(initial_text, changes, encoding)
    _G.assert((nil ~= encoding), "Missing argument encoding on src/fennel-ls\\utils.fnl:121")
    _G.assert((nil ~= changes), "Missing argument changes on src/fennel-ls\\utils.fnl:121")
    _G.assert((nil ~= initial_text), "Missing argument initial-text on src/fennel-ls\\utils.fnl:121")
    local contents = initial_text
    for _, change in ipairs(changes) do
      if ((_G.type(change) == "table") and ((_G.type(change.range) == "table") and (nil ~= change.range.start) and (nil ~= change.range["end"])) and (nil ~= change.text)) then
        local start = change.range.start
        local _end = change.range["end"]
        local text = change.text
        contents = replace(contents, start, _end, text, encoding)
      elseif ((_G.type(change) == "table") and (nil ~= change.text)) then
        local text = change.text
        contents = text
      else
        contents = nil
      end
    end
    return contents
  end
  local function apply_edits(initial_text, edits, encoding)
    _G.assert((nil ~= encoding), "Missing argument encoding on src/fennel-ls\\utils.fnl:134")
    _G.assert((nil ~= edits), "Missing argument edits on src/fennel-ls\\utils.fnl:134")
    _G.assert((nil ~= initial_text), "Missing argument initial-text on src/fennel-ls\\utils.fnl:134")
    local contents = initial_text
    for _, edit in ipairs(edits) do
      if ((_G.type(edit) == "table") and ((_G.type(edit.range) == "table") and (nil ~= edit.range.start) and (nil ~= edit.range["end"])) and (nil ~= edit.newText)) then
        local start = edit.range.start
        local _end = edit.range["end"]
        local newText = edit.newText
        contents = replace(contents, start, _end, newText, encoding)
      else
        contents = nil
      end
    end
    return contents
  end
  local function get_ast_info(ast, info)
    _G.assert((nil ~= info), "Missing argument info on src/fennel-ls\\utils.fnl:146")
    _G.assert((nil ~= ast), "Missing argument ast on src/fennel-ls\\utils.fnl:146")
    if ("number" == type(ast)) then
      return nil
    else
      local _18_
      do
        local t_17_ = getmetatable(ast)
        if (nil ~= t_17_) then
          t_17_ = t_17_[info]
        else
        end
        _18_ = t_17_
      end
      local or_20_ = _18_
      if not or_20_ then
        local t_21_ = ast
        if (nil ~= t_21_) then
          t_21_ = t_21_[info]
        else
        end
        or_20_ = t_21_
      end
      return or_20_
    end
  end
  local function multi_sym_split(symbol, _3foffset)
    local symbol0 = tostring(symbol)
    if ((symbol0 == ".") or (symbol0 == "..") or (symbol0 == "...") or (symbol0 == ":") or (symbol0 == "?.")) then
      return {symbol0}
    else
      local offset = (_3foffset or #symbol0)
      local next_separator = (symbol0:find(".[.:]", offset) or #symbol0)
      local symbol1 = symbol0:sub(1, next_separator)
      local tbl_21_auto = {}
      local i_22_auto = 0
      for word in (symbol1 .. "."):gmatch("(.-)[.:]") do
        local val_23_auto = word
        if (nil ~= val_23_auto) then
          i_22_auto = (i_22_auto + 1)
          tbl_21_auto[i_22_auto] = val_23_auto
        else
        end
      end
      return tbl_21_auto
    end
  end
  local function multi_sym_base(symbol)
    local symbol0 = tostring(symbol)
    if ((symbol0 == ".") or (symbol0 == "..") or (symbol0 == "...") or (symbol0 == ":") or (symbol0 == "?.")) then
      return symbol0
    else
      return (symbol0:match("[^.:]*"))
    end
  end
  local function uniq_by(list, key_fn)
    _G.assert((nil ~= key_fn), "Missing argument key-fn on src/fennel-ls\\utils.fnl:179")
    _G.assert((nil ~= list), "Missing argument list on src/fennel-ls\\utils.fnl:179")
    local seen = {}
    local tbl_21_auto = {}
    local i_22_auto = 0
    for _, new_item in ipairs(list) do
      local val_23_auto
      do
        local key = key_fn(new_item)
        if not seen[key] then
          seen[key] = true
          val_23_auto = new_item
        else
          val_23_auto = nil
        end
      end
      if (nil ~= val_23_auto) then
        i_22_auto = (i_22_auto + 1)
        tbl_21_auto[i_22_auto] = val_23_auto
      else
      end
    end
    return tbl_21_auto
  end
  local path_sep = package.config:sub(1, 1)
  local function absolute_path_3f(path)
    _G.assert((nil ~= path), "Missing argument path on src/fennel-ls\\utils.fnl:189")
    return (path:sub(2, 3):match(":\\") or (path:sub(1, 1) == "/"))
  end
  local function path_join(path, suffix)
    _G.assert((nil ~= suffix), "Missing argument suffix on src/fennel-ls\\utils.fnl:198")
    _G.assert((nil ~= path), "Missing argument path on src/fennel-ls\\utils.fnl:198")
    return ((path .. path_sep .. suffix):gsub("%.\\", ""):gsub("\\+", "\\"):gsub("%./", ""):gsub("/+", "/"))
  end
  return {["uri->path"] = uri__3epath, ["path->uri"] = path__3euri, ["pos->position"] = pos__3eposition, ["byte->position"] = byte__3eposition, ["position->byte"] = position__3ebyte, ["apply-changes"] = apply_changes, ["apply-edits"] = apply_edits, ["multi-sym-split"] = multi_sym_split, ["multi-sym-base"] = multi_sym_base, ["get-ast-info"] = get_ast_info, ["uniq-by"] = uniq_by, ["absolute-path?"] = absolute_path_3f, ["path-join"] = path_join, ["path-sep"] = path_sep, endswith = endswith}
end
package.preload["fennel-ls.files"] = package.preload["fennel-ls.files"] or function(...)
  local searcher = require("fennel-ls.searcher")
  local utils = require("fennel-ls.utils")
  local _local_168_ = require("fennel-ls.compiler")
  local compile = _local_168_["compile"]
  local function read_file(server, uri)
    _G.assert((nil ~= uri), "Missing argument uri on src/fennel-ls\\files.fnl:13")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\files.fnl:13")
    local _169_
    do
      local t_170_ = server.preload
      if (nil ~= t_170_) then
        t_170_ = t_170_[uri]
      else
      end
      _169_ = t_170_
    end
    if (nil ~= _169_) then
      local preload = _169_
      return {uri = uri, text = preload}
    else
      local _ = _169_
      local _172_ = io.open(utils["uri->path"](uri), "r")
      if (nil ~= _172_) then
        local file = _172_
        local text = file:read("*a")
        file:close()
        return {uri = uri, text = text}
      else
        return nil
      end
    end
  end
  local function get_by_uri(server, uri)
    _G.assert((nil ~= uri), "Missing argument uri on src/fennel-ls\\files.fnl:21")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\files.fnl:21")
    local or_175_ = server.files[uri]
    if not or_175_ then
      local _176_ = read_file(server, uri)
      if (nil ~= _176_) then
        local file = _176_
        compile(server, file)
        server.files[uri] = file
        or_175_ = file
      else
        or_175_ = nil
      end
    end
    return or_175_
  end
  local function get_by_module(server, module)
    _G.assert((nil ~= module), "Missing argument module on src/fennel-ls\\files.fnl:29")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\files.fnl:29")
    local _180_ = server.modules[module]
    if (nil ~= _180_) then
      local uri = _180_
      local or_181_ = get_by_uri(server, uri)
      if not or_181_ then
        server.modules[module] = nil
        or_181_ = get_by_module(server, module)
      end
      return or_181_
    elseif (_180_ == nil) then
      local _182_ = searcher.lookup(server, module)
      if (nil ~= _182_) then
        local uri = _182_
        server.modules[module] = uri
        return get_by_uri(server, uri)
      else
        return nil
      end
    else
      return nil
    end
  end
  local function set_uri_contents(server, uri, text)
    _G.assert((nil ~= text), "Missing argument text on src/fennel-ls\\files.fnl:44")
    _G.assert((nil ~= uri), "Missing argument uri on src/fennel-ls\\files.fnl:44")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\files.fnl:44")
    local _185_ = server.files[uri]
    if (nil ~= _185_) then
      local file = _185_
      if (text ~= file.text) then
        file.text = text
        compile(server, file)
      else
      end
      return file
    elseif (_185_ == nil) then
      local file = {uri = uri, text = text}
      server.files[uri] = file
      compile(server, file)
      return file
    else
      return nil
    end
  end
  local function flush_uri(server, uri)
    _G.assert((nil ~= uri), "Missing argument uri on src/fennel-ls\\files.fnl:61")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\files.fnl:61")
    server.files[uri] = nil
    return nil
  end
  return {["flush-uri"] = flush_uri, ["get-by-module"] = get_by_module, ["get-by-uri"] = get_by_uri, ["set-uri-contents"] = set_uri_contents, ["read-file"] = read_file}
end
package.preload["fennel-ls.searcher"] = package.preload["fennel-ls.searcher"] or function(...)
  local _local_29_ = require("fennel-ls.utils")
  local absolute_path_3f = _local_29_["absolute-path?"]
  local uri__3epath = _local_29_["uri->path"]
  local path__3euri = _local_29_["path->uri"]
  local path_sep = _local_29_["path-sep"]
  local path_join = _local_29_["path-join"]
  local function add_workspaces_to_path(path, _3fworkspaces)
    _G.assert((nil ~= path), "Missing argument path on src/fennel-ls\\searcher.fnl:12")
    local result = {}
    for path0 in path:gmatch("[^;]+") do
      if absolute_path_3f(path0) then
        table.insert(result, path0)
      else
        for _, workspace in ipairs((_3fworkspaces or {})) do
          table.insert(result, path_join(uri__3epath(workspace), path0))
        end
      end
    end
    return table.concat(result, ";")
  end
  local function file_exists_3f(server, uri)
    local _32_
    do
      local t_31_ = server.preload
      if (nil ~= t_31_) then
        t_31_ = t_31_[uri]
      else
      end
      _32_ = t_31_
    end
    local or_34_ = _32_
    if not or_34_ then
      local _35_ = io.open(uri__3epath(uri))
      if (nil ~= _35_) then
        local f = _35_
        f:close()
        or_34_ = true
      else
        or_34_ = nil
      end
    end
    return or_34_
  end
  local function lookup(_39_, mod)
    local _arg_40_ = _39_["configuration"]
    local fennel_path = _arg_40_["fennel-path"]
    local _3froot_uri = _39_["root-uri"]
    local server = _39_
    _G.assert((nil ~= mod), "Missing argument mod on src/fennel-ls\\searcher.fnl:27")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\searcher.fnl:27")
    _G.assert((nil ~= fennel_path), "Missing argument fennel-path on src/fennel-ls\\searcher.fnl:27")
    if _3froot_uri then
      local mod0 = mod:gsub("%.", path_sep)
      local root_path = uri__3epath(_3froot_uri)
      local uri = nil
      for segment in fennel_path:gmatch("[^;]+") do
        if uri then break end
        local segment0 = segment:gsub("%?", mod0)
        local segment1
        if absolute_path_3f(segment0) then
          segment1 = segment0
        else
          segment1 = path_join(root_path, segment0)
        end
        local segment2 = path__3euri(segment1)
        if file_exists_3f(server, segment2) then
          uri = segment2
        else
          uri = nil
        end
      end
      return uri
    else
      return nil
    end
  end
  return {lookup = lookup, ["add-workspaces-to-path"] = add_workspaces_to_path}
end
package.preload["fennel-ls.compiler"] = package.preload["fennel-ls.compiler"] or function(...)
  local _local_44_ = require("fennel")
  local sym_3f = _local_44_["sym?"]
  local list_3f = _local_44_["list?"]
  local sequence_3f = _local_44_["sequence?"]
  local table_3f = _local_44_["table?"]
  local sym = _local_44_["sym"]
  local view = _local_44_["view"]
  local fennel = _local_44_
  local docs = require("fennel-ls.docs")
  local message = require("fennel-ls.message")
  local searcher = require("fennel-ls.searcher")
  local utils = require("fennel-ls.utils")
  local nil_2a = sym("nil")
  local function scope_3f(candidate)
    return ((type(candidate) == "table") and (type(candidate.includes) == "table") and (type(candidate.macros) == "table") and (type(candidate.manglings) == "table") and (type(candidate.specials) == "table") and (type(candidate.gensyms) == "table"))
  end
  local function ast__3emacro_ast(ast)
    _G.assert((nil ~= ast), "Missing argument ast on src/fennel-ls\\compiler.fnl:26")
    return {fennel.list(sym("eval-compiler"), (table.unpack or _G.unpack)(ast))}
  end
  local function multisym_3f(t)
    _G.assert((nil ~= t), "Missing argument t on src/fennel-ls\\compiler.fnl:30")
    local and_71_ = sym_3f(t)
    if and_71_ then
      local t0 = tostring(t)
      and_71_ = (t0:find("%.") or t0:find(":"))
    end
    return and_71_
  end
  local function iter(t)
    _G.assert((nil ~= t), "Missing argument t on src/fennel-ls\\compiler.fnl:37")
    if (list_3f(t) or sequence_3f(t)) then
      return ipairs(t)
    else
      return pairs(t)
    end
  end
  local has_tables_mt
  local function _74_(self, key)
    _G.assert((nil ~= key), "Missing argument key on src/fennel-ls\\compiler.fnl:46")
    _G.assert((nil ~= self), "Missing argument self on src/fennel-ls\\compiler.fnl:46")
    local val = {}
    self[key] = val
    return val
  end
  has_tables_mt = {__index = _74_}
  local function line_2bbyte__3erange(server, file, line, byte)
    _G.assert((nil ~= byte), "Missing argument byte on src/fennel-ls\\compiler.fnl:51")
    _G.assert((nil ~= line), "Missing argument line on src/fennel-ls\\compiler.fnl:51")
    _G.assert((nil ~= file), "Missing argument file on src/fennel-ls\\compiler.fnl:51")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\compiler.fnl:51")
    local line0 = (line - 1)
    local byte0 = math.max(0, byte)
    local position = utils["pos->position"](file.text, line0, byte0, server["position-encoding"])
    return {start = position, ["end"] = position}
  end
  local function compile(_75_, file)
    local _arg_76_ = _75_["configuration"]
    local macro_path = _arg_76_["macro-path"]
    local _3froot_uri = _75_["root-uri"]
    local server = _75_
    _G.assert((nil ~= file), "Missing argument file on src/fennel-ls\\compiler.fnl:59")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\compiler.fnl:59")
    _G.assert((nil ~= macro_path), "Missing argument macro-path on src/fennel-ls\\compiler.fnl:59")
    local definitions_by_scope
    do
      local tmp_9_auto = {}
      setmetatable(tmp_9_auto, has_tables_mt)
      definitions_by_scope = tmp_9_auto
    end
    local definitions = {}
    local diagnostics = {}
    local references = {}
    local macro_refs = {}
    local scopes = {}
    local calls = {}
    local lexical = {}
    local require_calls = {}
    local defer = {}
    local function find_definition(name, _3fscope)
      _G.assert((nil ~= name), "Missing argument name on src/fennel-ls\\compiler.fnl:74")
      if _3fscope then
        return (definitions_by_scope[_3fscope][name] or find_definition(name, _3fscope.parent))
      else
        return nil
      end
    end
    local function reference(symbol, scope, ref_type)
      _G.assert((nil ~= ref_type), "Missing argument ref-type on src/fennel-ls\\compiler.fnl:79")
      _G.assert((nil ~= scope), "Missing argument scope on src/fennel-ls\\compiler.fnl:79")
      _G.assert((nil ~= symbol), "Missing argument symbol on src/fennel-ls\\compiler.fnl:79")
      assert(((ref_type == "read") or (ref_type == "write") or (ref_type == "mutate")), "wrong ref-type")
      assert(sym_3f(symbol), "not-a-symbol")
      assert(scope_3f(scope), "not-a-scope")
      local name = string.match(tostring(symbol), "[^%.:]+")
      local _78_ = (find_definition(tostring(name), scope) or docs["get-global"](server, name))
      if (nil ~= _78_) then
        local target = _78_
        local ref = {symbol = symbol, target = target, ["ref-type"] = ref_type}
        references[symbol] = ref
        if target["referenced-by"] then
          return table.insert(target["referenced-by"], ref)
        else
          return nil
        end
      else
        return nil
      end
    end
    local function symbol_to_expression(ast, scope, _3freference_3f)
      _G.assert((nil ~= scope), "Missing argument scope on src/fennel-ls\\compiler.fnl:92")
      _G.assert((nil ~= ast), "Missing argument ast on src/fennel-ls\\compiler.fnl:92")
      assert(sym_3f(ast), "symbols only")
      local function _81_()
        if _3freference_3f then
          return "read"
        elseif not multisym_3f(ast) then
          return "write"
        else
          return "mutate"
        end
      end
      return reference(ast, scope, _81_())
    end
    local function for_each_binding_in(binding, _3fdefinition, action)
      _G.assert((nil ~= action), "Missing argument action on src/fennel-ls\\compiler.fnl:100")
      _G.assert((nil ~= binding), "Missing argument binding on src/fennel-ls\\compiler.fnl:100")
      local function recurse(binding0, keys, depth)
        _G.assert((nil ~= depth), "Missing argument depth on src/fennel-ls\\compiler.fnl:101")
        _G.assert((nil ~= keys), "Missing argument keys on src/fennel-ls\\compiler.fnl:101")
        _G.assert((nil ~= binding0), "Missing argument binding on src/fennel-ls\\compiler.fnl:101")
        if sym_3f(binding0) then
          return action(binding0, _3fdefinition, keys, keys.multival)
        elseif list_3f(binding0) then
          local set_target_3f = sym_3f(binding0[1], ".")
          if set_target_3f then
            return action(binding0, _3fdefinition, keys, keys.multival)
          elseif (depth ~= 0) then
            return error(("I didn't expect to find a nested multival destructure in " .. view(binding0) .. " at " .. view(keys)))
          else
            for i, child in ipairs(binding0) do
              keys.multival = i
              recurse(child, keys, (depth + 1))
              keys.multival = nil
            end
            return nil
          end
        elseif table_3f(binding0) then
          local prev = nil
          for key, child in iter(binding0) do
            if (sym_3f(key, "&as") or sym_3f(prev, "&as")) then
              prev = recurse(child, keys, (depth + 1))
            elseif (sym_3f(key, "&") or sym_3f(prev, "&")) then
              prev = for_each_binding_in(child, {}, action)
            elseif (sym_3f(child, "&as") or sym_3f(child, "&")) then
              prev = child
            else
              table.insert(keys, key)
              recurse(child, keys, (depth + 1))
              prev = table.remove(keys)
            end
          end
          return prev
        else
          return nil
        end
      end
      return recurse(binding, {}, 0)
    end
    local function define(_3fdefinition, binding, scope, _3fopts)
      _G.assert((nil ~= scope), "Missing argument scope on src/fennel-ls\\compiler.fnl:131")
      _G.assert((nil ~= binding), "Missing argument binding on src/fennel-ls\\compiler.fnl:131")
      local function _85_(symbol, _3fdefinition0, keys, _3fmultival)
        if not (list_3f(symbol) or multisym_3f(symbol)) then
          local definition
          local _87_
          do
            local t_86_ = definitions
            if (nil ~= t_86_) then
              t_86_ = t_86_[symbol]
            else
            end
            if (nil ~= t_86_) then
              t_86_ = t_86_["referenced-by"]
            else
            end
            _87_ = t_86_
          end
          local _90_
          if keys[1] then
            local tbl_21_auto = {}
            local i_22_auto = 0
            for _, v in ipairs(keys) do
              local val_23_auto = v
              if (nil ~= val_23_auto) then
                i_22_auto = (i_22_auto + 1)
                tbl_21_auto[i_22_auto] = val_23_auto
              else
              end
            end
            _90_ = tbl_21_auto
          else
            _90_ = nil
          end
          local _95_
          do
            local t_94_ = _3fopts
            if (nil ~= t_94_) then
              t_94_ = t_94_.isvar
            else
            end
            _95_ = t_94_
          end
          definition = {binding = symbol, definition = _3fdefinition0, ["referenced-by"] = (_87_ or {}), keys = _90_, multival = _3fmultival, ["var?"] = _95_, file = file}
          definitions_by_scope[scope][tostring(symbol)] = definition
          definitions[symbol] = definition
          return nil
        else
          return nil
        end
      end
      return for_each_binding_in(binding, _3fdefinition, _85_)
    end
    local function mutate(_3fdefinition, binding, scope)
      _G.assert((nil ~= scope), "Missing argument scope on src/fennel-ls\\compiler.fnl:147")
      _G.assert((nil ~= binding), "Missing argument binding on src/fennel-ls\\compiler.fnl:147")
      local function _98_(symbol, __3fdefinition, _keys)
        if not (list_3f(symbol) or multisym_3f(symbol)) then
          reference(symbol, scope, "write")
          if references[symbol] then
            references[symbol].target["var-set"] = true
            return nil
          else
            return nil
          end
        else
          return nil
        end
      end
      return for_each_binding_in(binding, _3fdefinition, _98_)
    end
    local function destructure(to, from, scope, _101_)
      local _3fdeclaration_3f = _101_["declaration"]
      local symtype = _101_["symtype"]
      local opts = _101_
      _G.assert((nil ~= opts), "Missing argument opts on src/fennel-ls\\compiler.fnl:155")
      _G.assert((nil ~= symtype), "Missing argument symtype on src/fennel-ls\\compiler.fnl:155")
      _G.assert((nil ~= scope), "Missing argument scope on src/fennel-ls\\compiler.fnl:155")
      _G.assert((nil ~= from), "Missing argument from on src/fennel-ls\\compiler.fnl:155")
      _G.assert((nil ~= to), "Missing argument to on src/fennel-ls\\compiler.fnl:155")
      if (symtype ~= "pv") then
        if _3fdeclaration_3f then
          return define(to, from, scope, opts)
        else
          return mutate(to, from, scope)
        end
      else
        return nil
      end
    end
    local function add_field(ast, multisym, scope)
      _G.assert((nil ~= scope), "Missing argument scope on src/fennel-ls\\compiler.fnl:163")
      _G.assert((nil ~= multisym), "Missing argument multisym on src/fennel-ls\\compiler.fnl:163")
      _G.assert((nil ~= ast), "Missing argument ast on src/fennel-ls\\compiler.fnl:163")
      local function _104_(...)
        local _105_ = ...
        if ((_G.type(_105_) == "table") and (nil ~= _105_[1]) and (nil ~= _105_[2]) and (_105_[3] == nil)) then
          local ref = _105_[1]
          local field = _105_[2]
          local function _106_(...)
            local _107_ = ...
            if (nil ~= _107_) then
              local target = _107_
              target.fields = (target.fields or {})
              target.fields[field] = {binding = multisym, definition = ast, file = file}
              return nil
            else
              local __87_auto = _107_
              return ...
            end
          end
          return _106_(find_definition(ref, scope))
        else
          local __87_auto = _105_
          return ...
        end
      end
      return _104_(utils["multi-sym-split"](multisym))
    end
    local function define_function_name(ast, scope)
      _G.assert((nil ~= scope), "Missing argument scope on src/fennel-ls\\compiler.fnl:178")
      _G.assert((nil ~= ast), "Missing argument ast on src/fennel-ls\\compiler.fnl:178")
      local and_110_ = ((_G.type(ast) == "table") and true and (nil ~= ast[2]) and (nil ~= ast[3]))
      if and_110_ then
        local _fn = ast[1]
        local name = ast[2]
        local args = ast[3]
        and_110_ = (sym_3f(name) and sequence_3f(args))
      end
      if and_110_ then
        local _fn = ast[1]
        local name = ast[2]
        local args = ast[3]
        if multisym_3f(name) then
          return add_field(ast, name, scope)
        else
          return define(ast, name, scope)
        end
      else
        return nil
      end
    end
    local function define_function_args(ast, scope)
      _G.assert((nil ~= scope), "Missing argument scope on src/fennel-ls\\compiler.fnl:188")
      _G.assert((nil ~= ast), "Missing argument ast on src/fennel-ls\\compiler.fnl:188")
      local args
      local and_114_ = ((_G.type(ast) == "table") and true and (nil ~= ast[2]))
      if and_114_ then
        local _fn = ast[1]
        local args0 = ast[2]
        and_114_ = fennel["sequence?"](args0)
      end
      if and_114_ then
        local _fn = ast[1]
        local args0 = ast[2]
        args = args0
      else
        local and_116_ = ((_G.type(ast) == "table") and true and true and (nil ~= ast[3]))
        if and_116_ then
          local _fn = ast[1]
          local _name = ast[2]
          local args0 = ast[3]
          and_116_ = fennel["sequence?"](args0)
        end
        if and_116_ then
          local _fn = ast[1]
          local _name = ast[2]
          local args0 = ast[3]
          args = args0
        else
          local _ = ast
          args = {}
        end
      end
      for _, argument in ipairs(args) do
        if not sym_3f(argument, "&") then
          define(nil_2a, argument, scope)
        else
        end
      end
      return nil
    end
    local function define_function(ast, scope)
      _G.assert((nil ~= scope), "Missing argument scope on src/fennel-ls\\compiler.fnl:199")
      _G.assert((nil ~= ast), "Missing argument ast on src/fennel-ls\\compiler.fnl:199")
      return define_function_name(ast, scope)
    end
    local function compile_for(_ast, scope, binding)
      _G.assert((nil ~= binding), "Missing argument binding on src/fennel-ls\\compiler.fnl:203")
      _G.assert((nil ~= scope), "Missing argument scope on src/fennel-ls\\compiler.fnl:203")
      return define(nil_2a, binding, scope)
    end
    local function compile_each(_ast, scope, bindings)
      _G.assert((nil ~= bindings), "Missing argument bindings on src/fennel-ls\\compiler.fnl:206")
      _G.assert((nil ~= scope), "Missing argument scope on src/fennel-ls\\compiler.fnl:206")
      for _, binding in ipairs(bindings) do
        define(nil_2a, binding, scope)
      end
      return nil
    end
    local function compile_fn(ast, scope)
      _G.assert((nil ~= scope), "Missing argument scope on src/fennel-ls\\compiler.fnl:210")
      _G.assert((nil ~= ast), "Missing argument ast on src/fennel-ls\\compiler.fnl:210")
      scopes[ast] = scope
      return define_function_args(ast, scope)
    end
    local function compile_do(ast, scope)
      _G.assert((nil ~= scope), "Missing argument scope on src/fennel-ls\\compiler.fnl:214")
      _G.assert((nil ~= ast), "Missing argument ast on src/fennel-ls\\compiler.fnl:214")
      scopes[ast] = scope
      return nil
    end
    local function call(ast, scope)
      _G.assert((nil ~= scope), "Missing argument scope on src/fennel-ls\\compiler.fnl:217")
      _G.assert((nil ~= ast), "Missing argument ast on src/fennel-ls\\compiler.fnl:217")
      calls[ast] = true
      scopes[ast] = scope
      local head = ast[1]
      local _120_ = (sym_3f(head) and tostring(head))
      if (_120_ == "fn") then
        return define_function(ast, scope)
      elseif ((_120_ == "require") or (_120_ == "include")) then
        require_calls[ast] = true
        return nil
      else
        local and_121_ = (nil ~= _120_)
        if and_121_ then
          local method_call = _120_
          and_121_ = ((type(method_call) == "string") and method_call:find(":"))
        end
        if and_121_ then
          local method_call = _120_
          return reference(head, scope, "read")
        elseif (_120_ == "if") then
          local len = #ast
          local function _123_()
            ast[(len + 1)] = nil
            return nil
          end
          return table.insert(defer, _123_)
        else
          return nil
        end
      end
    end
    local function macroexpand(ast, _transformed, scope)
      local macro_id = ast[1]
      local macro_fn
      do
        local t = scope.macros
        for _, part in ipairs(utils["multi-sym-split"](macro_id)) do
          if (type(t) == "table") then
            t = t[part]
          else
            t = nil
          end
        end
        macro_fn = t
      end
      if (type(macro_fn) == "function") then
        assert(sym_3f(macro_id), "macros should be syms")
        macro_refs[macro_id] = macro_fn
        return nil
      else
        return nil
      end
    end
    local function attempt_to_recover_21(msg, _3fast)
      _G.assert((nil ~= msg), "Missing argument msg on src/fennel-ls\\compiler.fnl:249")
      local or_127_ = (1 == msg:find("unknown identifier")) or (1 == msg:find("local %S+ was overshadowed by a special form or macro")) or (1 == msg:find("expected var ")) or (1 == msg:find("expected local ")) or (1 == msg:find("cannot call literal value")) or (1 == msg:find("unexpected vararg")) or (1 == msg:find("expected closing delimiter")) or (1 == msg:find("expected body expression")) or (1 == msg:find(".*fennel/macros.fnl:%d+: expected body")) or (1 == msg:find("expected condition and body")) or (1 == msg:find("expected whitespace before opening delimiter")) or (1 == msg:find("malformed multisym")) or (1 == msg:find("expected at least one pattern/body pair")) or (1 == msg:find("module not found")) or (1 == msg:find("expected even number of values in table literal")) or (1 == msg:find("use $%.%.%. in hashfn"))
      if not or_127_ then
        if ((1 == msg:find("expected even number of name/value bindings")) and sequence_3f(_3fast) and (1 == (#_3fast % 2))) then
          table.insert(_3fast, nil_2a)
          local function _128_()
            return table.remove(_3fast)
          end
          table.insert(defer, _128_)
          or_127_ = true
        else
          or_127_ = nil
        end
      end
      if not or_127_ then
        if ((1 == msg:find("expected a function, macro, or special to call")) and list_3f(_3fast) and (#_3fast == 0)) then
          table.insert(_3fast, sym("do"))
          local function _130_()
            return table.remove(_3fast)
          end
          table.insert(defer, _130_)
          or_127_ = true
        else
          or_127_ = nil
        end
      end
      if not or_127_ then
        if (1 == msg:find("unexpected multi symbol")) then
          local old = tostring(_3fast)
          _3fast[1] = "!!invalid-multi-symbol!!"
          local function _133_()
            _3fast[1] = old
            return nil
          end
          table.insert(defer, _133_)
          or_127_ = true
        else
          or_127_ = nil
        end
      end
      return or_127_
    end
    local function on_compile_error(_, msg, ast, call_me_to_reset_the_compiler)
      _G.assert((nil ~= call_me_to_reset_the_compiler), "Missing argument call-me-to-reset-the-compiler on src/fennel-ls\\compiler.fnl:284")
      _G.assert((nil ~= ast), "Missing argument ast on src/fennel-ls\\compiler.fnl:284")
      _G.assert((nil ~= msg), "Missing argument msg on src/fennel-ls\\compiler.fnl:284")
      do
        local range = (message["ast->range"](server, file, ast) or line_2bbyte__3erange(server, file, 1, 1))
        table.insert(diagnostics, {range = range, message = msg, severity = message.severity.ERROR, code = 201, codeDescription = "compiler error"})
      end
      if attempt_to_recover_21(msg, ast) then
        return true
      else
        call_me_to_reset_the_compiler()
        return error("__NOT_AN_ERROR")
      end
    end
    local function on_parse_error(msg, _filename, line, byte, _source, call_me_to_reset_the_compiler)
      _G.assert((nil ~= call_me_to_reset_the_compiler), "Missing argument call-me-to-reset-the-compiler on src/fennel-ls\\compiler.fnl:299")
      _G.assert((nil ~= byte), "Missing argument byte on src/fennel-ls\\compiler.fnl:299")
      _G.assert((nil ~= line), "Missing argument line on src/fennel-ls\\compiler.fnl:299")
      _G.assert((nil ~= msg), "Missing argument msg on src/fennel-ls\\compiler.fnl:299")
      do
        local line0
        if (line == "?") then
          line0 = 1
        else
          line0 = line
        end
        local range = line_2bbyte__3erange(server, file, line0, byte)
        table.insert(diagnostics, {range = range, message = msg, severity = message.severity.ERROR, code = 101, codeDescription = "parse error"})
      end
      if attempt_to_recover_21(msg) then
        return true
      else
        call_me_to_reset_the_compiler()
        return error("__NOT_AN_ERROR")
      end
    end
    local allowed_globals = docs["get-all-globals"](server)
    do
      local tbl_19_auto = allowed_globals
      for extra_global in server.configuration["extra-globals"]:gmatch("[^ ]+") do
        local val_20_auto = extra_global
        table.insert(tbl_19_auto, val_20_auto)
      end
    end
    local function parse_ast(parser)
      local tbl_21_auto = {}
      local i_22_auto = 0
      for ok, ast in parser do
        if not ok then break end
        local val_23_auto = ast
        if (nil ~= val_23_auto) then
          i_22_auto = (i_22_auto + 1)
          tbl_21_auto[i_22_auto] = val_23_auto
        else
        end
      end
      return tbl_21_auto
    end
    local macro_file_3f = (file.text:sub(1, 24) == ";; fennel-ls: macro-file")
    local plugin = {name = "fennel-ls", versions = {"1.4.1", "1.4.2", "1.5.0", "1.5.1"}, ["symbol-to-expression"] = symbol_to_expression, call = call, destructure = destructure, macroexpand = macroexpand, ["assert-compile"] = on_compile_error, ["parse-error"] = on_parse_error, ["pre-for"] = compile_for, ["pre-each"] = compile_each, ["pre-fn"] = compile_fn, ["pre-do"] = compile_do}
    local scope = fennel.scope()
    local opts = {filename = file.uri, plugins = {plugin}, allowedGlobals = allowed_globals, useMetadata = true, scope = scope, requireAsInclude = false}
    local filter_errors
    local function _filter_errors(component, ...)
      local _139_, _140_, _141_ = ...
      if ((_139_ == true) and true and true) then
        local _3fitem1 = _140_
        local _3fitem2 = _141_
        return _3fitem1, _3fitem2
      else
        local matched_3f_142_, err_143_ = nil, nil
        local and_144_ = ((_139_ == nil) and (nil ~= _140_))
        if and_144_ then
          local err = _140_
          and_144_ = not err:find("^[^\n]-__NOT_AN_ERROR\n")
        end
        if and_144_ then
          local err = _140_
          matched_3f_142_, err_143_ = true, err
        else
          local and_146_ = ((_139_ == false) and (nil ~= _140_))
          if and_146_ then
            local err = _140_
            and_146_ = not err:find("^[^\n]-__NOT_AN_ERROR\n")
          end
          if and_146_ then
            local err = _140_
            matched_3f_142_, err_143_ = true, err
          else
            matched_3f_142_, err_143_ = nil
          end
        end
        if matched_3f_142_ then
          local err = err_143_
          if os.getenv("TESTING") then
            return error(("\nYou have crashed fennel-ls (or the fennel " .. component .. ") with the following message\n:" .. err .. "\n\n^^^ the error message above here is the root problem\n\n"))
          else
            return table.insert(diagnostics, {range = line_2bbyte__3erange(server, file, 1, 1), message = ("unrecoverable " .. component .. " error: " .. err)})
          end
        else
          return nil
        end
      end
    end
    filter_errors = _filter_errors
    local parser
    do
      local p = fennel.parser(file.text, file.uri, opts)
      local function _p1(p2, p3)
        local function _152_()
          return p(p2, p3)
        end
        return filter_errors("parser", xpcall(_152_, fennel.traceback))
      end
      parser = _p1
    end
    local ast = parse_ast(parser)
    local function parsed(ast0)
      _G.assert((nil ~= ast0), "Missing argument ast on src/fennel-ls\\compiler.fnl:364")
      if (sym_3f(ast0) and ast0.bytestart) then
        local _153_, _154_ = tostring(ast0), file.text:sub(ast0.bytestart, ast0.bytestart)
        if ((_153_ == "hashfn") and (_154_ == "#")) then
          local function _155_()
            ast0.byteend = ast0.bytestart
            return nil
          end
          table.insert(defer, _155_)
        elseif (((_153_ == "quote") and (_154_ == "'")) or ((_153_ == "quote") and (_154_ == "`"))) then
          local function _156_()
            ast0.byteend = ast0.bytestart
            return nil
          end
          table.insert(defer, _156_)
        elseif ((_153_ == "unquote") and (_154_ == ",")) then
          local function _157_()
            ast0.byteend = ast0.bytestart
            return nil
          end
          table.insert(defer, _157_)
        else
        end
      else
      end
      if (table_3f(ast0) or list_3f(ast0) or sym_3f(ast0)) then
        lexical[ast0] = true
      else
      end
      if (table_3f(ast0) or list_3f(ast0)) then
        for k, v in iter(ast0) do
          parsed(k)
          parsed(v)
        end
      else
      end
      if (list_3f(ast0) and (sym_3f(ast0[1], "\206\187") or sym_3f(ast0[1], "lambda"))) then
        local old_sym = ast0[1]
        ast0[1] = sym("fn")
        local function _162_()
          ast0[1] = old_sym
          return nil
        end
        return table.insert(defer, _162_)
      else
        return nil
      end
    end
    parsed(ast, lexical)
    do
      local old_macro_path = fennel["macro-path"]
      if _3froot_uri then
        fennel["macro-path"] = searcher["add-workspaces-to-path"](macro_path, {_3froot_uri})
      else
      end
      local function _165_()
        if macro_file_3f then
          return ast__3emacro_ast(ast)
        else
          return ast
        end
      end
      for _i, form in ipairs(_165_()) do
        local function _166_()
          return fennel.compile(form, opts)
        end
        filter_errors("compiler", xpcall(_166_, fennel.traceback))
      end
      if _3froot_uri then
        fennel["macro-path"] = old_macro_path
      else
      end
    end
    for _, cmd in ipairs(defer) do
      cmd()
    end
    file.ast = ast
    file.calls = calls
    file.lexical = lexical
    file.scope = scope
    file.scopes = scopes
    file.definitions = definitions
    file["definitions-by-scope"] = definitions_by_scope
    file.diagnostics = diagnostics
    file.references = references
    file["require-calls"] = require_calls
    file["allowed-globals"] = allowed_globals
    file["macro-refs"] = macro_refs
    return nil
  end
  return {compile = compile}
end
package.preload["fennel-ls.docs"] = package.preload["fennel-ls.docs"] or function(...)
  local fennel = require("fennel")
  local _local_45_ = require("fennel.compiler")
  local METADATA = _local_45_["metadata"]
  local _local_46_ = _local_45_["scopes"]
  local _local_47_ = _local_46_["global"]
  local SPECIALS = _local_47_["specials"]
  local MACROS = _local_47_["macros"]
  local specials
  do
    local tbl_16_auto = {}
    for name, value in pairs(SPECIALS) do
      local k_17_auto, v_18_auto = name, {binding = name, metadata = METADATA[value]}
      if ((k_17_auto ~= nil) and (v_18_auto ~= nil)) then
        tbl_16_auto[k_17_auto] = v_18_auto
      else
      end
    end
    specials = tbl_16_auto
  end
  local macros_2a
  do
    local tbl_16_auto = {}
    for name, value in pairs(MACROS) do
      local k_17_auto, v_18_auto = name, {binding = name, metadata = METADATA[value]}
      if ((k_17_auto ~= nil) and (v_18_auto ~= nil)) then
        tbl_16_auto[k_17_auto] = v_18_auto
      else
      end
    end
    macros_2a = tbl_16_auto
  end
  local lua_versions = {lua51 = require("fennel-ls.docs.generated.lua51"), lua52 = require("fennel-ls.docs.generated.lua52"), lua53 = require("fennel-ls.docs.generated.lua53"), lua54 = require("fennel-ls.docs.generated.lua54")}
  local function get_lua_version(version)
    if not lua_versions[version] then
      local function _51_()
        local tmp_9_auto
        do
          local tbl_21_auto = {}
          local i_22_auto = 0
          for key in pairs(lua_versions) do
            local val_23_auto = key
            if (nil ~= val_23_auto) then
              i_22_auto = (i_22_auto + 1)
              tbl_21_auto[i_22_auto] = val_23_auto
            else
            end
          end
          tmp_9_auto = tbl_21_auto
        end
        table.sort(tmp_9_auto)
        return tmp_9_auto
      end
      error(("fennel-ls doesn't know about lua version " .. version .. "\n" .. "The allowed versions are: " .. fennel.view(_51_())))
    else
    end
    return lua_versions[version]
  end
  local libraries = {["tic-80"] = require("fennel-ls.docs.generated.tic80")}
  do
    local _53_, _54_ = nil, nil
    local function _55_()
      return require("fennel-ls.docs.generated.love2d")
    end
    _53_, _54_ = pcall(_55_)
    if ((_53_ == true) and (nil ~= _54_)) then
      local love2d = _54_
      libraries.love2d = love2d
    else
    end
  end
  local function get_library(library)
    if not libraries[library] then
      local function _58_()
        local tmp_9_auto
        do
          local tbl_21_auto = {}
          local i_22_auto = 0
          for key in pairs(libraries) do
            local val_23_auto = key
            if (nil ~= val_23_auto) then
              i_22_auto = (i_22_auto + 1)
              tbl_21_auto[i_22_auto] = val_23_auto
            else
            end
          end
          tmp_9_auto = tbl_21_auto
        end
        table.sort(tmp_9_auto)
        return tmp_9_auto
      end
      error(("fennel-ls doesn't know about library " .. library .. "\n" .. "The builtin libraries are: " .. fennel.view(_58_())))
    else
    end
    return libraries[library]
  end
  local function get_all_globals(server)
    local result = {}
    for library, enabled_3f in pairs(server.configuration.libraries) do
      if enabled_3f then
        local tbl_19_auto = result
        for name in pairs(get_library(library)) do
          local val_20_auto = name
          table.insert(tbl_19_auto, val_20_auto)
        end
      else
      end
    end
    local tbl_19_auto = result
    for name in pairs(get_lua_version(server.configuration["lua-version"])) do
      local val_20_auto = name
      table.insert(tbl_19_auto, val_20_auto)
    end
    return tbl_19_auto
  end
  local function get_library_global(server, global_name)
    local g = nil
    for library_name, enabled_3f in pairs(server.configuration.libraries) do
      if g then break end
      g = (enabled_3f and get_library(library_name)[global_name])
    end
    return g
  end
  local function get_global(server, global_name)
    return (get_library_global(server, global_name) or get_lua_version(server.configuration["lua-version"])[global_name])
  end
  local function get_builtin(_server, builtin_name)
    return (specials[builtin_name] or macros_2a[builtin_name])
  end
  return {["get-global"] = get_global, ["get-builtin"] = get_builtin, ["get-all-globals"] = get_all_globals}
end
package.preload["fennel-ls.docs.generated.lua51"] = package.preload["fennel-ls.docs.generated.lua51"] or function(...)
  local docs = {_G = {binding = "_G", metadata = {["fnl/docstring"] = "A global variable (not a function) that\nholds the global environment (that is, `_G._G = _G`).\nLua itself does not use this variable;\nchanging its value does not affect any environment,\nnor vice-versa.\n(Use `setfenv` to change environments.)"}}, _VERSION = {binding = "_VERSION", metadata = {["fnl/docstring"] = "A global variable (not a function) that\nholds a string containing the current interpreter version.\nThe current contents of this variable is `\"Lua 5.1\"`."}}, arg = {binding = "arg", metadata = {["fnl/docstring"] = "Before starting to run the script,\n`lua` collects all arguments in the command line\nin a global table called `arg`.\nThe script name is stored at index 0,\nthe first argument after the script name goes to index 1,\nand so on.\nAny arguments before the script name\n(that is, the interpreter name plus the options)\ngo to negative indices.\nFor instance, in the call\n\n```lua\n     $ lua -la b.lua t1 t2\n```\nthe interpreter first runs the file `a.lua`,\nthen creates a table\n\n```lua\n     arg = { [-2] = \"lua\", [-1] = \"-la\",\n             [0] = \"b.lua\",\n             [1] = \"t1\", [2] = \"t2\" }\n```\nand finally runs the file `b.lua`.\nThe script is called with `arg[1]`, `arg[2]`, ...\nas arguments;\nit can also access these arguments with the vararg expression `\"...\"`."}}, assert = {binding = "assert", metadata = {["fnl/arglist"] = {"v", "?message"}, ["fnl/docstring"] = "Issues an  error when\nthe value of its argument `v` is false (i.e., **nil** or **false**);\notherwise, returns all its arguments.\n`?message` is an error message;\nwhen absent, it defaults to \"assertion failed!\""}}, collectgarbage = {binding = "collectgarbage", metadata = {["fnl/arglist"] = {"?opt", "?arg"}, ["fnl/docstring"] = "This function is a generic interface to the garbage collector.\nIt performs different functions according to its first argument, `?opt`:\n\n* **\"collect\":**\n  performs a full garbage-collection cycle.\n  This is the default option.\n\n* **\"stop\":**\n  stops the garbage collector.\n\n* **\"restart\":**\n  restarts the garbage collector.\n\n* **\"count\":**\n  returns the total memory in use by Lua (in Kbytes).\n\n* **\"step\":**\n  performs a garbage-collection step.\n  The step \"size\" is controlled by `?arg`\n  (larger values mean more steps) in a non-specified way.\n  If you want to control the step size\n  you must experimentally tune the value of `?arg`.\n  Returns **true** if the step finished a collection cycle.\n\n* **\"setpause\":**\n  sets `?arg` as the new value for the *pause* of\n  the collector\n  Returns the previous value for *pause*.\n\n* **\"setstepmul\":**\n  sets `?arg` as the new value for the *step multiplier* of\n  the collector\n  Returns the previous value for *step*."}}, coroutine = {binding = "coroutine", fields = {create = {binding = "coroutine.create", metadata = {["fnl/arglist"] = {"f"}, ["fnl/docstring"] = "Creates a new coroutine, with body `f`.\n`f` must be a Lua function.\nReturns this new coroutine,\nan object with type `\"thread\"`."}}, resume = {binding = "coroutine.resume", metadata = {["fnl/arglist"] = {"co", "?val1", "..."}, ["fnl/docstring"] = "Starts or continues the execution of coroutine `co`.\nThe first time you resume a coroutine,\nit starts running its body.\nThe values `?val1`, ... are passed\nas the arguments to the body function.\nIf the coroutine has yielded,\n`resume` restarts it;\nthe values `?val1`, ... are passed\nas the results from the yield.\n\nIf the coroutine runs without any errors,\n`resume` returns **true** plus any values passed to `yield`\n(if the coroutine yields) or any values returned by the body function\n(if the coroutine terminates).\nIf there is any error,\n`resume` returns **false** plus the error message."}}, running = {binding = "coroutine.running", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Returns the running coroutine,\nor **nil** when called by the main thread."}}, status = {binding = "coroutine.status", metadata = {["fnl/arglist"] = {"co"}, ["fnl/docstring"] = "Returns the status of coroutine `co`, as a string:\n`\"running\"`,\nif the coroutine is running (that is, it called `status`);\n`\"suspended\"`, if the coroutine is suspended in a call to `yield`,\nor if it has not started running yet;\n`\"normal\"` if the coroutine is active but not running\n(that is, it has resumed another coroutine);\nand `\"dead\"` if the coroutine has finished its body function,\nor if it has stopped with an error."}}, wrap = {binding = "coroutine.wrap", metadata = {["fnl/arglist"] = {"f"}, ["fnl/docstring"] = "Creates a new coroutine, with body `f`.\n`f` must be a Lua function.\nReturns a function that resumes the coroutine each time it is called.\nAny arguments passed to the function behave as the\nextra arguments to `resume`.\nReturns the same values returned by `resume`,\nexcept the first boolean.\nIn case of error, propagates the error."}}, yield = {binding = "coroutine.yield", metadata = {["fnl/arglist"] = {"..."}, ["fnl/docstring"] = "Suspends the execution of the calling coroutine.\nThe coroutine cannot be running a C function,\na metamethod, or an iterator.\nAny arguments to `yield` are passed as extra results to `resume`."}}}, metadata = {["fnl/docstring"] = "The operations related to coroutines comprise a sub-library of\nthe basic library and come inside the table `coroutine`."}}, debug = {binding = "debug", fields = {debug = {binding = "debug.debug", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Enters an interactive mode with the user,\nrunning each string that the user enters.\nUsing simple commands and other debug facilities,\nthe user can inspect global and local variables,\nchange their values, evaluate expressions, and so on.\nA line containing only the word `cont` finishes this function,\nso that the caller continues its execution.\n\nNote that commands for `debug.debug` are not lexically nested\nwithin any function, and so have no direct access to local variables."}}, getfenv = {binding = "debug.getfenv", metadata = {["fnl/arglist"] = {"o"}, ["fnl/docstring"] = "Returns the environment of object `o`."}}, gethook = {binding = "debug.gethook", metadata = {["fnl/arglist"] = {"?thread"}, ["fnl/docstring"] = "Returns the current hook settings of the thread, as three values:\nthe current hook function, the current hook mask,\nand the current hook count\n(as set by the `debug.sethook` function)."}}, getinfo = {binding = "debug.getinfo", metadata = {["fnl/arglist"] = {"function", "?what"}, ["fnl/docstring"] = "Returns a table with information about a function.\nYou can give the function directly,\nor you can give a number as the value of `function`,\nwhich means the function running at level `function` of the call stack\nof the given thread:\nlevel 0 is the current function (`getinfo` itself);\nlevel 1 is the function that called `getinfo`;\nand so on.\nIf `function` is a number larger than the number of active functions,\nthen `getinfo` returns **nil**.\n\nThe returned table can contain all the fields returned by `lua_getinfo`,\nwith the string `?what` describing which fields to fill in.\nThe default for `?what` is to get all information available,\nexcept the table of valid lines.\nIf present,\nthe option `\"f\"`\nadds a field named `func` with the function itself.\nIf present,\nthe option `\"L\"`\nadds a field named `activelines` with the table of\nvalid lines.\n\nFor instance, the expression `debug.getinfo(1,\"n\").name` returns\na table with a name for the current function,\nif a reasonable name can be found,\nand the expression `debug.getinfo(print)`\nreturns a table with all available information\nabout the `print` function."}}, getlocal = {binding = "debug.getlocal", metadata = {["fnl/arglist"] = {"level", "local"}, ["fnl/docstring"] = "This function returns the name and the value of the local variable\nwith index `local` of the function at level `level` of the stack.\n(The first parameter or local variable has index 1, and so on,\nuntil the last active local variable.)\nThe function returns **nil** if there is no local\nvariable with the given index,\nand raises an error when called with a `level` out of range.\n(You can call `debug.getinfo` to check whether the level is valid.)\n\nVariable names starting with `\"(\"` (open parentheses)\nrepresent internal variables\n(loop control variables, temporaries, and C function locals)."}}, getmetatable = {binding = "debug.getmetatable", metadata = {["fnl/arglist"] = {"object"}, ["fnl/docstring"] = "Returns the metatable of the given `object`\nor **nil** if it does not have a metatable."}}, getregistry = {binding = "debug.getregistry", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Returns the registry table"}}, getupvalue = {binding = "debug.getupvalue", metadata = {["fnl/arglist"] = {"func", "up"}, ["fnl/docstring"] = "This function returns the name and the value of the upvalue\nwith index `up` of the function `func`.\nThe function returns **nil** if there is no upvalue with the given index."}}, setfenv = {binding = "debug.setfenv", metadata = {["fnl/arglist"] = {"object", "table"}, ["fnl/docstring"] = "Sets the environment of the given `object` to the given `table`.\nReturns `object`."}}, sethook = {binding = "debug.sethook", metadata = {["fnl/arglist"] = {"hook", "mask", "?count"}, ["fnl/docstring"] = "Sets the given function as a hook.\nThe string `mask` and the number `?count` describe\nwhen the hook will be called.\nThe string mask may have the following characters,\nwith the given meaning:\n\n* **`\"c\"`:** the hook is called every time Lua calls a function;\n* **`\"r\"`:** the hook is called every time Lua returns from a function;\n* **`\"l\"`:** the hook is called every time Lua enters a new line of code.\n\nWith a `?count` different from zero,\nthe hook is called after every `?count` instructions.\n\nWhen called without arguments,\n`debug.sethook` turns off the hook.\n\nWhen the hook is called, its first parameter is a string\ndescribing the event that has triggered its call:\n`\"call\"`, `\"return\"` (or `\"tail return\"`,\nwhen simulating a return from a tail call),\n`\"line\"`, and `\"count\"`.\nFor line events,\nthe hook also gets the new line number as its second parameter.\nInside a hook,\nyou can call `getinfo` with level 2 to get more information about\nthe running function\n(level 0 is the `getinfo` function,\nand level 1 is the hook function),\nunless the event is `\"tail return\"`.\nIn this case, Lua is only simulating the return,\nand a call to `getinfo` will return invalid data."}}, setlocal = {binding = "debug.setlocal", metadata = {["fnl/arglist"] = {"level", "local", "value"}, ["fnl/docstring"] = "This function assigns the value `value` to the local variable\nwith index `local` of the function at level `level` of the stack.\nThe function returns **nil** if there is no local\nvariable with the given index,\nand raises an error when called with a `level` out of range.\n(You can call `getinfo` to check whether the level is valid.)\nOtherwise, it returns the name of the local variable."}}, setmetatable = {binding = "debug.setmetatable", metadata = {["fnl/arglist"] = {"object", "table"}, ["fnl/docstring"] = "Sets the metatable for the given `object` to the given `table`\n(which can be **nil**)."}}, setupvalue = {binding = "debug.setupvalue", metadata = {["fnl/arglist"] = {"func", "up", "value"}, ["fnl/docstring"] = "This function assigns the value `value` to the upvalue\nwith index `up` of the function `func`.\nThe function returns **nil** if there is no upvalue\nwith the given index.\nOtherwise, it returns the name of the upvalue."}}, traceback = {binding = "debug.traceback", metadata = {["fnl/arglist"] = {"?message", "?level"}, ["fnl/docstring"] = "Returns a string with a traceback of the call stack.\nAn optional `?message` string is appended\nat the beginning of the traceback.\nAn optional `?level` number tells at which level\nto start the traceback\n(default is 1, the function calling `traceback`)."}}}, metadata = {["fnl/docstring"] = "This library provides\nthe functionality of the debug interface to Lua programs.\nYou should exert care when using this library.\nThe functions provided here should be used exclusively for debugging\nand similar tasks, such as profiling.\nPlease resist the temptation to use them as a\nusual programming tool:\nthey can be very slow.\nMoreover, several of these functions\nviolate some assumptions about Lua code\n(e.g., that variables local to a function\ncannot be accessed from outside or\nthat userdata metatables cannot be changed by Lua code)\nand therefore can compromise otherwise secure code.\n\nAll functions in this library are provided\ninside the `debug` table.\nAll functions that operate over a thread\nhave an optional first argument which is the\nthread to operate over.\nThe default is always the current thread."}}, dofile = {binding = "dofile", metadata = {["fnl/arglist"] = {"?filename"}, ["fnl/docstring"] = "Opens the named file and executes its contents as a Lua chunk.\nWhen called without arguments,\n`dofile` executes the contents of the standard input (`stdin`).\nReturns all values returned by the chunk.\nIn case of errors, `dofile` propagates the error\nto its caller (that is, `dofile` does not run in protected mode)."}}, error = {binding = "error", metadata = {["fnl/arglist"] = {"message", "?level"}, ["fnl/docstring"] = "Terminates the last protected function called\nand returns `message` as the error message.\nFunction `error` never returns.\n\nUsually, `error` adds some information about the error position\nat the beginning of the message.\nThe `?level` argument specifies how to get the error position.\nWith level 1 (the default), the error position is where the\n`error` function was called.\nLevel 2 points the error to where the function\nthat called `error` was called; and so on.\nPassing a level 0 avoids the addition of error position information\nto the message."}}, getfenv = {binding = "getfenv", metadata = {["fnl/arglist"] = {"?f"}, ["fnl/docstring"] = "Returns the current environment in use by the function.\n`?f` can be a Lua function or a number\nthat specifies the function at that stack level:\nLevel 1 is the function calling `getfenv`.\nIf the given function is not a Lua function,\nor if `?f` is 0,\n`getfenv` returns the global environment.\nThe default for `?f` is 1."}}, getmetatable = {binding = "getmetatable", metadata = {["fnl/arglist"] = {"object"}, ["fnl/docstring"] = "If `object` does not have a metatable, returns **nil**.\nOtherwise,\nif the object's metatable has a `\"__metatable\"` field,\nreturns the associated value.\nOtherwise, returns the metatable of the given object."}}, io = {binding = "io", fields = {close = {binding = "io.close", metadata = {["fnl/arglist"] = {"?file"}, ["fnl/docstring"] = "Equivalent to `file:close()`.\nWithout a `?file`, closes the default output file."}}, flush = {binding = "io.flush", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Equivalent to `file:flush` over the default output file."}}, input = {binding = "io.input", metadata = {["fnl/arglist"] = {"?file"}, ["fnl/docstring"] = "When called with a file name, it opens the named file (in text mode),\nand sets its handle as the default input file.\nWhen called with a file handle,\nit simply sets this file handle as the default input file.\nWhen called without parameters,\nit returns the current default input file.\n\nIn case of errors this function raises the error,\ninstead of returning an error code."}}, lines = {binding = "io.lines", metadata = {["fnl/arglist"] = {"?filename"}, ["fnl/docstring"] = "Opens the given file name in read mode\nand returns an iterator function that,\neach time it is called,\nreturns a new line from the file.\nTherefore, the construction\n\n```lua\n     for line in io.lines(filename) do *body* end\n```\nwill iterate over all lines of the file.\nWhen the iterator function detects the end of file,\nit returns **nil** (to finish the loop) and automatically closes the file.\n\nThe call `io.lines()` (with no file name) is equivalent\nto `io.input():lines()`;\nthat is, it iterates over the lines of the default input file.\nIn this case it does not close the file when the loop ends."}}, open = {binding = "io.open", metadata = {["fnl/arglist"] = {"filename", "?mode"}, ["fnl/docstring"] = "This function opens a file,\nin the mode specified in the string `?mode`.\nIt returns a new file handle,\nor, in case of errors, **nil** plus an error message.\n\nThe `?mode` string can be any of the following:\n\n* **\"r\":** read mode (the default);\n* **\"w\":** write mode;\n* **\"a\":** append mode;\n* **\"r+\":** update mode, all previous data is preserved;\n* **\"w+\":** update mode, all previous data is erased;\n* **\"a+\":** append update mode, previous data is preserved,\n    writing is only allowed at the end of file.\n\nThe `?mode` string can also have a `\"b\"` at the end,\nwhich is needed in some systems to open the file in binary mode.\nThis string is exactly what is used in the\nstandard C function `fopen`."}}, output = {binding = "io.output", metadata = {["fnl/arglist"] = {"?file"}, ["fnl/docstring"] = "Similar to `io.input`, but operates over the default output file."}}, popen = {binding = "io.popen", metadata = {["fnl/arglist"] = {"prog", "?mode"}, ["fnl/docstring"] = "Starts program `prog` in a separated process and returns\na file handle that you can use to read data from this program\n(if `?mode` is `\"r\"`, the default)\nor to write data to this program\n(if `?mode` is `\"w\"`).\n\nThis function is system dependent and is not available\non all platforms."}}, read = {binding = "io.read", metadata = {["fnl/arglist"] = {"..."}, ["fnl/docstring"] = "Equivalent to `io.input():read`."}}, stderr = {binding = "io.stderr", metadata = {["fnl/docstring"] = "stderr file"}}, stdin = {binding = "io.stdin", metadata = {["fnl/docstring"] = "stdin file"}}, stdout = {binding = "io.stdout", metadata = {["fnl/docstring"] = "stdout file"}}, tmpfile = {binding = "io.tmpfile", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Returns a handle for a temporary file.\nThis file is opened in update mode\nand it is automatically removed when the program ends."}}, type = {binding = "io.type", metadata = {["fnl/arglist"] = {"obj"}, ["fnl/docstring"] = "Checks whether `obj` is a valid file handle.\nReturns the string `\"file\"` if `obj` is an open file handle,\n`\"closed file\"` if `obj` is a closed file handle,\nor **nil** if `obj` is not a file handle."}}, write = {binding = "io.write", metadata = {["fnl/arglist"] = {"..."}, ["fnl/docstring"] = "Equivalent to `io.output():write`."}}}, metadata = {["fnl/docstring"] = "The I/O library provides two different styles for file manipulation.\nThe first one uses implicit file descriptors;\nthat is, there are operations to set a default input file and a\ndefault output file,\nand all input/output operations are over these default files.\nThe second style uses explicit file descriptors.\n\nWhen using implicit file descriptors,\nall operations are supplied by table `io`.\nWhen using explicit file descriptors,\nthe operation `io.open` returns a file descriptor\nand then all operations are supplied as methods of the file descriptor.\n\nThe table `io` also provides\nthree predefined file descriptors with their usual meanings from C:\n`io.stdin`, `io.stdout`, and `io.stderr`.\nThe I/O library never closes these files.\n\nUnless otherwise stated,\nall I/O functions return **nil** on failure\n(plus an error message as a second result and\na system-dependent error code as a third result)\nand some value different from **nil** on success."}}, ipairs = {binding = "ipairs", metadata = {["fnl/arglist"] = {"t"}, ["fnl/docstring"] = "Returns three values: an iterator function, the table `t`, and 0,\nso that the construction\n\n```lua\n     for i,v in ipairs(t) do *body* end\n```\nwill iterate over the pairs (`1,t[1]`), (`2,t[2]`), ...,\nup to the first integer key absent from the table."}}, load = {binding = "load", metadata = {["fnl/arglist"] = {"func", "?chunkname"}, ["fnl/docstring"] = "Loads a chunk using function `func` to get its pieces.\nEach call to `func` must return a string that concatenates\nwith previous results.\nA return of an empty string, **nil**, or no value signals the end of the chunk.\n\nIf there are no errors, \nreturns the compiled chunk as a function;\notherwise, returns **nil** plus the error message.\nThe environment of the returned function is the global environment.\n\n`?chunkname` is used as the chunk name for error messages\nand debug information.\nWhen absent,\nit defaults to `\"=(load)\"`."}}, loadfile = {binding = "loadfile", metadata = {["fnl/arglist"] = {"?filename"}, ["fnl/docstring"] = "Similar to `load`,\nbut gets the chunk from file `?filename`\nor from the standard input,\nif no file name is given."}}, loadstring = {binding = "loadstring", metadata = {["fnl/arglist"] = {"string", "?chunkname"}, ["fnl/docstring"] = "Similar to `load`,\nbut gets the chunk from the given string.\n\nTo load and run a given string, use the idiom\n\n```lua\n     assert(loadstring(s))()\n```\n\nWhen absent,\n`?chunkname` defaults to the given string."}}, math = {binding = "math", fields = {abs = {binding = "math.abs", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the absolute value of `x`."}}, acos = {binding = "math.acos", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the arc cosine of `x` (in radians)."}}, asin = {binding = "math.asin", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the arc sine of `x` (in radians)."}}, atan = {binding = "math.atan", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the arc tangent of `x` (in radians)."}}, atan2 = {binding = "math.atan2", metadata = {["fnl/arglist"] = {"y", "x"}, ["fnl/docstring"] = "Returns the arc tangent of `y/x` (in radians),\nbut uses the signs of both parameters to find the\nquadrant of the result.\n(It also handles correctly the case of `x` being zero.)"}}, ceil = {binding = "math.ceil", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the smallest integer larger than or equal to `x`."}}, cos = {binding = "math.cos", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the cosine of `x` (assumed to be in radians)."}}, cosh = {binding = "math.cosh", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the hyperbolic cosine of `x`."}}, deg = {binding = "math.deg", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the angle `x` (given in radians) in degrees."}}, exp = {binding = "math.exp", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the value *e\203\163*."}}, floor = {binding = "math.floor", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the largest integer smaller than or equal to `x`."}}, fmod = {binding = "math.fmod", metadata = {["fnl/arglist"] = {"x", "y"}, ["fnl/docstring"] = "Returns the remainder of the division of `x` by `y`\nthat rounds the quotient towards zero."}}, frexp = {binding = "math.frexp", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns `m` and `e` such that *x = m2\225\181\137*,\n`e` is an integer and the absolute value of `m` is\nin the range *[0.5, 1)*\n(or zero when `x` is zero)."}}, huge = {binding = "math.huge", metadata = {["fnl/docstring"] = "The value `HUGE_VAL`,\na value larger than or equal to any other numerical value."}}, ldexp = {binding = "math.ldexp", metadata = {["fnl/arglist"] = {"m", "e"}, ["fnl/docstring"] = "Returns *m2\225\181\137* (`e` should be an integer)."}}, log = {binding = "math.log", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the natural logarithm of `x`."}}, log10 = {binding = "math.log10", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the base-10 logarithm of `x`."}}, max = {binding = "math.max", metadata = {["fnl/arglist"] = {"x", "..."}, ["fnl/docstring"] = "Returns the maximum value among its arguments."}}, min = {binding = "math.min", metadata = {["fnl/arglist"] = {"x", "..."}, ["fnl/docstring"] = "Returns the minimum value among its arguments."}}, modf = {binding = "math.modf", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns two numbers,\nthe integral part of `x` and the fractional part of `x`."}}, pi = {binding = "math.pi", metadata = {["fnl/docstring"] = "The value of *pi*."}}, pow = {binding = "math.pow", metadata = {["fnl/arglist"] = {"x", "y"}, ["fnl/docstring"] = "Returns *x\202\184*.\n(You can also use the expression `x^y` to compute this value.)"}}, rad = {binding = "math.rad", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the angle `x` (given in degrees) in radians."}}, random = {binding = "math.random", metadata = {["fnl/arglist"] = {"?m", "?n"}, ["fnl/docstring"] = "This function is an interface to the simple\npseudo-random generator function `rand` provided by ANSI C.\n(No guarantees can be given for its statistical properties.)\n\nWhen called without arguments,\nreturns a uniform pseudo-random real number\nin the range *[0,1)*.  \nWhen called with an integer number `?m`,\n`math.random` returns\na uniform pseudo-random integer in the range *[1, m]*.\nWhen called with two integer numbers `?m` and `?n`,\n`math.random` returns a uniform pseudo-random\ninteger in the range *[m, n]*."}}, randomseed = {binding = "math.randomseed", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Sets `x` as the \"seed\"\nfor the pseudo-random generator:\nequal seeds produce equal sequences of numbers."}}, sin = {binding = "math.sin", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the sine of `x` (assumed to be in radians)."}}, sinh = {binding = "math.sinh", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the hyperbolic sine of `x`."}}, sqrt = {binding = "math.sqrt", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the square root of `x`.\n(You can also use the expression `x^0.5` to compute this value.)"}}, tan = {binding = "math.tan", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the tangent of `x` (assumed to be in radians)."}}, tanh = {binding = "math.tanh", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the hyperbolic tangent of `x`."}}}, metadata = {["fnl/docstring"] = "This library is an interface to the standard C math library.\nIt provides all its functions inside the table `math`."}}, module = {binding = "module", metadata = {["fnl/arglist"] = {"name", "?..."}, ["fnl/docstring"] = "Creates a module.\nIf there is a table in `package.loaded[name]`,\nthis table is the module.\nOtherwise, if there is a global table `t` with the given name,\nthis table is the module.\nOtherwise creates a new table `t` and\nsets it as the value of the global `name` and\nthe value of `package.loaded[name]`.\nThis function also initializes `t._NAME` with the given name,\n`t._M` with the module (`t` itself),\nand `t._PACKAGE` with the package name\n(the full module name minus last component; see below).\nFinally, `module` sets `t` as the new environment\nof the current function and the new value of `package.loaded[name]`,\nso that `require` returns `t`.\n\nIf `name` is a compound name\n(that is, one with components separated by dots),\n`module` creates (or reuses, if they already exist)\ntables for each component.\nFor instance, if `name` is `a.b.c`,\nthen `module` stores the module table in field `c` of\nfield `b` of global `a`.\n\nThis function can receive optional *options* after\nthe module name,\nwhere each option is a function to be applied over the module."}}, next = {binding = "next", metadata = {["fnl/arglist"] = {"table", "?index"}, ["fnl/docstring"] = "Allows a program to traverse all fields of a table.\nIts first argument is a table and its second argument\nis an index in this table.\n`next` returns the next index of the table\nand its associated value.\nWhen called with **nil** as its second argument,\n`next` returns an initial index\nand its associated value.\nWhen called with the last index,\nor with **nil** in an empty table,\n`next` returns **nil**.\nIf the second argument is absent, then it is interpreted as **nil**.\nIn particular,\nyou can use `next(t)` to check whether a table is empty.\n\nThe order in which the indices are enumerated is not specified,\n*even for numeric indices*.\n(To traverse a table in numeric order,\nuse a numerical **for** or the `ipairs` function.)\n\nThe behavior of `next` is *undefined* if,\nduring the traversal,\nyou assign any value to a non-existent field in the table.\nYou may however modify existing fields.\nIn particular, you may clear existing fields."}}, os = {binding = "os", fields = {clock = {binding = "os.clock", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Returns an approximation of the amount in seconds of CPU time\nused by the program."}}, date = {binding = "os.date", metadata = {["fnl/arglist"] = {"?format", "?time"}, ["fnl/docstring"] = "Returns a string or a table containing date and time,\nformatted according to the given string `?format`.\n\nIf the `?time` argument is present,\nthis is the time to be formatted\n(see the `os.time` function for a description of this value).\nOtherwise, `date` formats the current time.\n\nIf `?format` starts with `\"!\"`,\nthen the date is formatted in Coordinated Universal Time.\nAfter this optional character,\nif `?format` is the string `\"*t\"`,\nthen `date` returns a table with the following fields:\n`year` (four digits), `month` (1--12), `day` (1--31),\n`hour` (0--23), `min` (0--59), `sec` (0--61),\n`wday` (weekday, Sunday is 1),\n`yday` (day of the year),\nand `isdst` (daylight saving flag, a boolean).\n\nIf `?format` is not `\"*t\"`,\nthen `date` returns the date as a string,\nformatted according to the same rules as the C function `strftime`.\n\nWhen called without arguments,\n`date` returns a reasonable date and time representation that depends on\nthe host system and on the current locale\n(that is, `os.date()` is equivalent to `os.date(\"%c\")`)."}}, difftime = {binding = "os.difftime", metadata = {["fnl/arglist"] = {"t2", "t1"}, ["fnl/docstring"] = "Returns the number of seconds from time `t1` to time `t2`.\nIn POSIX, Windows, and some other systems,\nthis value is exactly `t2`*-*`t1`."}}, execute = {binding = "os.execute", metadata = {["fnl/arglist"] = {"?command"}, ["fnl/docstring"] = "This function is equivalent to the C function `system`.\nIt passes `?command` to be executed by an operating system shell.\nIt returns a status code, which is system-dependent.\nIf `?command` is absent, then it returns nonzero if a shell is available\nand zero otherwise."}}, exit = {binding = "os.exit", metadata = {["fnl/arglist"] = {"?code"}, ["fnl/docstring"] = "Calls the C function `exit`,\nwith an optional `?code`,\nto terminate the host program.\nThe default value for `?code` is the success code."}}, getenv = {binding = "os.getenv", metadata = {["fnl/arglist"] = {"varname"}, ["fnl/docstring"] = "Returns the value of the process environment variable `varname`,\nor **nil** if the variable is not defined."}}, remove = {binding = "os.remove", metadata = {["fnl/arglist"] = {"filename"}, ["fnl/docstring"] = "Deletes the file or directory with the given name.\nDirectories must be empty to be removed.\nIf this function fails, it returns **nil**,\nplus a string describing the error."}}, rename = {binding = "os.rename", metadata = {["fnl/arglist"] = {"oldname", "newname"}, ["fnl/docstring"] = "Renames file or directory named `oldname` to `newname`.\nIf this function fails, it returns **nil**,\nplus a string describing the error."}}, setlocale = {binding = "os.setlocale", metadata = {["fnl/arglist"] = {"locale", "?category"}, ["fnl/docstring"] = "Sets the current locale of the program.\n`locale` is a string specifying a locale;\n`?category` is an optional string describing which category to change:\n`\"all\"`, `\"collate\"`, `\"ctype\"`,\n`\"monetary\"`, `\"numeric\"`, or `\"time\"`;\nthe default category is `\"all\"`.\nThe function returns the name of the new locale,\nor **nil** if the request cannot be honored.\n\nIf `locale` is the empty string,\nthe current locale is set to an implementation-defined native locale.\nIf `locale` is the string `\"C\"`,\nthe current locale is set to the standard C locale.\n\nWhen called with **nil** as the first argument,\nthis function only returns the name of the current locale\nfor the given category."}}, time = {binding = "os.time", metadata = {["fnl/arglist"] = {"?table"}, ["fnl/docstring"] = "Returns the current time when called without arguments,\nor a time representing the date and time specified by the given table.\nThis table must have fields `year`, `month`, and `day`,\nand may have fields `hour`, `min`, `sec`, and `isdst`\n(for a description of these fields, see the `os.date` function).\n\nThe returned value is a number, whose meaning depends on your system.\nIn POSIX, Windows, and some other systems, this number counts the number\nof seconds since some given start time (the \"epoch\").\nIn other systems, the meaning is not specified,\nand the number returned by `time` can be used only as an argument to\n`date` and `difftime`."}}, tmpname = {binding = "os.tmpname", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Returns a string with a file name that can\nbe used for a temporary file.\nThe file must be explicitly opened before its use\nand explicitly removed when no longer needed.\n\nOn some systems (POSIX),\nthis function also creates a file with that name,\nto avoid security risks.\n(Someone else might create the file with wrong permissions\nin the time between getting the name and creating the file.)\nYou still have to open the file to use it\nand to remove it (even if you do not use it).\n\nWhen possible,\nyou may prefer to use `io.tmpfile`,\nwhich automatically removes the file when the program ends."}}}, metadata = {["fnl/docstring"] = "This library is implemented through table `os`."}}, package = {binding = "package", fields = {cpath = {binding = "package.cpath", metadata = {["fnl/docstring"] = "The path used by `require` to search for a C loader.\n\nLua initializes the C path `package.cpath` in the same way\nit initializes the Lua path `package.path`,\nusing the environment variable `LUA_CPATH`\nor a default path defined in `luaconf.h`."}}, loaded = {binding = "package.loaded", metadata = {["fnl/docstring"] = "A table used by `require` to control which\nmodules are already loaded.\nWhen you require a module `modname` and\n`package.loaded[modname]` is not false,\n`require` simply returns the value stored there."}}, loaders = {binding = "package.loaders", metadata = {["fnl/docstring"] = "A table used by `require` to control how to load modules.\n\nEach entry in this table is a *searcher function*.\nWhen looking for a module,\n`require` calls each of these searchers in ascending order,\nwith the module name (the argument given to `require`) as its\nsole parameter.\nThe function can return another function (the module *loader*)\nor a string explaining why it did not find that module\n(or **nil** if it has nothing to say).\nLua initializes this table with four functions.\n\nThe first searcher simply looks for a loader in the\n`package.preload` table.\n\nThe second searcher looks for a loader as a Lua library,\nusing the path stored at `package.path`.\nA path is a sequence of *templates* separated by semicolons.\nFor each template,\nthe searcher will change each interrogation\nmark in the template by `filename`,\nwhich is the module name with each dot replaced by a\n\"directory separator\" (such as `\"/\"` in Unix);\nthen it will try to open the resulting file name.\nSo, for instance, if the Lua path is the string\n\n```lua\n     \"./?.lua;./?.lc;/usr/local/?/init.lua\"\n```\nthe search for a Lua file for module `foo`\nwill try to open the files\n`./foo.lua`, `./foo.lc`, and\n`/usr/local/foo/init.lua`, in that order.\n\nThe third searcher looks for a loader as a C library,\nusing the path given by the variable `package.cpath`.\nFor instance,\nif the C path is the string\n\n```lua\n     \"./?.so;./?.dll;/usr/local/?/init.so\"\n```\nthe searcher for module `foo`\nwill try to open the files `./foo.so`, `./foo.dll`,\nand `/usr/local/foo/init.so`, in that order.\nOnce it finds a C library,\nthis searcher first uses a dynamic link facility to link the\napplication with the library.\nThen it tries to find a C function inside the library to\nbe used as the loader.\nThe name of this C function is the string `\"luaopen_\"`\nconcatenated with a copy of the module name where each dot\nis replaced by an underscore.\nMoreover, if the module name has a hyphen,\nits prefix up to (and including) the first hyphen is removed.\nFor instance, if the module name is `a.v1-b.c`,\nthe function name will be `luaopen_b_c`.\n\nThe fourth searcher tries an *all-in-one loader*.\nIt searches the C path for a library for\nthe root name of the given module.\nFor instance, when requiring `a.b.c`,\nit will search for a C library for `a`.\nIf found, it looks into it for an open function for\nthe submodule;\nin our example, that would be `luaopen_a_b_c`.\nWith this facility, a package can pack several C submodules\ninto one single library,\nwith each submodule keeping its original open function."}}, loadlib = {binding = "package.loadlib", metadata = {["fnl/arglist"] = {"libname", "funcname"}, ["fnl/docstring"] = "Dynamically links the host program with the C library `libname`.\nInside this library, looks for a function `funcname`\nand returns this function as a C function.\n(So, `funcname` must follow the protocol (see `lua_CFunction`)).\n\nThis is a low-level function.\nIt completely bypasses the package and module system.\nUnlike `require`,\nit does not perform any path searching and\ndoes not automatically adds extensions.\n`libname` must be the complete file name of the C library,\nincluding if necessary a path and extension.\n`funcname` must be the exact name exported by the C library\n(which may depend on the C compiler and linker used).\n\nThis function is not supported by ANSI C.\nAs such, it is only available on some platforms\n(Windows, Linux, Mac OS X, Solaris, BSD,\nplus other Unix systems that support the `dlfcn` standard)."}}, path = {binding = "package.path", metadata = {["fnl/docstring"] = "The path used by `require` to search for a Lua loader.\n\nAt start-up, Lua initializes this variable with\nthe value of the environment variable `LUA_PATH` or\nwith a default path defined in `luaconf.h`,\nif the environment variable is not defined.\nAny `\";;\"` in the value of the environment variable\nis replaced by the default path."}}, preload = {binding = "package.preload", metadata = {["fnl/docstring"] = "A table to store loaders for specific modules\n(see `require`)."}}, seeall = {binding = "package.seeall", metadata = {["fnl/arglist"] = {"module"}, ["fnl/docstring"] = "Sets a metatable for `module` with\nits `__index` field referring to the global environment,\nso that this module inherits values\nfrom the global environment.\nTo be used as an option to function `module`."}}}, metadata = {["fnl/docstring"] = "The package library provides basic\nfacilities for loading and building modules in Lua.\nIt exports two of its functions directly in the global environment:\n`require` and `module`.\nEverything else is exported in a table `package`."}}, pairs = {binding = "pairs", metadata = {["fnl/arglist"] = {"t"}, ["fnl/docstring"] = "Returns three values: the `next` function, the table `t`, and **nil**,\nso that the construction\n\n```lua\n     for k,v in pairs(t) do *body* end\n```\nwill iterate over all key\226\128\147value pairs of table `t`.\n\nSee function `next` for the caveats of modifying\nthe table during its traversal."}}, pcall = {binding = "pcall", metadata = {["fnl/arglist"] = {"f", "arg1", "..."}, ["fnl/docstring"] = "Calls function `f` with\nthe given arguments in *protected mode*.\nThis means that any error inside `f` is not propagated;\ninstead, `pcall` catches the error\nand returns a status code.\nIts first result is the status code (a boolean),\nwhich is true if the call succeeds without errors.\nIn such case, `pcall` also returns all results from the call,\nafter this first result.\nIn case of any error, `pcall` returns **false** plus the error message."}}, print = {binding = "print", metadata = {["fnl/arglist"] = {"..."}, ["fnl/docstring"] = "Receives any number of arguments,\nand prints their values to `stdout`,\nusing the `tostring` function to convert them to strings.\n`print` is not intended for formatted output,\nbut only as a quick way to show a value,\ntypically for debugging.\nFor formatted output, use `string.format`."}}, rawequal = {binding = "rawequal", metadata = {["fnl/arglist"] = {"v1", "v2"}, ["fnl/docstring"] = "Checks whether `v1` is equal to `v2`,\nwithout invoking any metamethod.\nReturns a boolean."}}, rawget = {binding = "rawget", metadata = {["fnl/arglist"] = {"table", "index"}, ["fnl/docstring"] = "Gets the real value of `table[index]`,\nwithout invoking any metamethod.\n`table` must be a table;\n`index` may be any value."}}, rawset = {binding = "rawset", metadata = {["fnl/arglist"] = {"table", "index", "value"}, ["fnl/docstring"] = "Sets the real value of `table[index]` to `value`,\nwithout invoking any metamethod.\n`table` must be a table,\n`index` any value different from **nil**,\nand `value` any Lua value.\n\nThis function returns `table`."}}, require = {binding = "require", metadata = {["fnl/arglist"] = {"modname"}, ["fnl/docstring"] = "Loads the given module.\nThe function starts by looking into the `package.loaded` table\nto determine whether `modname` is already loaded.\nIf it is, then `require` returns the value stored\nat `package.loaded[modname]`.\nOtherwise, it tries to find a *loader* for the module.\n\nTo find a loader,\n`require` is guided by the `package.loaders` array.\nBy changing this array,\nwe can change how `require` looks for a module.\nThe following explanation is based on the default configuration\nfor `package.loaders`.\n\nFirst `require` queries `package.preload[modname]`.\nIf it has a value,\nthis value (which should be a function) is the loader.\nOtherwise `require` searches for a Lua loader using the\npath stored in `package.path`.\nIf that also fails, it searches for a C loader using the\npath stored in `package.cpath`.\nIf that also fails,\nit tries an *all-in-one* loader (see `package.loaders`).\n\nOnce a loader is found,\n`require` calls the loader with a single argument, `modname`.\nIf the loader returns any value,\n`require` assigns the returned value to `package.loaded[modname]`.\nIf the loader returns no value and\nhas not assigned any value to `package.loaded[modname]`,\nthen `require` assigns **true** to this entry.\nIn any case, `require` returns the\nfinal value of `package.loaded[modname]`.\n\nIf there is any error loading or running the module,\nor if it cannot find any loader for the module,\nthen `require` signals an error."}}, select = {binding = "select", metadata = {["fnl/arglist"] = {"index", "..."}, ["fnl/docstring"] = "If `index` is a number,\nreturns all arguments after argument number `index`.\nOtherwise, `index` must be the string `\"#\"`,\nand `select` returns the total number of extra arguments it received."}}, setfenv = {binding = "setfenv", metadata = {["fnl/arglist"] = {"f", "table"}, ["fnl/docstring"] = "Sets the environment to be used by the given function.\n`f` can be a Lua function or a number\nthat specifies the function at that stack level:\nLevel 1 is the function calling `setfenv`.\n`setfenv` returns the given function.\n\nAs a special case, when `f` is 0 `setfenv` changes\nthe environment of the running thread.\nIn this case, `setfenv` returns no values."}}, setmetatable = {binding = "setmetatable", metadata = {["fnl/arglist"] = {"table", "metatable"}, ["fnl/docstring"] = "Sets the metatable for the given table.\n(You cannot change the metatable of other types from Lua, only from C.)\nIf `metatable` is **nil**,\nremoves the metatable of the given table.\nIf the original metatable has a `\"__metatable\"` field,\nraises an error.\n\nThis function returns `table`."}}, string = {binding = "string", fields = {byte = {binding = "string.byte", metadata = {["fnl/arglist"] = {"s", "?i", "?j"}, ["fnl/docstring"] = "Returns the internal numerical codes of the characters `s[i]`,\n`s[i+1]`, ..., `s[j]`.\nThe default value for `?i` is 1;\nthe default value for `?j` is `?i`.\n\nNote that numerical codes are not necessarily portable across platforms."}}, char = {binding = "string.char", metadata = {["fnl/arglist"] = {"..."}, ["fnl/docstring"] = "Receives zero or more integers.\nReturns a string with length equal to the number of arguments,\nin which each character has the internal numerical code equal\nto its corresponding argument.\n\nNote that numerical codes are not necessarily portable across platforms."}}, dump = {binding = "string.dump", metadata = {["fnl/arglist"] = {"function"}, ["fnl/docstring"] = "Returns a string containing a binary representation of the given function,\nso that a later `loadstring` on this string returns\na copy of the function.\n`function` must be a Lua function without upvalues."}}, find = {binding = "string.find", metadata = {["fnl/arglist"] = {"s", "pattern", "?init", "?plain"}, ["fnl/docstring"] = "Looks for the first match of\n`pattern` in the string `s`.\nIf it finds a match, then `find` returns the indices of `s`\nwhere this occurrence starts and ends;\notherwise, it returns **nil**.\nA third, optional numerical argument `?init` specifies\nwhere to start the search;\nits default value is 1 and can be negative.\nA value of **true** as a fourth, optional argument `?plain`\nturns off the pattern matching facilities,\nso the function does a plain \"find substring\" operation,\nwith no characters in `pattern` being considered \"magic\".\nNote that if `?plain` is given, then `?init` must be given as well.\n\nIf the pattern has captures,\nthen in a successful match\nthe captured values are also returned,\nafter the two indices."}}, format = {binding = "string.format", metadata = {["fnl/arglist"] = {"formatstring", "..."}, ["fnl/docstring"] = "Returns a formatted version of its variable number of arguments\nfollowing the description given in its first argument (which must be a string).\nThe format string follows the same rules as the `printf` family of\nstandard C functions.\nThe only differences are that the options/modifiers\n`*`, `l`, `L`, `n`, `p`,\nand `h` are not supported\nand that there is an extra option, `q`.\nThe `q` option formats a string in a form suitable to be safely read\nback by the Lua interpreter:\nthe string is written between double quotes,\nand all double quotes, newlines, embedded zeros,\nand backslashes in the string\nare correctly escaped when written.\nFor instance, the call\n\n```lua\n     string.format('%q', 'a string with \"quotes\" and \\n new line')\n```\nwill produce the string:\n\n```lua\n     \"a string with \\\"quotes\\\" and \\\n      new line\"\n```\n\nThe options `c`, `d`, `E`, `e`, `f`,\n`g`, `G`, `i`, `o`, `u`, `X`, and `x` all\nexpect a number as argument,\nwhereas `q` and `s` expect a string.\n\nThis function does not accept string values\ncontaining embedded zeros,\nexcept as arguments to the `q` option."}}, gmatch = {binding = "string.gmatch", metadata = {["fnl/arglist"] = {"s", "pattern"}, ["fnl/docstring"] = "Returns an iterator function that,\neach time it is called,\nreturns the next captures from `pattern` over string `s`.\nIf `pattern` specifies no captures,\nthen the whole match is produced in each call.\n\nAs an example, the following loop\n\n```lua\n     s = \"hello world from Lua\"\n     for w in string.gmatch(s, \"%a+\") do\n       print(w)\n     end\n```\nwill iterate over all the words from string `s`,\nprinting one per line.\nThe next example collects all pairs `key=value` from the\ngiven string into a table:\n\n```lua\n     t = {}\n     s = \"from=world, to=Lua\"\n     for k, v in string.gmatch(s, \"(%w+)=(%w+)\") do\n       t[k] = v\n     end\n```\n\nFor this function, a `\"^\"` at the start of a pattern does not\nwork as an anchor, as this would prevent the iteration."}}, gsub = {binding = "string.gsub", metadata = {["fnl/arglist"] = {"s", "pattern", "repl", "?n"}, ["fnl/docstring"] = "Returns a copy of `s`\nin which all (or the first `?n`, if given)\noccurrences of the `pattern` have been\nreplaced by a replacement string specified by `repl`,\nwhich can be a string, a table, or a function.\n`gsub` also returns, as its second value,\nthe total number of matches that occurred.\n\nIf `repl` is a string, then its value is used for replacement.\nThe character `%` works as an escape character:\nany sequence in `repl` of the form `%*n*`,\nwith *n* between 1 and 9,\nstands for the value of the *n*-th captured substring (see below).\nThe sequence `%0` stands for the whole match.\nThe sequence `%%` stands for a single `%`.\n\nIf `repl` is a table, then the table is queried for every match,\nusing the first capture as the key;\nif the pattern specifies no captures,\nthen the whole match is used as the key.\n\nIf `repl` is a function, then this function is called every time a\nmatch occurs, with all captured substrings passed as arguments,\nin order;\nif the pattern specifies no captures,\nthen the whole match is passed as a sole argument.\n\nIf the value returned by the table query or by the function call\nis a string or a number,\nthen it is used as the replacement string;\notherwise, if it is **false** or **nil**,\nthen there is no replacement\n(that is, the original match is kept in the string).\n\nHere are some examples:\n\n```lua\n     x = string.gsub(\"hello world\", \"(%w+)\", \"%1 %1\")\n     --> x=\"hello hello world world\"\n     \n     x = string.gsub(\"hello world\", \"%w+\", \"%0 %0\", 1)\n     --> x=\"hello hello world\"\n     \n     x = string.gsub(\"hello world from Lua\", \"(%w+)%s*(%w+)\", \"%2 %1\")\n     --> x=\"world hello Lua from\"\n     \n     x = string.gsub(\"home = $HOME, user = $USER\", \"%$(%w+)\", os.getenv)\n     --> x=\"home = /home/roberto, user = roberto\"\n     \n     x = string.gsub(\"4+5 = $return 4+5$\", \"%$(.-)%$\", function (s)\n           return loadstring(s)()\n         end)\n     --> x=\"4+5 = 9\"\n     \n     local t = {name=\"lua\", version=\"5.1\"}\n     x = string.gsub(\"$name-$version.tar.gz\", \"%$(%w+)\", t)\n     --> x=\"lua-5.1.tar.gz\"\n```"}}, len = {binding = "string.len", metadata = {["fnl/arglist"] = {"s"}, ["fnl/docstring"] = "Receives a string and returns its length.\nThe empty string `\"\"` has length 0.\nEmbedded zeros are counted,\nso `\"a\\000bc\\000\"` has length 5."}}, lower = {binding = "string.lower", metadata = {["fnl/arglist"] = {"s"}, ["fnl/docstring"] = "Receives a string and returns a copy of this string with all\nuppercase letters changed to lowercase.\nAll other characters are left unchanged.\nThe definition of what an uppercase letter is depends on the current locale."}}, match = {binding = "string.match", metadata = {["fnl/arglist"] = {"s", "pattern", "?init"}, ["fnl/docstring"] = "Looks for the first *match* of\n`pattern` in the string `s`.\nIf it finds one, then `match` returns\nthe captures from the pattern;\notherwise it returns **nil**.\nIf `pattern` specifies no captures,\nthen the whole match is returned.\nA third, optional numerical argument `?init` specifies\nwhere to start the search;\nits default value is 1 and can be negative."}}, rep = {binding = "string.rep", metadata = {["fnl/arglist"] = {"s", "n"}, ["fnl/docstring"] = "Returns a string that is the concatenation of `n` copies of\nthe string `s`."}}, reverse = {binding = "string.reverse", metadata = {["fnl/arglist"] = {"s"}, ["fnl/docstring"] = "Returns a string that is the string `s` reversed."}}, sub = {binding = "string.sub", metadata = {["fnl/arglist"] = {"s", "i", "?j"}, ["fnl/docstring"] = "Returns the substring of `s` that\nstarts at `i`  and continues until `?j`;\n`i` and `?j` can be negative.\nIf `?j` is absent, then it is assumed to be equal to -1\n(which is the same as the string length).\nIn particular,\nthe call `string.sub(s,1,j)` returns a prefix of `s`\nwith length `?j`,\nand `string.sub(s, -i)` returns a suffix of `s`\nwith length `i`."}}, upper = {binding = "string.upper", metadata = {["fnl/arglist"] = {"s"}, ["fnl/docstring"] = "Receives a string and returns a copy of this string with all\nlowercase letters changed to uppercase.\nAll other characters are left unchanged.\nThe definition of what a lowercase letter is depends on the current locale."}}}, metadata = {["fnl/docstring"] = "This library provides generic functions for string manipulation,\nsuch as finding and extracting substrings, and pattern matching.\nWhen indexing a string in Lua, the first character is at position 1\n(not at 0, as in C).\nIndices are allowed to be negative and are interpreted as indexing backwards,\nfrom the end of the string.\nThus, the last character is at position -1, and so on.\n\nThe string library provides all its functions inside the table\n`string`.\nIt also sets a metatable for strings\nwhere the `__index` field points to the `string` table.\nTherefore, you can use the string functions in object-oriented style.\nFor instance, `string.byte(s, i)`\ncan be written as `s:byte(i)`.\n\nThe string library assumes one-byte character encodings."}}, table = {binding = "table", fields = {concat = {binding = "table.concat", metadata = {["fnl/arglist"] = {"table", "?sep", "?i", "?j"}, ["fnl/docstring"] = "Given an array where all elements are strings or numbers,\nreturns `table[i]..sep..table[i+1] ... sep..table[j]`.\nThe default value for `?sep` is the empty string,\nthe default for `?i` is 1,\nand the default for `?j` is the length of the table.\nIf `?i` is greater than `?j`, returns the empty string."}}, insert = {binding = "table.insert", metadata = {["fnl/arglist"] = {"table", "value"}, ["fnl/docstring"] = "Inserts element `value` at position `pos` in `table`,\nshifting up other elements to open space, if necessary.\nThe default value for `pos` is `n+1`,\nwhere `n` is the length of the table\nso that a call `table.insert(t,x)` inserts `x` at the end\nof table `t`."}}, maxn = {binding = "table.maxn", metadata = {["fnl/arglist"] = {"table"}, ["fnl/docstring"] = "Returns the largest positive numerical index of the given table,\nor zero if the table has no positive numerical indices.\n(To do its job this function does a linear traversal of\nthe whole table.)"}}, remove = {binding = "table.remove", metadata = {["fnl/arglist"] = {"table", "?pos"}, ["fnl/docstring"] = "Removes from `table` the element at position `?pos`,\nshifting down other elements to close the space, if necessary.\nReturns the value of the removed element.\nThe default value for `?pos` is `n`,\nwhere `n` is the length of the table,\nso that a call `table.remove(t)` removes the last element\nof table `t`."}}, sort = {binding = "table.sort", metadata = {["fnl/arglist"] = {"table", "?comp"}, ["fnl/docstring"] = "Sorts table elements in a given order, *in-place*,\nfrom `table[1]` to `table[n]`,\nwhere `n` is the length of the table.\nIf `?comp` is given,\nthen it must be a function that receives two table elements,\nand returns true\nwhen the first is less than the second\n(so that `not comp(a[i+1],a[i])` will be true after the sort).\nIf `?comp` is not given,\nthen the standard Lua operator `<` is used instead.\n\nThe sort algorithm is not stable;\nthat is, elements considered equal by the given order\nmay have their relative positions changed by the sort."}}}, metadata = {["fnl/docstring"] = "This library provides generic functions for table manipulation.\nIt provides all its functions inside the table `table`.\n\nMost functions in the table library assume that the table\nrepresents an array or a list.\nFor these functions, when we talk about the \"length\" of a table\nwe mean the result of the length operator."}}, tonumber = {binding = "tonumber", metadata = {["fnl/arglist"] = {"e", "?base"}, ["fnl/docstring"] = "Tries to convert its argument to a number.\nIf the argument is already a number or a string convertible\nto a number, then `tonumber` returns this number;\notherwise, it returns **nil**.\n\nAn optional argument specifies the base to interpret the numeral.\nThe base may be any integer between 2 and 36, inclusive.\nIn bases above 10, the letter `\"A\"` (in either upper or lower case)\nrepresents 10, `\"B\"` represents 11, and so forth,\nwith `\"Z\"` representing 35.\nIn base 10 (the default), the number can have a decimal part,\nas well as an optional exponent part\nIn other bases, only unsigned integers are accepted."}}, tostring = {binding = "tostring", metadata = {["fnl/arglist"] = {"e"}, ["fnl/docstring"] = "Receives an argument of any type and\nconverts it to a string in a reasonable format.\nFor complete control of how numbers are converted,\nuse `string.format`.\n\nIf the metatable of `e` has a `\"__tostring\"` field,\nthen `tostring` calls the corresponding value\nwith `e` as argument,\nand uses the result of the call as its result."}}, type = {binding = "type", metadata = {["fnl/arglist"] = {"v"}, ["fnl/docstring"] = "Returns the type of its only argument, coded as a string.\nThe possible results of this function are\n`\"nil\"` (a string, not the value **nil**),\n`\"number\"`,\n`\"string\"`,\n`\"boolean\"`,\n`\"table\"`,\n`\"function\"`,\n`\"thread\"`,\nand `\"userdata\"`."}}, unpack = {binding = "unpack", metadata = {["fnl/arglist"] = {"list", "?i", "?j"}, ["fnl/docstring"] = "Returns the elements from the given table.\nThis function is equivalent to\n\n```lua\n     return list[i], list[i+1], ..., list[j]\n```\nexcept that the above code can be written only for a fixed number\nof elements.\nBy default, `?i` is 1 and `?j` is the length of the list,\nas defined by the length operator"}}, xpcall = {binding = "xpcall", metadata = {["fnl/arglist"] = {"f", "err"}, ["fnl/docstring"] = "This function is similar to `pcall`,\nexcept that you can set a new error handler.\n\n`xpcall` calls function `f` in protected mode,\nusing `err` as the error handler.\nAny error inside `f` is not propagated;\ninstead, `xpcall` catches the error,\ncalls the `err` function with the original error object,\nand returns a status code.\nIts first result is the status code (a boolean),\nwhich is true if the call succeeds without errors.\nIn this case, `xpcall` also returns all results from the call,\nafter this first result.\nIn case of any error,\n`xpcall` returns **false** plus the result from `err`."}}}
  docs._G.fields = docs
  docs.io.fields.stdin.fields = docs.io.fields
  docs.io.fields.stdout.fields = docs.io.fields
  docs.io.fields.stderr.fields = docs.io.fields
  return docs
end
package.preload["fennel-ls.docs.generated.lua52"] = package.preload["fennel-ls.docs.generated.lua52"] or function(...)
  local docs = {_G = {binding = "_G", metadata = {["fnl/docstring"] = "A global variable (not a function) that\nholds the global environment\nLua itself does not use this variable;\nchanging its value does not affect any environment,\nnor vice-versa."}}, _VERSION = {binding = "_VERSION", metadata = {["fnl/docstring"] = "A global variable (not a function) that\nholds a string containing the current interpreter version.\nThe current contents of this variable is `\"Lua 5.2\"`."}}, arg = {binding = "arg", metadata = {["fnl/docstring"] = "Before starting to run the script,\n`lua` collects all arguments in the command line\nin a global table called `arg`.\nThe script name is stored at index 0,\nthe first argument after the script name goes to index 1,\nand so on.\nAny arguments before the script name\n(that is, the interpreter name plus the options)\ngo to negative indices.\nFor instance, in the call\n\n```lua\n     $ lua -la b.lua t1 t2\n```\nthe interpreter first runs the file `a.lua`,\nthen creates a table\n\n```lua\n     arg = { [-2] = \"lua\", [-1] = \"-la\",\n             [0] = \"b.lua\",\n             [1] = \"t1\", [2] = \"t2\" }\n```\nand finally runs the file `b.lua`.\nThe script is called with `arg[1]`, `arg[2]`, ...\nas arguments;\nit can also access these arguments with the vararg expression `\"...\"`."}}, assert = {binding = "assert", metadata = {["fnl/arglist"] = {"v", "?message"}, ["fnl/docstring"] = "Issues an  error when\nthe value of its argument `v` is false (i.e., **nil** or **false**);\notherwise, returns all its arguments.\n`?message` is an error message;\nwhen absent, it defaults to \"assertion failed!\""}}, bit32 = {binding = "bit32", fields = {arshift = {binding = "bit32.arshift", metadata = {["fnl/arglist"] = {"x", "disp"}, ["fnl/docstring"] = "Returns the number `x` shifted `disp` bits to the right.\nThe number `disp` may be any representable integer.\nNegative displacements shift to the left.\n\nThis shift operation is what is called arithmetic shift.\nVacant bits on the left are filled\nwith copies of the higher bit of `x`;\nvacant bits on the right are filled with zeros.\nIn particular,\ndisplacements with absolute values higher than 31\nresult in zero or `0xFFFFFFFF` (all original bits are shifted out)."}}, band = {binding = "bit32.band", metadata = {["fnl/arglist"] = {"..."}, ["fnl/docstring"] = "Returns the bitwise *and* of its operands."}}, bnot = {binding = "bit32.bnot", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the bitwise negation of `x`.\nFor any integer `x`,\nthe following identity holds:\n\n```lua\n     assert(bit32.bnot(x) == (-1 - x) % 2^32)\n```"}}, bor = {binding = "bit32.bor", metadata = {["fnl/arglist"] = {"..."}, ["fnl/docstring"] = "Returns the bitwise *or* of its operands."}}, btest = {binding = "bit32.btest", metadata = {["fnl/arglist"] = {"..."}, ["fnl/docstring"] = "Returns a boolean signaling\nwhether the bitwise *and* of its operands is different from zero."}}, bxor = {binding = "bit32.bxor", metadata = {["fnl/arglist"] = {"..."}, ["fnl/docstring"] = "Returns the bitwise *exclusive or* of its operands."}}, extract = {binding = "bit32.extract", metadata = {["fnl/arglist"] = {"n", "field", "?width"}, ["fnl/docstring"] = "Returns the unsigned number formed by the bits\n`field` to `field + width - 1` from `n`.\nBits are numbered from 0 (least significant) to 31 (most significant).\nAll accessed bits must be in the range *[0, 31]*.\n\nThe default for `?width` is 1."}}, lrotate = {binding = "bit32.lrotate", metadata = {["fnl/arglist"] = {"x", "disp"}, ["fnl/docstring"] = "Returns the number `x` rotated `disp` bits to the left.\nThe number `disp` may be any representable integer.\n\nFor any valid displacement,\nthe following identity holds:\n\n```lua\n     assert(bit32.lrotate(x, disp) == bit32.lrotate(x, disp % 32))\n```\nIn particular,\nnegative displacements rotate to the right."}}, lshift = {binding = "bit32.lshift", metadata = {["fnl/arglist"] = {"x", "disp"}, ["fnl/docstring"] = "Returns the number `x` shifted `disp` bits to the left.\nThe number `disp` may be any representable integer.\nNegative displacements shift to the right.\nIn any direction, vacant bits are filled with zeros.\nIn particular,\ndisplacements with absolute values higher than 31\nresult in zero (all bits are shifted out).\n\nFor positive displacements,\nthe following equality holds:\n\n```lua\n     assert(bit32.lshift(b, disp) == (b * 2^disp) % 2^32)\n```"}}, replace = {binding = "bit32.replace", metadata = {["fnl/arglist"] = {"n", "v", "field", "?width"}, ["fnl/docstring"] = "Returns a copy of `n` with\nthe bits `field` to `field + width - 1`\nreplaced by the value `v`.\nSee `bit32.extract` for details about `field` and `?width`."}}, rrotate = {binding = "bit32.rrotate", metadata = {["fnl/arglist"] = {"x", "disp"}, ["fnl/docstring"] = "Returns the number `x` rotated `disp` bits to the right.\nThe number `disp` may be any representable integer.\n\nFor any valid displacement,\nthe following identity holds:\n\n```lua\n     assert(bit32.rrotate(x, disp) == bit32.rrotate(x, disp % 32))\n```\nIn particular,\nnegative displacements rotate to the left."}}, rshift = {binding = "bit32.rshift", metadata = {["fnl/arglist"] = {"x", "disp"}, ["fnl/docstring"] = "Returns the number `x` shifted `disp` bits to the right.\nThe number `disp` may be any representable integer.\nNegative displacements shift to the left.\nIn any direction, vacant bits are filled with zeros.\nIn particular,\ndisplacements with absolute values higher than 31\nresult in zero (all bits are shifted out).\n\nFor positive displacements,\nthe following equality holds:\n\n```lua\n     assert(bit32.rshift(b, disp) == math.floor(b % 2^32 / 2^disp))\n```\n\nThis shift operation is what is called logical shift."}}}, metadata = {["fnl/docstring"] = "This library provides bitwise operations.\nIt provides all its functions inside the table `bit32`.\n\nUnless otherwise stated,\nall functions accept numeric arguments in the range\n*(-2\226\129\181\194\185,+2\226\129\181\194\185)*;\neach argument is normalized to\nthe remainder of its division by *2\194\179\194\178*\nand truncated to an integer (in some unspecified way),\nso that its final value falls in the range *[0,2\194\179\194\178 - 1]*.\nSimilarly, all results are in the range *[0,2\194\179\194\178 - 1]*.\nNote that `bit32.bnot(0)` is `0xFFFFFFFF`,\nwhich is different from `-1`."}}, collectgarbage = {binding = "collectgarbage", metadata = {["fnl/arglist"] = {"?opt", "?arg"}, ["fnl/docstring"] = "This function is a generic interface to the garbage collector.\nIt performs different functions according to its first argument, `?opt`:\n\n* **`\"collect\"`: **\n  performs a full garbage-collection cycle.\n  This is the default option.\n\n* **`\"stop\"`: **\n  stops automatic execution of the garbage collector.\n  The collector will run only when explicitly invoked,\n  until a call to restart it.\n\n* **`\"restart\"`: **\n  restarts automatic execution of the garbage collector.\n\n* **`\"count\"`: **\n  returns the total memory in use by Lua (in Kbytes) and\n  a second value with the total memory in bytes modulo 1024.\n  The first value has a fractional part,\n  so the following equality is always true:\n  \n  ```lua\n       k, b = collectgarbage(\"count\")\n       assert(k*1024 == math.floor(k)*1024 + b)\n  ```\n  (The second result is useful when Lua is compiled\n  with a non floating-point type for numbers.)\n\n* **`\"step\"`: **\n  performs a garbage-collection step.\n  The step \"size\" is controlled by `?arg`\n  (larger values mean more steps) in a non-specified way.\n  If you want to control the step size\n  you must experimentally tune the value of `?arg`.\n  Returns **true** if the step finished a collection cycle.\n\n* **`\"setpause\"`: **\n  sets `?arg` as the new value for the *pause* of\n  the collector\n  Returns the previous value for *pause*.\n\n* **`\"setstepmul\"`: **\n  sets `?arg` as the new value for the *step multiplier* of\n  the collector\n  Returns the previous value for *step*.\n\n* **`\"isrunning\"`: **\n  returns a boolean that tells whether the collector is running\n  (i.e., not stopped).\n\n* **`\"generational\"`: **\n  changes the collector to generational mode.\n  This is an experimental feature\n\n* **`\"incremental\"`: **\n  changes the collector to incremental mode.\n  This is the default mode."}}, coroutine = {binding = "coroutine", fields = {create = {binding = "coroutine.create", metadata = {["fnl/arglist"] = {"f"}, ["fnl/docstring"] = "Creates a new coroutine, with body `f`.\n`f` must be a Lua function.\nReturns this new coroutine,\nan object with type `\"thread\"`."}}, resume = {binding = "coroutine.resume", metadata = {["fnl/arglist"] = {"co", "?val1", "..."}, ["fnl/docstring"] = "Starts or continues the execution of coroutine `co`.\nThe first time you resume a coroutine,\nit starts running its body.\nThe values `?val1`, ... are passed\nas the arguments to the body function.\nIf the coroutine has yielded,\n`resume` restarts it;\nthe values `?val1`, ... are passed\nas the results from the yield.\n\nIf the coroutine runs without any errors,\n`resume` returns **true** plus any values passed to `yield`\n(if the coroutine yields) or any values returned by the body function\n(if the coroutine terminates).\nIf there is any error,\n`resume` returns **false** plus the error message."}}, running = {binding = "coroutine.running", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Returns the running coroutine plus a boolean,\ntrue when the running coroutine is the main one."}}, status = {binding = "coroutine.status", metadata = {["fnl/arglist"] = {"co"}, ["fnl/docstring"] = "Returns the status of coroutine `co`, as a string:\n`\"running\"`,\nif the coroutine is running (that is, it called `status`);\n`\"suspended\"`, if the coroutine is suspended in a call to `yield`,\nor if it has not started running yet;\n`\"normal\"` if the coroutine is active but not running\n(that is, it has resumed another coroutine);\nand `\"dead\"` if the coroutine has finished its body function,\nor if it has stopped with an error."}}, wrap = {binding = "coroutine.wrap", metadata = {["fnl/arglist"] = {"f"}, ["fnl/docstring"] = "Creates a new coroutine, with body `f`.\n`f` must be a Lua function.\nReturns a function that resumes the coroutine each time it is called.\nAny arguments passed to the function behave as the\nextra arguments to `resume`.\nReturns the same values returned by `resume`,\nexcept the first boolean.\nIn case of error, propagates the error."}}, yield = {binding = "coroutine.yield", metadata = {["fnl/arglist"] = {"..."}, ["fnl/docstring"] = "Suspends the execution of the calling coroutine.\nAny arguments to `yield` are passed as extra results to `resume`."}}}, metadata = {["fnl/docstring"] = "The operations related to coroutines comprise a sub-library of\nthe basic library and come inside the table `coroutine`."}}, debug = {binding = "debug", fields = {debug = {binding = "debug.debug", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Enters an interactive mode with the user,\nrunning each string that the user enters.\nUsing simple commands and other debug facilities,\nthe user can inspect global and local variables,\nchange their values, evaluate expressions, and so on.\nA line containing only the word `cont` finishes this function,\nso that the caller continues its execution.\n\nNote that commands for `debug.debug` are not lexically nested\nwithin any function and so have no direct access to local variables."}}, gethook = {binding = "debug.gethook", metadata = {["fnl/arglist"] = {"?thread"}, ["fnl/docstring"] = "Returns the current hook settings of the thread, as three values:\nthe current hook function, the current hook mask,\nand the current hook count\n(as set by the `debug.sethook` function)."}}, getinfo = {binding = "debug.getinfo", metadata = {["fnl/arglist"] = {"f", "?what"}, ["fnl/docstring"] = "Returns a table with information about a function.\nYou can give the function directly\nor you can give a number as the value of `f`,\nwhich means the function running at level `f` of the call stack\nof the given thread:\nlevel 0 is the current function (`getinfo` itself);\nlevel 1 is the function that called `getinfo`\n(except for tail calls, which do not count on the stack);\nand so on.\nIf `f` is a number larger than the number of active functions,\nthen `getinfo` returns **nil**.\n\nThe returned table can contain all the fields returned by `lua_getinfo`,\nwith the string `?what` describing which fields to fill in.\nThe default for `?what` is to get all information available,\nexcept the table of valid lines.\nIf present,\nthe option `\"f\"`\nadds a field named `func` with the function itself.\nIf present,\nthe option `\"L\"`\nadds a field named `activelines` with the table of\nvalid lines.\n\nFor instance, the expression `debug.getinfo(1,\"n\").name` returns\na table with a name for the current function,\nif a reasonable name can be found,\nand the expression `debug.getinfo(print)`\nreturns a table with all available information\nabout the `print` function."}}, getlocal = {binding = "debug.getlocal", metadata = {["fnl/arglist"] = {"f", "local"}, ["fnl/docstring"] = "This function returns the name and the value of the local variable\nwith index `local` of the function at level `f` of the stack.\nThis function accesses not only explicit local variables,\nbut also parameters, temporaries, etc.\n\nThe first parameter or local variable has index 1, and so on,\nuntil the last active variable.\nNegative indices refer to vararg parameters;\n-1 is the first vararg parameter.\nThe function returns **nil** if there is no variable with the given index,\nand raises an error when called with a level out of range.\n(You can call `debug.getinfo` to check whether the level is valid.)\n\nVariable names starting with `\"(\"` (open parenthesis)\nrepresent internal variables\n(loop control variables, temporaries, varargs, and C function locals).\n\nThe parameter `f` may also be a function.\nIn that case, `getlocal` returns only the name of function parameters."}}, getmetatable = {binding = "debug.getmetatable", metadata = {["fnl/arglist"] = {"value"}, ["fnl/docstring"] = "Returns the metatable of the given `value`\nor **nil** if it does not have a metatable."}}, getregistry = {binding = "debug.getregistry", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Returns the registry table"}}, getupvalue = {binding = "debug.getupvalue", metadata = {["fnl/arglist"] = {"f", "up"}, ["fnl/docstring"] = "This function returns the name and the value of the upvalue\nwith index `up` of the function `f`.\nThe function returns **nil** if there is no upvalue with the given index."}}, getuservalue = {binding = "debug.getuservalue", metadata = {["fnl/arglist"] = {"u"}, ["fnl/docstring"] = "Returns the Lua value associated to `u`.\nIf `u` is not a userdata,\nreturns **nil**."}}, sethook = {binding = "debug.sethook", metadata = {["fnl/arglist"] = {"hook", "mask", "?count"}, ["fnl/docstring"] = "Sets the given function as a hook.\nThe string `mask` and the number `?count` describe\nwhen the hook will be called.\nThe string mask may have any combination of the following characters,\nwith the given meaning:\n\n* **`\"c\"`: ** the hook is called every time Lua calls a function;\n* **`\"r\"`: ** the hook is called every time Lua returns from a function;\n* **`\"l\"`: ** the hook is called every time Lua enters a new line of code.\n\nMoreover,\nwith a `?count` different from zero,\nthe hook is called also after every `?count` instructions.\n\nWhen called without arguments,\n`debug.sethook` turns off the hook.\n\nWhen the hook is called, its first parameter is a string\ndescribing the event that has triggered its call:\n`\"call\"` (or `\"tail call\"`),\n`\"return\"`,\n`\"line\"`, and `\"count\"`.\nFor line events,\nthe hook also gets the new line number as its second parameter.\nInside a hook,\nyou can call `getinfo` with level 2 to get more information about\nthe running function\n(level 0 is the `getinfo` function,\nand level 1 is the hook function)."}}, setlocal = {binding = "debug.setlocal", metadata = {["fnl/arglist"] = {"level", "local", "value"}, ["fnl/docstring"] = "This function assigns the value `value` to the local variable\nwith index `local` of the function at level `level` of the stack.\nThe function returns **nil** if there is no local\nvariable with the given index,\nand raises an error when called with a `level` out of range.\n(You can call `getinfo` to check whether the level is valid.)\nOtherwise, it returns the name of the local variable.\n\nSee `debug.getlocal` for more information about\nvariable indices and names."}}, setmetatable = {binding = "debug.setmetatable", metadata = {["fnl/arglist"] = {"value", "table"}, ["fnl/docstring"] = "Sets the metatable for the given `value` to the given `table`\n(which can be **nil**).\nReturns `value`."}}, setupvalue = {binding = "debug.setupvalue", metadata = {["fnl/arglist"] = {"f", "up", "value"}, ["fnl/docstring"] = "This function assigns the value `value` to the upvalue\nwith index `up` of the function `f`.\nThe function returns **nil** if there is no upvalue\nwith the given index.\nOtherwise, it returns the name of the upvalue."}}, setuservalue = {binding = "debug.setuservalue", metadata = {["fnl/arglist"] = {"udata", "value"}, ["fnl/docstring"] = "Sets the given `value` as\nthe Lua value associated to the given `udata`.\n`value` must be a table or **nil**;\n`udata` must be a full userdata.\n\nReturns `udata`."}}, traceback = {binding = "debug.traceback", metadata = {["fnl/arglist"] = {"?message", "?level"}, ["fnl/docstring"] = "If `?message` is present but is neither a string nor **nil**,\nthis function returns `?message` without further processing.\nOtherwise,\nit returns a string with a traceback of the call stack.\nAn optional `?message` string is appended\nat the beginning of the traceback.\nAn optional `?level` number tells at which level\nto start the traceback\n(default is 1, the function calling `traceback`)."}}, upvalueid = {binding = "debug.upvalueid", metadata = {["fnl/arglist"] = {"f", "n"}, ["fnl/docstring"] = "Returns an unique identifier (as a light userdata)\nfor the upvalue numbered `n`\nfrom the given function.\n\nThese unique identifiers allow a program to check whether different\nclosures share upvalues.\nLua closures that share an upvalue\n(that is, that access a same external local variable)\nwill return identical ids for those upvalue indices."}}, upvaluejoin = {binding = "debug.upvaluejoin", metadata = {["fnl/arglist"] = {"f1", "n1", "f2", "n2"}, ["fnl/docstring"] = "Make the `n1`-th upvalue of the Lua closure `f1`\nrefer to the `n2`-th upvalue of the Lua closure `f2`."}}}, metadata = {["fnl/docstring"] = "This library provides\nthe functionality of the debug interfaceto Lua programs.\nYou should exert care when using this library.\nSeveral of its functions\nviolate basic assumptions about Lua code\n(e.g., that variables local to a function\ncannot be accessed from outside;\nthat userdata metatables cannot be changed by Lua code;\nthat Lua programs do not crash)\nand therefore can compromise otherwise secure code.\nMoreover, some functions in this library may be slow.\n\nAll functions in this library are provided\ninside the `debug` table.\nAll functions that operate over a thread\nhave an optional first argument which is the\nthread to operate over.\nThe default is always the current thread."}}, dofile = {binding = "dofile", metadata = {["fnl/arglist"] = {"?filename"}, ["fnl/docstring"] = "Opens the named file and executes its contents as a Lua chunk.\nWhen called without arguments,\n`dofile` executes the contents of the standard input (`stdin`).\nReturns all values returned by the chunk.\nIn case of errors, `dofile` propagates the error\nto its caller (that is, `dofile` does not run in protected mode)."}}, error = {binding = "error", metadata = {["fnl/arglist"] = {"message", "?level"}, ["fnl/docstring"] = "Terminates the last protected function called\nand returns `message` as the error message.\nFunction `error` never returns.\n\nUsually, `error` adds some information about the error position\nat the beginning of the message, if the message is a string.\nThe `?level` argument specifies how to get the error position.\nWith level 1 (the default), the error position is where the\n`error` function was called.\nLevel 2 points the error to where the function\nthat called `error` was called; and so on.\nPassing a level 0 avoids the addition of error position information\nto the message."}}, getmetatable = {binding = "getmetatable", metadata = {["fnl/arglist"] = {"object"}, ["fnl/docstring"] = "If `object` does not have a metatable, returns **nil**.\nOtherwise,\nif the object's metatable has a `\"__metatable\"` field,\nreturns the associated value.\nOtherwise, returns the metatable of the given object."}}, io = {binding = "io", fields = {close = {binding = "io.close", metadata = {["fnl/arglist"] = {"?file"}, ["fnl/docstring"] = "Equivalent to `file:close()`.\nWithout a `?file`, closes the default output file."}}, flush = {binding = "io.flush", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Equivalent to `io.output():flush()`."}}, input = {binding = "io.input", metadata = {["fnl/arglist"] = {"?file"}, ["fnl/docstring"] = "When called with a file name, it opens the named file (in text mode),\nand sets its handle as the default input file.\nWhen called with a file handle,\nit simply sets this file handle as the default input file.\nWhen called without parameters,\nit returns the current default input file.\n\nIn case of errors this function raises the error,\ninstead of returning an error code."}}, lines = {binding = "io.lines", metadata = {["fnl/arglist"] = {"?filename", "..."}, ["fnl/docstring"] = "Opens the given file name in read mode\nand returns an iterator function that\nworks like `file:lines(...)` over the opened file.\nWhen the iterator function detects the end of file,\nit returns **nil** (to finish the loop) and automatically closes the file.\n\nThe call `io.lines()` (with no file name) is equivalent\nto `io.input():lines()`;\nthat is, it iterates over the lines of the default input file.\nIn this case it does not close the file when the loop ends.\n\nIn case of errors this function raises the error,\ninstead of returning an error code."}}, open = {binding = "io.open", metadata = {["fnl/arglist"] = {"filename", "?mode"}, ["fnl/docstring"] = "This function opens a file,\nin the mode specified in the string `?mode`.\nIt returns a new file handle,\nor, in case of errors, **nil** plus an error message.\n\nThe `?mode` string can be any of the following:\n\n* **`\"r\"`: ** read mode (the default);\n* **`\"w\"`: ** write mode;\n* **`\"a\"`: ** append mode;\n* **`\"r+\"`: ** update mode, all previous data is preserved;\n* **`\"w+\"`: ** update mode, all previous data is erased;\n* **`\"a+\"`: ** append update mode, previous data is preserved,\n    writing is only allowed at the end of file.\n\nThe `?mode` string can also have a `\"b\"` at the end,\nwhich is needed in some systems to open the file in binary mode."}}, output = {binding = "io.output", metadata = {["fnl/arglist"] = {"?file"}, ["fnl/docstring"] = "Similar to `io.input`, but operates over the default output file."}}, popen = {binding = "io.popen", metadata = {["fnl/arglist"] = {"prog", "?mode"}, ["fnl/docstring"] = "This function is system dependent and is not available\non all platforms.\n\nStarts program `prog` in a separated process and returns\na file handle that you can use to read data from this program\n(if `?mode` is `\"r\"`, the default)\nor to write data to this program\n(if `?mode` is `\"w\"`)."}}, read = {binding = "io.read", metadata = {["fnl/arglist"] = {"..."}, ["fnl/docstring"] = "Equivalent to `io.input():read(...)`."}}, stderr = {binding = "io.stderr", metadata = {["fnl/docstring"] = "stderr file"}}, stdin = {binding = "io.stdin", metadata = {["fnl/docstring"] = "stdin file"}}, stdout = {binding = "io.stdout", metadata = {["fnl/docstring"] = "stdout file"}}, tmpfile = {binding = "io.tmpfile", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Returns a handle for a temporary file.\nThis file is opened in update mode\nand it is automatically removed when the program ends."}}, type = {binding = "io.type", metadata = {["fnl/arglist"] = {"obj"}, ["fnl/docstring"] = "Checks whether `obj` is a valid file handle.\nReturns the string `\"file\"` if `obj` is an open file handle,\n`\"closed file\"` if `obj` is a closed file handle,\nor **nil** if `obj` is not a file handle."}}, write = {binding = "io.write", metadata = {["fnl/arglist"] = {"..."}, ["fnl/docstring"] = "Equivalent to `io.output():write(...)`."}}}, metadata = {["fnl/docstring"] = "The I/O library provides two different styles for file manipulation.\nThe first one uses implicit file descriptors;\nthat is, there are operations to set a default input file and a\ndefault output file,\nand all input/output operations are over these default files.\nThe second style uses explicit file descriptors.\n\nWhen using implicit file descriptors,\nall operations are supplied by table `io`.\nWhen using explicit file descriptors,\nthe operation `io.open` returns a file descriptor\nand then all operations are supplied as methods of the file descriptor.\n\nThe table `io` also provides\nthree predefined file descriptors with their usual meanings from C:\n`io.stdin`, `io.stdout`, and `io.stderr`.\nThe I/O library never closes these files.\n\nUnless otherwise stated,\nall I/O functions return **nil** on failure\n(plus an error message as a second result and\na system-dependent error code as a third result)\nand some value different from **nil** on success.\nOn non-Posix systems,\nthe computation of the error message and error code\nin case of errors\nmay be not thread safe,\nbecause they rely on the global C variable `errno`."}}, ipairs = {binding = "ipairs", metadata = {["fnl/arglist"] = {"t"}, ["fnl/docstring"] = "If `t` has a metamethod `__ipairs`,\ncalls it with `t` as argument and returns the first three\nresults from the call.\n\nOtherwise,\nreturns three values: an iterator function, the table `t`, and 0,\nso that the construction\n\n```lua\n     for i,v in ipairs(t) do *body* end\n```\nwill iterate over the pairs (`1,t[1]`), (`2,t[2]`), ...,\nup to the first integer key absent from the table."}}, load = {binding = "load", metadata = {["fnl/arglist"] = {"ld", "?source", "?mode", "?env"}, ["fnl/docstring"] = "Loads a chunk.\n\nIf `ld` is a string, the chunk is this string.\nIf `ld` is a function,\n`load` calls it repeatedly to get the chunk pieces.\nEach call to `ld` must return a string that concatenates\nwith previous results.\nA return of an empty string, **nil**, or no value signals the end of the chunk.\n\nIf there are no syntactic errors,\nreturns the compiled chunk as a function;\notherwise, returns **nil** plus the error message.\n\nIf the resulting function has upvalues,\nthe first upvalue is set to the value of `?env`,\nif that parameter is given,\nor to the value of the global environment.\n(When you load a main chunk,\nthe resulting function will always have exactly one upvalue,\nthe `_ENV` variable\nWhen you load a binary chunk created from a function (see `string.dump`),\nthe resulting function can have arbitrary upvalues.)\n\n`?source` is used as the source of the chunk for error messages\nand debug information\nWhen absent,\nit defaults to `ld`, if `ld` is a string,\nor to `\"=(load)\"` otherwise.\n\nThe string `?mode` controls whether the chunk can be text or binary\n(that is, a precompiled chunk).\nIt may be the string `\"b\"` (only binary chunks),\n`\"t\"` (only text chunks),\nor `\"bt\"` (both binary and text).\nThe default is `\"bt\"`."}}, loadfile = {binding = "loadfile", metadata = {["fnl/arglist"] = {"?filename", "?mode", "?env"}, ["fnl/docstring"] = "Similar to `load`,\nbut gets the chunk from file `?filename`\nor from the standard input,\nif no file name is given."}}, math = {binding = "math", fields = {abs = {binding = "math.abs", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the absolute value of `x`."}}, acos = {binding = "math.acos", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the arc cosine of `x` (in radians)."}}, asin = {binding = "math.asin", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the arc sine of `x` (in radians)."}}, atan = {binding = "math.atan", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the arc tangent of `x` (in radians)."}}, atan2 = {binding = "math.atan2", metadata = {["fnl/arglist"] = {"y", "x"}, ["fnl/docstring"] = "Returns the arc tangent of `y/x` (in radians),\nbut uses the signs of both parameters to find the\nquadrant of the result.\n(It also handles correctly the case of `x` being zero.)"}}, ceil = {binding = "math.ceil", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the smallest integer larger than or equal to `x`."}}, cos = {binding = "math.cos", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the cosine of `x` (assumed to be in radians)."}}, cosh = {binding = "math.cosh", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the hyperbolic cosine of `x`."}}, deg = {binding = "math.deg", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the angle `x` (given in radians) in degrees."}}, exp = {binding = "math.exp", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the value *e\203\163*."}}, floor = {binding = "math.floor", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the largest integer smaller than or equal to `x`."}}, fmod = {binding = "math.fmod", metadata = {["fnl/arglist"] = {"x", "y"}, ["fnl/docstring"] = "Returns the remainder of the division of `x` by `y`\nthat rounds the quotient towards zero."}}, frexp = {binding = "math.frexp", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns `m` and `e` such that *x = m2\225\181\137*,\n`e` is an integer and the absolute value of `m` is\nin the range *[0.5, 1)*\n(or zero when `x` is zero)."}}, huge = {binding = "math.huge", metadata = {["fnl/docstring"] = "The value `HUGE_VAL`,\na value larger than or equal to any other numerical value."}}, ldexp = {binding = "math.ldexp", metadata = {["fnl/arglist"] = {"m", "e"}, ["fnl/docstring"] = "Returns *m2\225\181\137* (`e` should be an integer)."}}, log = {binding = "math.log", metadata = {["fnl/arglist"] = {"x", "?base"}, ["fnl/docstring"] = "Returns the logarithm of `x` in the given base.\nThe default for `?base` is *e*\n(so that the function returns the natural logarithm of `x`)."}}, max = {binding = "math.max", metadata = {["fnl/arglist"] = {"x", "..."}, ["fnl/docstring"] = "Returns the maximum value among its arguments."}}, min = {binding = "math.min", metadata = {["fnl/arglist"] = {"x", "..."}, ["fnl/docstring"] = "Returns the minimum value among its arguments."}}, modf = {binding = "math.modf", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns two numbers,\nthe integral part of `x` and the fractional part of `x`."}}, pi = {binding = "math.pi", metadata = {["fnl/docstring"] = "The value of *\207\128*."}}, pow = {binding = "math.pow", metadata = {["fnl/arglist"] = {"x", "y"}, ["fnl/docstring"] = "Returns *x\202\184*.\n(You can also use the expression `x^y` to compute this value.)"}}, rad = {binding = "math.rad", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the angle `x` (given in degrees) in radians."}}, random = {binding = "math.random", metadata = {["fnl/arglist"] = {"?m", "?n"}, ["fnl/docstring"] = "This function is an interface to the simple\npseudo-random generator function `rand` provided by Standard C.\n(No guarantees can be given for its statistical properties.)\n\nWhen called without arguments,\nreturns a uniform pseudo-random real number\nin the range *[0,1)*.  \nWhen called with an integer number `?m`,\n`math.random` returns\na uniform pseudo-random integer in the range *[1, m]*.\nWhen called with two integer numbers `?m` and `?n`,\n`math.random` returns a uniform pseudo-random\ninteger in the range *[m, n]*."}}, randomseed = {binding = "math.randomseed", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Sets `x` as the \"seed\"\nfor the pseudo-random generator:\nequal seeds produce equal sequences of numbers."}}, sin = {binding = "math.sin", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the sine of `x` (assumed to be in radians)."}}, sinh = {binding = "math.sinh", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the hyperbolic sine of `x`."}}, sqrt = {binding = "math.sqrt", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the square root of `x`.\n(You can also use the expression `x^0.5` to compute this value.)"}}, tan = {binding = "math.tan", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the tangent of `x` (assumed to be in radians)."}}, tanh = {binding = "math.tanh", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the hyperbolic tangent of `x`."}}}, metadata = {["fnl/docstring"] = "This library is an interface to the standard C math library.\nIt provides all its functions inside the table `math`."}}, next = {binding = "next", metadata = {["fnl/arglist"] = {"table", "?index"}, ["fnl/docstring"] = "Allows a program to traverse all fields of a table.\nIts first argument is a table and its second argument\nis an index in this table.\n`next` returns the next index of the table\nand its associated value.\nWhen called with **nil** as its second argument,\n`next` returns an initial index\nand its associated value.\nWhen called with the last index,\nor with **nil** in an empty table,\n`next` returns **nil**.\nIf the second argument is absent, then it is interpreted as **nil**.\nIn particular,\nyou can use `next(t)` to check whether a table is empty.\n\nThe order in which the indices are enumerated is not specified,\n*even for numeric indices*.\n(To traverse a table in numeric order,\nuse a numerical **for**.)\n\nThe behavior of `next` is undefined if,\nduring the traversal,\nyou assign any value to a non-existent field in the table.\nYou may however modify existing fields.\nIn particular, you may clear existing fields."}}, os = {binding = "os", fields = {clock = {binding = "os.clock", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Returns an approximation of the amount in seconds of CPU time\nused by the program."}}, date = {binding = "os.date", metadata = {["fnl/arglist"] = {"?format", "?time"}, ["fnl/docstring"] = "Returns a string or a table containing date and time,\nformatted according to the given string `?format`.\n\nIf the `?time` argument is present,\nthis is the time to be formatted\n(see the `os.time` function for a description of this value).\nOtherwise, `date` formats the current time.\n\nIf `?format` starts with `\"!\"`,\nthen the date is formatted in Coordinated Universal Time.\nAfter this optional character,\nif `?format` is the string `\"*t\"`,\nthen `date` returns a table with the following fields:\n`year` (four digits), `month` (1\226\128\14712), `day` (1\226\128\14731),\n`hour` (0\226\128\14723), `min` (0\226\128\14759), `sec` (0\226\128\14761),\n`wday` (weekday, Sunday is 1),\n`yday` (day of the year),\nand `isdst` (daylight saving flag, a boolean).\nThis last field may be absent\nif the information is not available.\n\nIf `?format` is not `\"*t\"`,\nthen `date` returns the date as a string,\nformatted according to the same rules as the ISO C function `strftime`.\n\nWhen called without arguments,\n`date` returns a reasonable date and time representation that depends on\nthe host system and on the current locale\n(that is, `os.date()` is equivalent to `os.date(\"%c\")`).\n\nOn non-Posix systems,\nthis function may be not thread safe\nbecause of its reliance on C function `gmtime` and C function `localtime`."}}, difftime = {binding = "os.difftime", metadata = {["fnl/arglist"] = {"t2", "t1"}, ["fnl/docstring"] = "Returns the number of seconds from time `t1` to time `t2`.\nIn POSIX, Windows, and some other systems,\nthis value is exactly `t2`*-*`t1`."}}, execute = {binding = "os.execute", metadata = {["fnl/arglist"] = {"?command"}, ["fnl/docstring"] = "This function is equivalent to the ISO C function `system`.\nIt passes `?command` to be executed by an operating system shell.\nIts first result is **true**\nif the command terminated successfully,\nor **nil** otherwise.\nAfter this first result\nthe function returns a string and a number,\nas follows:\n\n* **`\"exit\"`: **\n  the command terminated normally;\n  the following number is the exit status of the command.\n\n* **`\"signal\"`: **\n  the command was terminated by a signal;\n  the following number is the signal that terminated the command.\n\nWhen called without a `?command`,\n`os.execute` returns a boolean that is true if a shell is available."}}, exit = {binding = "os.exit", metadata = {["fnl/arglist"] = {"?code", "?close"}, ["fnl/docstring"] = "Calls the ISO C function `exit` to terminate the host program.\nIf `?code` is **true**,\nthe returned status is `EXIT_SUCCESS`;\nif `?code` is **false**,\nthe returned status is `EXIT_FAILURE`;\nif `?code` is a number,\nthe returned status is this number.\nThe default value for `?code` is **true**.\n\nIf the optional second argument `?close` is true,\ncloses the Lua state before exiting."}}, getenv = {binding = "os.getenv", metadata = {["fnl/arglist"] = {"varname"}, ["fnl/docstring"] = "Returns the value of the process environment variable `varname`,\nor **nil** if the variable is not defined."}}, remove = {binding = "os.remove", metadata = {["fnl/arglist"] = {"filename"}, ["fnl/docstring"] = "Deletes the file (or empty directory, on POSIX systems)\nwith the given name.\nIf this function fails, it returns **nil**,\nplus a string describing the error and the error code."}}, rename = {binding = "os.rename", metadata = {["fnl/arglist"] = {"oldname", "newname"}, ["fnl/docstring"] = "Renames file or directory named `oldname` to `newname`.\nIf this function fails, it returns **nil**,\nplus a string describing the error and the error code."}}, setlocale = {binding = "os.setlocale", metadata = {["fnl/arglist"] = {"locale", "?category"}, ["fnl/docstring"] = "Sets the current locale of the program.\n`locale` is a system-dependent string specifying a locale;\n`?category` is an optional string describing which category to change:\n`\"all\"`, `\"collate\"`, `\"ctype\"`,\n`\"monetary\"`, `\"numeric\"`, or `\"time\"`;\nthe default category is `\"all\"`.\nThe function returns the name of the new locale,\nor **nil** if the request cannot be honored.\n\nIf `locale` is the empty string,\nthe current locale is set to an implementation-defined native locale.\nIf `locale` is the string `\"C\"`,\nthe current locale is set to the standard C locale.\n\nWhen called with **nil** as the first argument,\nthis function only returns the name of the current locale\nfor the given category.\n\nThis function may be not thread safe\nbecause of its reliance on C function `setlocale`."}}, time = {binding = "os.time", metadata = {["fnl/arglist"] = {"?table"}, ["fnl/docstring"] = "Returns the current time when called without arguments,\nor a time representing the date and time specified by the given table.\nThis table must have fields `year`, `month`, and `day`,\nand may have fields\n`hour` (default is 12),\n`min` (default is 0),\n`sec` (default is 0),\nand `isdst` (default is **nil**).\nFor a description of these fields, see the `os.date` function.\n\nThe returned value is a number, whose meaning depends on your system.\nIn POSIX, Windows, and some other systems,\nthis number counts the number\nof seconds since some given start time (the \"epoch\").\nIn other systems, the meaning is not specified,\nand the number returned by `time` can be used only as an argument to\n`os.date` and `os.difftime`."}}, tmpname = {binding = "os.tmpname", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Returns a string with a file name that can\nbe used for a temporary file.\nThe file must be explicitly opened before its use\nand explicitly removed when no longer needed.\n\nOn POSIX systems,\nthis function also creates a file with that name,\nto avoid security risks.\n(Someone else might create the file with wrong permissions\nin the time between getting the name and creating the file.)\nYou still have to open the file to use it\nand to remove it (even if you do not use it).\n\nWhen possible,\nyou may prefer to use `io.tmpfile`,\nwhich automatically removes the file when the program ends."}}}, metadata = {["fnl/docstring"] = "This library is implemented through table `os`."}}, package = {binding = "package", fields = {config = {binding = "package.config", metadata = {["fnl/docstring"] = "A string describing some compile-time configurations for packages.\nThis string is a sequence of lines:\n\n* The first line is the directory separator string.\n  Default is `\"\\\"` for Windows and `\"/\"` for all other systems.\n\n* The second line is the character that separates templates in a path.\n  Default is `\";\"`.\n\n* The third line is the string that marks the\n  substitution points in a template.\n  Default is `\"?\"`.\n\n* The fourth line is a string that, in a path in Windows,\n  is replaced by the executable's directory.\n  Default is `\"!\"`.\n\n* The fifth line is a mark to ignore all text before it\n  when building the `luaopen_` function name.\n  Default is `\"-\"`."}}, cpath = {binding = "package.cpath", metadata = {["fnl/docstring"] = "The path used by `require` to search for a C loader.\n\nLua initializes the C path `package.cpath` in the same way\nit initializes the Lua path `package.path`,\nusing the environment variable `LUA_CPATH_5_2`\nor the environment variable `LUA_CPATH`\nor a default path defined in `luaconf.h`."}}, loaded = {binding = "package.loaded", metadata = {["fnl/docstring"] = "A table used by `require` to control which\nmodules are already loaded.\nWhen you require a module `modname` and\n`package.loaded[modname]` is not false,\n`require` simply returns the value stored there.\n\nThis variable is only a reference to the real table;\nassignments to this variable do not change the\ntable used by `require`."}}, loadlib = {binding = "package.loadlib", metadata = {["fnl/arglist"] = {"libname", "funcname"}, ["fnl/docstring"] = "Dynamically links the host program with the C library `libname`.\n\nIf `funcname` is `\"*\"`,\nthen it only links with the library,\nmaking the symbols exported by the library\navailable to other dynamically linked libraries.\nOtherwise,\nit looks for a function `funcname` inside the library\nand returns this function as a C function.\nSo, `funcname` must follow the `lua_CFunction` prototype\n(see `lua_CFunction`).\n\nThis is a low-level function.\nIt completely bypasses the package and module system.\nUnlike `require`,\nit does not perform any path searching and\ndoes not automatically adds extensions.\n`libname` must be the complete file name of the C library,\nincluding if necessary a path and an extension.\n`funcname` must be the exact name exported by the C library\n(which may depend on the C compiler and linker used).\n\nThis function is not supported by Standard C.\nAs such, it is only available on some platforms\n(Windows, Linux, Mac OS X, Solaris, BSD,\nplus other Unix systems that support the `dlfcn` standard)."}}, path = {binding = "package.path", metadata = {["fnl/docstring"] = "The path used by `require` to search for a Lua loader.\n\nAt start-up, Lua initializes this variable with\nthe value of the environment variable `LUA_PATH_5_2` or\nthe environment variable `LUA_PATH` or\nwith a default path defined in `luaconf.h`,\nif those environment variables are not defined.\nAny `\";;\"` in the value of the environment variable\nis replaced by the default path."}}, preload = {binding = "package.preload", metadata = {["fnl/docstring"] = "A table to store loaders for specific modules\n(see `require`).\n\nThis variable is only a reference to the real table;\nassignments to this variable do not change the\ntable used by `require`."}}, searchers = {binding = "package.searchers", metadata = {["fnl/docstring"] = "A table used by `require` to control how to load modules.\n\nEach entry in this table is a *searcher function*.\nWhen looking for a module,\n`require` calls each of these searchers in ascending order,\nwith the module name (the argument given to `require`) as its\nsole parameter.\nThe function can return another function (the module *loader*)\nplus an extra value that will be passed to that loader,\nor a string explaining why it did not find that module\n(or **nil** if it has nothing to say).\n\nLua initializes this table with four searcher functions.\n\nThe first searcher simply looks for a loader in the\n`package.preload` table.\n\nThe second searcher looks for a loader as a Lua library,\nusing the path stored at `package.path`.\nThe search is done as described in function `package.searchpath`.\n\nThe third searcher looks for a loader as a C library,\nusing the path given by the variable `package.cpath`.\nAgain,\nthe search is done as described in function `package.searchpath`.\nFor instance,\nif the C path is the string\n\n```lua\n     \"./?.so;./?.dll;/usr/local/?/init.so\"\n```\nthe searcher for module `foo`\nwill try to open the files `./foo.so`, `./foo.dll`,\nand `/usr/local/foo/init.so`, in that order.\nOnce it finds a C library,\nthis searcher first uses a dynamic link facility to link the\napplication with the library.\nThen it tries to find a C function inside the library to\nbe used as the loader.\nThe name of this C function is the string `\"luaopen_\"`\nconcatenated with a copy of the module name where each dot\nis replaced by an underscore.\nMoreover, if the module name has a hyphen,\nits prefix up to (and including) the first hyphen is removed.\nFor instance, if the module name is `a.v1-b.c`,\nthe function name will be `luaopen_b_c`.\n\nThe fourth searcher tries an *all-in-one loader*.\nIt searches the C path for a library for\nthe root name of the given module.\nFor instance, when requiring `a.b.c`,\nit will search for a C library for `a`.\nIf found, it looks into it for an open function for\nthe submodule;\nin our example, that would be `luaopen_a_b_c`.\nWith this facility, a package can pack several C submodules\ninto one single library,\nwith each submodule keeping its original open function.\n\nAll searchers except the first one (preload) return as the extra value\nthe file name where the module was found,\nas returned by `package.searchpath`.\nThe first searcher returns no extra value."}}, searchpath = {binding = "package.searchpath", metadata = {["fnl/arglist"] = {"name", "path", "?sep", "?rep"}, ["fnl/docstring"] = "Searches for the given `name` in the given `path`.\n\nA path is a string containing a sequence of\n*templates* separated by semicolons.\nFor each template,\nthe function replaces each interrogation mark (if any)\nin the template with a copy of `name`\nwherein all occurrences of `?sep`\n(a dot, by default)\nwere replaced by `?rep`\n(the system's directory separator, by default),\nand then tries to open the resulting file name.\n\nFor instance, if the path is the string\n\n```lua\n     \"./?.lua;./?.lc;/usr/local/?/init.lua\"\n```\nthe search for the name `foo.a`\nwill try to open the files\n`./foo/a.lua`, `./foo/a.lc`, and\n`/usr/local/foo/a/init.lua`, in that order.\n\nReturns the resulting name of the first file that it can\nopen in read mode (after closing the file),\nor **nil** plus an error message if none succeeds.\n(This error message lists all file names it tried to open.)"}}}, metadata = {["fnl/docstring"] = "The package library provides basic\nfacilities for loading modules in Lua.\nIt exports one function directly in the global environment:\n`require`.\nEverything else is exported in a table `package`."}}, pairs = {binding = "pairs", metadata = {["fnl/arglist"] = {"t"}, ["fnl/docstring"] = "If `t` has a metamethod `__pairs`,\ncalls it with `t` as argument and returns the first three\nresults from the call.\n\nOtherwise,\nreturns three values: the `next` function, the table `t`, and **nil**,\nso that the construction\n\n```lua\n     for k,v in pairs(t) do *body* end\n```\nwill iterate over all key\226\128\147value pairs of table `t`.\n\nSee function `next` for the caveats of modifying\nthe table during its traversal."}}, pcall = {binding = "pcall", metadata = {["fnl/arglist"] = {"f", "?arg1", "..."}, ["fnl/docstring"] = "Calls function `f` with\nthe given arguments in *protected mode*.\nThis means that any error inside `f` is not propagated;\ninstead, `pcall` catches the error\nand returns a status code.\nIts first result is the status code (a boolean),\nwhich is true if the call succeeds without errors.\nIn such case, `pcall` also returns all results from the call,\nafter this first result.\nIn case of any error, `pcall` returns **false** plus the error message."}}, print = {binding = "print", metadata = {["fnl/arglist"] = {"..."}, ["fnl/docstring"] = "Receives any number of arguments\nand prints their values to `stdout`,\nusing the `tostring` function to convert each argument to a string.\n`print` is not intended for formatted output,\nbut only as a quick way to show a value,\nfor instance for debugging.\nFor complete control over the output,\nuse `string.format` and `io.write`."}}, rawequal = {binding = "rawequal", metadata = {["fnl/arglist"] = {"v1", "v2"}, ["fnl/docstring"] = "Checks whether `v1` is equal to `v2`,\nwithout invoking any metamethod.\nReturns a boolean."}}, rawget = {binding = "rawget", metadata = {["fnl/arglist"] = {"table", "index"}, ["fnl/docstring"] = "Gets the real value of `table[index]`,\nwithout invoking any metamethod.\n`table` must be a table;\n`index` may be any value."}}, rawlen = {binding = "rawlen", metadata = {["fnl/arglist"] = {"v"}, ["fnl/docstring"] = "Returns the length of the object `v`,\nwhich must be a table or a string,\nwithout invoking any metamethod.\nReturns an integer number."}}, rawset = {binding = "rawset", metadata = {["fnl/arglist"] = {"table", "index", "value"}, ["fnl/docstring"] = "Sets the real value of `table[index]` to `value`,\nwithout invoking any metamethod.\n`table` must be a table,\n`index` any value different from **nil** and NaN,\nand `value` any Lua value.\n\nThis function returns `table`."}}, require = {binding = "require", metadata = {["fnl/arglist"] = {"modname"}, ["fnl/docstring"] = "Loads the given module.\nThe function starts by looking into the `package.loaded` table\nto determine whether `modname` is already loaded.\nIf it is, then `require` returns the value stored\nat `package.loaded[modname]`.\nOtherwise, it tries to find a *loader* for the module.\n\nTo find a loader,\n`require` is guided by the `package.searchers` sequence.\nBy changing this sequence,\nwe can change how `require` looks for a module.\nThe following explanation is based on the default configuration\nfor `package.searchers`.\n\nFirst `require` queries `package.preload[modname]`.\nIf it has a value,\nthis value (which should be a function) is the loader.\nOtherwise `require` searches for a Lua loader using the\npath stored in `package.path`.\nIf that also fails, it searches for a C loader using the\npath stored in `package.cpath`.\nIf that also fails,\nit tries an *all-in-one* loader (see `package.searchers`).\n\nOnce a loader is found,\n`require` calls the loader with two arguments:\n`modname` and an extra value dependent on how it got the loader.\n(If the loader came from a file,\nthis extra value is the file name.)\nIf the loader returns any non-nil value,\n`require` assigns the returned value to `package.loaded[modname]`.\nIf the loader does not return a non-nil value and\nhas not assigned any value to `package.loaded[modname]`,\nthen `require` assigns **true** to this entry.\nIn any case, `require` returns the\nfinal value of `package.loaded[modname]`.\n\nIf there is any error loading or running the module,\nor if it cannot find any loader for the module,\nthen `require` raises an error."}}, select = {binding = "select", metadata = {["fnl/arglist"] = {"index", "..."}, ["fnl/docstring"] = "If `index` is a number,\nreturns all arguments after argument number `index`;\na negative number indexes from the end (-1 is the last argument).\nOtherwise, `index` must be the string `\"#\"`,\nand `select` returns the total number of extra arguments it received."}}, setmetatable = {binding = "setmetatable", metadata = {["fnl/arglist"] = {"table", "metatable"}, ["fnl/docstring"] = "Sets the metatable for the given table.\n(You cannot change the metatable of other types from Lua, only from C.)\nIf `metatable` is **nil**,\nremoves the metatable of the given table.\nIf the original metatable has a `\"__metatable\"` field,\nraises an error.\n\nThis function returns `table`."}}, string = {binding = "string", fields = {byte = {binding = "string.byte", metadata = {["fnl/arglist"] = {"s", "?i", "?j"}, ["fnl/docstring"] = "Returns the internal numerical codes of the characters `s[i]`,\n`s[i+1]`, ..., `s[j]`.\nThe default value for `?i` is 1;\nthe default value for `?j` is `?i`.\nThese indices are corrected\nfollowing the same rules of function `string.sub`.\n\nNumerical codes are not necessarily portable across platforms."}}, char = {binding = "string.char", metadata = {["fnl/arglist"] = {"..."}, ["fnl/docstring"] = "Receives zero or more integers.\nReturns a string with length equal to the number of arguments,\nin which each character has the internal numerical code equal\nto its corresponding argument.\n\nNumerical codes are not necessarily portable across platforms."}}, dump = {binding = "string.dump", metadata = {["fnl/arglist"] = {"function"}, ["fnl/docstring"] = "Returns a string containing a binary representation of the given function,\nso that a later `load` on this string returns\na copy of the function (but with new upvalues)."}}, find = {binding = "string.find", metadata = {["fnl/arglist"] = {"s", "pattern", "?init", "?plain"}, ["fnl/docstring"] = "Looks for the first match of\n`pattern` in the string `s`.\nIf it finds a match, then `find` returns the indices of `s`\nwhere this occurrence starts and ends;\notherwise, it returns **nil**.\nA third, optional numerical argument `?init` specifies\nwhere to start the search;\nits default value is 1 and can be negative.\nA value of **true** as a fourth, optional argument `?plain`\nturns off the pattern matching facilities,\nso the function does a plain \"find substring\" operation,\nwith no characters in `pattern` being considered magic.\nNote that if `?plain` is given, then `?init` must be given as well.\n\nIf the pattern has captures,\nthen in a successful match\nthe captured values are also returned,\nafter the two indices."}}, format = {binding = "string.format", metadata = {["fnl/arglist"] = {"formatstring", "..."}, ["fnl/docstring"] = "Returns a formatted version of its variable number of arguments\nfollowing the description given in its first argument (which must be a string).\nThe format string follows the same rules as the ISO C function `sprintf`.\nThe only differences are that the options/modifiers\n`*`, `h`, `L`, `l`, `n`,\nand `p` are not supported\nand that there is an extra option, `q`.\nThe `q` option formats a string between double quotes,\nusing escape sequences when necessary to ensure that\nit can safely be read back by the Lua interpreter.\nFor instance, the call\n\n```lua\n     string.format('%q', 'a string with \"quotes\" and \\n new line')\n```\nmay produce the string:\n\n```lua\n     \"a string with \\\"quotes\\\" and \\\n      new line\"\n```\n\nOptions\n`A` and `a` (when available),\n`E`, `e`, `f`,\n`G`, and `g` all expect a number as argument.\nOptions `c`, `d`,\n`i`, `o`, `u`, `X`, and `x`\nalso expect a number,\nbut the range of that number may be limited by\nthe underlying C implementation.\nFor options `o`, `u`, `X`, and `x`,\nthe number cannot be negative.\nOption `q` expects a string;\noption `s` expects a string without embedded zeros.\nIf the argument to option `s` is not a string,\nit is converted to one following the same rules of `tostring`."}}, gmatch = {binding = "string.gmatch", metadata = {["fnl/arglist"] = {"s", "pattern"}, ["fnl/docstring"] = "Returns an iterator function that,\neach time it is called,\nreturns the next captures from `pattern` over the string `s`.\nIf `pattern` specifies no captures,\nthen the whole match is produced in each call.\n\nAs an example, the following loop\nwill iterate over all the words from string `s`,\nprinting one per line:\n\n```lua\n     s = \"hello world from Lua\"\n     for w in string.gmatch(s, \"%a+\") do\n       print(w)\n     end\n```\nThe next example collects all pairs `key=value` from the\ngiven string into a table:\n\n```lua\n     t = {}\n     s = \"from=world, to=Lua\"\n     for k, v in string.gmatch(s, \"(%w+)=(%w+)\") do\n       t[k] = v\n     end\n```\n\nFor this function, a caret `\"^\"` at the start of a pattern does not\nwork as an anchor, as this would prevent the iteration."}}, gsub = {binding = "string.gsub", metadata = {["fnl/arglist"] = {"s", "pattern", "repl", "?n"}, ["fnl/docstring"] = "Returns a copy of `s`\nin which all (or the first `?n`, if given)\noccurrences of the `pattern` have been\nreplaced by a replacement string specified by `repl`,\nwhich can be a string, a table, or a function.\n`gsub` also returns, as its second value,\nthe total number of matches that occurred.\nThe name `gsub` comes from *Global SUBstitution*.\n\nIf `repl` is a string, then its value is used for replacement.\nThe character `%` works as an escape character:\nany sequence in `repl` of the form `%*d*`,\nwith *d* between 1 and 9,\nstands for the value of the *d*-th captured substring.\nThe sequence `%0` stands for the whole match.\nThe sequence `%%` stands for a single `%`.\n\nIf `repl` is a table, then the table is queried for every match,\nusing the first capture as the key.\n\nIf `repl` is a function, then this function is called every time a\nmatch occurs, with all captured substrings passed as arguments,\nin order.\n\nIn any case,\nif the pattern specifies no captures,\nthen it behaves as if the whole pattern was inside a capture.\n\nIf the value returned by the table query or by the function call\nis a string or a number,\nthen it is used as the replacement string;\notherwise, if it is **false** or **nil**,\nthen there is no replacement\n(that is, the original match is kept in the string).\n\nHere are some examples:\n\n```lua\n     x = string.gsub(\"hello world\", \"(%w+)\", \"%1 %1\")\n     --> x=\"hello hello world world\"\n     \n     x = string.gsub(\"hello world\", \"%w+\", \"%0 %0\", 1)\n     --> x=\"hello hello world\"\n     \n     x = string.gsub(\"hello world from Lua\", \"(%w+)%s*(%w+)\", \"%2 %1\")\n     --> x=\"world hello Lua from\"\n     \n     x = string.gsub(\"home = $HOME, user = $USER\", \"%$(%w+)\", os.getenv)\n     --> x=\"home = /home/roberto, user = roberto\"\n     \n     x = string.gsub(\"4+5 = $return 4+5$\", \"%$(.-)%$\", function (s)\n           return load(s)()\n         end)\n     --> x=\"4+5 = 9\"\n     \n     local t = {name=\"lua\", version=\"5.2\"}\n     x = string.gsub(\"$name-$version.tar.gz\", \"%$(%w+)\", t)\n     --> x=\"lua-5.2.tar.gz\"\n```"}}, len = {binding = "string.len", metadata = {["fnl/arglist"] = {"s"}, ["fnl/docstring"] = "Receives a string and returns its length.\nThe empty string `\"\"` has length 0.\nEmbedded zeros are counted,\nso `\"a\\000bc\\000\"` has length 5."}}, lower = {binding = "string.lower", metadata = {["fnl/arglist"] = {"s"}, ["fnl/docstring"] = "Receives a string and returns a copy of this string with all\nuppercase letters changed to lowercase.\nAll other characters are left unchanged.\nThe definition of what an uppercase letter is depends on the current locale."}}, match = {binding = "string.match", metadata = {["fnl/arglist"] = {"s", "pattern", "?init"}, ["fnl/docstring"] = "Looks for the first *match* of\n`pattern` in the string `s`.\nIf it finds one, then `match` returns\nthe captures from the pattern;\notherwise it returns **nil**.\nIf `pattern` specifies no captures,\nthen the whole match is returned.\nA third, optional numerical argument `?init` specifies\nwhere to start the search;\nits default value is 1 and can be negative."}}, rep = {binding = "string.rep", metadata = {["fnl/arglist"] = {"s", "n", "?sep"}, ["fnl/docstring"] = "Returns a string that is the concatenation of `n` copies of\nthe string `s` separated by the string `?sep`.\nThe default value for `?sep` is the empty string\n(that is, no separator)."}}, reverse = {binding = "string.reverse", metadata = {["fnl/arglist"] = {"s"}, ["fnl/docstring"] = "Returns a string that is the string `s` reversed."}}, sub = {binding = "string.sub", metadata = {["fnl/arglist"] = {"s", "i", "?j"}, ["fnl/docstring"] = "Returns the substring of `s` that\nstarts at `i`  and continues until `?j`;\n`i` and `?j` can be negative.\nIf `?j` is absent, then it is assumed to be equal to -1\n(which is the same as the string length).\nIn particular,\nthe call `string.sub(s,1,j)` returns a prefix of `s`\nwith length `?j`,\nand `string.sub(s, -i)` returns a suffix of `s`\nwith length `i`.\n\nIf, after the translation of negative indices,\n`i` is less than 1,\nit is corrected to 1.\nIf `?j` is greater than the string length,\nit is corrected to that length.\nIf, after these corrections,\n`i` is greater than `?j`,\nthe function returns the empty string."}}, upper = {binding = "string.upper", metadata = {["fnl/arglist"] = {"s"}, ["fnl/docstring"] = "Receives a string and returns a copy of this string with all\nlowercase letters changed to uppercase.\nAll other characters are left unchanged.\nThe definition of what a lowercase letter is depends on the current locale."}}}, metadata = {["fnl/docstring"] = "This library provides generic functions for string manipulation,\nsuch as finding and extracting substrings, and pattern matching.\nWhen indexing a string in Lua, the first character is at position 1\n(not at 0, as in C).\nIndices are allowed to be negative and are interpreted as indexing backwards,\nfrom the end of the string.\nThus, the last character is at position -1, and so on.\n\nThe string library provides all its functions inside the table\n`string`.\nIt also sets a metatable for strings\nwhere the `__index` field points to the `string` table.\nTherefore, you can use the string functions in object-oriented style.\nFor instance, `string.byte(s,i)`\ncan be written as `s:byte(i)`.\n\nThe string library assumes one-byte character encodings."}}, table = {binding = "table", fields = {concat = {binding = "table.concat", metadata = {["fnl/arglist"] = {"list", "?sep", "?i", "?j"}, ["fnl/docstring"] = "Given a list where all elements are strings or numbers,\nreturns the string `list[i]..sep..list[i+1] ... sep..list[j]`.\nThe default value for `?sep` is the empty string,\nthe default for `?i` is 1,\nand the default for `?j` is `#list`.\nIf `?i` is greater than `?j`, returns the empty string."}}, insert = {binding = "table.insert", metadata = {["fnl/arglist"] = {"list", "value"}, ["fnl/docstring"] = "Inserts element `value` at position `pos` in `list`,\nshifting up the elements\n`list[pos], list[pos+1], ..., list[#list]`.\nThe default value for `pos` is `#list+1`,\nso that a call `table.insert(t,x)` inserts `x` at the end\nof list `t`."}}, pack = {binding = "table.pack", metadata = {["fnl/arglist"] = {"..."}, ["fnl/docstring"] = "Returns a new table with all parameters stored into keys 1, 2, etc.\nand with a field `\"n\"` with the total number of parameters.\nNote that the resulting table may not be a sequence."}}, remove = {binding = "table.remove", metadata = {["fnl/arglist"] = {"list", "?pos"}, ["fnl/docstring"] = "Removes from `list` the element at position `?pos`,\nreturning the value of the removed element.\nWhen `?pos` is an integer between 1 and `#list`,\nit shifts down the elements\n`list[pos+1], list[pos+2], ..., list[#list]`\nand erases element `list[#list]`;\nThe index `?pos` can also be 0 when `#list` is 0,\nor `#list + 1`;\nin those cases, the function erases the element `list[pos]`.\n\nThe default value for `?pos` is `#list`,\nso that a call `table.remove(t)` removes the last element\nof list `t`."}}, sort = {binding = "table.sort", metadata = {["fnl/arglist"] = {"list", "?comp"}, ["fnl/docstring"] = "Sorts list elements in a given order, *in-place*,\nfrom `list[1]` to `list[#list]`.\nIf `?comp` is given,\nthen it must be a function that receives two list elements\nand returns true when the first element must come\nbefore the second in the final order\n(so that `not comp(list[i+1],list[i])` will be true after the sort).\nIf `?comp` is not given,\nthen the standard Lua operator `<` is used instead.\n\nThe sort algorithm is not stable;\nthat is, elements considered equal by the given order\nmay have their relative positions changed by the sort."}}, unpack = {binding = "table.unpack", metadata = {["fnl/arglist"] = {"list", "?i", "?j"}, ["fnl/docstring"] = "Returns the elements from the given table.\nThis function is equivalent to\n\n```lua\n     return list[i], list[i+1], ..., list[j]\n```\nBy default, `?i` is 1 and `?j` is `#list`."}}}, metadata = {["fnl/docstring"] = "This library provides generic functions for table manipulation.\nIt provides all its functions inside the table `table`.\n\nRemember that, whenever an operation needs the length of a table,\nthe table should be a proper sequence\nor have a `__len` metamethod\nAll functions ignore non-numeric keys\nin tables given as arguments.\n\nFor performance reasons,\nall table accesses (get/set) performed by these functions are raw."}}, tonumber = {binding = "tonumber", metadata = {["fnl/arglist"] = {"e", "?base"}, ["fnl/docstring"] = "When called with no `?base`,\n`tonumber` tries to convert its argument to a number.\nIf the argument is already a number or\na string convertible to a number\nthen `tonumber` returns this number;\notherwise, it returns **nil**.\n\nWhen called with `?base`,\nthen `e` should be a string to be interpreted as\nan integer numeral in that base.\nThe base may be any integer between 2 and 36, inclusive.\nIn bases above 10, the letter `\"A\"` (in either upper or lower case)\nrepresents 10, `\"B\"` represents 11, and so forth,\nwith `\"Z\"` representing 35.\nIf the string `e` is not a valid numeral in the given base,\nthe function returns **nil**."}}, tostring = {binding = "tostring", metadata = {["fnl/arglist"] = {"v"}, ["fnl/docstring"] = "Receives a value of any type and\nconverts it to a string in a reasonable format.\n(For complete control of how numbers are converted,\nuse `string.format`.)\n\nIf the metatable of `v` has a `\"__tostring\"` field,\nthen `tostring` calls the corresponding value\nwith `v` as argument,\nand uses the result of the call as its result."}}, type = {binding = "type", metadata = {["fnl/arglist"] = {"v"}, ["fnl/docstring"] = "Returns the type of its only argument, coded as a string.\nThe possible results of this function are\n`\"nil\"` (a string, not the value **nil**),\n`\"number\"`,\n`\"string\"`,\n`\"boolean\"`,\n`\"table\"`,\n`\"function\"`,\n`\"thread\"`,\nand `\"userdata\"`."}}, xpcall = {binding = "xpcall", metadata = {["fnl/arglist"] = {"f", "msgh", "?arg1", "..."}, ["fnl/docstring"] = "This function is similar to `pcall`,\nexcept that it sets a new message handler `msgh`."}}}
  docs._G.fields = docs
  docs.io.fields.stdin.fields = docs.io.fields
  docs.io.fields.stdout.fields = docs.io.fields
  docs.io.fields.stderr.fields = docs.io.fields
  return docs
end
package.preload["fennel-ls.docs.generated.lua53"] = package.preload["fennel-ls.docs.generated.lua53"] or function(...)
  local docs = {_G = {binding = "_G", metadata = {["fnl/docstring"] = "A global variable (not a function) that\nholds the global environment\nLua itself does not use this variable;\nchanging its value does not affect any environment,\nnor vice versa."}}, _VERSION = {binding = "_VERSION", metadata = {["fnl/docstring"] = "A global variable (not a function) that\nholds a string containing the running Lua version.\nThe current value of this variable is `\"Lua 5.3\"`."}}, arg = {binding = "arg", metadata = {["fnl/docstring"] = "Before running any code,\n`lua` collects all command-line arguments\nin a global table called `arg`.\nThe script name goes to index 0,\nthe first argument after the script name goes to index 1,\nand so on.\nAny arguments before the script name\n(that is, the interpreter name plus its options)\ngo to negative indices.\nFor instance, in the call\n\n```lua\n     $ lua -la b.lua t1 t2\n```\nthe table is like this:\n\n```lua\n     arg = { [-2] = \"lua\", [-1] = \"-la\",\n             [0] = \"b.lua\",\n             [1] = \"t1\", [2] = \"t2\" }\n```\nIf there is no script in the call,\nthe interpreter name goes to index 0,\nfollowed by the other arguments.\nFor instance, the call\n\n```lua\n     $ lua -e \"print(arg[1])\"\n```\nwill print `\"-e\"`.\nIf there is a script,\nthe script is called with arguments\n`arg[1]`, ..., `arg[#arg]`.\n(Like all chunks in Lua,\nthe script is compiled as a vararg function.)"}}, assert = {binding = "assert", metadata = {["fnl/arglist"] = {"v", "?message"}, ["fnl/docstring"] = "Calls `error` if\nthe value of its argument `v` is false (i.e., **nil** or **false**);\notherwise, returns all its arguments.\nIn case of error,\n`?message` is the error object;\nwhen absent, it defaults to `\"assertion failed!\"`"}}, collectgarbage = {binding = "collectgarbage", metadata = {["fnl/arglist"] = {"?opt", "?arg"}, ["fnl/docstring"] = "This function is a generic interface to the garbage collector.\nIt performs different functions according to its first argument, `?opt`:\n\n* **`\"collect\"`: **\n  performs a full garbage-collection cycle.\n  This is the default option.\n\n* **`\"stop\"`: **\n  stops automatic execution of the garbage collector.\n  The collector will run only when explicitly invoked,\n  until a call to restart it.\n\n* **`\"restart\"`: **\n  restarts automatic execution of the garbage collector.\n\n* **`\"count\"`: **\n  returns the total memory in use by Lua in Kbytes.\n  The value has a fractional part,\n  so that it multiplied by 1024\n  gives the exact number of bytes in use by Lua\n  (except for overflows).\n\n* **`\"step\"`: **\n  performs a garbage-collection step.\n  The step \"size\" is controlled by `?arg`.\n  With a zero value,\n  the collector will perform one basic (indivisible) step.\n  For non-zero values,\n  the collector will perform as if that amount of memory\n  (in KBytes) had been allocated by Lua.\n  Returns **true** if the step finished a collection cycle.\n\n* **`\"setpause\"`: **\n  sets `?arg` as the new value for the *pause* of\n  the collector\n  Returns the previous value for *pause*.\n\n* **`\"setstepmul\"`: **\n  sets `?arg` as the new value for the *step multiplier* of\n  the collector\n  Returns the previous value for *step*.\n\n* **`\"isrunning\"`: **\n  returns a boolean that tells whether the collector is running\n  (i.e., not stopped)."}}, coroutine = {binding = "coroutine", fields = {create = {binding = "coroutine.create", metadata = {["fnl/arglist"] = {"f"}, ["fnl/docstring"] = "Creates a new coroutine, with body `f`.\n`f` must be a function.\nReturns this new coroutine,\nan object with type `\"thread\"`."}}, isyieldable = {binding = "coroutine.isyieldable", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Returns true when the running coroutine can yield.\n\nA running coroutine is yieldable if it is not the main thread and\nit is not inside a non-yieldable C function."}}, resume = {binding = "coroutine.resume", metadata = {["fnl/arglist"] = {"co", "?val1", "..."}, ["fnl/docstring"] = "Starts or continues the execution of coroutine `co`.\nThe first time you resume a coroutine,\nit starts running its body.\nThe values `?val1`, ... are passed\nas the arguments to the body function.\nIf the coroutine has yielded,\n`resume` restarts it;\nthe values `?val1`, ... are passed\nas the results from the yield.\n\nIf the coroutine runs without any errors,\n`resume` returns **true** plus any values passed to `yield`\n(when the coroutine yields) or any values returned by the body function\n(when the coroutine terminates).\nIf there is any error,\n`resume` returns **false** plus the error message."}}, running = {binding = "coroutine.running", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Returns the running coroutine plus a boolean,\ntrue when the running coroutine is the main one."}}, status = {binding = "coroutine.status", metadata = {["fnl/arglist"] = {"co"}, ["fnl/docstring"] = "Returns the status of coroutine `co`, as a string:\n`\"running\"`,\nif the coroutine is running (that is, it called `status`);\n`\"suspended\"`, if the coroutine is suspended in a call to `yield`,\nor if it has not started running yet;\n`\"normal\"` if the coroutine is active but not running\n(that is, it has resumed another coroutine);\nand `\"dead\"` if the coroutine has finished its body function,\nor if it has stopped with an error."}}, wrap = {binding = "coroutine.wrap", metadata = {["fnl/arglist"] = {"f"}, ["fnl/docstring"] = "Creates a new coroutine, with body `f`.\n`f` must be a function.\nReturns a function that resumes the coroutine each time it is called.\nAny arguments passed to the function behave as the\nextra arguments to `resume`.\nReturns the same values returned by `resume`,\nexcept the first boolean.\nIn case of error, propagates the error."}}, yield = {binding = "coroutine.yield", metadata = {["fnl/arglist"] = {"..."}, ["fnl/docstring"] = "Suspends the execution of the calling coroutine.\nAny arguments to `yield` are passed as extra results to `resume`."}}}, metadata = {["fnl/docstring"] = "This library comprises the operations to manipulate coroutines,\nwhich come inside the table `coroutine`."}}, debug = {binding = "debug", fields = {debug = {binding = "debug.debug", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Enters an interactive mode with the user,\nrunning each string that the user enters.\nUsing simple commands and other debug facilities,\nthe user can inspect global and local variables,\nchange their values, evaluate expressions, and so on.\nA line containing only the word `cont` finishes this function,\nso that the caller continues its execution.\n\nNote that commands for `debug.debug` are not lexically nested\nwithin any function and so have no direct access to local variables."}}, gethook = {binding = "debug.gethook", metadata = {["fnl/arglist"] = {"?thread"}, ["fnl/docstring"] = "Returns the current hook settings of the thread, as three values:\nthe current hook function, the current hook mask,\nand the current hook count\n(as set by the `debug.sethook` function)."}}, getinfo = {binding = "debug.getinfo", metadata = {["fnl/arglist"] = {"f", "?what"}, ["fnl/docstring"] = "Returns a table with information about a function.\nYou can give the function directly\nor you can give a number as the value of `f`,\nwhich means the function running at level `f` of the call stack\nof the given thread:\nlevel 0 is the current function (`getinfo` itself);\nlevel 1 is the function that called `getinfo`\n(except for tail calls, which do not count on the stack);\nand so on.\nIf `f` is a number larger than the number of active functions,\nthen `getinfo` returns **nil**.\n\nThe returned table can contain all the fields returned by `lua_getinfo`,\nwith the string `?what` describing which fields to fill in.\nThe default for `?what` is to get all information available,\nexcept the table of valid lines.\nIf present,\nthe option `\"f\"`\nadds a field named `func` with the function itself.\nIf present,\nthe option `\"L\"`\nadds a field named `activelines` with the table of\nvalid lines.\n\nFor instance, the expression `debug.getinfo(1,\"n\").name` returns\na name for the current function,\nif a reasonable name can be found,\nand the expression `debug.getinfo(print)`\nreturns a table with all available information\nabout the `print` function."}}, getlocal = {binding = "debug.getlocal", metadata = {["fnl/arglist"] = {"f", "local"}, ["fnl/docstring"] = "This function returns the name and the value of the local variable\nwith index `local` of the function at level `f` of the stack.\nThis function accesses not only explicit local variables,\nbut also parameters, temporaries, etc.\n\nThe first parameter or local variable has index 1, and so on,\nfollowing the order that they are declared in the code,\ncounting only the variables that are active\nin the current scope of the function.\nNegative indices refer to vararg arguments;\n-1 is the first vararg argument.\nThe function returns **nil** if there is no variable with the given index,\nand raises an error when called with a level out of range.\n(You can call `debug.getinfo` to check whether the level is valid.)\n\nVariable names starting with `\"(\"` (open parenthesis) \nrepresent variables with no known names\n(internal variables such as loop control variables,\nand variables from chunks saved without debug information).\n\nThe parameter `f` may also be a function.\nIn that case, `getlocal` returns only the name of function parameters."}}, getmetatable = {binding = "debug.getmetatable", metadata = {["fnl/arglist"] = {"value"}, ["fnl/docstring"] = "Returns the metatable of the given `value`\nor **nil** if it does not have a metatable."}}, getregistry = {binding = "debug.getregistry", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Returns the registry table"}}, getupvalue = {binding = "debug.getupvalue", metadata = {["fnl/arglist"] = {"f", "up"}, ["fnl/docstring"] = "This function returns the name and the value of the upvalue\nwith index `up` of the function `f`.\nThe function returns **nil** if there is no upvalue with the given index.\n\nVariable names starting with `\"(\"` (open parenthesis) \nrepresent variables with no known names\n(variables from chunks saved without debug information)."}}, getuservalue = {binding = "debug.getuservalue", metadata = {["fnl/arglist"] = {"u"}, ["fnl/docstring"] = "Returns the Lua value associated to `u`.\nIf `u` is not a full userdata,\nreturns **nil**."}}, sethook = {binding = "debug.sethook", metadata = {["fnl/arglist"] = {"hook", "mask", "?count"}, ["fnl/docstring"] = "Sets the given function as a hook.\nThe string `mask` and the number `?count` describe\nwhen the hook will be called.\nThe string mask may have any combination of the following characters,\nwith the given meaning:\n\n* **`\"c\"`: ** the hook is called every time Lua calls a function;\n* **`\"r\"`: ** the hook is called every time Lua returns from a function;\n* **`\"l\"`: ** the hook is called every time Lua enters a new line of code.\n\nMoreover,\nwith a `?count` different from zero,\nthe hook is called also after every `?count` instructions.\n\nWhen called without arguments,\n`debug.sethook` turns off the hook.\n\nWhen the hook is called, its first argument is a string\ndescribing the event that has triggered its call:\n`\"call\"` (or `\"tail call\"`),\n`\"return\"`,\n`\"line\"`, and `\"count\"`.\nFor line events,\nthe hook also gets the new line number as its second parameter.\nInside a hook,\nyou can call `getinfo` with level 2 to get more information about\nthe running function\n(level 0 is the `getinfo` function,\nand level 1 is the hook function)."}}, setlocal = {binding = "debug.setlocal", metadata = {["fnl/arglist"] = {"level", "local", "value"}, ["fnl/docstring"] = "This function assigns the value `value` to the local variable\nwith index `local` of the function at level `level` of the stack.\nThe function returns **nil** if there is no local\nvariable with the given index,\nand raises an error when called with a `level` out of range.\n(You can call `getinfo` to check whether the level is valid.)\nOtherwise, it returns the name of the local variable.\n\nSee `debug.getlocal` for more information about\nvariable indices and names."}}, setmetatable = {binding = "debug.setmetatable", metadata = {["fnl/arglist"] = {"value", "table"}, ["fnl/docstring"] = "Sets the metatable for the given `value` to the given `table`\n(which can be **nil**).\nReturns `value`."}}, setupvalue = {binding = "debug.setupvalue", metadata = {["fnl/arglist"] = {"f", "up", "value"}, ["fnl/docstring"] = "This function assigns the value `value` to the upvalue\nwith index `up` of the function `f`.\nThe function returns **nil** if there is no upvalue\nwith the given index.\nOtherwise, it returns the name of the upvalue."}}, setuservalue = {binding = "debug.setuservalue", metadata = {["fnl/arglist"] = {"udata", "value"}, ["fnl/docstring"] = "Sets the given `value` as\nthe Lua value associated to the given `udata`.\n`udata` must be a full userdata.\n\nReturns `udata`."}}, traceback = {binding = "debug.traceback", metadata = {["fnl/arglist"] = {"?message", "?level"}, ["fnl/docstring"] = "If `?message` is present but is neither a string nor **nil**,\nthis function returns `?message` without further processing.\nOtherwise,\nit returns a string with a traceback of the call stack.\nThe optional `?message` string is appended\nat the beginning of the traceback.\nAn optional `?level` number tells at which level\nto start the traceback\n(default is 1, the function calling `traceback`)."}}, upvalueid = {binding = "debug.upvalueid", metadata = {["fnl/arglist"] = {"f", "n"}, ["fnl/docstring"] = "Returns a unique identifier (as a light userdata)\nfor the upvalue numbered `n`\nfrom the given function.\n\nThese unique identifiers allow a program to check whether different\nclosures share upvalues.\nLua closures that share an upvalue\n(that is, that access a same external local variable)\nwill return identical ids for those upvalue indices."}}, upvaluejoin = {binding = "debug.upvaluejoin", metadata = {["fnl/arglist"] = {"f1", "n1", "f2", "n2"}, ["fnl/docstring"] = "Make the `n1`-th upvalue of the Lua closure `f1`\nrefer to the `n2`-th upvalue of the Lua closure `f2`."}}}, metadata = {["fnl/docstring"] = "This library provides\nthe functionality of the debug interfaceto Lua programs.\nYou should exert care when using this library.\nSeveral of its functions\nviolate basic assumptions about Lua code\n(e.g., that variables local to a function\ncannot be accessed from outside;\nthat userdata metatables cannot be changed by Lua code;\nthat Lua programs do not crash)\nand therefore can compromise otherwise secure code.\nMoreover, some functions in this library may be slow.\n\nAll functions in this library are provided\ninside the `debug` table.\nAll functions that operate over a thread\nhave an optional first argument which is the\nthread to operate over.\nThe default is always the current thread."}}, dofile = {binding = "dofile", metadata = {["fnl/arglist"] = {"?filename"}, ["fnl/docstring"] = "Opens the named file and executes its contents as a Lua chunk.\nWhen called without arguments,\n`dofile` executes the contents of the standard input (`stdin`).\nReturns all values returned by the chunk.\nIn case of errors, `dofile` propagates the error\nto its caller (that is, `dofile` does not run in protected mode)."}}, error = {binding = "error", metadata = {["fnl/arglist"] = {"message", "?level"}, ["fnl/docstring"] = "Terminates the last protected function called\nand returns `message` as the error object.\nFunction `error` never returns.\n\nUsually, `error` adds some information about the error position\nat the beginning of the message, if the message is a string.\nThe `?level` argument specifies how to get the error position.\nWith level 1 (the default), the error position is where the\n`error` function was called.\nLevel 2 points the error to where the function\nthat called `error` was called; and so on.\nPassing a level 0 avoids the addition of error position information\nto the message."}}, getmetatable = {binding = "getmetatable", metadata = {["fnl/arglist"] = {"object"}, ["fnl/docstring"] = "If `object` does not have a metatable, returns **nil**.\nOtherwise,\nif the object's metatable has a `__metatable` field,\nreturns the associated value.\nOtherwise, returns the metatable of the given object."}}, io = {binding = "io", fields = {close = {binding = "io.close", metadata = {["fnl/arglist"] = {"?file"}, ["fnl/docstring"] = "Equivalent to `file:close()`.\nWithout a `?file`, closes the default output file."}}, flush = {binding = "io.flush", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Equivalent to `io.output():flush()`."}}, input = {binding = "io.input", metadata = {["fnl/arglist"] = {"?file"}, ["fnl/docstring"] = "When called with a file name, it opens the named file (in text mode),\nand sets its handle as the default input file.\nWhen called with a file handle,\nit simply sets this file handle as the default input file.\nWhen called without arguments,\nit returns the current default input file.\n\nIn case of errors this function raises the error,\ninstead of returning an error code."}}, lines = {binding = "io.lines", metadata = {["fnl/arglist"] = {"?filename", "..."}, ["fnl/docstring"] = "Opens the given file name in read mode\nand returns an iterator function that\nworks like `file:lines(...)` over the opened file.\nWhen the iterator function detects the end of file,\nit returns no values (to finish the loop) and automatically closes the file.\n\nThe call `io.lines()` (with no file name) is equivalent\nto `io.input():lines(\"*l\")`;\nthat is, it iterates over the lines of the default input file.\nIn this case, the iterator does not close the file when the loop ends.\n\nIn case of errors this function raises the error,\ninstead of returning an error code."}}, open = {binding = "io.open", metadata = {["fnl/arglist"] = {"filename", "?mode"}, ["fnl/docstring"] = "This function opens a file,\nin the mode specified in the string `?mode`.\nIn case of success,\nit returns a new file handle.\n\nThe `?mode` string can be any of the following:\n\n* **`\"r\"`: ** read mode (the default);\n* **`\"w\"`: ** write mode;\n* **`\"a\"`: ** append mode;\n* **`\"r+\"`: ** update mode, all previous data is preserved;\n* **`\"w+\"`: ** update mode, all previous data is erased;\n* **`\"a+\"`: ** append update mode, previous data is preserved,\n    writing is only allowed at the end of file.\n\nThe `?mode` string can also have a `\"b\"` at the end,\nwhich is needed in some systems to open the file in binary mode."}}, output = {binding = "io.output", metadata = {["fnl/arglist"] = {"?file"}, ["fnl/docstring"] = "Similar to `io.input`, but operates over the default output file."}}, popen = {binding = "io.popen", metadata = {["fnl/arglist"] = {"prog", "?mode"}, ["fnl/docstring"] = "This function is system dependent and is not available\non all platforms.\n\nStarts program `prog` in a separated process and returns\na file handle that you can use to read data from this program\n(if `?mode` is `\"r\"`, the default)\nor to write data to this program\n(if `?mode` is `\"w\"`)."}}, read = {binding = "io.read", metadata = {["fnl/arglist"] = {"..."}, ["fnl/docstring"] = "Equivalent to `io.input():read(...)`."}}, stderr = {binding = "io.stderr", metadata = {["fnl/docstring"] = "stderr file"}}, stdin = {binding = "io.stdin", metadata = {["fnl/docstring"] = "stdin file"}}, stdout = {binding = "io.stdout", metadata = {["fnl/docstring"] = "stdout file"}}, tmpfile = {binding = "io.tmpfile", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "In case of success,\nreturns a handle for a temporary file.\nThis file is opened in update mode\nand it is automatically removed when the program ends."}}, type = {binding = "io.type", metadata = {["fnl/arglist"] = {"obj"}, ["fnl/docstring"] = "Checks whether `obj` is a valid file handle.\nReturns the string `\"file\"` if `obj` is an open file handle,\n`\"closed file\"` if `obj` is a closed file handle,\nor **nil** if `obj` is not a file handle."}}, write = {binding = "io.write", metadata = {["fnl/arglist"] = {"..."}, ["fnl/docstring"] = "Equivalent to `io.output():write(...)`."}}}, metadata = {["fnl/docstring"] = "The I/O library provides two different styles for file manipulation.\nThe first one uses implicit file handles;\nthat is, there are operations to set a default input file and a\ndefault output file,\nand all input/output operations are over these default files.\nThe second style uses explicit file handles.\n\nWhen using implicit file handles,\nall operations are supplied by table `io`.\nWhen using explicit file handles,\nthe operation `io.open` returns a file handle\nand then all operations are supplied as methods of the file handle.\n\nThe table `io` also provides\nthree predefined file handles with their usual meanings from C:\n`io.stdin`, `io.stdout`, and `io.stderr`.\nThe I/O library never closes these files.\n\nUnless otherwise stated,\nall I/O functions return **nil** on failure\n(plus an error message as a second result and\na system-dependent error code as a third result)\nand some value different from **nil** on success.\nIn non-POSIX systems,\nthe computation of the error message and error code\nin case of errors\nmay be not thread safe,\nbecause they rely on the global C variable `errno`."}}, ipairs = {binding = "ipairs", metadata = {["fnl/arglist"] = {"t"}, ["fnl/docstring"] = "Returns three values (an iterator function, the table `t`, and 0)\nso that the construction\n\n```lua\n     for i,v in ipairs(t) do *body* end\n```\nwill iterate over the key\226\128\147value pairs\n(`1,t[1]`), (`2,t[2]`), ...,\nup to the first nil value."}}, load = {binding = "load", metadata = {["fnl/arglist"] = {"chunk", "?chunkname", "?mode", "?env"}, ["fnl/docstring"] = "Loads a chunk.\n\nIf `chunk` is a string, the chunk is this string.\nIf `chunk` is a function,\n`load` calls it repeatedly to get the chunk pieces.\nEach call to `chunk` must return a string that concatenates\nwith previous results.\nA return of an empty string, **nil**, or no value signals the end of the chunk.\n\nIf there are no syntactic errors,\nreturns the compiled chunk as a function;\notherwise, returns **nil** plus the error message.\n\nIf the resulting function has upvalues,\nthe first upvalue is set to the value of `?env`,\nif that parameter is given,\nor to the value of the global environment.\nOther upvalues are initialized with **nil**.\n(When you load a main chunk,\nthe resulting function will always have exactly one upvalue,\nthe `_ENV` variable\nHowever,\nwhen you load a binary chunk created from a function (see `string.dump`),\nthe resulting function can have an arbitrary number of upvalues.)\nAll upvalues are fresh, that is,\nthey are not shared with any other function.\n\n`?chunkname` is used as the name of the chunk for error messages\nand debug information\nWhen absent,\nit defaults to `chunk`, if `chunk` is a string,\nor to `\"=(load)\"` otherwise.\n\nThe string `?mode` controls whether the chunk can be text or binary\n(that is, a precompiled chunk).\nIt may be the string `\"b\"` (only binary chunks),\n`\"t\"` (only text chunks),\nor `\"bt\"` (both binary and text).\nThe default is `\"bt\"`.\n\nLua does not check the consistency of binary chunks.\nMaliciously crafted binary chunks can crash\nthe interpreter."}}, loadfile = {binding = "loadfile", metadata = {["fnl/arglist"] = {"?filename", "?mode", "?env"}, ["fnl/docstring"] = "Similar to `load`,\nbut gets the chunk from file `?filename`\nor from the standard input,\nif no file name is given."}}, math = {binding = "math", fields = {abs = {binding = "math.abs", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the absolute value of `x`. (integer/float)"}}, acos = {binding = "math.acos", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the arc cosine of `x` (in radians)."}}, asin = {binding = "math.asin", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the arc sine of `x` (in radians)."}}, atan = {binding = "math.atan", metadata = {["fnl/arglist"] = {"y", "?x"}, ["fnl/docstring"] = "Returns the arc tangent of `y/x` (in radians),\nbut uses the signs of both arguments to find the\nquadrant of the result.\n(It also handles correctly the case of `?x` being zero.)\n\nThe default value for `?x` is 1,\nso that the call `math.atan(y)`\nreturns the arc tangent of `y`."}}, ceil = {binding = "math.ceil", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the smallest integral value larger than or equal to `x`."}}, cos = {binding = "math.cos", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the cosine of `x` (assumed to be in radians)."}}, deg = {binding = "math.deg", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Converts the angle `x` from radians to degrees."}}, exp = {binding = "math.exp", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the value *e\203\163*\n(where `e` is the base of natural logarithms)."}}, floor = {binding = "math.floor", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the largest integral value smaller than or equal to `x`."}}, fmod = {binding = "math.fmod", metadata = {["fnl/arglist"] = {"x", "y"}, ["fnl/docstring"] = "Returns the remainder of the division of `x` by `y`\nthat rounds the quotient towards zero. (integer/float)"}}, huge = {binding = "math.huge", metadata = {["fnl/docstring"] = "The float value `HUGE_VAL`,\na value larger than any other numeric value."}}, log = {binding = "math.log", metadata = {["fnl/arglist"] = {"x", "?base"}, ["fnl/docstring"] = "Returns the logarithm of `x` in the given base.\nThe default for `?base` is *e*\n(so that the function returns the natural logarithm of `x`)."}}, max = {binding = "math.max", metadata = {["fnl/arglist"] = {"x", "..."}, ["fnl/docstring"] = "Returns the argument with the maximum value,\naccording to the Lua operator `<`. (integer/float)"}}, maxinteger = {binding = "math.maxinteger", metadata = {["fnl/docstring"] = "An integer with the maximum value for an integer."}}, min = {binding = "math.min", metadata = {["fnl/arglist"] = {"x", "..."}, ["fnl/docstring"] = "Returns the argument with the minimum value,\naccording to the Lua operator `<`. (integer/float)"}}, mininteger = {binding = "math.mininteger", metadata = {["fnl/docstring"] = "An integer with the minimum value for an integer."}}, modf = {binding = "math.modf", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the integral part of `x` and the fractional part of `x`.\nIts second result is always a float."}}, pi = {binding = "math.pi", metadata = {["fnl/docstring"] = "The value of *\207\128*."}}, rad = {binding = "math.rad", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Converts the angle `x` from degrees to radians."}}, random = {binding = "math.random", metadata = {["fnl/arglist"] = {"?m", "?n"}, ["fnl/docstring"] = "When called without arguments,\nreturns a pseudo-random float with uniform distribution\nin the range  *[0,1)*.  \nWhen called with two integers `?m` and `?n`,\n`math.random` returns a pseudo-random integer\nwith uniform distribution in the range *[m, n]*.\n(The value *n-m* cannot be negative and must fit in a Lua integer.)\nThe call `math.random(n)` is equivalent to `math.random(1,n)`.\n\nThis function is an interface to the underling\npseudo-random generator function provided by C."}}, randomseed = {binding = "math.randomseed", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Sets `x` as the \"seed\"\nfor the pseudo-random generator:\nequal seeds produce equal sequences of numbers."}}, sin = {binding = "math.sin", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the sine of `x` (assumed to be in radians)."}}, sqrt = {binding = "math.sqrt", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the square root of `x`.\n(You can also use the expression `x^0.5` to compute this value.)"}}, tan = {binding = "math.tan", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the tangent of `x` (assumed to be in radians)."}}, tointeger = {binding = "math.tointeger", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "If the value `x` is convertible to an integer,\nreturns that integer.\nOtherwise, returns **nil**."}}, type = {binding = "math.type", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns `\"integer\"` if `x` is an integer,\n`\"float\"` if it is a float,\nor **nil** if `x` is not a number."}}, ult = {binding = "math.ult", metadata = {["fnl/arglist"] = {"m", "n"}, ["fnl/docstring"] = "Returns a boolean,\ntrue if and only if integer `m` is below integer `n` when\nthey are compared as unsigned integers."}}}, metadata = {["fnl/docstring"] = "This library provides basic mathematical functions.\nIt provides all its functions and constants inside the table `math`.\nFunctions with the annotation `\"integer/float\"` give\ninteger results for integer arguments\nand float results for float (or mixed) arguments.\nRounding functions\n(`math.ceil`, `math.floor`, and `math.modf`)\nreturn an integer when the result fits in the range of an integer,\nor a float otherwise."}}, next = {binding = "next", metadata = {["fnl/arglist"] = {"table", "?index"}, ["fnl/docstring"] = "Allows a program to traverse all fields of a table.\nIts first argument is a table and its second argument\nis an index in this table.\n`next` returns the next index of the table\nand its associated value.\nWhen called with **nil** as its second argument,\n`next` returns an initial index\nand its associated value.\nWhen called with the last index,\nor with **nil** in an empty table,\n`next` returns **nil**.\nIf the second argument is absent, then it is interpreted as **nil**.\nIn particular,\nyou can use `next(t)` to check whether a table is empty.\n\nThe order in which the indices are enumerated is not specified,\n*even for numeric indices*.\n(To traverse a table in numerical order,\nuse a numerical **for**.)\n\nThe behavior of `next` is undefined if,\nduring the traversal,\nyou assign any value to a non-existent field in the table.\nYou may however modify existing fields.\nIn particular, you may clear existing fields."}}, os = {binding = "os", fields = {clock = {binding = "os.clock", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Returns an approximation of the amount in seconds of CPU time\nused by the program."}}, date = {binding = "os.date", metadata = {["fnl/arglist"] = {"?format", "?time"}, ["fnl/docstring"] = "Returns a string or a table containing date and time,\nformatted according to the given string `?format`.\n\nIf the `?time` argument is present,\nthis is the time to be formatted\n(see the `os.time` function for a description of this value).\nOtherwise, `date` formats the current time.\n\nIf `?format` starts with `\"!\"`,\nthen the date is formatted in Coordinated Universal Time.\nAfter this optional character,\nif `?format` is the string `\"*t\"`,\nthen `date` returns a table with the following fields:\n`year`, `month` (1\226\128\14712), `day` (1\226\128\14731),\n`hour` (0\226\128\14723), `min` (0\226\128\14759), `sec` (0\226\128\14761),\n`wday` (weekday, 1\226\128\1477, Sunday is 1),\n`yday` (day of the year, 1\226\128\147366),\nand `isdst` (daylight saving flag, a boolean).\nThis last field may be absent\nif the information is not available.\n\nIf `?format` is not `\"*t\"`,\nthen `date` returns the date as a string,\nformatted according to the same rules as the ISO C function `strftime`.\n\nWhen called without arguments,\n`date` returns a reasonable date and time representation that depends on\nthe host system and on the current locale.\n(More specifically, `os.date()` is equivalent to `os.date(\"%c\")`.)\n\nIn non-POSIX systems,\nthis function may be not thread safe\nbecause of its reliance on C function `gmtime` and C function `localtime`."}}, difftime = {binding = "os.difftime", metadata = {["fnl/arglist"] = {"t2", "t1"}, ["fnl/docstring"] = "Returns the difference, in seconds,\nfrom time `t1` to time `t2`\n(where the times are values returned by `os.time`).\nIn POSIX, Windows, and some other systems,\nthis value is exactly `t2`*-*`t1`."}}, execute = {binding = "os.execute", metadata = {["fnl/arglist"] = {"?command"}, ["fnl/docstring"] = "This function is equivalent to the ISO C function `system`.\nIt passes `?command` to be executed by an operating system shell.\nIts first result is **true**\nif the command terminated successfully,\nor **nil** otherwise.\nAfter this first result\nthe function returns a string plus a number,\nas follows:\n\n* **`\"exit\"`: **\n  the command terminated normally;\n  the following number is the exit status of the command.\n\n* **`\"signal\"`: **\n  the command was terminated by a signal;\n  the following number is the signal that terminated the command.\n\nWhen called without a `?command`,\n`os.execute` returns a boolean that is true if a shell is available."}}, exit = {binding = "os.exit", metadata = {["fnl/arglist"] = {"?code", "?close"}, ["fnl/docstring"] = "Calls the ISO C function `exit` to terminate the host program.\nIf `?code` is **true**,\nthe returned status is `EXIT_SUCCESS`;\nif `?code` is **false**,\nthe returned status is `EXIT_FAILURE`;\nif `?code` is a number,\nthe returned status is this number.\nThe default value for `?code` is **true**.\n\nIf the optional second argument `?close` is true,\ncloses the Lua state before exiting."}}, getenv = {binding = "os.getenv", metadata = {["fnl/arglist"] = {"varname"}, ["fnl/docstring"] = "Returns the value of the process environment variable `varname`,\nor **nil** if the variable is not defined."}}, remove = {binding = "os.remove", metadata = {["fnl/arglist"] = {"filename"}, ["fnl/docstring"] = "Deletes the file (or empty directory, on POSIX systems)\nwith the given name.\nIf this function fails, it returns **nil**,\nplus a string describing the error and the error code.\nOtherwise, it returns true."}}, rename = {binding = "os.rename", metadata = {["fnl/arglist"] = {"oldname", "newname"}, ["fnl/docstring"] = "Renames the file or directory named `oldname` to `newname`.\nIf this function fails, it returns **nil**,\nplus a string describing the error and the error code.\nOtherwise, it returns true."}}, setlocale = {binding = "os.setlocale", metadata = {["fnl/arglist"] = {"locale", "?category"}, ["fnl/docstring"] = "Sets the current locale of the program.\n`locale` is a system-dependent string specifying a locale;\n`?category` is an optional string describing which category to change:\n`\"all\"`, `\"collate\"`, `\"ctype\"`,\n`\"monetary\"`, `\"numeric\"`, or `\"time\"`;\nthe default category is `\"all\"`.\nThe function returns the name of the new locale,\nor **nil** if the request cannot be honored.\n\nIf `locale` is the empty string,\nthe current locale is set to an implementation-defined native locale.\nIf `locale` is the string `\"C\"`,\nthe current locale is set to the standard C locale.\n\nWhen called with **nil** as the first argument,\nthis function only returns the name of the current locale\nfor the given category.\n\nThis function may be not thread safe\nbecause of its reliance on C function `setlocale`."}}, time = {binding = "os.time", metadata = {["fnl/arglist"] = {"?table"}, ["fnl/docstring"] = "Returns the current time when called without arguments,\nor a time representing the local date and time specified by the given table.\nThis table must have fields `year`, `month`, and `day`,\nand may have fields\n`hour` (default is 12),\n`min` (default is 0),\n`sec` (default is 0),\nand `isdst` (default is **nil**).\nOther fields are ignored.\nFor a description of these fields, see the `os.date` function.\n\nThe values in these fields do not need to be inside their valid ranges.\nFor instance, if `sec` is -10,\nit means -10 seconds from the time specified by the other fields;\nif `hour` is 1000,\nit means +1000 hours from the time specified by the other fields.\n\nThe returned value is a number, whose meaning depends on your system.\nIn POSIX, Windows, and some other systems,\nthis number counts the number\nof seconds since some given start time (the \"epoch\").\nIn other systems, the meaning is not specified,\nand the number returned by `time` can be used only as an argument to\n`os.date` and `os.difftime`."}}, tmpname = {binding = "os.tmpname", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Returns a string with a file name that can\nbe used for a temporary file.\nThe file must be explicitly opened before its use\nand explicitly removed when no longer needed.\n\nIn POSIX systems,\nthis function also creates a file with that name,\nto avoid security risks.\n(Someone else might create the file with wrong permissions\nin the time between getting the name and creating the file.)\nYou still have to open the file to use it\nand to remove it (even if you do not use it).\n\nWhen possible,\nyou may prefer to use `io.tmpfile`,\nwhich automatically removes the file when the program ends."}}}, metadata = {["fnl/docstring"] = "This library is implemented through table `os`."}}, package = {binding = "package", fields = {config = {binding = "package.config", metadata = {["fnl/docstring"] = "A string describing some compile-time configurations for packages.\nThis string is a sequence of lines:\n\n* The first line is the directory separator string.\n  Default is `\"\\\"` for Windows and `\"/\"` for all other systems.\n\n* The second line is the character that separates templates in a path.\n  Default is `\";\"`.\n\n* The third line is the string that marks the\n  substitution points in a template.\n  Default is `\"?\"`.\n\n* The fourth line is a string that, in a path in Windows,\n  is replaced by the executable's directory.\n  Default is `\"!\"`.\n\n* The fifth line is a mark to ignore all text after it\n  when building the `luaopen_` function name.\n  Default is `\"-\"`."}}, cpath = {binding = "package.cpath", metadata = {["fnl/docstring"] = "The path used by `require` to search for a C loader.\n\nLua initializes the C path `package.cpath` in the same way\nit initializes the Lua path `package.path`,\nusing the environment variable `LUA_CPATH_5_3`,\nor the environment variable `LUA_CPATH`,\nor a default path defined in `luaconf.h`."}}, loaded = {binding = "package.loaded", metadata = {["fnl/docstring"] = "A table used by `require` to control which\nmodules are already loaded.\nWhen you require a module `modname` and\n`package.loaded[modname]` is not false,\n`require` simply returns the value stored there.\n\nThis variable is only a reference to the real table;\nassignments to this variable do not change the\ntable used by `require`."}}, loadlib = {binding = "package.loadlib", metadata = {["fnl/arglist"] = {"libname", "funcname"}, ["fnl/docstring"] = "Dynamically links the host program with the C library `libname`.\n\nIf `funcname` is `\"*\"`,\nthen it only links with the library,\nmaking the symbols exported by the library\navailable to other dynamically linked libraries.\nOtherwise,\nit looks for a function `funcname` inside the library\nand returns this function as a C function.\nSo, `funcname` must follow the `lua_CFunction` prototype\n(see `lua_CFunction`).\n\nThis is a low-level function.\nIt completely bypasses the package and module system.\nUnlike `require`,\nit does not perform any path searching and\ndoes not automatically adds extensions.\n`libname` must be the complete file name of the C library,\nincluding if necessary a path and an extension.\n`funcname` must be the exact name exported by the C library\n(which may depend on the C compiler and linker used).\n\nThis function is not supported by Standard C.\nAs such, it is only available on some platforms\n(Windows, Linux, Mac OS X, Solaris, BSD,\nplus other Unix systems that support the `dlfcn` standard)."}}, path = {binding = "package.path", metadata = {["fnl/docstring"] = "The path used by `require` to search for a Lua loader.\n\nAt start-up, Lua initializes this variable with\nthe value of the environment variable `LUA_PATH_5_3` or\nthe environment variable `LUA_PATH` or\nwith a default path defined in `luaconf.h`,\nif those environment variables are not defined.\nAny `\";;\"` in the value of the environment variable\nis replaced by the default path."}}, preload = {binding = "package.preload", metadata = {["fnl/docstring"] = "A table to store loaders for specific modules\n(see `require`).\n\nThis variable is only a reference to the real table;\nassignments to this variable do not change the\ntable used by `require`."}}, searchers = {binding = "package.searchers", metadata = {["fnl/docstring"] = "A table used by `require` to control how to load modules.\n\nEach entry in this table is a *searcher function*.\nWhen looking for a module,\n`require` calls each of these searchers in ascending order,\nwith the module name (the argument given to `require`) as its\nsole parameter.\nThe function can return another function (the module *loader*)\nplus an extra value that will be passed to that loader,\nor a string explaining why it did not find that module\n(or **nil** if it has nothing to say).\n\nLua initializes this table with four searcher functions.\n\nThe first searcher simply looks for a loader in the\n`package.preload` table.\n\nThe second searcher looks for a loader as a Lua library,\nusing the path stored at `package.path`.\nThe search is done as described in function `package.searchpath`.\n\nThe third searcher looks for a loader as a C library,\nusing the path given by the variable `package.cpath`.\nAgain,\nthe search is done as described in function `package.searchpath`.\nFor instance,\nif the C path is the string\n\n```lua\n     \"./?.so;./?.dll;/usr/local/?/init.so\"\n```\nthe searcher for module `foo`\nwill try to open the files `./foo.so`, `./foo.dll`,\nand `/usr/local/foo/init.so`, in that order.\nOnce it finds a C library,\nthis searcher first uses a dynamic link facility to link the\napplication with the library.\nThen it tries to find a C function inside the library to\nbe used as the loader.\nThe name of this C function is the string `\"luaopen_\"`\nconcatenated with a copy of the module name where each dot\nis replaced by an underscore.\nMoreover, if the module name has a hyphen,\nits suffix after (and including) the first hyphen is removed.\nFor instance, if the module name is `a.b.c-v2.1`,\nthe function name will be `luaopen_a_b_c`.\n\nThe fourth searcher tries an *all-in-one loader*.\nIt searches the C path for a library for\nthe root name of the given module.\nFor instance, when requiring `a.b.c`,\nit will search for a C library for `a`.\nIf found, it looks into it for an open function for\nthe submodule;\nin our example, that would be `luaopen_a_b_c`.\nWith this facility, a package can pack several C submodules\ninto one single library,\nwith each submodule keeping its original open function.\n\nAll searchers except the first one (preload) return as the extra value\nthe file name where the module was found,\nas returned by `package.searchpath`.\nThe first searcher returns no extra value."}}, searchpath = {binding = "package.searchpath", metadata = {["fnl/arglist"] = {"name", "path", "?sep", "?rep"}, ["fnl/docstring"] = "Searches for the given `name` in the given `path`.\n\nA path is a string containing a sequence of\n*templates* separated by semicolons.\nFor each template,\nthe function replaces each interrogation mark (if any)\nin the template with a copy of `name`\nwherein all occurrences of `?sep`\n(a dot, by default)\nwere replaced by `?rep`\n(the system's directory separator, by default),\nand then tries to open the resulting file name.\n\nFor instance, if the path is the string\n\n```lua\n     \"./?.lua;./?.lc;/usr/local/?/init.lua\"\n```\nthe search for the name `foo.a`\nwill try to open the files\n`./foo/a.lua`, `./foo/a.lc`, and\n`/usr/local/foo/a/init.lua`, in that order.\n\nReturns the resulting name of the first file that it can\nopen in read mode (after closing the file),\nor **nil** plus an error message if none succeeds.\n(This error message lists all file names it tried to open.)"}}}, metadata = {["fnl/docstring"] = "The package library provides basic\nfacilities for loading modules in Lua.\nIt exports one function directly in the global environment:\n`require`.\nEverything else is exported in a table `package`."}}, pairs = {binding = "pairs", metadata = {["fnl/arglist"] = {"t"}, ["fnl/docstring"] = "If `t` has a metamethod `__pairs`,\ncalls it with `t` as argument and returns the first three\nresults from the call.\n\nOtherwise,\nreturns three values: the `next` function, the table `t`, and **nil**,\nso that the construction\n\n```lua\n     for k,v in pairs(t) do *body* end\n```\nwill iterate over all key\226\128\147value pairs of table `t`.\n\nSee function `next` for the caveats of modifying\nthe table during its traversal."}}, pcall = {binding = "pcall", metadata = {["fnl/arglist"] = {"f", "?arg1", "..."}, ["fnl/docstring"] = "Calls function `f` with\nthe given arguments in *protected mode*.\nThis means that any error inside `f` is not propagated;\ninstead, `pcall` catches the error\nand returns a status code.\nIts first result is the status code (a boolean),\nwhich is true if the call succeeds without errors.\nIn such case, `pcall` also returns all results from the call,\nafter this first result.\nIn case of any error, `pcall` returns **false** plus the error message."}}, print = {binding = "print", metadata = {["fnl/arglist"] = {"..."}, ["fnl/docstring"] = "Receives any number of arguments\nand prints their values to `stdout`,\nusing the `tostring` function to convert each argument to a string.\n`print` is not intended for formatted output,\nbut only as a quick way to show a value,\nfor instance for debugging.\nFor complete control over the output,\nuse `string.format` and `io.write`."}}, rawequal = {binding = "rawequal", metadata = {["fnl/arglist"] = {"v1", "v2"}, ["fnl/docstring"] = "Checks whether `v1` is equal to `v2`,\nwithout invoking the `__eq` metamethod.\nReturns a boolean."}}, rawget = {binding = "rawget", metadata = {["fnl/arglist"] = {"table", "index"}, ["fnl/docstring"] = "Gets the real value of `table[index]`,\nwithout invoking the `__index` metamethod.\n`table` must be a table;\n`index` may be any value."}}, rawlen = {binding = "rawlen", metadata = {["fnl/arglist"] = {"v"}, ["fnl/docstring"] = "Returns the length of the object `v`,\nwhich must be a table or a string,\nwithout invoking the `__len` metamethod.\nReturns an integer."}}, rawset = {binding = "rawset", metadata = {["fnl/arglist"] = {"table", "index", "value"}, ["fnl/docstring"] = "Sets the real value of `table[index]` to `value`,\nwithout invoking the `__newindex` metamethod.\n`table` must be a table,\n`index` any value different from **nil** and NaN,\nand `value` any Lua value.\n\nThis function returns `table`."}}, require = {binding = "require", metadata = {["fnl/arglist"] = {"modname"}, ["fnl/docstring"] = "Loads the given module.\nThe function starts by looking into the `package.loaded` table\nto determine whether `modname` is already loaded.\nIf it is, then `require` returns the value stored\nat `package.loaded[modname]`.\nOtherwise, it tries to find a *loader* for the module.\n\nTo find a loader,\n`require` is guided by the `package.searchers` sequence.\nBy changing this sequence,\nwe can change how `require` looks for a module.\nThe following explanation is based on the default configuration\nfor `package.searchers`.\n\nFirst `require` queries `package.preload[modname]`.\nIf it has a value,\nthis value (which must be a function) is the loader.\nOtherwise `require` searches for a Lua loader using the\npath stored in `package.path`.\nIf that also fails, it searches for a C loader using the\npath stored in `package.cpath`.\nIf that also fails,\nit tries an *all-in-one* loader (see `package.searchers`).\n\nOnce a loader is found,\n`require` calls the loader with two arguments:\n`modname` and an extra value dependent on how it got the loader.\n(If the loader came from a file,\nthis extra value is the file name.)\nIf the loader returns any non-nil value,\n`require` assigns the returned value to `package.loaded[modname]`.\nIf the loader does not return a non-nil value and\nhas not assigned any value to `package.loaded[modname]`,\nthen `require` assigns **true** to this entry.\nIn any case, `require` returns the\nfinal value of `package.loaded[modname]`.\n\nIf there is any error loading or running the module,\nor if it cannot find any loader for the module,\nthen `require` raises an error."}}, select = {binding = "select", metadata = {["fnl/arglist"] = {"index", "..."}, ["fnl/docstring"] = "If `index` is a number,\nreturns all arguments after argument number `index`;\na negative number indexes from the end (-1 is the last argument).\nOtherwise, `index` must be the string `\"#\"`,\nand `select` returns the total number of extra arguments it received."}}, setmetatable = {binding = "setmetatable", metadata = {["fnl/arglist"] = {"table", "metatable"}, ["fnl/docstring"] = "Sets the metatable for the given table.\n(To change the metatable of other types from Lua code,\nyou must use the debug library)\nIf `metatable` is **nil**,\nremoves the metatable of the given table.\nIf the original metatable has a `__metatable` field,\nraises an error.\n\nThis function returns `table`."}}, string = {binding = "string", fields = {byte = {binding = "string.byte", metadata = {["fnl/arglist"] = {"s", "?i", "?j"}, ["fnl/docstring"] = "Returns the internal numeric codes of the characters `s[i]`,\n`s[i+1]`, ..., `s[j]`.\nThe default value for `?i` is 1;\nthe default value for `?j` is `?i`.\nThese indices are corrected\nfollowing the same rules of function `string.sub`.\n\nNumeric codes are not necessarily portable across platforms."}}, char = {binding = "string.char", metadata = {["fnl/arglist"] = {"..."}, ["fnl/docstring"] = "Receives zero or more integers.\nReturns a string with length equal to the number of arguments,\nin which each character has the internal numeric code equal\nto its corresponding argument.\n\nNumeric codes are not necessarily portable across platforms."}}, dump = {binding = "string.dump", metadata = {["fnl/arglist"] = {"function", "?strip"}, ["fnl/docstring"] = "Returns a string containing a binary representation\n(a *binary chunk*)\nof the given function,\nso that a later `load` on this string returns\na copy of the function (but with new upvalues).\nIf `?strip` is a true value,\nthe binary representation may not include all debug information\nabout the function,\nto save space.\n\nFunctions with upvalues have only their number of upvalues saved.\nWhen (re)loaded,\nthose upvalues receive fresh instances containing **nil**.\n(You can use the debug library to serialize\nand reload the upvalues of a function\nin a way adequate to your needs.)"}}, find = {binding = "string.find", metadata = {["fnl/arglist"] = {"s", "pattern", "?init", "?plain"}, ["fnl/docstring"] = "Looks for the first match of\n`pattern`in the string `s`.\nIf it finds a match, then `find` returns the indices of `s`\nwhere this occurrence starts and ends;\notherwise, it returns **nil**.\nA third, optional numeric argument `?init` specifies\nwhere to start the search;\nits default value is 1 and can be negative.\nA value of **true** as a fourth, optional argument `?plain`\nturns off the pattern matching facilities,\nso the function does a plain \"find substring\" operation,\nwith no characters in `pattern` being considered magic.\nNote that if `?plain` is given, then `?init` must be given as well.\n\nIf the pattern has captures,\nthen in a successful match\nthe captured values are also returned,\nafter the two indices."}}, format = {binding = "string.format", metadata = {["fnl/arglist"] = {"formatstring", "..."}, ["fnl/docstring"] = "Returns a formatted version of its variable number of arguments\nfollowing the description given in its first argument (which must be a string).\nThe format string follows the same rules as the ISO C function `sprintf`.\nThe only differences are that the options/modifiers\n`*`, `h`, `L`, `l`, `n`,\nand `p` are not supported\nand that there is an extra option, `q`.\n\nThe `q` option formats a string between double quotes,\nusing escape sequences when necessary to ensure that\nit can safely be read back by the Lua interpreter.\nFor instance, the call\n\n```lua\n     string.format('%q', 'a string with \"quotes\" and \\n new line')\n```\nmay produce the string:\n\n```lua\n     \"a string with \\\"quotes\\\" and \\\n      new line\"\n```\n\nOptions\n`A`, `a`, `E`, `e`, `f`,\n`G`, and `g` all expect a number as argument.\nOptions `c`, `d`,\n`i`, `o`, `u`, `X`, and `x`\nexpect an integer.\nWhen Lua is compiled with a C89 compiler,\noptions `A` and `a` (hexadecimal floats)\ndo not support any modifier (flags, width, length).\n\nOption `s` expects a string;\nif its argument is not a string,\nit is converted to one following the same rules of `tostring`.\nIf the option has any modifier (flags, width, length),\nthe string argument should not contain embedded zeros."}}, gmatch = {binding = "string.gmatch", metadata = {["fnl/arglist"] = {"s", "pattern"}, ["fnl/docstring"] = "Returns an iterator function that,\neach time it is called,\nreturns the next captures from `pattern`over the string `s`.\nIf `pattern` specifies no captures,\nthen the whole match is produced in each call.\n\nAs an example, the following loop\nwill iterate over all the words from string `s`,\nprinting one per line:\n\n```lua\n     s = \"hello world from Lua\"\n     for w in string.gmatch(s, \"%a+\") do\n       print(w)\n     end\n```\nThe next example collects all pairs `key=value` from the\ngiven string into a table:\n\n```lua\n     t = {}\n     s = \"from=world, to=Lua\"\n     for k, v in string.gmatch(s, \"(%w+)=(%w+)\") do\n       t[k] = v\n     end\n```\n\nFor this function, a caret `\"^\"` at the start of a pattern does not\nwork as an anchor, as this would prevent the iteration."}}, gsub = {binding = "string.gsub", metadata = {["fnl/arglist"] = {"s", "pattern", "repl", "?n"}, ["fnl/docstring"] = "Returns a copy of `s`\nin which all (or the first `?n`, if given)\noccurrences of the `pattern`have been\nreplaced by a replacement string specified by `repl`,\nwhich can be a string, a table, or a function.\n`gsub` also returns, as its second value,\nthe total number of matches that occurred.\nThe name `gsub` comes from *Global SUBstitution*.\n\nIf `repl` is a string, then its value is used for replacement.\nThe character `%` works as an escape character:\nany sequence in `repl` of the form `%*d*`,\nwith *d* between 1 and 9,\nstands for the value of the *d*-th captured substring.\nThe sequence `%0` stands for the whole match.\nThe sequence `%%` stands for a single `%`.\n\nIf `repl` is a table, then the table is queried for every match,\nusing the first capture as the key.\n\nIf `repl` is a function, then this function is called every time a\nmatch occurs, with all captured substrings passed as arguments,\nin order.\n\nIn any case,\nif the pattern specifies no captures,\nthen it behaves as if the whole pattern was inside a capture.\n\nIf the value returned by the table query or by the function call\nis a string or a number,\nthen it is used as the replacement string;\notherwise, if it is **false** or **nil**,\nthen there is no replacement\n(that is, the original match is kept in the string).\n\nHere are some examples:\n\n```lua\n     x = string.gsub(\"hello world\", \"(%w+)\", \"%1 %1\")\n     --> x=\"hello hello world world\"\n     \n     x = string.gsub(\"hello world\", \"%w+\", \"%0 %0\", 1)\n     --> x=\"hello hello world\"\n     \n     x = string.gsub(\"hello world from Lua\", \"(%w+)%s*(%w+)\", \"%2 %1\")\n     --> x=\"world hello Lua from\"\n     \n     x = string.gsub(\"home = $HOME, user = $USER\", \"%$(%w+)\", os.getenv)\n     --> x=\"home = /home/roberto, user = roberto\"\n     \n     x = string.gsub(\"4+5 = $return 4+5$\", \"%$(.-)%$\", function (s)\n           return load(s)()\n         end)\n     --> x=\"4+5 = 9\"\n     \n     local t = {name=\"lua\", version=\"5.3\"}\n     x = string.gsub(\"$name-$version.tar.gz\", \"%$(%w+)\", t)\n     --> x=\"lua-5.3.tar.gz\"\n```"}}, len = {binding = "string.len", metadata = {["fnl/arglist"] = {"s"}, ["fnl/docstring"] = "Receives a string and returns its length.\nThe empty string `\"\"` has length 0.\nEmbedded zeros are counted,\nso `\"a\\000bc\\000\"` has length 5."}}, lower = {binding = "string.lower", metadata = {["fnl/arglist"] = {"s"}, ["fnl/docstring"] = "Receives a string and returns a copy of this string with all\nuppercase letters changed to lowercase.\nAll other characters are left unchanged.\nThe definition of what an uppercase letter is depends on the current locale."}}, match = {binding = "string.match", metadata = {["fnl/arglist"] = {"s", "pattern", "?init"}, ["fnl/docstring"] = "Looks for the first *match* of\n`pattern`in the string `s`.\nIf it finds one, then `match` returns\nthe captures from the pattern;\notherwise it returns **nil**.\nIf `pattern` specifies no captures,\nthen the whole match is returned.\nA third, optional numeric argument `?init` specifies\nwhere to start the search;\nits default value is 1 and can be negative."}}, pack = {binding = "string.pack", metadata = {["fnl/arglist"] = {"fmt", "v1", "v2", "..."}, ["fnl/docstring"] = "Returns a binary string containing the values `v1`, `v2`, etc.\npacked (that is, serialized in binary form)\naccording to the format string `fmt`"}}, packsize = {binding = "string.packsize", metadata = {["fnl/arglist"] = {"fmt"}, ["fnl/docstring"] = "Returns the size of a string resulting from `string.pack`\nwith the given format.\nThe format string cannot have the variable-length options\n`\"s\"` or `\"z\"`"}}, rep = {binding = "string.rep", metadata = {["fnl/arglist"] = {"s", "n", "?sep"}, ["fnl/docstring"] = "Returns a string that is the concatenation of `n` copies of\nthe string `s` separated by the string `?sep`.\nThe default value for `?sep` is the empty string\n(that is, no separator).\nReturns the empty string if `n` is not positive.\n\n(Note that it is very easy to exhaust the memory of your machine\nwith a single call to this function.)"}}, reverse = {binding = "string.reverse", metadata = {["fnl/arglist"] = {"s"}, ["fnl/docstring"] = "Returns a string that is the string `s` reversed."}}, sub = {binding = "string.sub", metadata = {["fnl/arglist"] = {"s", "i", "?j"}, ["fnl/docstring"] = "Returns the substring of `s` that\nstarts at `i`  and continues until `?j`;\n`i` and `?j` can be negative.\nIf `?j` is absent, then it is assumed to be equal to -1\n(which is the same as the string length).\nIn particular,\nthe call `string.sub(s,1,j)` returns a prefix of `s`\nwith length `?j`,\nand `string.sub(s, -i)` (for a positive `i`)\nreturns a suffix of `s`\nwith length `i`.\n\nIf, after the translation of negative indices,\n`i` is less than 1,\nit is corrected to 1.\nIf `?j` is greater than the string length,\nit is corrected to that length.\nIf, after these corrections,\n`i` is greater than `?j`,\nthe function returns the empty string."}}, unpack = {binding = "string.unpack", metadata = {["fnl/arglist"] = {"fmt", "s", "?pos"}, ["fnl/docstring"] = "Returns the values packed in string `s` (see `string.pack`)\naccording to the format string `fmt`\nAn optional `?pos` marks where\nto start reading in `s` (default is 1).\nAfter the read values,\nthis function also returns the index of the first unread byte in `s`."}}, upper = {binding = "string.upper", metadata = {["fnl/arglist"] = {"s"}, ["fnl/docstring"] = "Receives a string and returns a copy of this string with all\nlowercase letters changed to uppercase.\nAll other characters are left unchanged.\nThe definition of what a lowercase letter is depends on the current locale."}}}, metadata = {["fnl/docstring"] = "This library provides generic functions for string manipulation,\nsuch as finding and extracting substrings, and pattern matching.\nWhen indexing a string in Lua, the first character is at position 1\n(not at 0, as in C).\nIndices are allowed to be negative and are interpreted as indexing backwards,\nfrom the end of the string.\nThus, the last character is at position -1, and so on.\n\nThe string library provides all its functions inside the table\n`string`.\nIt also sets a metatable for strings\nwhere the `__index` field points to the `string` table.\nTherefore, you can use the string functions in object-oriented style.\nFor instance, `string.byte(s,i)`\ncan be written as `s:byte(i)`.\n\nThe string library assumes one-byte character encodings."}}, table = {binding = "table", fields = {concat = {binding = "table.concat", metadata = {["fnl/arglist"] = {"list", "?sep", "?i", "?j"}, ["fnl/docstring"] = "Given a list where all elements are strings or numbers,\nreturns the string `list[i]..sep..list[i+1] ... sep..list[j]`.\nThe default value for `?sep` is the empty string,\nthe default for `?i` is 1,\nand the default for `?j` is `#list`.\nIf `?i` is greater than `?j`, returns the empty string."}}, insert = {binding = "table.insert", metadata = {["fnl/arglist"] = {"list", "value"}, ["fnl/docstring"] = "Inserts element `value` at position `pos` in `list`,\nshifting up the elements\n`list[pos], list[pos+1], ..., list[#list]`.\nThe default value for `pos` is `#list+1`,\nso that a call `table.insert(t,x)` inserts `x` at the end\nof list `t`."}}, move = {binding = "table.move", metadata = {["fnl/arglist"] = {"a1", "f", "e", "t", "?a2"}, ["fnl/docstring"] = "Moves elements from table `a1` to table `?a2`,\nperforming the equivalent to the following\nmultiple assignment:\n`a2[t],... = a1[f],...,a1[e]`.\nThe default for `?a2` is `a1`.\nThe destination range can overlap with the source range.\nThe number of elements to be moved must fit in a Lua integer.\n\nReturns the destination table `?a2`."}}, pack = {binding = "table.pack", metadata = {["fnl/arglist"] = {"..."}, ["fnl/docstring"] = "Returns a new table with all arguments stored into keys 1, 2, etc.\nand with a field `\"n\"` with the total number of arguments.\nNote that the resulting table may not be a sequence."}}, remove = {binding = "table.remove", metadata = {["fnl/arglist"] = {"list", "?pos"}, ["fnl/docstring"] = "Removes from `list` the element at position `?pos`,\nreturning the value of the removed element.\nWhen `?pos` is an integer between 1 and `#list`,\nit shifts down the elements\n`list[pos+1], list[pos+2], ..., list[#list]`\nand erases element `list[#list]`;\nThe index `?pos` can also be 0 when `#list` is 0,\nor `#list + 1`;\nin those cases, the function erases the element `list[pos]`.\n\nThe default value for `?pos` is `#list`,\nso that a call `table.remove(l)` removes the last element\nof list `l`."}}, sort = {binding = "table.sort", metadata = {["fnl/arglist"] = {"list", "?comp"}, ["fnl/docstring"] = "Sorts list elements in a given order, *in-place*,\nfrom `list[1]` to `list[#list]`.\nIf `?comp` is given,\nthen it must be a function that receives two list elements\nand returns true when the first element must come\nbefore the second in the final order\n(so that, after the sort,\n`i < j` implies `not comp(list[j],list[i])`).\nIf `?comp` is not given,\nthen the standard Lua operator `<` is used instead.\n\nNote that the `?comp` function must define\na strict partial order over the elements in the list;\nthat is, it must be asymmetric and transitive.\nOtherwise, no valid sort may be possible.\n\nThe sort algorithm is not stable:\nelements considered equal by the given order\nmay have their relative positions changed by the sort."}}, unpack = {binding = "table.unpack", metadata = {["fnl/arglist"] = {"list", "?i", "?j"}, ["fnl/docstring"] = "Returns the elements from the given list.\nThis function is equivalent to\n\n```lua\n     return list[i], list[i+1], ..., list[j]\n```\nBy default, `?i` is 1 and `?j` is `#list`."}}}, metadata = {["fnl/docstring"] = "This library provides generic functions for table manipulation.\nIt provides all its functions inside the table `table`.\n\nRemember that, whenever an operation needs the length of a table,\nall caveats about the length operator apply\nAll functions ignore non-numeric keys\nin the tables given as arguments."}}, tonumber = {binding = "tonumber", metadata = {["fnl/arglist"] = {"e", "?base"}, ["fnl/docstring"] = "When called with no `?base`,\n`tonumber` tries to convert its argument to a number.\nIf the argument is already a number or\na string convertible to a number,\nthen `tonumber` returns this number;\notherwise, it returns **nil**.\n\nThe conversion of strings can result in integers or floats,\naccording to the lexical conventions of Lua\n(The string may have leading and trailing spaces and a sign.)\n\nWhen called with `?base`,\nthen `e` must be a string to be interpreted as\nan integer numeral in that base.\nThe base may be any integer between 2 and 36, inclusive.\nIn bases above 10, the letter `\"A\"` (in either upper or lower case)\nrepresents 10, `\"B\"` represents 11, and so forth,\nwith `\"Z\"` representing 35.\nIf the string `e` is not a valid numeral in the given base,\nthe function returns **nil**."}}, tostring = {binding = "tostring", metadata = {["fnl/arglist"] = {"v"}, ["fnl/docstring"] = "Receives a value of any type and\nconverts it to a string in a human-readable format.\n(For complete control of how numbers are converted,\nuse `string.format`.)\n\nIf the metatable of `v` has a `__tostring` field,\nthen `tostring` calls the corresponding value\nwith `v` as argument,\nand uses the result of the call as its result."}}, type = {binding = "type", metadata = {["fnl/arglist"] = {"v"}, ["fnl/docstring"] = "Returns the type of its only argument, coded as a string.\nThe possible results of this function are\n`\"nil\"` (a string, not the value **nil**),\n`\"number\"`,\n`\"string\"`,\n`\"boolean\"`,\n`\"table\"`,\n`\"function\"`,\n`\"thread\"`,\nand `\"userdata\"`."}}, utf8 = {binding = "utf8", fields = {char = {binding = "utf8.char", metadata = {["fnl/arglist"] = {"..."}, ["fnl/docstring"] = "Receives zero or more integers,\nconverts each one to its corresponding UTF-8 byte sequence\nand returns a string with the concatenation of all these sequences."}}, charpattern = {binding = "utf8.charpattern", metadata = {["fnl/docstring"] = "The pattern (a string, not a function) `\"[\\0-\\x7F\\xC2-\\xF4][\\x80-\\xBF]*\"`\nwhich matches exactly one UTF-8 byte sequence,\nassuming that the subject is a valid UTF-8 string."}}, codepoint = {binding = "utf8.codepoint", metadata = {["fnl/arglist"] = {"s", "?i", "?j"}, ["fnl/docstring"] = "Returns the codepoints (as integers) from all characters in `s`\nthat start between byte position `?i` and `?j` (both included).\nThe default for `?i` is 1 and for `?j` is `?i`.\nIt raises an error if it meets any invalid byte sequence."}}, codes = {binding = "utf8.codes", metadata = {["fnl/arglist"] = {"s"}, ["fnl/docstring"] = "Returns values so that the construction\n\n```lua\n     for p, c in utf8.codes(s) do *body* end\n```\nwill iterate over all characters in string `s`,\nwith `p` being the position (in bytes) and `c` the code point\nof each character.\nIt raises an error if it meets any invalid byte sequence."}}, len = {binding = "utf8.len", metadata = {["fnl/arglist"] = {"s", "?i", "?j"}, ["fnl/docstring"] = "Returns the number of UTF-8 characters in string `s`\nthat start between positions `?i` and `?j` (both inclusive).\nThe default for `?i` is 1 and for `?j` is -1.\nIf it finds any invalid byte sequence,\nreturns a false value plus the position of the first invalid byte."}}, offset = {binding = "utf8.offset", metadata = {["fnl/arglist"] = {"s", "n", "?i"}, ["fnl/docstring"] = "Returns the position (in bytes) where the encoding of the\n`n`-th character of `s`\n(counting from position `?i`) starts.\nA negative `n` gets characters before position `?i`.\nThe default for `?i` is 1 when `n` is non-negative\nand `#s + 1` otherwise,\nso that `utf8.offset(s, -n)` gets the offset of the\n`n`-th character from the end of the string.\nIf the specified character is neither in the subject\nnor right after its end,\nthe function returns **nil**.\n\nAs a special case,\nwhen `n` is 0 the function returns the start of the encoding\nof the character that contains the `?i`-th byte of `s`.\n\nThis function assumes that `s` is a valid UTF-8 string."}}}, metadata = {["fnl/docstring"] = "This library provides basic support for UTF-8 encoding.\nIt provides all its functions inside the table `utf8`.\nThis library does not provide any support for Unicode other\nthan the handling of the encoding.\nAny operation that needs the meaning of a character,\nsuch as character classification, is outside its scope.\n\nUnless stated otherwise,\nall functions that expect a byte position as a parameter\nassume that the given position is either the start of a byte sequence\nor one plus the length of the subject string.\nAs in the string library,\nnegative indices count from the end of the string."}}, xpcall = {binding = "xpcall", metadata = {["fnl/arglist"] = {"f", "msgh", "?arg1", "..."}, ["fnl/docstring"] = "This function is similar to `pcall`,\nexcept that it sets a new message handler `msgh`."}}}
  docs._G.fields = docs
  docs.io.fields.stdin.fields = docs.io.fields
  docs.io.fields.stdout.fields = docs.io.fields
  docs.io.fields.stderr.fields = docs.io.fields
  return docs
end
package.preload["fennel-ls.docs.generated.lua54"] = package.preload["fennel-ls.docs.generated.lua54"] or function(...)
  local docs = {_G = {binding = "_G", metadata = {["fnl/docstring"] = "A global variable (not a function) that\nholds the global environment\nLua itself does not use this variable;\nchanging its value does not affect any environment,\nnor vice versa."}}, _VERSION = {binding = "_VERSION", metadata = {["fnl/docstring"] = "A global variable (not a function) that\nholds a string containing the running Lua version.\nThe current value of this variable is `\"Lua 5.4\"`."}}, arg = {binding = "arg", metadata = {["fnl/docstring"] = "Before running any code,\n`lua` collects all command-line arguments\nin a global table called `arg`.\nThe script name goes to index 0,\nthe first argument after the script name goes to index 1,\nand so on.\nAny arguments before the script name\n(that is, the interpreter name plus its options)\ngo to negative indices.\nFor instance, in the call\n\n```lua\n     $ lua -la b.lua t1 t2\n```\nthe table is like this:\n\n```lua\n     arg = { [-2] = \"lua\", [-1] = \"-la\",\n             [0] = \"b.lua\",\n             [1] = \"t1\", [2] = \"t2\" }\n```\nIf there is no script in the call,\nthe interpreter name goes to index 0,\nfollowed by the other arguments.\nFor instance, the call\n\n```lua\n     $ lua -e \"print(arg[1])\"\n```\nwill print `\"-e\"`.\nIf there is a script,\nthe script is called with arguments\n`arg[1]`, ..., `arg[#arg]`.\nLike all chunks in Lua,\nthe script is compiled as a variadic function."}}, assert = {binding = "assert", metadata = {["fnl/arglist"] = {"v", "?message"}, ["fnl/docstring"] = "Raises an error if\nthe value of its argument `v` is false (i.e., **nil** or **false**);\notherwise, returns all its arguments.\nIn case of error,\n`?message` is the error object;\nwhen absent, it defaults to `\"assertion failed!\"`"}}, collectgarbage = {binding = "collectgarbage", metadata = {["fnl/arglist"] = {"?opt", "?arg"}, ["fnl/docstring"] = "This function is a generic interface to the garbage collector.\nIt performs different functions according to its first argument, `?opt`:\n\n* **`\"collect\"`: **\n  Performs a full garbage-collection cycle.\n  This is the default option.\n\n* **`\"stop\"`: **\n  Stops automatic execution of the garbage collector.\n  The collector will run only when explicitly invoked,\n  until a call to restart it.\n\n* **`\"restart\"`: **\n  Restarts automatic execution of the garbage collector.\n\n* **`\"count\"`: **\n  Returns the total memory in use by Lua in Kbytes.\n  The value has a fractional part,\n  so that it multiplied by 1024\n  gives the exact number of bytes in use by Lua.\n\n* **`\"step\"`: **\n  Performs a garbage-collection step.\n  The step \"size\" is controlled by `?arg`.\n  With a zero value,\n  the collector will perform one basic (indivisible) step.\n  For non-zero values,\n  the collector will perform as if that amount of memory\n  (in Kbytes) had been allocated by Lua.\n  Returns **true** if the step finished a collection cycle.\n\n* **`\"isrunning\"`: **\n  Returns a boolean that tells whether the collector is running\n  (i.e., not stopped).\n\n* **`\"incremental\"`: **\n  Change the collector mode to incremental.\n  This option can be followed by three numbers:\n  the garbage-collector pause,\n  the step multiplier,\n  and the step size\n  A zero means to not change that value.\n\n* **`\"generational\"`: **\n  Change the collector mode to generational.\n  This option can be followed by two numbers:\n  the garbage-collector minor multiplier\n  and the major multiplier\n  A zero means to not change that value.\n\nThis function should not be called by a finalizer."}}, coroutine = {binding = "coroutine", fields = {close = {binding = "coroutine.close", metadata = {["fnl/arglist"] = {"co"}, ["fnl/docstring"] = "Closes coroutine `co`,\nthat is,\ncloses all its pending to-be-closed variables\nand puts the coroutine in a dead state.\nThe given coroutine must be dead or suspended.\nIn case of error\n(either the original error that stopped the coroutine or\nerrors in closing methods),\nreturns **false** plus the error object;\notherwise returns **true**."}}, create = {binding = "coroutine.create", metadata = {["fnl/arglist"] = {"f"}, ["fnl/docstring"] = "Creates a new coroutine, with body `f`.\n`f` must be a function.\nReturns this new coroutine,\nan object with type `\"thread\"`."}}, isyieldable = {binding = "coroutine.isyieldable", metadata = {["fnl/arglist"] = {"?co"}, ["fnl/docstring"] = "Returns **true** when the coroutine `?co` can yield.\nThe default for `?co` is the running coroutine.\n\nA coroutine is yieldable if it is not the main thread and\nit is not inside a non-yieldable C function."}}, resume = {binding = "coroutine.resume", metadata = {["fnl/arglist"] = {"co", "?val1", "..."}, ["fnl/docstring"] = "Starts or continues the execution of coroutine `co`.\nThe first time you resume a coroutine,\nit starts running its body.\nThe values `?val1`, ... are passed\nas the arguments to the body function.\nIf the coroutine has yielded,\n`resume` restarts it;\nthe values `?val1`, ... are passed\nas the results from the yield.\n\nIf the coroutine runs without any errors,\n`resume` returns **true** plus any values passed to `yield`\n(when the coroutine yields) or any values returned by the body function\n(when the coroutine terminates).\nIf there is any error,\n`resume` returns **false** plus the error message."}}, running = {binding = "coroutine.running", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Returns the running coroutine plus a boolean,\n**true** when the running coroutine is the main one."}}, status = {binding = "coroutine.status", metadata = {["fnl/arglist"] = {"co"}, ["fnl/docstring"] = "Returns the status of the coroutine `co`, as a string:\n`\"running\"`,\nif the coroutine is running\n(that is, it is the one that called `status`);\n`\"suspended\"`, if the coroutine is suspended in a call to `yield`,\nor if it has not started running yet;\n`\"normal\"` if the coroutine is active but not running\n(that is, it has resumed another coroutine);\nand `\"dead\"` if the coroutine has finished its body function,\nor if it has stopped with an error."}}, wrap = {binding = "coroutine.wrap", metadata = {["fnl/arglist"] = {"f"}, ["fnl/docstring"] = "Creates a new coroutine, with body `f`;\n`f` must be a function.\nReturns a function that resumes the coroutine each time it is called.\nAny arguments passed to this function behave as the\nextra arguments to `resume`.\nThe function returns the same values returned by `resume`,\nexcept the first boolean.\nIn case of error,\nthe function closes the coroutine and propagates the error."}}, yield = {binding = "coroutine.yield", metadata = {["fnl/arglist"] = {"..."}, ["fnl/docstring"] = "Suspends the execution of the calling coroutine.\nAny arguments to `yield` are passed as extra results to `resume`."}}}, metadata = {["fnl/docstring"] = "This library comprises the operations to manipulate coroutines,\nwhich come inside the table `coroutine`."}}, debug = {binding = "debug", fields = {debug = {binding = "debug.debug", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Enters an interactive mode with the user,\nrunning each string that the user enters.\nUsing simple commands and other debug facilities,\nthe user can inspect global and local variables,\nchange their values, evaluate expressions, and so on.\nA line containing only the word `cont` finishes this function,\nso that the caller continues its execution.\n\nNote that commands for `debug.debug` are not lexically nested\nwithin any function and so have no direct access to local variables."}}, gethook = {binding = "debug.gethook", metadata = {["fnl/arglist"] = {"?thread"}, ["fnl/docstring"] = "Returns the current hook settings of the thread, as three values:\nthe current hook function, the current hook mask,\nand the current hook count,\nas set by the `debug.sethook` function.\n\nReturns **fail** if there is no active hook."}}, getinfo = {binding = "debug.getinfo", metadata = {["fnl/arglist"] = {"f", "?what"}, ["fnl/docstring"] = "Returns a table with information about a function.\nYou can give the function directly\nor you can give a number as the value of `f`,\nwhich means the function running at level `f` of the call stack\nof the given thread:\nlevel 0 is the current function (`getinfo` itself);\nlevel 1 is the function that called `getinfo`\n(except for tail calls, which do not count in the stack);\nand so on.\nIf `f` is a number greater than the number of active functions,\nthen `getinfo` returns **fail**.\n\nThe returned table can contain all the fields returned by `lua_getinfo`,\nwith the string `?what` describing which fields to fill in.\nThe default for `?what` is to get all information available,\nexcept the table of valid lines.\nThe option `\"f\"`\nadds a field named `func` with the function itself.\nThe option `\"L\"` adds a field named `activelines`\nwith the table of valid lines,\nprovided the function is a Lua function.\nIf the function has no debug information,\nthe table is empty.\n\nFor instance, the expression `debug.getinfo(1,\"n\").name` returns\na name for the current function,\nif a reasonable name can be found,\nand the expression `debug.getinfo(print)`\nreturns a table with all available information\nabout the `print` function."}}, getlocal = {binding = "debug.getlocal", metadata = {["fnl/arglist"] = {"f", "local"}, ["fnl/docstring"] = "This function returns the name and the value of the local variable\nwith index `local` of the function at level `f` of the stack.\nThis function accesses not only explicit local variables,\nbut also parameters and temporary values.\n\nThe first parameter or local variable has index 1, and so on,\nfollowing the order that they are declared in the code,\ncounting only the variables that are active\nin the current scope of the function.\nCompile-time constants may not appear in this listing,\nif they were optimized away by the compiler.\nNegative indices refer to vararg arguments;\n-1 is the first vararg argument.\nThe function returns **fail**\nif there is no variable with the given index,\nand raises an error when called with a level out of range.\n(You can call `debug.getinfo` to check whether the level is valid.)\n\nVariable names starting with `\"(\"` (open parenthesis) \nrepresent variables with no known names\n(internal variables such as loop control variables,\nand variables from chunks saved without debug information).\n\nThe parameter `f` may also be a function.\nIn that case, `getlocal` returns only the name of function parameters."}}, getmetatable = {binding = "debug.getmetatable", metadata = {["fnl/arglist"] = {"value"}, ["fnl/docstring"] = "Returns the metatable of the given `value`\nor **nil** if it does not have a metatable."}}, getregistry = {binding = "debug.getregistry", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Returns the registry table"}}, getupvalue = {binding = "debug.getupvalue", metadata = {["fnl/arglist"] = {"f", "up"}, ["fnl/docstring"] = "This function returns the name and the value of the upvalue\nwith index `up` of the function `f`.\nThe function returns **fail**\nif there is no upvalue with the given index.\n\n(For Lua functions,\nupvalues are the external local variables that the function uses,\nand that are consequently included in its closure.)\n\nFor C functions, this function uses the empty string `\"\"`\nas a name for all upvalues.\n\nVariable name `\"?\"` (interrogation mark)\nrepresents variables with no known names\n(variables from chunks saved without debug information)."}}, getuservalue = {binding = "debug.getuservalue", metadata = {["fnl/arglist"] = {"u", "n"}, ["fnl/docstring"] = "Returns the `n`-th user value associated\nto the userdata `u` plus a boolean,\n**false** if the userdata does not have that value."}}, sethook = {binding = "debug.sethook", metadata = {["fnl/arglist"] = {"hook", "mask", "?count"}, ["fnl/docstring"] = "Sets the given function as the debug hook.\nThe string `mask` and the number `?count` describe\nwhen the hook will be called.\nThe string mask may have any combination of the following characters,\nwith the given meaning:\n\n* **`\"c\"`: ** the hook is called every time Lua calls a function;\n* **`\"r\"`: ** the hook is called every time Lua returns from a function;\n* **`\"l\"`: ** the hook is called every time Lua enters a new line of code.\n\nMoreover,\nwith a `?count` different from zero,\nthe hook is called also after every `?count` instructions.\n\nWhen called without arguments,\n`debug.sethook` turns off the hook.\n\nWhen the hook is called, its first parameter is a string\ndescribing the event that has triggered its call:\n`\"call\"`, `\"tail call\"`, `\"return\"`,\n`\"line\"`, and `\"count\"`.\nFor line events,\nthe hook also gets the new line number as its second parameter.\nInside a hook,\nyou can call `getinfo` with level 2 to get more information about\nthe running function.\n(Level 0 is the `getinfo` function,\nand level 1 is the hook function.)"}}, setlocal = {binding = "debug.setlocal", metadata = {["fnl/arglist"] = {"level", "local", "value"}, ["fnl/docstring"] = "This function assigns the value `value` to the local variable\nwith index `local` of the function at level `level` of the stack.\nThe function returns **fail** if there is no local\nvariable with the given index,\nand raises an error when called with a `level` out of range.\n(You can call `getinfo` to check whether the level is valid.)\nOtherwise, it returns the name of the local variable.\n\nSee `debug.getlocal` for more information about\nvariable indices and names."}}, setmetatable = {binding = "debug.setmetatable", metadata = {["fnl/arglist"] = {"value", "table"}, ["fnl/docstring"] = "Sets the metatable for the given `value` to the given `table`\n(which can be **nil**).\nReturns `value`."}}, setupvalue = {binding = "debug.setupvalue", metadata = {["fnl/arglist"] = {"f", "up", "value"}, ["fnl/docstring"] = "This function assigns the value `value` to the upvalue\nwith index `up` of the function `f`.\nThe function returns **fail** if there is no upvalue\nwith the given index.\nOtherwise, it returns the name of the upvalue.\n\nSee `debug.getupvalue` for more information about upvalues."}}, setuservalue = {binding = "debug.setuservalue", metadata = {["fnl/arglist"] = {"udata", "value", "n"}, ["fnl/docstring"] = "Sets the given `value` as\nthe `n`-th user value associated to the given `udata`.\n`udata` must be a full userdata.\n\nReturns `udata`,\nor **fail** if the userdata does not have that value."}}, traceback = {binding = "debug.traceback", metadata = {["fnl/arglist"] = {"?message", "?level"}, ["fnl/docstring"] = "If `?message` is present but is neither a string nor **nil**,\nthis function returns `?message` without further processing.\nOtherwise,\nit returns a string with a traceback of the call stack.\nThe optional `?message` string is appended\nat the beginning of the traceback.\nAn optional `?level` number tells at which level\nto start the traceback\n(default is 1, the function calling `traceback`)."}}, upvalueid = {binding = "debug.upvalueid", metadata = {["fnl/arglist"] = {"f", "n"}, ["fnl/docstring"] = "Returns a unique identifier (as a light userdata)\nfor the upvalue numbered `n`\nfrom the given function.\n\nThese unique identifiers allow a program to check whether different\nclosures share upvalues.\nLua closures that share an upvalue\n(that is, that access a same external local variable)\nwill return identical ids for those upvalue indices."}}, upvaluejoin = {binding = "debug.upvaluejoin", metadata = {["fnl/arglist"] = {"f1", "n1", "f2", "n2"}, ["fnl/docstring"] = "Make the `n1`-th upvalue of the Lua closure `f1`\nrefer to the `n2`-th upvalue of the Lua closure `f2`."}}}, metadata = {["fnl/docstring"] = "This library provides\nthe functionality of the debug interfaceto Lua programs.\nYou should exert care when using this library.\nSeveral of its functions\nviolate basic assumptions about Lua code\n(e.g., that variables local to a function\ncannot be accessed from outside;\nthat userdata metatables cannot be changed by Lua code;\nthat Lua programs do not crash)\nand therefore can compromise otherwise secure code.\nMoreover, some functions in this library may be slow.\n\nAll functions in this library are provided\ninside the `debug` table.\nAll functions that operate over a thread\nhave an optional first argument which is the\nthread to operate over.\nThe default is always the current thread."}}, dofile = {binding = "dofile", metadata = {["fnl/arglist"] = {"?filename"}, ["fnl/docstring"] = "Opens the named file and executes its content as a Lua chunk.\nWhen called without arguments,\n`dofile` executes the content of the standard input (`stdin`).\nReturns all values returned by the chunk.\nIn case of errors, `dofile` propagates the error\nto its caller.\n(That is, `dofile` does not run in protected mode.)"}}, error = {binding = "error", metadata = {["fnl/arglist"] = {"message", "?level"}, ["fnl/docstring"] = "Raises an errorwith `message` as the error object.\nThis function never returns.\n\nUsually, `error` adds some information about the error position\nat the beginning of the message, if the message is a string.\nThe `?level` argument specifies how to get the error position.\nWith level 1 (the default), the error position is where the\n`error` function was called.\nLevel 2 points the error to where the function\nthat called `error` was called; and so on.\nPassing a level 0 avoids the addition of error position information\nto the message."}}, getmetatable = {binding = "getmetatable", metadata = {["fnl/arglist"] = {"object"}, ["fnl/docstring"] = "If `object` does not have a metatable, returns **nil**.\nOtherwise,\nif the object's metatable has a `__metatable` field,\nreturns the associated value.\nOtherwise, returns the metatable of the given object."}}, io = {binding = "io", fields = {close = {binding = "io.close", metadata = {["fnl/arglist"] = {"?file"}, ["fnl/docstring"] = "Equivalent to `file:close()`.\nWithout a `?file`, closes the default output file."}}, flush = {binding = "io.flush", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Equivalent to `io.output():flush()`."}}, input = {binding = "io.input", metadata = {["fnl/arglist"] = {"?file"}, ["fnl/docstring"] = "When called with a file name, it opens the named file (in text mode),\nand sets its handle as the default input file.\nWhen called with a file handle,\nit simply sets this file handle as the default input file.\nWhen called without arguments,\nit returns the current default input file.\n\nIn case of errors this function raises the error,\ninstead of returning an error code."}}, lines = {binding = "io.lines", metadata = {["fnl/arglist"] = {"?filename", "..."}, ["fnl/docstring"] = "Opens the given file name in read mode\nand returns an iterator function that\nworks like `file:lines(...)` over the opened file.\nWhen the iterator function fails to read any value,\nit automatically closes the file.\nBesides the iterator function,\n`io.lines` returns three other values:\ntwo **nil** values as placeholders,\nplus the created file handle.\nTherefore, when used in a generic **for** loop,\nthe file is closed also if the loop is interrupted by an\nerror or a **break**.\n\nThe call `io.lines()` (with no file name) is equivalent\nto `io.input():lines(\"l\")`;\nthat is, it iterates over the lines of the default input file.\nIn this case, the iterator does not close the file when the loop ends.\n\nIn case of errors opening the file,\nthis function raises the error,\ninstead of returning an error code."}}, open = {binding = "io.open", metadata = {["fnl/arglist"] = {"filename", "?mode"}, ["fnl/docstring"] = "This function opens a file,\nin the mode specified in the string `?mode`.\nIn case of success,\nit returns a new file handle.\n\nThe `?mode` string can be any of the following:\n\n* **`\"r\"`: ** read mode (the default);\n* **`\"w\"`: ** write mode;\n* **`\"a\"`: ** append mode;\n* **`\"r+\"`: ** update mode, all previous data is preserved;\n* **`\"w+\"`: ** update mode, all previous data is erased;\n* **`\"a+\"`: ** append update mode, previous data is preserved,\n    writing is only allowed at the end of file.\n\nThe `?mode` string can also have a `\"b\"` at the end,\nwhich is needed in some systems to open the file in binary mode."}}, output = {binding = "io.output", metadata = {["fnl/arglist"] = {"?file"}, ["fnl/docstring"] = "Similar to `io.input`, but operates over the default output file."}}, popen = {binding = "io.popen", metadata = {["fnl/arglist"] = {"prog", "?mode"}, ["fnl/docstring"] = "This function is system dependent and is not available\non all platforms.\n\nStarts the program `prog` in a separated process and returns\na file handle that you can use to read data from this program\n(if `?mode` is `\"r\"`, the default)\nor to write data to this program\n(if `?mode` is `\"w\"`)."}}, read = {binding = "io.read", metadata = {["fnl/arglist"] = {"..."}, ["fnl/docstring"] = "Equivalent to `io.input():read(...)`."}}, stderr = {binding = "io.stderr", metadata = {["fnl/docstring"] = "stderr file"}}, stdin = {binding = "io.stdin", metadata = {["fnl/docstring"] = "stdin file"}}, stdout = {binding = "io.stdout", metadata = {["fnl/docstring"] = "stdout file"}}, tmpfile = {binding = "io.tmpfile", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "In case of success,\nreturns a handle for a temporary file.\nThis file is opened in update mode\nand it is automatically removed when the program ends."}}, type = {binding = "io.type", metadata = {["fnl/arglist"] = {"obj"}, ["fnl/docstring"] = "Checks whether `obj` is a valid file handle.\nReturns the string `\"file\"` if `obj` is an open file handle,\n`\"closed file\"` if `obj` is a closed file handle,\nor **fail** if `obj` is not a file handle."}}, write = {binding = "io.write", metadata = {["fnl/arglist"] = {"..."}, ["fnl/docstring"] = "Equivalent to `io.output():write(...)`."}}}, metadata = {["fnl/docstring"] = "The I/O library provides two different styles for file manipulation.\nThe first one uses implicit file handles;\nthat is, there are operations to set a default input file and a\ndefault output file,\nand all input/output operations are done over these default files.\nThe second style uses explicit file handles.\n\nWhen using implicit file handles,\nall operations are supplied by table `io`.\nWhen using explicit file handles,\nthe operation `io.open` returns a file handle\nand then all operations are supplied as methods of the file handle.\n\nThe metatable for file handles provides metamethods\nfor `__gc` and `__close` that try\nto close the file when called.\n\nThe table `io` also provides\nthree predefined file handles with their usual meanings from C:\n`io.stdin`, `io.stdout`, and `io.stderr`.\nThe I/O library never closes these files.\n\nUnless otherwise stated,\nall I/O functions return **fail** on failure,\nplus an error message as a second result and\na system-dependent error code as a third result,\nand some non-false value on success.\nOn non-POSIX systems,\nthe computation of the error message and error code\nin case of errors\nmay be not thread safe,\nbecause they rely on the global C variable `errno`."}}, ipairs = {binding = "ipairs", metadata = {["fnl/arglist"] = {"t"}, ["fnl/docstring"] = "Returns three values (an iterator function, the table `t`, and 0)\nso that the construction\n\n```lua\n     for i,v in ipairs(t) do *body* end\n```\nwill iterate over the key\226\128\147value pairs\n(`1,t[1]`), (`2,t[2]`), ...,\nup to the first absent index."}}, load = {binding = "load", metadata = {["fnl/arglist"] = {"chunk", "?chunkname", "?mode", "?env"}, ["fnl/docstring"] = "Loads a chunk.\n\nIf `chunk` is a string, the chunk is this string.\nIf `chunk` is a function,\n`load` calls it repeatedly to get the chunk pieces.\nEach call to `chunk` must return a string that concatenates\nwith previous results.\nA return of an empty string, **nil**, or no value signals the end of the chunk.\n\nIf there are no syntactic errors,\n`load` returns the compiled chunk as a function;\notherwise, it returns **fail** plus the error message.\n\nWhen you load a main chunk,\nthe resulting function will always have exactly one upvalue,\nthe `_ENV` variable\nHowever,\nwhen you load a binary chunk created from a function (see `string.dump`),\nthe resulting function can have an arbitrary number of upvalues,\nand there is no guarantee that its first upvalue will be\nthe `_ENV` variable.\n(A non-main function may not even have an `_ENV` upvalue.)\n\nRegardless, if the resulting function has any upvalues,\nits first upvalue is set to the value of `?env`,\nif that parameter is given,\nor to the value of the global environment.\nOther upvalues are initialized with **nil**.\nAll upvalues are fresh, that is,\nthey are not shared with any other function.\n\n`?chunkname` is used as the name of the chunk for error messages\nand debug information\nWhen absent,\nit defaults to `chunk`, if `chunk` is a string,\nor to `\"=(load)\"` otherwise.\n\nThe string `?mode` controls whether the chunk can be text or binary\n(that is, a precompiled chunk).\nIt may be the string `\"b\"` (only binary chunks),\n`\"t\"` (only text chunks),\nor `\"bt\"` (both binary and text).\nThe default is `\"bt\"`.\n\nIt is safe to load malformed binary chunks;\n`load` signals an appropriate error.\nHowever,\nLua does not check the consistency of the code inside binary chunks;\nrunning maliciously crafted bytecode can crash the interpreter."}}, loadfile = {binding = "loadfile", metadata = {["fnl/arglist"] = {"?filename", "?mode", "?env"}, ["fnl/docstring"] = "Similar to `load`,\nbut gets the chunk from file `?filename`\nor from the standard input,\nif no file name is given."}}, math = {binding = "math", fields = {abs = {binding = "math.abs", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the maximum value between `x` and `-x`. (integer/float)"}}, acos = {binding = "math.acos", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the arc cosine of `x` (in radians)."}}, asin = {binding = "math.asin", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the arc sine of `x` (in radians)."}}, atan = {binding = "math.atan", metadata = {["fnl/arglist"] = {"y", "?x"}, ["fnl/docstring"] = "Returns the arc tangent of `y/x` (in radians),\nusing the signs of both arguments to find the\nquadrant of the result.\nIt also handles correctly the case of `?x` being zero.\n\nThe default value for `?x` is 1,\nso that the call `math.atan(y)`\nreturns the arc tangent of `y`."}}, ceil = {binding = "math.ceil", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the smallest integral value greater than or equal to `x`."}}, cos = {binding = "math.cos", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the cosine of `x` (assumed to be in radians)."}}, deg = {binding = "math.deg", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Converts the angle `x` from radians to degrees."}}, exp = {binding = "math.exp", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the value *e\203\163*\n(where `e` is the base of natural logarithms)."}}, floor = {binding = "math.floor", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the largest integral value less than or equal to `x`."}}, fmod = {binding = "math.fmod", metadata = {["fnl/arglist"] = {"x", "y"}, ["fnl/docstring"] = "Returns the remainder of the division of `x` by `y`\nthat rounds the quotient towards zero. (integer/float)"}}, huge = {binding = "math.huge", metadata = {["fnl/docstring"] = "The float value `HUGE_VAL`,\na value greater than any other numeric value."}}, log = {binding = "math.log", metadata = {["fnl/arglist"] = {"x", "?base"}, ["fnl/docstring"] = "Returns the logarithm of `x` in the given base.\nThe default for `?base` is *e*\n(so that the function returns the natural logarithm of `x`)."}}, max = {binding = "math.max", metadata = {["fnl/arglist"] = {"x", "..."}, ["fnl/docstring"] = "Returns the argument with the maximum value,\naccording to the Lua operator `<`."}}, maxinteger = {binding = "math.maxinteger", metadata = {["fnl/docstring"] = "An integer with the maximum value for an integer."}}, min = {binding = "math.min", metadata = {["fnl/arglist"] = {"x", "..."}, ["fnl/docstring"] = "Returns the argument with the minimum value,\naccording to the Lua operator `<`."}}, mininteger = {binding = "math.mininteger", metadata = {["fnl/docstring"] = "An integer with the minimum value for an integer."}}, modf = {binding = "math.modf", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the integral part of `x` and the fractional part of `x`.\nIts second result is always a float."}}, pi = {binding = "math.pi", metadata = {["fnl/docstring"] = "The value of *\207\128*."}}, rad = {binding = "math.rad", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Converts the angle `x` from degrees to radians."}}, random = {binding = "math.random", metadata = {["fnl/arglist"] = {"?m", "?n"}, ["fnl/docstring"] = "When called without arguments,\nreturns a pseudo-random float with uniform distribution\nin the range  *[0,1)*.  \nWhen called with two integers `?m` and `?n`,\n`math.random` returns a pseudo-random integer\nwith uniform distribution in the range *[m, n]*.\nThe call `math.random(n)`, for a positive `?n`,\nis equivalent to `math.random(1,n)`.\nThe call `math.random(0)` produces an integer with\nall bits (pseudo)random.\n\nThis function uses the `xoshiro256**` algorithm to produce\npseudo-random 64-bit integers,\nwhich are the results of calls with argument 0.\nOther results (ranges and floats)\nare unbiased extracted from these integers.\n\nLua initializes its pseudo-random generator with the equivalent of\na call to `math.randomseed` with no arguments,\nso that `math.random` should generate\ndifferent sequences of results each time the program runs."}}, randomseed = {binding = "math.randomseed", metadata = {["fnl/arglist"] = {"?x", "?y"}, ["fnl/docstring"] = "When called with at least one argument,\nthe integer parameters `?x` and `?y` are\njoined into a 128-bit *seed* that\nis used to reinitialize the pseudo-random generator;\nequal seeds produce equal sequences of numbers.\nThe default for `?y` is zero.\n\nWhen called with no arguments,\nLua generates a seed with\na weak attempt for randomness.\n\nThis function returns the two seed components\nthat were effectively used,\nso that setting them again repeats the sequence.\n\nTo ensure a required level of randomness to the initial state\n(or contrarily, to have a deterministic sequence,\nfor instance when debugging a program),\nyou should call `math.randomseed` with explicit arguments."}}, sin = {binding = "math.sin", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the sine of `x` (assumed to be in radians)."}}, sqrt = {binding = "math.sqrt", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the square root of `x`.\n(You can also use the expression `x^0.5` to compute this value.)"}}, tan = {binding = "math.tan", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns the tangent of `x` (assumed to be in radians)."}}, tointeger = {binding = "math.tointeger", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "If the value `x` is convertible to an integer,\nreturns that integer.\nOtherwise, returns **fail**."}}, type = {binding = "math.type", metadata = {["fnl/arglist"] = {"x"}, ["fnl/docstring"] = "Returns `\"integer\"` if `x` is an integer,\n`\"float\"` if it is a float,\nor **fail** if `x` is not a number."}}, ult = {binding = "math.ult", metadata = {["fnl/arglist"] = {"m", "n"}, ["fnl/docstring"] = "Returns a boolean,\n**true** if and only if integer `m` is below integer `n` when\nthey are compared as unsigned integers."}}}, metadata = {["fnl/docstring"] = "This library provides basic mathematical functions.\nIt provides all its functions and constants inside the table `math`.\nFunctions with the annotation `\"integer/float\"` give\ninteger results for integer arguments\nand float results for non-integer arguments.\nThe rounding functions\n`math.ceil`, `math.floor`, and `math.modf`\nreturn an integer when the result fits in the range of an integer,\nor a float otherwise."}}, next = {binding = "next", metadata = {["fnl/arglist"] = {"table", "?index"}, ["fnl/docstring"] = "Allows a program to traverse all fields of a table.\nIts first argument is a table and its second argument\nis an index in this table.\nA call to `next` returns the next index of the table\nand its associated value.\nWhen called with **nil** as its second argument,\n`next` returns an initial index\nand its associated value.\nWhen called with the last index,\nor with **nil** in an empty table,\n`next` returns **nil**.\nIf the second argument is absent, then it is interpreted as **nil**.\nIn particular,\nyou can use `next(t)` to check whether a table is empty.\n\nThe order in which the indices are enumerated is not specified,\n*even for numeric indices*.\n(To traverse a table in numerical order,\nuse a numerical **for**.)\n\nYou should not assign any value to a non-existent field in a table\nduring its traversal.\nYou may however modify existing fields.\nIn particular, you may set existing fields to nil."}}, os = {binding = "os", fields = {clock = {binding = "os.clock", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Returns an approximation of the amount in seconds of CPU time\nused by the program,\nas returned by the underlying ISO C function `clock`."}}, date = {binding = "os.date", metadata = {["fnl/arglist"] = {"?format", "?time"}, ["fnl/docstring"] = "Returns a string or a table containing date and time,\nformatted according to the given string `?format`.\n\nIf the `?time` argument is present,\nthis is the time to be formatted\n(see the `os.time` function for a description of this value).\nOtherwise, `date` formats the current time.\n\nIf `?format` starts with `\"!\"`,\nthen the date is formatted in Coordinated Universal Time.\nAfter this optional character,\nif `?format` is the string `\"*t\"`,\nthen `date` returns a table with the following fields:\n`year`, `month` (1\226\128\14712), `day` (1\226\128\14731),\n`hour` (0\226\128\14723), `min` (0\226\128\14759),\n`sec` (0\226\128\14761, due to leap seconds),\n`wday` (weekday, 1\226\128\1477, Sunday is 1),\n`yday` (day of the year, 1\226\128\147366),\nand `isdst` (daylight saving flag, a boolean).\nThis last field may be absent\nif the information is not available.\n\nIf `?format` is not `\"*t\"`,\nthen `date` returns the date as a string,\nformatted according to the same rules as the ISO C function `strftime`.\n\nIf `?format` is absent, it defaults to `\"%c\"`,\nwhich gives a human-readable date and time representation\nusing the current locale.\n\nOn non-POSIX systems,\nthis function may be not thread safe\nbecause of its reliance on C function `gmtime` and C function `localtime`."}}, difftime = {binding = "os.difftime", metadata = {["fnl/arglist"] = {"t2", "t1"}, ["fnl/docstring"] = "Returns the difference, in seconds,\nfrom time `t1` to time `t2`\n(where the times are values returned by `os.time`).\nIn POSIX, Windows, and some other systems,\nthis value is exactly `t2`*-*`t1`."}}, execute = {binding = "os.execute", metadata = {["fnl/arglist"] = {"?command"}, ["fnl/docstring"] = "This function is equivalent to the ISO C function `system`.\nIt passes `?command` to be executed by an operating system shell.\nIts first result is **true**\nif the command terminated successfully,\nor **fail** otherwise.\nAfter this first result\nthe function returns a string plus a number,\nas follows:\n\n* **`\"exit\"`: **\n  the command terminated normally;\n  the following number is the exit status of the command.\n\n* **`\"signal\"`: **\n  the command was terminated by a signal;\n  the following number is the signal that terminated the command.\n\nWhen called without a `?command`,\n`os.execute` returns a boolean that is true if a shell is available."}}, exit = {binding = "os.exit", metadata = {["fnl/arglist"] = {"?code", "?close"}, ["fnl/docstring"] = "Calls the ISO C function `exit` to terminate the host program.\nIf `?code` is **true**,\nthe returned status is `EXIT_SUCCESS`;\nif `?code` is **false**,\nthe returned status is `EXIT_FAILURE`;\nif `?code` is a number,\nthe returned status is this number.\nThe default value for `?code` is **true**.\n\nIf the optional second argument `?close` is true,\nthe function closes the Lua state before exiting (see `lua_close`)."}}, getenv = {binding = "os.getenv", metadata = {["fnl/arglist"] = {"varname"}, ["fnl/docstring"] = "Returns the value of the process environment variable `varname`\nor **fail** if the variable is not defined."}}, remove = {binding = "os.remove", metadata = {["fnl/arglist"] = {"filename"}, ["fnl/docstring"] = "Deletes the file (or empty directory, on POSIX systems)\nwith the given name.\nIf this function fails, it returns **fail**\nplus a string describing the error and the error code.\nOtherwise, it returns true."}}, rename = {binding = "os.rename", metadata = {["fnl/arglist"] = {"oldname", "newname"}, ["fnl/docstring"] = "Renames the file or directory named `oldname` to `newname`.\nIf this function fails, it returns **fail**,\nplus a string describing the error and the error code.\nOtherwise, it returns true."}}, setlocale = {binding = "os.setlocale", metadata = {["fnl/arglist"] = {"locale", "?category"}, ["fnl/docstring"] = "Sets the current locale of the program.\n`locale` is a system-dependent string specifying a locale;\n`?category` is an optional string describing which category to change:\n`\"all\"`, `\"collate\"`, `\"ctype\"`,\n`\"monetary\"`, `\"numeric\"`, or `\"time\"`;\nthe default category is `\"all\"`.\nThe function returns the name of the new locale,\nor **fail** if the request cannot be honored.\n\nIf `locale` is the empty string,\nthe current locale is set to an implementation-defined native locale.\nIf `locale` is the string `\"C\"`,\nthe current locale is set to the standard C locale.\n\nWhen called with **nil** as the first argument,\nthis function only returns the name of the current locale\nfor the given category.\n\nThis function may be not thread safe\nbecause of its reliance on C function `setlocale`."}}, time = {binding = "os.time", metadata = {["fnl/arglist"] = {"?table"}, ["fnl/docstring"] = "Returns the current time when called without arguments,\nor a time representing the local date and time specified by the given table.\nThis table must have fields `year`, `month`, and `day`,\nand may have fields\n`hour` (default is 12),\n`min` (default is 0),\n`sec` (default is 0),\nand `isdst` (default is **nil**).\nOther fields are ignored.\nFor a description of these fields, see the `os.date` function.\n\nWhen the function is called,\nthe values in these fields do not need to be inside their valid ranges.\nFor instance, if `sec` is -10,\nit means 10 seconds before the time specified by the other fields;\nif `hour` is 1000,\nit means 1000 hours after the time specified by the other fields.\n\nThe returned value is a number, whose meaning depends on your system.\nIn POSIX, Windows, and some other systems,\nthis number counts the number\nof seconds since some given start time (the \"epoch\").\nIn other systems, the meaning is not specified,\nand the number returned by `time` can be used only as an argument to\n`os.date` and `os.difftime`.\n\nWhen called with a table,\n`os.time` also normalizes all the fields\ndocumented in the `os.date` function,\nso that they represent the same time as before the call\nbut with values inside their valid ranges."}}, tmpname = {binding = "os.tmpname", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Returns a string with a file name that can\nbe used for a temporary file.\nThe file must be explicitly opened before its use\nand explicitly removed when no longer needed.\n\nIn POSIX systems,\nthis function also creates a file with that name,\nto avoid security risks.\n(Someone else might create the file with wrong permissions\nin the time between getting the name and creating the file.)\nYou still have to open the file to use it\nand to remove it (even if you do not use it).\n\nWhen possible,\nyou may prefer to use `io.tmpfile`,\nwhich automatically removes the file when the program ends."}}}, metadata = {["fnl/docstring"] = "This library is implemented through table `os`."}}, package = {binding = "package", fields = {config = {binding = "package.config", metadata = {["fnl/docstring"] = "A string describing some compile-time configurations for packages.\nThis string is a sequence of lines:\n\n* The first line is the directory separator string.\n  Default is `\"\\\"` for Windows and `\"/\"` for all other systems.\n\n* The second line is the character that separates templates in a path.\n  Default is `\";\"`.\n\n* The third line is the string that marks the\n  substitution points in a template.\n  Default is `\"?\"`.\n\n* The fourth line is a string that, in a path in Windows,\n  is replaced by the executable's directory.\n  Default is `\"!\"`.\n\n* The fifth line is a mark to ignore all text after it\n  when building the `luaopen_` function name.\n  Default is `\"-\"`."}}, cpath = {binding = "package.cpath", metadata = {["fnl/docstring"] = "A string with the path used by `require`\nto search for a C loader.\n\nLua initializes the C path `package.cpath` in the same way\nit initializes the Lua path `package.path`,\nusing the environment variable `LUA_CPATH_5_4`,\nor the environment variable `LUA_CPATH`,\nor a default path defined in `luaconf.h`."}}, loaded = {binding = "package.loaded", metadata = {["fnl/docstring"] = "A table used by `require` to control which\nmodules are already loaded.\nWhen you require a module `modname` and\n`package.loaded[modname]` is not false,\n`require` simply returns the value stored there.\n\nThis variable is only a reference to the real table;\nassignments to this variable do not change the\ntable used by `require`.\nThe real table is stored in the C registry\nindexed by the key `LUA_LOADED_TABLE`, a string."}}, loadlib = {binding = "package.loadlib", metadata = {["fnl/arglist"] = {"libname", "funcname"}, ["fnl/docstring"] = "Dynamically links the host program with the C library `libname`.\n\nIf `funcname` is `\"*\"`,\nthen it only links with the library,\nmaking the symbols exported by the library\navailable to other dynamically linked libraries.\nOtherwise,\nit looks for a function `funcname` inside the library\nand returns this function as a C function.\nSo, `funcname` must follow the `lua_CFunction` prototype\n(see `lua_CFunction`).\n\nThis is a low-level function.\nIt completely bypasses the package and module system.\nUnlike `require`,\nit does not perform any path searching and\ndoes not automatically adds extensions.\n`libname` must be the complete file name of the C library,\nincluding if necessary a path and an extension.\n`funcname` must be the exact name exported by the C library\n(which may depend on the C compiler and linker used).\n\nThis functionality is not supported by ISO C.\nAs such, it is only available on some platforms\n(Windows, Linux, Mac OS X, Solaris, BSD,\nplus other Unix systems that support the `dlfcn` standard).\n\nThis function is inherently insecure,\nas it allows Lua to call any function in any readable dynamic\nlibrary in the system.\n(Lua calls any function assuming the function\nhas a proper prototype and respects a proper protocol\n(see `lua_CFunction`).\nTherefore,\ncalling an arbitrary function in an arbitrary dynamic library\nmore often than not results in an access violation.)"}}, path = {binding = "package.path", metadata = {["fnl/docstring"] = "A string with the path used by `require`\nto search for a Lua loader.\n\nAt start-up, Lua initializes this variable with\nthe value of the environment variable `LUA_PATH_5_4` or\nthe environment variable `LUA_PATH` or\nwith a default path defined in `luaconf.h`,\nif those environment variables are not defined.\nA `\";;\"` in the value of the environment variable\nis replaced by the default path."}}, preload = {binding = "package.preload", metadata = {["fnl/docstring"] = "A table to store loaders for specific modules\n(see `require`).\n\nThis variable is only a reference to the real table;\nassignments to this variable do not change the\ntable used by `require`.\nThe real table is stored in the C registry\nindexed by the key `LUA_PRELOAD_TABLE`, a string."}}, searchers = {binding = "package.searchers", metadata = {["fnl/docstring"] = "A table used by `require` to control how to find modules.\n\nEach entry in this table is a *searcher function*.\nWhen looking for a module,\n`require` calls each of these searchers in ascending order,\nwith the module name (the argument given to `require`) as its\nsole argument.\nIf the searcher finds the module,\nit returns another function, the module *loader*,\nplus an extra value, a *loader data*,\nthat will be passed to that loader and\nreturned as a second result by `require`.\nIf it cannot find the module,\nit returns a string explaining why\n(or **nil** if it has nothing to say).\n\nLua initializes this table with four searcher functions.\n\nThe first searcher simply looks for a loader in the\n`package.preload` table.\n\nThe second searcher looks for a loader as a Lua library,\nusing the path stored at `package.path`.\nThe search is done as described in function `package.searchpath`.\n\nThe third searcher looks for a loader as a C library,\nusing the path given by the variable `package.cpath`.\nAgain,\nthe search is done as described in function `package.searchpath`.\nFor instance,\nif the C path is the string\n\n```lua\n     \"./?.so;./?.dll;/usr/local/?/init.so\"\n```\nthe searcher for module `foo`\nwill try to open the files `./foo.so`, `./foo.dll`,\nand `/usr/local/foo/init.so`, in that order.\nOnce it finds a C library,\nthis searcher first uses a dynamic link facility to link the\napplication with the library.\nThen it tries to find a C function inside the library to\nbe used as the loader.\nThe name of this C function is the string `\"luaopen_\"`\nconcatenated with a copy of the module name where each dot\nis replaced by an underscore.\nMoreover, if the module name has a hyphen,\nits suffix after (and including) the first hyphen is removed.\nFor instance, if the module name is `a.b.c-v2.1`,\nthe function name will be `luaopen_a_b_c`.\n\nThe fourth searcher tries an *all-in-one loader*.\nIt searches the C path for a library for\nthe root name of the given module.\nFor instance, when requiring `a.b.c`,\nit will search for a C library for `a`.\nIf found, it looks into it for an open function for\nthe submodule;\nin our example, that would be `luaopen_a_b_c`.\nWith this facility, a package can pack several C submodules\ninto one single library,\nwith each submodule keeping its original open function.\n\nAll searchers except the first one (preload) return as the extra value\nthe file path where the module was found,\nas returned by `package.searchpath`.\nThe first searcher always returns the string `\":preload:\"`.\n\nSearchers should raise no errors and have no side effects in Lua.\n(They may have side effects in C,\nfor instance by linking the application with a library.)"}}, searchpath = {binding = "package.searchpath", metadata = {["fnl/arglist"] = {"name", "path", "?sep", "?rep"}, ["fnl/docstring"] = "Searches for the given `name` in the given `path`.\n\nA path is a string containing a sequence of\n*templates* separated by semicolons.\nFor each template,\nthe function replaces each interrogation mark (if any)\nin the template with a copy of `name`\nwherein all occurrences of `?sep`\n(a dot, by default)\nwere replaced by `?rep`\n(the system's directory separator, by default),\nand then tries to open the resulting file name.\n\nFor instance, if the path is the string\n\n```lua\n     \"./?.lua;./?.lc;/usr/local/?/init.lua\"\n```\nthe search for the name `foo.a`\nwill try to open the files\n`./foo/a.lua`, `./foo/a.lc`, and\n`/usr/local/foo/a/init.lua`, in that order.\n\nReturns the resulting name of the first file that it can\nopen in read mode (after closing the file),\nor **fail** plus an error message if none succeeds.\n(This error message lists all file names it tried to open.)"}}}, metadata = {["fnl/docstring"] = "The package library provides basic\nfacilities for loading modules in Lua.\nIt exports one function directly in the global environment:\n`require`.\nEverything else is exported in the table `package`."}}, pairs = {binding = "pairs", metadata = {["fnl/arglist"] = {"t"}, ["fnl/docstring"] = "If `t` has a metamethod `__pairs`,\ncalls it with `t` as argument and returns the first three\nresults from the call.\n\nOtherwise,\nreturns three values: the `next` function, the table `t`, and **nil**,\nso that the construction\n\n```lua\n     for k,v in pairs(t) do *body* end\n```\nwill iterate over all key\226\128\147value pairs of table `t`.\n\nSee function `next` for the caveats of modifying\nthe table during its traversal."}}, pcall = {binding = "pcall", metadata = {["fnl/arglist"] = {"f", "?arg1", "..."}, ["fnl/docstring"] = "Calls the function `f` with\nthe given arguments in *protected mode*.\nThis means that any error inside `f` is not propagated;\ninstead, `pcall` catches the error\nand returns a status code.\nIts first result is the status code (a boolean),\nwhich is **true** if the call succeeds without errors.\nIn such case, `pcall` also returns all results from the call,\nafter this first result.\nIn case of any error, `pcall` returns **false** plus the error object.\nNote that errors caught by `pcall` do not call a message handler."}}, print = {binding = "print", metadata = {["fnl/arglist"] = {"..."}, ["fnl/docstring"] = "Receives any number of arguments\nand prints their values to `stdout`,\nconverting each argument to a string\nfollowing the same rules of `tostring`.\n\nThe function `print` is not intended for formatted output,\nbut only as a quick way to show a value,\nfor instance for debugging.\nFor complete control over the output,\nuse `string.format` and `io.write`."}}, rawequal = {binding = "rawequal", metadata = {["fnl/arglist"] = {"v1", "v2"}, ["fnl/docstring"] = "Checks whether `v1` is equal to `v2`,\nwithout invoking the `__eq` metamethod.\nReturns a boolean."}}, rawget = {binding = "rawget", metadata = {["fnl/arglist"] = {"table", "index"}, ["fnl/docstring"] = "Gets the real value of `table[index]`,\nwithout using the `__index` metavalue.\n`table` must be a table;\n`index` may be any value."}}, rawlen = {binding = "rawlen", metadata = {["fnl/arglist"] = {"v"}, ["fnl/docstring"] = "Returns the length of the object `v`,\nwhich must be a table or a string,\nwithout invoking the `__len` metamethod.\nReturns an integer."}}, rawset = {binding = "rawset", metadata = {["fnl/arglist"] = {"table", "index", "value"}, ["fnl/docstring"] = "Sets the real value of `table[index]` to `value`,\nwithout using the `__newindex` metavalue.\n`table` must be a table,\n`index` any value different from **nil** and NaN,\nand `value` any Lua value.\n\nThis function returns `table`."}}, require = {binding = "require", metadata = {["fnl/arglist"] = {"modname"}, ["fnl/docstring"] = "Loads the given module.\nThe function starts by looking into the `package.loaded` table\nto determine whether `modname` is already loaded.\nIf it is, then `require` returns the value stored\nat `package.loaded[modname]`.\n(The absence of a second result in this case\nsignals that this call did not have to load the module.)\nOtherwise, it tries to find a *loader* for the module.\n\nTo find a loader,\n`require` is guided by the table `package.searchers`.\nEach item in this table is a search function,\nthat searches for the module in a particular way.\nBy changing this table,\nwe can change how `require` looks for a module.\nThe following explanation is based on the default configuration\nfor `package.searchers`.\n\nFirst `require` queries `package.preload[modname]`.\nIf it has a value,\nthis value (which must be a function) is the loader.\nOtherwise `require` searches for a Lua loader using the\npath stored in `package.path`.\nIf that also fails, it searches for a C loader using the\npath stored in `package.cpath`.\nIf that also fails,\nit tries an *all-in-one* loader (see `package.searchers`).\n\nOnce a loader is found,\n`require` calls the loader with two arguments:\n`modname` and an extra value,\na *loader data*,\nalso returned by the searcher.\nThe loader data can be any value useful to the module;\nfor the default searchers,\nit indicates where the loader was found.\n(For instance, if the loader came from a file,\nthis extra value is the file path.)\nIf the loader returns any non-nil value,\n`require` assigns the returned value to `package.loaded[modname]`.\nIf the loader does not return a non-nil value and\nhas not assigned any value to `package.loaded[modname]`,\nthen `require` assigns **true** to this entry.\nIn any case, `require` returns the\nfinal value of `package.loaded[modname]`.\nBesides that value, `require` also returns as a second result\nthe loader data returned by the searcher,\nwhich indicates how `require` found the module.\n\nIf there is any error loading or running the module,\nor if it cannot find any loader for the module,\nthen `require` raises an error."}}, select = {binding = "select", metadata = {["fnl/arglist"] = {"index", "..."}, ["fnl/docstring"] = "If `index` is a number,\nreturns all arguments after argument number `index`;\na negative number indexes from the end (-1 is the last argument).\nOtherwise, `index` must be the string `\"#\"`,\nand `select` returns the total number of extra arguments it received."}}, setmetatable = {binding = "setmetatable", metadata = {["fnl/arglist"] = {"table", "metatable"}, ["fnl/docstring"] = "Sets the metatable for the given table.\nIf `metatable` is **nil**,\nremoves the metatable of the given table.\nIf the original metatable has a `__metatable` field,\nraises an error.\n\nThis function returns `table`.\n\nTo change the metatable of other types from Lua code,\nyou must use the debug library"}}, string = {binding = "string", fields = {byte = {binding = "string.byte", metadata = {["fnl/arglist"] = {"s", "?i", "?j"}, ["fnl/docstring"] = "Returns the internal numeric codes of the characters `s[i]`,\n`s[i+1]`, ..., `s[j]`.\nThe default value for `?i` is 1;\nthe default value for `?j` is `?i`.\nThese indices are corrected\nfollowing the same rules of function `string.sub`.\n\nNumeric codes are not necessarily portable across platforms."}}, char = {binding = "string.char", metadata = {["fnl/arglist"] = {"..."}, ["fnl/docstring"] = "Receives zero or more integers.\nReturns a string with length equal to the number of arguments,\nin which each character has the internal numeric code equal\nto its corresponding argument.\n\nNumeric codes are not necessarily portable across platforms."}}, dump = {binding = "string.dump", metadata = {["fnl/arglist"] = {"function", "?strip"}, ["fnl/docstring"] = "Returns a string containing a binary representation\n(a *binary chunk*)\nof the given function,\nso that a later `load` on this string returns\na copy of the function (but with new upvalues).\nIf `?strip` is a true value,\nthe binary representation may not include all debug information\nabout the function,\nto save space.\n\nFunctions with upvalues have only their number of upvalues saved.\nWhen (re)loaded,\nthose upvalues receive fresh instances.\n(See the `load` function for details about\nhow these upvalues are initialized.\nYou can use the debug library to serialize\nand reload the upvalues of a function\nin a way adequate to your needs.)"}}, find = {binding = "string.find", metadata = {["fnl/arglist"] = {"s", "pattern", "?init", "?plain"}, ["fnl/docstring"] = "Looks for the first match of\n`pattern`in the string `s`.\nIf it finds a match, then `find` returns the indices of `s`\nwhere this occurrence starts and ends;\notherwise, it returns **fail**.\nA third, optional numeric argument `?init` specifies\nwhere to start the search;\nits default value is 1 and can be negative.\nA **true** as a fourth, optional argument `?plain`\nturns off the pattern matching facilities,\nso the function does a plain \"find substring\" operation,\nwith no characters in `pattern` being considered magic.\n\nIf the pattern has captures,\nthen in a successful match\nthe captured values are also returned,\nafter the two indices."}}, format = {binding = "string.format", metadata = {["fnl/arglist"] = {"formatstring", "..."}, ["fnl/docstring"] = "Returns a formatted version of its variable number of arguments\nfollowing the description given in its first argument,\nwhich must be a string.\nThe format string follows the same rules as the ISO C function `sprintf`.\nThe only differences are that the conversion specifiers and modifiers\n`F`, `n`, `*`, `h`, `L`, and `l` are not supported\nand that there is an extra specifier, `q`.\nBoth width and precision, when present,\nare limited to two digits.\n\nThe specifier `q` formats booleans, nil, numbers, and strings\nin a way that the result is a valid constant in Lua source code.\nBooleans and nil are written in the obvious way\n(`true`, `false`, `nil`).\nFloats are written in hexadecimal,\nto preserve full precision.\nA string is written between double quotes,\nusing escape sequences when necessary to ensure that\nit can safely be read back by the Lua interpreter.\nFor instance, the call\n\n```lua\n     string.format('%q', 'a string with \"quotes\" and \\n new line')\n```\nmay produce the string:\n\n```lua\n     \"a string with \\\"quotes\\\" and \\\n      new line\"\n```\nThis specifier does not support modifiers (flags, width, precision).\n\nThe conversion specifiers\n`A`, `a`, `E`, `e`, `f`,\n`G`, and `g` all expect a number as argument.\nThe specifiers `c`, `d`,\n`i`, `o`, `u`, `X`, and `x`\nexpect an integer.\nWhen Lua is compiled with a C89 compiler,\nthe specifiers `A` and `a` (hexadecimal floats)\ndo not support modifiers.\n\nThe specifier `s` expects a string;\nif its argument is not a string,\nit is converted to one following the same rules of `tostring`.\nIf the specifier has any modifier,\nthe corresponding string argument should not contain embedded zeros.\n\nThe specifier `p` formats the pointer\nreturned by `lua_topointer`.\nThat gives a unique string identifier for tables, userdata,\nthreads, strings, and functions.\nFor other values (numbers, nil, booleans),\nthis specifier results in a string representing\nthe pointer `NULL`."}}, gmatch = {binding = "string.gmatch", metadata = {["fnl/arglist"] = {"s", "pattern", "?init"}, ["fnl/docstring"] = "Returns an iterator function that,\neach time it is called,\nreturns the next captures from `pattern`over the string `s`.\nIf `pattern` specifies no captures,\nthen the whole match is produced in each call.\nA third, optional numeric argument `?init` specifies\nwhere to start the search;\nits default value is 1 and can be negative.\n\nAs an example, the following loop\nwill iterate over all the words from string `s`,\nprinting one per line:\n\n```lua\n     s = \"hello world from Lua\"\n     for w in string.gmatch(s, \"%a+\") do\n       print(w)\n     end\n```\nThe next example collects all pairs `key=value` from the\ngiven string into a table:\n\n```lua\n     t = {}\n     s = \"from=world, to=Lua\"\n     for k, v in string.gmatch(s, \"(%w+)=(%w+)\") do\n       t[k] = v\n     end\n```\n\nFor this function, a caret `\"^\"` at the start of a pattern does not\nwork as an anchor, as this would prevent the iteration."}}, gsub = {binding = "string.gsub", metadata = {["fnl/arglist"] = {"s", "pattern", "repl", "?n"}, ["fnl/docstring"] = "Returns a copy of `s`\nin which all (or the first `?n`, if given)\noccurrences of the `pattern`have been\nreplaced by a replacement string specified by `repl`,\nwhich can be a string, a table, or a function.\n`gsub` also returns, as its second value,\nthe total number of matches that occurred.\nThe name `gsub` comes from *Global SUBstitution*.\n\nIf `repl` is a string, then its value is used for replacement.\nThe character `%` works as an escape character:\nany sequence in `repl` of the form `%*d*`,\nwith *d* between 1 and 9,\nstands for the value of the *d*-th captured substring;\nthe sequence `%0` stands for the whole match;\nthe sequence `%%` stands for a single `%`.\n\nIf `repl` is a table, then the table is queried for every match,\nusing the first capture as the key.\n\nIf `repl` is a function, then this function is called every time a\nmatch occurs, with all captured substrings passed as arguments,\nin order.\n\nIn any case,\nif the pattern specifies no captures,\nthen it behaves as if the whole pattern was inside a capture.\n\nIf the value returned by the table query or by the function call\nis a string or a number,\nthen it is used as the replacement string;\notherwise, if it is **false** or **nil**,\nthen there is no replacement\n(that is, the original match is kept in the string).\n\nHere are some examples:\n\n```lua\n     x = string.gsub(\"hello world\", \"(%w+)\", \"%1 %1\")\n     --> x=\"hello hello world world\"\n     \n     x = string.gsub(\"hello world\", \"%w+\", \"%0 %0\", 1)\n     --> x=\"hello hello world\"\n     \n     x = string.gsub(\"hello world from Lua\", \"(%w+)%s*(%w+)\", \"%2 %1\")\n     --> x=\"world hello Lua from\"\n     \n     x = string.gsub(\"home = $HOME, user = $USER\", \"%$(%w+)\", os.getenv)\n     --> x=\"home = /home/roberto, user = roberto\"\n     \n     x = string.gsub(\"4+5 = $return 4+5$\", \"%$(.-)%$\", function (s)\n           return load(s)()\n         end)\n     --> x=\"4+5 = 9\"\n     \n     local t = {name=\"lua\", version=\"5.4\"}\n     x = string.gsub(\"$name-$version.tar.gz\", \"%$(%w+)\", t)\n     --> x=\"lua-5.4.tar.gz\"\n```"}}, len = {binding = "string.len", metadata = {["fnl/arglist"] = {"s"}, ["fnl/docstring"] = "Receives a string and returns its length.\nThe empty string `\"\"` has length 0.\nEmbedded zeros are counted,\nso `\"a\\000bc\\000\"` has length 5."}}, lower = {binding = "string.lower", metadata = {["fnl/arglist"] = {"s"}, ["fnl/docstring"] = "Receives a string and returns a copy of this string with all\nuppercase letters changed to lowercase.\nAll other characters are left unchanged.\nThe definition of what an uppercase letter is depends on the current locale."}}, match = {binding = "string.match", metadata = {["fnl/arglist"] = {"s", "pattern", "?init"}, ["fnl/docstring"] = "Looks for the first *match* of\nthe `pattern`in the string `s`.\nIf it finds one, then `match` returns\nthe captures from the pattern;\notherwise it returns **fail**.\nIf `pattern` specifies no captures,\nthen the whole match is returned.\nA third, optional numeric argument `?init` specifies\nwhere to start the search;\nits default value is 1 and can be negative."}}, pack = {binding = "string.pack", metadata = {["fnl/arglist"] = {"fmt", "v1", "v2", "..."}, ["fnl/docstring"] = "Returns a binary string containing the values `v1`, `v2`, etc.\nserialized in binary form (packed)\naccording to the format string `fmt`"}}, packsize = {binding = "string.packsize", metadata = {["fnl/arglist"] = {"fmt"}, ["fnl/docstring"] = "Returns the length of a string resulting from `string.pack`\nwith the given format.\nThe format string cannot have the variable-length options\n`\"s\"` or `\"z\"`"}}, rep = {binding = "string.rep", metadata = {["fnl/arglist"] = {"s", "n", "?sep"}, ["fnl/docstring"] = "Returns a string that is the concatenation of `n` copies of\nthe string `s` separated by the string `?sep`.\nThe default value for `?sep` is the empty string\n(that is, no separator).\nReturns the empty string if `n` is not positive.\n\n(Note that it is very easy to exhaust the memory of your machine\nwith a single call to this function.)"}}, reverse = {binding = "string.reverse", metadata = {["fnl/arglist"] = {"s"}, ["fnl/docstring"] = "Returns a string that is the string `s` reversed."}}, sub = {binding = "string.sub", metadata = {["fnl/arglist"] = {"s", "i", "?j"}, ["fnl/docstring"] = "Returns the substring of `s` that\nstarts at `i`  and continues until `?j`;\n`i` and `?j` can be negative.\nIf `?j` is absent, then it is assumed to be equal to -1\n(which is the same as the string length).\nIn particular,\nthe call `string.sub(s,1,j)` returns a prefix of `s`\nwith length `?j`,\nand `string.sub(s, -i)` (for a positive `i`)\nreturns a suffix of `s`\nwith length `i`.\n\nIf, after the translation of negative indices,\n`i` is less than 1,\nit is corrected to 1.\nIf `?j` is greater than the string length,\nit is corrected to that length.\nIf, after these corrections,\n`i` is greater than `?j`,\nthe function returns the empty string."}}, unpack = {binding = "string.unpack", metadata = {["fnl/arglist"] = {"fmt", "s", "?pos"}, ["fnl/docstring"] = "Returns the values packed in string `s` (see `string.pack`)\naccording to the format string `fmt`\nAn optional `?pos` marks where\nto start reading in `s` (default is 1).\nAfter the read values,\nthis function also returns the index of the first unread byte in `s`."}}, upper = {binding = "string.upper", metadata = {["fnl/arglist"] = {"s"}, ["fnl/docstring"] = "Receives a string and returns a copy of this string with all\nlowercase letters changed to uppercase.\nAll other characters are left unchanged.\nThe definition of what a lowercase letter is depends on the current locale."}}}, metadata = {["fnl/docstring"] = "This library provides generic functions for string manipulation,\nsuch as finding and extracting substrings, and pattern matching.\nWhen indexing a string in Lua, the first character is at position 1\n(not at 0, as in C).\nIndices are allowed to be negative and are interpreted as indexing backwards,\nfrom the end of the string.\nThus, the last character is at position -1, and so on.\n\nThe string library provides all its functions inside the table\n`string`.\nIt also sets a metatable for strings\nwhere the `__index` field points to the `string` table.\nTherefore, you can use the string functions in object-oriented style.\nFor instance, `string.byte(s,i)`\ncan be written as `s:byte(i)`.\n\nThe string library assumes one-byte character encodings."}}, table = {binding = "table", fields = {concat = {binding = "table.concat", metadata = {["fnl/arglist"] = {"list", "?sep", "?i", "?j"}, ["fnl/docstring"] = "Given a list where all elements are strings or numbers,\nreturns the string `list[i]..sep..list[i+1] ... sep..list[j]`.\nThe default value for `?sep` is the empty string,\nthe default for `?i` is 1,\nand the default for `?j` is `#list`.\nIf `?i` is greater than `?j`, returns the empty string."}}, insert = {binding = "table.insert", metadata = {["fnl/arglist"] = {"list", "value"}, ["fnl/docstring"] = "Inserts element `value` at position `pos` in `list`,\nshifting up the elements\n`list[pos], list[pos+1], ..., list[#list]`.\nThe default value for `pos` is `#list+1`,\nso that a call `table.insert(t,x)` inserts `x` at the end\nof the list `t`."}}, move = {binding = "table.move", metadata = {["fnl/arglist"] = {"a1", "f", "e", "t", "?a2"}, ["fnl/docstring"] = "Moves elements from the table `a1` to the table `?a2`,\nperforming the equivalent to the following\nmultiple assignment:\n`a2[t],... = a1[f],...,a1[e]`.\nThe default for `?a2` is `a1`.\nThe destination range can overlap with the source range.\nThe number of elements to be moved must fit in a Lua integer.\n\nReturns the destination table `?a2`."}}, pack = {binding = "table.pack", metadata = {["fnl/arglist"] = {"..."}, ["fnl/docstring"] = "Returns a new table with all arguments stored into keys 1, 2, etc.\nand with a field `\"n\"` with the total number of arguments.\nNote that the resulting table may not be a sequence,\nif some arguments are **nil**."}}, remove = {binding = "table.remove", metadata = {["fnl/arglist"] = {"list", "?pos"}, ["fnl/docstring"] = "Removes from `list` the element at position `?pos`,\nreturning the value of the removed element.\nWhen `?pos` is an integer between 1 and `#list`,\nit shifts down the elements\n`list[pos+1], list[pos+2], ..., list[#list]`\nand erases element `list[#list]`;\nThe index `?pos` can also be 0 when `#list` is 0,\nor `#list + 1`.\n\nThe default value for `?pos` is `#list`,\nso that a call `table.remove(l)` removes the last element\nof the list `l`."}}, sort = {binding = "table.sort", metadata = {["fnl/arglist"] = {"list", "?comp"}, ["fnl/docstring"] = "Sorts the list elements in a given order, *in-place*,\nfrom `list[1]` to `list[#list]`.\nIf `?comp` is given,\nthen it must be a function that receives two list elements\nand returns true when the first element must come\nbefore the second in the final order,\nso that, after the sort,\n`i <= j` implies `not comp(list[j],list[i])`.\nIf `?comp` is not given,\nthen the standard Lua operator `<` is used instead.\n\nThe `?comp` function must define a consistent order;\nmore formally, the function must define a strict weak order.\n(A weak order is similar to a total order,\nbut it can equate different elements for comparison purposes.)\n\nThe sort algorithm is not stable:\nDifferent elements considered equal by the given order\nmay have their relative positions changed by the sort."}}, unpack = {binding = "table.unpack", metadata = {["fnl/arglist"] = {"list", "?i", "?j"}, ["fnl/docstring"] = "Returns the elements from the given list.\nThis function is equivalent to\n\n```lua\n     return list[i], list[i+1], ..., list[j]\n```\nBy default, `?i` is 1 and `?j` is `#list`."}}}, metadata = {["fnl/docstring"] = "This library provides generic functions for table manipulation.\nIt provides all its functions inside the table `table`.\n\nRemember that, whenever an operation needs the length of a table,\nall caveats about the length operator apply\nAll functions ignore non-numeric keys\nin the tables given as arguments."}}, tonumber = {binding = "tonumber", metadata = {["fnl/arglist"] = {"e", "?base"}, ["fnl/docstring"] = "When called with no `?base`,\n`tonumber` tries to convert its argument to a number.\nIf the argument is already a number or\na string convertible to a number,\nthen `tonumber` returns this number;\notherwise, it returns **fail**.\n\nThe conversion of strings can result in integers or floats,\naccording to the lexical conventions of Lua\nThe string may have leading and trailing spaces and a sign.\n\nWhen called with `?base`,\nthen `e` must be a string to be interpreted as\nan integer numeral in that base.\nThe base may be any integer between 2 and 36, inclusive.\nIn bases above 10, the letter `\"A\"` (in either upper or lower case)\nrepresents 10, `\"B\"` represents 11, and so forth,\nwith `\"Z\"` representing 35.\nIf the string `e` is not a valid numeral in the given base,\nthe function returns **fail**."}}, tostring = {binding = "tostring", metadata = {["fnl/arglist"] = {"v"}, ["fnl/docstring"] = "Receives a value of any type and\nconverts it to a string in a human-readable format.\n\nIf the metatable of `v` has a `__tostring` field,\nthen `tostring` calls the corresponding value\nwith `v` as argument,\nand uses the result of the call as its result.\nOtherwise, if the metatable of `v` has a `__name` field\nwith a string value,\n`tostring` may use that string in its final result.\n\nFor complete control of how numbers are converted,\nuse `string.format`."}}, type = {binding = "type", metadata = {["fnl/arglist"] = {"v"}, ["fnl/docstring"] = "Returns the type of its only argument, coded as a string.\nThe possible results of this function are\n`\"nil\"` (a string, not the value **nil**),\n`\"number\"`,\n`\"string\"`,\n`\"boolean\"`,\n`\"table\"`,\n`\"function\"`,\n`\"thread\"`,\nand `\"userdata\"`."}}, utf8 = {binding = "utf8", fields = {char = {binding = "utf8.char", metadata = {["fnl/arglist"] = {"..."}, ["fnl/docstring"] = "Receives zero or more integers,\nconverts each one to its corresponding UTF-8 byte sequence\nand returns a string with the concatenation of all these sequences."}}, charpattern = {binding = "utf8.charpattern", metadata = {["fnl/docstring"] = "The pattern (a string, not a function) `\"[\\0-\\x7F\\xC2-\\xFD][\\x80-\\xBF]*\"`\nwhich matches exactly one UTF-8 byte sequence,\nassuming that the subject is a valid UTF-8 string."}}, codepoint = {binding = "utf8.codepoint", metadata = {["fnl/arglist"] = {"s", "?i", "?j", "?lax"}, ["fnl/docstring"] = "Returns the code points (as integers) from all characters in `s`\nthat start between byte position `?i` and `?j` (both included).\nThe default for `?i` is 1 and for `?j` is `?i`.\nIt raises an error if it meets any invalid byte sequence."}}, codes = {binding = "utf8.codes", metadata = {["fnl/arglist"] = {"s", "?lax"}, ["fnl/docstring"] = "Returns values so that the construction\n\n```lua\n     for p, c in utf8.codes(s) do *body* end\n```\nwill iterate over all UTF-8 characters in string `s`,\nwith `p` being the position (in bytes) and `c` the code point\nof each character.\nIt raises an error if it meets any invalid byte sequence."}}, len = {binding = "utf8.len", metadata = {["fnl/arglist"] = {"s", "?i", "?j", "?lax"}, ["fnl/docstring"] = "Returns the number of UTF-8 characters in string `s`\nthat start between positions `?i` and `?j` (both inclusive).\nThe default for `?i` is 1 and for `?j` is -1.\nIf it finds any invalid byte sequence,\nreturns **fail** plus the position of the first invalid byte."}}, offset = {binding = "utf8.offset", metadata = {["fnl/arglist"] = {"s", "n", "?i"}, ["fnl/docstring"] = "Returns the position (in bytes) where the encoding of the\n`n`-th character of `s`\n(counting from position `?i`) starts.\nA negative `n` gets characters before position `?i`.\nThe default for `?i` is 1 when `n` is non-negative\nand `#s + 1` otherwise,\nso that `utf8.offset(s, -n)` gets the offset of the\n`n`-th character from the end of the string.\nIf the specified character is neither in the subject\nnor right after its end,\nthe function returns **fail**.\n\nAs a special case,\nwhen `n` is 0 the function returns the start of the encoding\nof the character that contains the `?i`-th byte of `s`.\n\nThis function assumes that `s` is a valid UTF-8 string."}}}, metadata = {["fnl/docstring"] = "This library provides basic support for UTF-8 encoding.\nIt provides all its functions inside the table `utf8`.\nThis library does not provide any support for Unicode other\nthan the handling of the encoding.\nAny operation that needs the meaning of a character,\nsuch as character classification, is outside its scope.\n\nUnless stated otherwise,\nall functions that expect a byte position as a parameter\nassume that the given position is either the start of a byte sequence\nor one plus the length of the subject string.\nAs in the string library,\nnegative indices count from the end of the string.\n\nFunctions that create byte sequences\naccept all values up to `0x7FFFFFFF`,\nas defined in the original UTF-8 specification;\nthat implies byte sequences of up to six bytes.\n\nFunctions that interpret byte sequences only accept\nvalid sequences (well formed and not overlong).\nBy default, they only accept byte sequences\nthat result in valid Unicode code points,\nrejecting values greater than `10FFFF` and surrogates.\nA boolean argument `lax`, when available,\nlifts these checks,\nso that all values up to `0x7FFFFFFF` are accepted.\n(Not well formed and overlong sequences are still rejected.)"}}, warn = {binding = "warn", metadata = {["fnl/arglist"] = {"msg1", "..."}, ["fnl/docstring"] = "Emits a warning with a message composed by the concatenation\nof all its arguments (which should be strings).\n\nBy convention,\na one-piece message starting with `\"@\"`\nis intended to be a *control message*,\nwhich is a message to the warning system itself.\nIn particular, the standard warning function in Lua\nrecognizes the control messages `\"@off\"`,\nto stop the emission of warnings,\nand `\"@on\"`, to (re)start the emission;\nit ignores unknown control messages."}}, xpcall = {binding = "xpcall", metadata = {["fnl/arglist"] = {"f", "msgh", "?arg1", "..."}, ["fnl/docstring"] = "This function is similar to `pcall`,\nexcept that it sets a new message handler `msgh`."}}}
  docs._G.fields = docs
  docs.io.fields.stdin.fields = docs.io.fields
  docs.io.fields.stdout.fields = docs.io.fields
  docs.io.fields.stderr.fields = docs.io.fields
  return docs
end
package.preload["fennel-ls.docs.generated.tic80"] = package.preload["fennel-ls.docs.generated.tic80"] or function(...)
  return {BDR = {binding = "BDR", metadata = {["fnl/arglist"] = {"row"}, ["fnl/docstring"] = "Allows you to execute code between the drawing of each fullscreen scanline, for example, to manipulate the palette.\n"}}, BOOT = {binding = "BOOT", metadata = {["fnl/docstring"] = "Startup function.\n"}}, MENU = {binding = "MENU", metadata = {["fnl/arglist"] = {"index"}, ["fnl/docstring"] = "Game Menu handler.\n"}}, SCN = {binding = "SCN", metadata = {["fnl/arglist"] = {"row"}, ["fnl/docstring"] = "Allows you to execute code between the drawing of each scanline, for example, to manipulate the palette.\n"}}, TIC = {binding = "TIC", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Main function. It's called at 60fps (60 times every second).\n"}}, btn = {binding = "btn", metadata = {["fnl/arglist"] = {"id"}, ["fnl/docstring"] = "This function allows you to read the status of one of the buttons attached to TIC.\nThe function returns true if the key with the supplied id is currently in the pressed state.\nIt remains true for as long as the key is held down.\nIf you want to test if a key was just pressed, use `btnp()` instead.\n"}}, btnp = {binding = "btnp", metadata = {["fnl/arglist"] = {"id", "?hold", "?period"}, ["fnl/docstring"] = "This function allows you to read the status of one of TIC's buttons.\nIt returns true only if the key has been pressed since the last frame.\nYou can also use the optional hold and period parameters which allow you to check if a button is being held down.\nAfter the time specified by hold has elapsed, btnp will return true each time period is passed if the key is still down.\nFor example, to re-examine the state of button `0` after 2 seconds and continue to check its state every 1/10th of a second, you would use btnp(0, 120, 6).\nSince time is expressed in ticks and TIC runs at 60 frames per second, we use the value of 120 to wait 2 seconds and 6 ticks (ie 60/10) as the interval for re-checking.\n"}}, circ = {binding = "circ", metadata = {["fnl/arglist"] = {"x", "y", "radius", "color"}, ["fnl/docstring"] = "This function draws a filled circle of the desired radius and color with its center at x, y.\nIt uses the Bresenham algorithm.\n"}}, circb = {binding = "circb", metadata = {["fnl/arglist"] = {"x", "y", "radius", "color"}, ["fnl/docstring"] = "Draws the circumference of a circle with its center at x, y using the radius and color requested.\nIt uses the Bresenham algorithm.\n"}}, clip = {binding = "clip", metadata = {["fnl/arglist"] = {"x", "y", "width", "height"}, ["fnl/docstring"] = "This function limits drawing to a clipping region or `viewport` defined by x,y,w,h.\nThings drawn outside of this area will not be visible.\nCalling clip() with no parameters will reset the drawing area to the entire screen.\n"}}, cls = {binding = "cls", metadata = {["fnl/arglist"] = {"?color"}, ["fnl/docstring"] = "Clear the screen.\nWhen called this function clear all the screen using the color passed as argument.\nIf no parameter is passed first color (0) is used.\n"}}, elli = {binding = "elli", metadata = {["fnl/arglist"] = {"x", "y", "a", "b", "color"}, ["fnl/docstring"] = "This function draws a filled ellipse of the desired a, b radiuses and color with its center at x, y.\nIt uses the Bresenham algorithm.\n"}}, ellib = {binding = "ellib", metadata = {["fnl/arglist"] = {"x", "y", "a", "b", "color"}, ["fnl/docstring"] = "This function draws an ellipse border with the desired radiuses a b and color with its center at x, y.\nIt uses the Bresenham algorithm.\n"}}, exit = {binding = "exit", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Interrupts program execution and returns to the console when the TIC function ends.\n"}}, fget = {binding = "fget", metadata = {["fnl/arglist"] = {"sprite_id", "flag"}, ["fnl/docstring"] = "Returns true if the specified flag of the sprite is set. See `fset()` for more details.\n"}}, font = {binding = "font", metadata = {["fnl/arglist"] = {"text", "x", "y", "chromakey", "char_width", "char_height", "?fixed", "?scale", "?alt"}, ["fnl/docstring"] = "Print string with font defined in foreground sprites.\nTo simply print to the screen, check out `print()`.\nTo print to the console, check out `trace()`.\n"}}, fset = {binding = "fset", metadata = {["fnl/arglist"] = {"sprite_id", "flag", "bool"}, ["fnl/docstring"] = "Each sprite has eight flags which can be used to store information or signal different conditions.\nFor example, flag 0 might be used to indicate that the sprite is invisible, flag 6 might indicate that the flag should be draw scaled etc.\nSee algo `fget()`.\n"}}, key = {binding = "key", metadata = {["fnl/arglist"] = {"?code"}, ["fnl/docstring"] = "The function returns true if the key denoted by keycode is pressed.\n"}}, keyp = {binding = "keyp", metadata = {["fnl/arglist"] = {"?code", "?hold", "?period"}, ["fnl/docstring"] = "This function returns true if the given key is pressed but wasn't pressed in the previous frame.\nRefer to `btnp()` for an explanation of the optional hold and period parameters.\n"}}, line = {binding = "line", metadata = {["fnl/arglist"] = {"x0", "y0", "x1", "y1", "color"}, ["fnl/docstring"] = "Draws a straight line from point (x0,y0) to point (x1,y1) in the specified color.\n"}}, map = {binding = "map", metadata = {["fnl/arglist"] = {"?x", "?y", "?w", "?h", "?sx", "?sy", "?colorkey", "?scale", "?remap"}, ["fnl/docstring"] = "The map consists of cells of 8x8 pixels, each of which can be filled with a sprite using the map editor.\nThe map can be up to 240 cells wide by 136 deep.\nThis function will draw the desired area of the map to a specified screen position.\nFor example, map(5,5,12,10,0,0) will draw a 12x10 section of the map, starting from map coordinates (5,5) to screen position (0,0).\nThe map function's last parameter is a powerful callback function for changing how map cells (sprites) are drawn when map is called.\nIt can be used to rotate, flip and replace sprites while the game is running.\nUnlike mset, which saves changes to the map, this special function can be used to create animated tiles or replace them completely.\nSome examples include changing sprites to open doorways, hiding sprites used to spawn objects in your game and even to emit the objects themselves.\nThe tilemap is laid out sequentially in RAM - writing 1 to 0x08000 will cause tile(sprite) #1 to appear at top left when map() is called.\nTo set the tile immediately below this we need to write to 0x08000 + 240, ie 0x080F0.\n"}}, memcpy = {binding = "memcpy", metadata = {["fnl/arglist"] = {"dest", "source", "size"}, ["fnl/docstring"] = "This function allows you to copy a continuous block of TIC's 96K RAM from one address to another.\nAddresses are specified are in hexadecimal format, values are decimal.\n"}}, memset = {binding = "memset", metadata = {["fnl/arglist"] = {"dest", "value", "size"}, ["fnl/docstring"] = "This function allows you to set a continuous block of any part of TIC's RAM to the same value.\nThe address is specified in hexadecimal format, the value in decimal.\n"}}, mget = {binding = "mget", metadata = {["fnl/arglist"] = {"x", "y"}, ["fnl/docstring"] = "Gets the sprite id at the given x and y map coordinate.\n"}}, mouse = {binding = "mouse", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "This function returns the mouse coordinates and a boolean value for the state of each mouse button,with true indicating that a button is pressed.\n"}}, mset = {binding = "mset", metadata = {["fnl/arglist"] = {"x", "y", "tile_id"}, ["fnl/docstring"] = "This function will change the tile at the specified map coordinates.\nBy default, changes made are only kept while the current game is running.\nTo make permanent changes to the map, see `sync()`.\nRelated: `map()` `mget()` `sync()`.\n"}}, music = {binding = "music", metadata = {["fnl/arglist"] = {"?track", "?frame", "?row", "?loop", "?sustain", "?tempo", "?speed"}, ["fnl/docstring"] = "This function starts playing a track created in the Music Editor.\nCall without arguments to stop the music.\n"}}, peek = {binding = "peek", metadata = {["fnl/arglist"] = {"addr", "?bits"}, ["fnl/docstring"] = "This function allows to read the memory from TIC.\nIt's useful to access resources created with the integrated tools like sprite, maps, sounds, cartridges data?\nNever dream to sound a sprite?\nAddress are in hexadecimal format but values are decimal.\nTo write to a memory address, use `poke()`.\n`bits` allowed to be 1,2,4,8.\n"}}, peek1 = {binding = "peek1", metadata = {["fnl/arglist"] = {"addr"}, ["fnl/docstring"] = "This function enables you to read single bit values from TIC's RAM.\nThe address is often specified in hexadecimal format.\n"}}, peek2 = {binding = "peek2", metadata = {["fnl/arglist"] = {"addr"}, ["fnl/docstring"] = "This function enables you to read two bits values from TIC's RAM.\nThe address is often specified in hexadecimal format.\n"}}, peek4 = {binding = "peek4", metadata = {["fnl/arglist"] = {"addr"}, ["fnl/docstring"] = "This function enables you to read values from TIC's RAM.\nThe address is often specified in hexadecimal format.\nSee 'poke4()' for detailed information on how nibble addressing compares with byte addressing.\n"}}, pix = {binding = "pix", metadata = {["fnl/arglist"] = {"x", "y", "color"}, ["fnl/docstring"] = "This function can read or write pixel color values.\nWhen called with a color parameter, the pixel at the specified coordinates is set to that color.\nCalling the function without a color parameter returns the color of the pixel at the specified position.\n"}}, pmem = {binding = "pmem", metadata = {["fnl/arglist"] = {"index", "value"}, ["fnl/docstring"] = "This function allows you to save and retrieve data in one of the 256 individual 32-bit slots available in the cartridge's persistent memory.\nThis is useful for saving high-scores, level advancement or achievements.\nThe data is stored as unsigned 32-bit integers (from 0 to 4294967295).\n\nTips:\n- pmem depends on the cartridge hash (md5), so don't change your lua script if you want to keep the data.\n- Use `saveid:` with a personalized string in the header metadata to override the default MD5 calculation.\nThis allows the user to update a cart without losing their saved data.\n"}}, poke = {binding = "poke", metadata = {["fnl/arglist"] = {"addr", "value", "?bits"}, ["fnl/docstring"] = "This function allows you to write a single byte to any address in TIC's RAM.\nThe address should be specified in hexadecimal format, the value in decimal.\n`bits` allowed to be 1,2,4,8.\n"}}, poke1 = {binding = "poke1", metadata = {["fnl/arglist"] = {"addr", "value"}, ["fnl/docstring"] = "This function allows you to write single bit values directly to RAM.\nThe address is often specified in hexadecimal format.\n"}}, poke2 = {binding = "poke2", metadata = {["fnl/arglist"] = {"addr", "value"}, ["fnl/docstring"] = "This function allows you to write two bits values directly to RAM.\nThe address is often specified in hexadecimal format.\n"}}, poke4 = {binding = "poke4", metadata = {["fnl/arglist"] = {"addr", "value"}, ["fnl/docstring"] = "This function allows you to write directly to RAM.\nThe address is often specified in hexadecimal format.\nFor both peek4 and poke4 RAM is addressed in 4 bit segments (nibbles).\nTherefore, to access the the RAM at byte address 0x4000\nyou would need to access both the 0x8000 and 0x8001 nibble addresses.\n"}}, print = {binding = "print", metadata = {["fnl/arglist"] = {"text", "?x", "?y", "?color", "?fixed", "?scale", "?smallfont"}, ["fnl/docstring"] = "This will simply print text to the screen using the font defined in config.\nWhen set to true, the fixed width option ensures that each character will be printed in a `box` of the same size, so the character `i` will occupy the same width as the character `w` for example.\nWhen fixed width is false, there will be a single space between each character.\n\nTips:\n- To use a custom rastered font, check out `font()`.\n- To print to the console, check out `trace()`.\n"}}, rect = {binding = "rect", metadata = {["fnl/arglist"] = {"x", "y", "w", "h", "color"}, ["fnl/docstring"] = "This function draws a filled rectangle of the desired size and color at the specified position.\nIf you only need to draw the the border or outline of a rectangle (ie not filled) see `rectb()`.\n"}}, rectb = {binding = "rectb", metadata = {["fnl/arglist"] = {"x", "y", "w", "h", "color"}, ["fnl/docstring"] = "This function draws a one pixel thick rectangle border at the position requested.\nIf you need to fill the rectangle with a color, see `rect()` instead.\n"}}, reset = {binding = "reset", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "Resets the cartridge. To return to the console, see the `exit()`.\n"}}, sfx = {binding = "sfx", metadata = {["fnl/arglist"] = {"id", "?note", "?duration", "?channel", "?volume", "?speed"}, ["fnl/docstring"] = "This function will play the sound with `id` created in the sfx editor.\nCalling the function with id set to -1 will stop playing the channel.\nThe note can be supplied as an integer between 0 and 95 (representing 8 octaves of 12 notes each) or as a string giving the note name and octave.\nFor example, a note value of `14` will play the note `D` in the second octave.\nThe same note could be specified by the string `D-2`.\nNote names consist of two characters, the note itself (in upper case) followed by `-` to represent the natural note or `#` to represent a sharp.\nThere is no option to indicate flat values.\nThe available note names are therefore: C-, C#, D-, D#, E-, F-, F#, G-, G#, A-, A#, B-.\nThe `octave` is specified using a single digit in the range 0 to 8.\nThe `duration` specifies how many ticks to play the sound for since TIC-80 runs at 60 frames per second, a value of 30 represents half a second.\nA value of -1 will play the sound continuously.\nThe `channel` parameter indicates which of the four channels to use. Allowed values are 0 to 3.\nThe `volume` can be between 0 and 15.\nThe `speed` in the range -4 to 3 can be specified and means how many `ticks+1` to play each step, so speed==0 means 1 tick per step.\n"}}, spr = {binding = "spr", metadata = {["fnl/arglist"] = {"id", "x", "y", "?colorkey", "?scale", "?flip", "?rotate", "?w", "?h"}, ["fnl/docstring"] = "Draws the sprite number index at the x and y coordinate.\nYou can specify a colorkey in the palette which will be used as the transparent color or use a value of -1 for an opaque sprite.\nThe sprite can be scaled up by a desired factor. For example, a scale factor of 2 means an 8x8 pixel sprite is drawn to a 16x16 area of the screen.\nYou can flip the sprite where:\n- 0 = No Flip\n- 1 = Flip horizontally\n- 2 = Flip vertically\n- 3 = Flip both vertically and horizontally\nWhen you rotate the sprite, it's rotated clockwise in 90 steps:\n- 0 = No rotation\n- 1 = 90 rotation\n- 2 = 180 rotation\n- 3 = 270 rotation\nYou can draw a composite sprite (consisting of a rectangular region of sprites from the sprite sheet) by specifying the `w` and `h` parameters (which default to 1).\n"}}, sync = {binding = "sync", metadata = {["fnl/arglist"] = {"?mask", "?bank", "?tocart"}, ["fnl/docstring"] = "The pro version of TIC-80 contains 8 memory banks.\nTo switch between these banks, sync can be used to either load contents from a memory bank to runtime, or save contents from the active runtime to a bank.\nThe function can only be called once per frame.If you have manipulated the runtime memory (e.g. by using mset), you can reset the active state by calling sync(0,0,false).\nThis resets the whole runtime memory to the contents of bank 0.Note that sync is not used to load code from banks; this is done automatically.\n"}}, time = {binding = "time", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "This function returns the number of milliseconds elapsed since the cartridge began execution.\nUseful for keeping track of time, animating items and triggering events.\n"}}, trace = {binding = "trace", metadata = {["fnl/arglist"] = {"message", "?color"}, ["fnl/docstring"] = "This is a service function, useful for debugging your code.\nIt prints the message parameter to the console in the (optional) color specified.\n\nTips:\n- The Lua concatenator for strings is .. (two points).\n- Use console cls command to clear the output from trace.\n"}}, tri = {binding = "tri", metadata = {["fnl/arglist"] = {"x1", "y1", "x2", "y2", "x3", "y3", "color"}, ["fnl/docstring"] = "This function draws a triangle filled with color, using the supplied vertices.\n"}}, trib = {binding = "trib", metadata = {["fnl/arglist"] = {"x1", "y1", "x2", "y2", "x3", "y3", "color"}, ["fnl/docstring"] = "This function draws a triangle border with color, using the supplied vertices.\n"}}, tstamp = {binding = "tstamp", metadata = {["fnl/arglist"] = {}, ["fnl/docstring"] = "This function returns the number of seconds elapsed since January 1st, 1970.\nUseful for creating persistent games which evolve over time between plays.\n"}}, ttri = {binding = "ttri", metadata = {["fnl/arglist"] = {"x1", "y1", "x2", "y2", "x3", "y3", "u1", "v1", "u2", "v2", "u3", "v3", "?texsrc", "?chromakey", "?z1", "?z2", "?z3"}, ["fnl/docstring"] = "It renders a triangle filled with texture from image ram, map ram or vbank.\nUse in 3D graphics.\nIn particular, if the vertices in the triangle have different 3D depth, you may see some distortion.\nThese can be thought of as the window inside image ram (sprite sheet), map ram or another vbank.\nNote that the sprite sheet or map in this case is treated as a single large image, with U and V addressing its pixels directly, rather than by sprite ID.\nSo for example the top left corner of sprite #2 would be located at u=16, v=0.\n"}}}
end
package.preload["fennel-ls.message"] = package.preload["fennel-ls.message"] or function(...)
  local fennel = require("fennel")
  local utils = require("fennel-ls.utils")
  local json = require("dkjson")
  local function nullify(_3fvalue)
    if (_3fvalue == nil) then
      return json.null
    elseif (nil ~= _3fvalue) then
      local v = _3fvalue
      return v
    else
      return nil
    end
  end
  local error_codes = {ParseError = -32700, InvalidRequest = -32600, MethodNotFound = -32601, InvalidParams = -32602, InternalError = -32603, ServerNotInitialized = -32002, UnknownErrorCode = -32001, RequestFailed = -32803, ServerCancelled = -32802, ContentModified = -32801, RequestCancelled = -32800}
  local severity = {ERROR = 1, WARN = 2, INFO = 3, HINT = 4}
  local function create_error(code, message, _3fid, _3fdata)
    _G.assert((nil ~= message), "Missing argument message on src/fennel-ls\\message.fnl:38")
    _G.assert((nil ~= code), "Missing argument code on src/fennel-ls\\message.fnl:38")
    return {jsonrpc = "2.0", id = _3fid, error = {code = (error_codes[code] or code), data = _3fdata, message = message}}
  end
  local function create_request(id, method, _3fparams)
    _G.assert((nil ~= method), "Missing argument method on src/fennel-ls\\message.fnl:45")
    _G.assert((nil ~= id), "Missing argument id on src/fennel-ls\\message.fnl:45")
    return {jsonrpc = "2.0", id = id, method = method, params = _3fparams}
  end
  local function create_notification(method, _3fparams)
    _G.assert((nil ~= method), "Missing argument method on src/fennel-ls\\message.fnl:51")
    return {jsonrpc = "2.0", method = method, params = _3fparams}
  end
  local function create_response(id, _3fresult)
    _G.assert((nil ~= id), "Missing argument id on src/fennel-ls\\message.fnl:56")
    return {jsonrpc = "2.0", id = id, result = nullify(_3fresult)}
  end
  local function ast__3erange(server, file, _3fast)
    _G.assert((nil ~= file), "Missing argument file on src/fennel-ls\\message.fnl:61")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\message.fnl:61")
    local _62_ = fennel["ast-source"](_3fast)
    if ((_G.type(_62_) == "table") and (nil ~= _62_.bytestart) and (nil ~= _62_.byteend)) then
      local bytestart = _62_.bytestart
      local byteend = _62_.byteend
      return {start = utils["byte->position"](file.text, bytestart, server["position-encoding"]), ["end"] = utils["byte->position"](file.text, (byteend + 1), server["position-encoding"])}
    else
      return nil
    end
  end
  local function multisym__3erange(server, file, ast, n)
    _G.assert((nil ~= n), "Missing argument n on src/fennel-ls\\message.fnl:68")
    _G.assert((nil ~= ast), "Missing argument ast on src/fennel-ls\\message.fnl:68")
    _G.assert((nil ~= file), "Missing argument file on src/fennel-ls\\message.fnl:68")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\message.fnl:68")
    local spl = utils["multi-sym-split"](ast)
    local n0
    if (n < 0) then
      n0 = (n + 1 + #spl)
    else
      n0 = n
    end
    local _65_, _66_ = utils["get-ast-info"](ast, "bytestart"), utils["get-ast-info"](ast, "byteend")
    if ((nil ~= _65_) and (nil ~= _66_)) then
      local bytestart = _65_
      local byteend = _66_
      local bytesubstart
      do
        local b = bytestart
        for i = 1, (n0 - 1) do
          b = (b + #spl[i] + 1)
        end
        bytesubstart = b
      end
      local bytesubend
      do
        local b = byteend
        for i = (n0 + 1), #spl do
          b = (b - #spl[i] - 1)
        end
        bytesubend = b
      end
      return {start = utils["byte->position"](file.text, bytesubstart, server["position-encoding"]), ["end"] = utils["byte->position"](file.text, (bytesubend + 1), server["position-encoding"])}
    else
      return nil
    end
  end
  local function range_and_uri(server, _68_, _3fast)
    local uri = _68_["uri"]
    local file = _68_
    _G.assert((nil ~= file), "Missing argument file on src/fennel-ls\\message.fnl:83")
    _G.assert((nil ~= uri), "Missing argument uri on src/fennel-ls\\message.fnl:83")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\message.fnl:83")
    local _69_ = ast__3erange(server, file, _3fast)
    if (nil ~= _69_) then
      local range = _69_
      return {range = range, uri = uri}
    else
      return nil
    end
  end
  local function diagnostics(file)
    _G.assert((nil ~= file), "Missing argument file on src/fennel-ls\\message.fnl:88")
    return create_notification("textDocument/publishDiagnostics", {uri = file.uri, diagnostics = file.diagnostics})
  end
  return {["create-notification"] = create_notification, ["create-request"] = create_request, ["create-response"] = create_response, ["create-error"] = create_error, ["ast->range"] = ast__3erange, ["multisym->range"] = multisym__3erange, ["range-and-uri"] = range_and_uri, diagnostics = diagnostics, severity = severity}
end
package.preload["dkjson"] = package.preload["dkjson"] or function(...)
  -- Module options:
  local always_use_lpeg = false
  local register_global_module_table = false
  local global_module_name = 'json'
  
  --[==[
  
  David Kolf's JSON module for Lua 5.1 - 5.4
  
  Version 2.7
  
  
  For the documentation see the corresponding readme.txt or visit
  <http://dkolf.de/src/dkjson-lua.fsl/>.
  
  You can contact the author by sending an e-mail to 'david' at the
  domain 'dkolf.de'.
  
  
  Copyright (C) 2010-2024 David Heiko Kolf
  
  Permission is hereby granted, free of charge, to any person obtaining
  a copy of this software and associated documentation files (the
  "Software"), to deal in the Software without restriction, including
  without limitation the rights to use, copy, modify, merge, publish,
  distribute, sublicense, and/or sell copies of the Software, and to
  permit persons to whom the Software is furnished to do so, subject to
  the following conditions:
  
  The above copyright notice and this permission notice shall be
  included in all copies or substantial portions of the Software.
  
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
  
  --]==]
  
  -- global dependencies:
  local pairs, type, tostring, tonumber, getmetatable, setmetatable, rawset =
        pairs, type, tostring, tonumber, getmetatable, setmetatable, rawset
  local error, require, pcall, select = error, require, pcall, select
  local floor, huge = math.floor, math.huge
  local strrep, gsub, strsub, strbyte, strchar, strfind, strlen, strformat =
        string.rep, string.gsub, string.sub, string.byte, string.char,
        string.find, string.len, string.format
  local strmatch = string.match
  local concat = table.concat
  
  local json = { version = "dkjson 2.7" }
  
  local jsonlpeg = {}
  
  if register_global_module_table then
    if always_use_lpeg then
      _G[global_module_name] = jsonlpeg
    else
      _G[global_module_name] = json
    end
  end
  
  local _ENV = nil -- blocking globals in Lua 5.2 and later
  
  pcall (function()
    -- Enable access to blocked metatables.
    -- Don't worry, this module doesn't change anything in them.
    local debmeta = require "debug".getmetatable
    if debmeta then getmetatable = debmeta end
  end)
  
  json.null = setmetatable ({}, {
    __tojson = function () return "null" end
  })
  
  local function isarray (tbl)
    local max, n, arraylen = 0, 0, 0
    for k,v in pairs (tbl) do
      if k == 'n' and type(v) == 'number' then
        arraylen = v
        if v > max then
          max = v
        end
      else
        if type(k) ~= 'number' or k < 1 or floor(k) ~= k then
          return false
        end
        if k > max then
          max = k
        end
        n = n + 1
      end
    end
    if max > 10 and max > arraylen and max > n * 2 then
      return false -- don't create an array with too many holes
    end
    return true, max
  end
  
  local escapecodes = {
    ["\""] = "\\\"", ["\\"] = "\\\\", ["\b"] = "\\b", ["\f"] = "\\f",
    ["\n"] = "\\n",  ["\r"] = "\\r",  ["\t"] = "\\t"
  }
  
  local function escapeutf8 (uchar)
    local value = escapecodes[uchar]
    if value then
      return value
    end
    local a, b, c, d = strbyte (uchar, 1, 4)
    a, b, c, d = a or 0, b or 0, c or 0, d or 0
    if a <= 0x7f then
      value = a
    elseif 0xc0 <= a and a <= 0xdf and b >= 0x80 then
      value = (a - 0xc0) * 0x40 + b - 0x80
    elseif 0xe0 <= a and a <= 0xef and b >= 0x80 and c >= 0x80 then
      value = ((a - 0xe0) * 0x40 + b - 0x80) * 0x40 + c - 0x80
    elseif 0xf0 <= a and a <= 0xf7 and b >= 0x80 and c >= 0x80 and d >= 0x80 then
      value = (((a - 0xf0) * 0x40 + b - 0x80) * 0x40 + c - 0x80) * 0x40 + d - 0x80
    else
      return ""
    end
    if value <= 0xffff then
      return strformat ("\\u%.4x", value)
    elseif value <= 0x10ffff then
      -- encode as UTF-16 surrogate pair
      value = value - 0x10000
      local highsur, lowsur = 0xD800 + floor (value/0x400), 0xDC00 + (value % 0x400)
      return strformat ("\\u%.4x\\u%.4x", highsur, lowsur)
    else
      return ""
    end
  end
  
  local function fsub (str, pattern, repl)
    -- gsub always builds a new string in a buffer, even when no match
    -- exists. First using find should be more efficient when most strings
    -- don't contain the pattern.
    if strfind (str, pattern) then
      return gsub (str, pattern, repl)
    else
      return str
    end
  end
  
  local function quotestring (value)
    -- based on the regexp "escapable" in https://github.com/douglascrockford/JSON-js
    value = fsub (value, "[%z\1-\31\"\\\127]", escapeutf8)
    if strfind (value, "[\194\216\220\225\226\239]") then
      value = fsub (value, "\194[\128-\159\173]", escapeutf8)
      value = fsub (value, "\216[\128-\132]", escapeutf8)
      value = fsub (value, "\220\143", escapeutf8)
      value = fsub (value, "\225\158[\180\181]", escapeutf8)
      value = fsub (value, "\226\128[\140-\143\168-\175]", escapeutf8)
      value = fsub (value, "\226\129[\160-\175]", escapeutf8)
      value = fsub (value, "\239\187\191", escapeutf8)
      value = fsub (value, "\239\191[\176-\191]", escapeutf8)
    end
    return "\"" .. value .. "\""
  end
  json.quotestring = quotestring
  
  local function replace(str, o, n)
    local i, j = strfind (str, o, 1, true)
    if i then
      return strsub(str, 1, i-1) .. n .. strsub(str, j+1, -1)
    else
      return str
    end
  end
  
  -- locale independent num2str and str2num functions
  local decpoint, numfilter
  
  local function updatedecpoint ()
    decpoint = strmatch(tostring(0.5), "([^05+])")
    -- build a filter that can be used to remove group separators
    numfilter = "[^0-9%-%+eE" .. gsub(decpoint, "[%^%$%(%)%%%.%[%]%*%+%-%?]", "%%%0") .. "]+"
  end
  
  updatedecpoint()
  
  local function num2str (num)
    return replace(fsub(tostring(num), numfilter, ""), decpoint, ".")
  end
  
  local function str2num (str)
    local num = tonumber(replace(str, ".", decpoint))
    if not num then
      updatedecpoint()
      num = tonumber(replace(str, ".", decpoint))
    end
    return num
  end
  
  local function addnewline2 (level, buffer, buflen)
    buffer[buflen+1] = "\n"
    buffer[buflen+2] = strrep ("  ", level)
    buflen = buflen + 2
    return buflen
  end
  
  function json.addnewline (state)
    if state.indent then
      state.bufferlen = addnewline2 (state.level or 0,
                             state.buffer, state.bufferlen or #(state.buffer))
    end
  end
  
  local encode2 -- forward declaration
  
  local function addpair (key, value, prev, indent, level, buffer, buflen, tables, globalorder, state)
    local kt = type (key)
    if kt ~= 'string' and kt ~= 'number' then
      return nil, "type '" .. kt .. "' is not supported as a key by JSON."
    end
    if prev then
      buflen = buflen + 1
      buffer[buflen] = ","
    end
    if indent then
      buflen = addnewline2 (level, buffer, buflen)
    end
    buffer[buflen+1] = quotestring (key)
    buffer[buflen+2] = ":"
    return encode2 (value, indent, level, buffer, buflen + 2, tables, globalorder, state)
  end
  
  local function appendcustom(res, buffer, state)
    local buflen = state.bufferlen
    if type (res) == 'string' then
      buflen = buflen + 1
      buffer[buflen] = res
    end
    return buflen
  end
  
  local function exception(reason, value, state, buffer, buflen, defaultmessage)
    defaultmessage = defaultmessage or reason
    local handler = state.exception
    if not handler then
      return nil, defaultmessage
    else
      state.bufferlen = buflen
      local ret, msg = handler (reason, value, state, defaultmessage)
      if not ret then return nil, msg or defaultmessage end
      return appendcustom(ret, buffer, state)
    end
  end
  
  function json.encodeexception(reason, value, state, defaultmessage)
    return quotestring("<" .. defaultmessage .. ">")
  end
  
  encode2 = function (value, indent, level, buffer, buflen, tables, globalorder, state)
    local valtype = type (value)
    local valmeta = getmetatable (value)
    valmeta = type (valmeta) == 'table' and valmeta -- only tables
    local valtojson = valmeta and valmeta.__tojson
    if valtojson then
      if tables[value] then
        return exception('reference cycle', value, state, buffer, buflen)
      end
      tables[value] = true
      state.bufferlen = buflen
      local ret, msg = valtojson (value, state)
      if not ret then return exception('custom encoder failed', value, state, buffer, buflen, msg) end
      tables[value] = nil
      buflen = appendcustom(ret, buffer, state)
    elseif value == nil then
      buflen = buflen + 1
      buffer[buflen] = "null"
    elseif valtype == 'number' then
      local s
      if value ~= value or value >= huge or -value >= huge then
        -- This is the behaviour of the original JSON implementation.
        s = "null"
      else
        s = num2str (value)
      end
      buflen = buflen + 1
      buffer[buflen] = s
    elseif valtype == 'boolean' then
      buflen = buflen + 1
      buffer[buflen] = value and "true" or "false"
    elseif valtype == 'string' then
      buflen = buflen + 1
      buffer[buflen] = quotestring (value)
    elseif valtype == 'table' then
      if tables[value] then
        return exception('reference cycle', value, state, buffer, buflen)
      end
      tables[value] = true
      level = level + 1
      local isa, n = isarray (value)
      if n == 0 and valmeta and valmeta.__jsontype == 'object' then
        isa = false
      end
      local msg
      if isa then -- JSON array
        buflen = buflen + 1
        buffer[buflen] = "["
        for i = 1, n do
          buflen, msg = encode2 (value[i], indent, level, buffer, buflen, tables, globalorder, state)
          if not buflen then return nil, msg end
          if i < n then
            buflen = buflen + 1
            buffer[buflen] = ","
          end
        end
        buflen = buflen + 1
        buffer[buflen] = "]"
      else -- JSON object
        local prev = false
        buflen = buflen + 1
        buffer[buflen] = "{"
        local order = valmeta and valmeta.__jsonorder or globalorder
        if order then
          local used = {}
          n = #order
          for i = 1, n do
            local k = order[i]
            local v = value[k]
            if v ~= nil then
              used[k] = true
              buflen, msg = addpair (k, v, prev, indent, level, buffer, buflen, tables, globalorder, state)
              if not buflen then return nil, msg end
              prev = true -- add a seperator before the next element
            end
          end
          for k,v in pairs (value) do
            if not used[k] then
              buflen, msg = addpair (k, v, prev, indent, level, buffer, buflen, tables, globalorder, state)
              if not buflen then return nil, msg end
              prev = true -- add a seperator before the next element
            end
          end
        else -- unordered
          for k,v in pairs (value) do
            buflen, msg = addpair (k, v, prev, indent, level, buffer, buflen, tables, globalorder, state)
            if not buflen then return nil, msg end
            prev = true -- add a seperator before the next element
          end
        end
        if indent then
          buflen = addnewline2 (level - 1, buffer, buflen)
        end
        buflen = buflen + 1
        buffer[buflen] = "}"
      end
      tables[value] = nil
    else
      return exception ('unsupported type', value, state, buffer, buflen,
        "type '" .. valtype .. "' is not supported by JSON.")
    end
    return buflen
  end
  
  function json.encode (value, state)
    state = state or {}
    local oldbuffer = state.buffer
    local buffer = oldbuffer or {}
    state.buffer = buffer
    updatedecpoint()
    local ret, msg = encode2 (value, state.indent, state.level or 0,
                     buffer, state.bufferlen or 0, state.tables or {}, state.keyorder, state)
    if not ret then
      error (msg, 2)
    elseif oldbuffer == buffer then
      state.bufferlen = ret
      return true
    else
      state.bufferlen = nil
      state.buffer = nil
      return concat (buffer)
    end
  end
  
  local function loc (str, where)
    local line, pos, linepos = 1, 1, 0
    while true do
      pos = strfind (str, "\n", pos, true)
      if pos and pos < where then
        line = line + 1
        linepos = pos
        pos = pos + 1
      else
        break
      end
    end
    return "line " .. line .. ", column " .. (where - linepos)
  end
  
  local function unterminated (str, what, where)
    return nil, strlen (str) + 1, "unterminated " .. what .. " at " .. loc (str, where)
  end
  
  local function scanwhite (str, pos)
    while true do
      pos = strfind (str, "%S", pos)
      if not pos then return nil end
      local sub2 = strsub (str, pos, pos + 1)
      if sub2 == "\239\187" and strsub (str, pos + 2, pos + 2) == "\191" then
        -- UTF-8 Byte Order Mark
        pos = pos + 3
      elseif sub2 == "//" then
        pos = strfind (str, "[\n\r]", pos + 2)
        if not pos then return nil end
      elseif sub2 == "/*" then
        pos = strfind (str, "*/", pos + 2)
        if not pos then return nil end
        pos = pos + 2
      else
        return pos
      end
    end
  end
  
  local escapechars = {
    ["\""] = "\"", ["\\"] = "\\", ["/"] = "/", ["b"] = "\b", ["f"] = "\f",
    ["n"] = "\n", ["r"] = "\r", ["t"] = "\t"
  }
  
  local function unichar (value)
    if value < 0 then
      return nil
    elseif value <= 0x007f then
      return strchar (value)
    elseif value <= 0x07ff then
      return strchar (0xc0 + floor(value/0x40),
                      0x80 + (floor(value) % 0x40))
    elseif value <= 0xffff then
      return strchar (0xe0 + floor(value/0x1000),
                      0x80 + (floor(value/0x40) % 0x40),
                      0x80 + (floor(value) % 0x40))
    elseif value <= 0x10ffff then
      return strchar (0xf0 + floor(value/0x40000),
                      0x80 + (floor(value/0x1000) % 0x40),
                      0x80 + (floor(value/0x40) % 0x40),
                      0x80 + (floor(value) % 0x40))
    else
      return nil
    end
  end
  
  local function scanstring (str, pos)
    local lastpos = pos + 1
    local buffer, n = {}, 0
    while true do
      local nextpos = strfind (str, "[\"\\]", lastpos)
      if not nextpos then
        return unterminated (str, "string", pos)
      end
      if nextpos > lastpos then
        n = n + 1
        buffer[n] = strsub (str, lastpos, nextpos - 1)
      end
      if strsub (str, nextpos, nextpos) == "\"" then
        lastpos = nextpos + 1
        break
      else
        local escchar = strsub (str, nextpos + 1, nextpos + 1)
        local value
        if escchar == "u" then
          value = tonumber (strsub (str, nextpos + 2, nextpos + 5), 16)
          if value then
            local value2
            if 0xD800 <= value and value <= 0xDBff then
              -- we have the high surrogate of UTF-16. Check if there is a
              -- low surrogate escaped nearby to combine them.
              if strsub (str, nextpos + 6, nextpos + 7) == "\\u" then
                value2 = tonumber (strsub (str, nextpos + 8, nextpos + 11), 16)
                if value2 and 0xDC00 <= value2 and value2 <= 0xDFFF then
                  value = (value - 0xD800)  * 0x400 + (value2 - 0xDC00) + 0x10000
                else
                  value2 = nil -- in case it was out of range for a low surrogate
                end
              end
            end
            value = value and unichar (value)
            if value then
              if value2 then
                lastpos = nextpos + 12
              else
                lastpos = nextpos + 6
              end
            end
          end
        end
        if not value then
          value = escapechars[escchar] or escchar
          lastpos = nextpos + 2
        end
        n = n + 1
        buffer[n] = value
      end
    end
    if n == 1 then
      return buffer[1], lastpos
    elseif n > 1 then
      return concat (buffer), lastpos
    else
      return "", lastpos
    end
  end
  
  local scanvalue -- forward declaration
  
  local function scantable (what, closechar, str, startpos, nullval, objectmeta, arraymeta)
    local len = strlen (str)
    local tbl, n = {}, 0
    local pos = startpos + 1
    if what == 'object' then
      setmetatable (tbl, objectmeta)
    else
      setmetatable (tbl, arraymeta)
    end
    while true do
      pos = scanwhite (str, pos)
      if not pos then return unterminated (str, what, startpos) end
      local char = strsub (str, pos, pos)
      if char == closechar then
        return tbl, pos + 1
      end
      local val1, err
      val1, pos, err = scanvalue (str, pos, nullval, objectmeta, arraymeta)
      if err then return nil, pos, err end
      pos = scanwhite (str, pos)
      if not pos then return unterminated (str, what, startpos) end
      char = strsub (str, pos, pos)
      if char == ":" then
        if val1 == nil then
          return nil, pos, "cannot use nil as table index (at " .. loc (str, pos) .. ")"
        end
        pos = scanwhite (str, pos + 1)
        if not pos then return unterminated (str, what, startpos) end
        local val2
        val2, pos, err = scanvalue (str, pos, nullval, objectmeta, arraymeta)
        if err then return nil, pos, err end
        tbl[val1] = val2
        pos = scanwhite (str, pos)
        if not pos then return unterminated (str, what, startpos) end
        char = strsub (str, pos, pos)
      else
        n = n + 1
        tbl[n] = val1
      end
      if char == "," then
        pos = pos + 1
      end
    end
  end
  
  scanvalue = function (str, pos, nullval, objectmeta, arraymeta)
    pos = pos or 1
    pos = scanwhite (str, pos)
    if not pos then
      return nil, strlen (str) + 1, "no valid JSON value (reached the end)"
    end
    local char = strsub (str, pos, pos)
    if char == "{" then
      return scantable ('object', "}", str, pos, nullval, objectmeta, arraymeta)
    elseif char == "[" then
      return scantable ('array', "]", str, pos, nullval, objectmeta, arraymeta)
    elseif char == "\"" then
      return scanstring (str, pos)
    else
      local pstart, pend = strfind (str, "^%-?[%d%.]+[eE]?[%+%-]?%d*", pos)
      if pstart then
        local number = str2num (strsub (str, pstart, pend))
        if number then
          return number, pend + 1
        end
      end
      pstart, pend = strfind (str, "^%a%w*", pos)
      if pstart then
        local name = strsub (str, pstart, pend)
        if name == "true" then
          return true, pend + 1
        elseif name == "false" then
          return false, pend + 1
        elseif name == "null" then
          return nullval, pend + 1
        end
      end
      return nil, pos, "no valid JSON value at " .. loc (str, pos)
    end
  end
  
  local function optionalmetatables(...)
    if select("#", ...) > 0 then
      return ...
    else
      return {__jsontype = 'object'}, {__jsontype = 'array'}
    end
  end
  
  function json.decode (str, pos, nullval, ...)
    local objectmeta, arraymeta = optionalmetatables(...)
    return scanvalue (str, pos, nullval, objectmeta, arraymeta)
  end
  
  function json.use_lpeg ()
    local g = require ("lpeg")
  
    if type(g.version) == 'function' and g.version() == "0.11" then
      error "due to a bug in LPeg 0.11, it cannot be used for JSON matching"
    end
  
    local pegmatch = g.match
    local P, S, R = g.P, g.S, g.R
  
    local function ErrorCall (str, pos, msg, state)
      if not state.msg then
        state.msg = msg .. " at " .. loc (str, pos)
        state.pos = pos
      end
      return false
    end
  
    local function Err (msg)
      return g.Cmt (g.Cc (msg) * g.Carg (2), ErrorCall)
    end
  
    local function ErrorUnterminatedCall (str, pos, what, state)
      return ErrorCall (str, pos - 1, "unterminated " .. what, state)
    end
  
    local SingleLineComment = P"//" * (1 - S"\n\r")^0
    local MultiLineComment = P"/*" * (1 - P"*/")^0 * P"*/"
    local Space = (S" \n\r\t" + P"\239\187\191" + SingleLineComment + MultiLineComment)^0
  
    local function ErrUnterminated (what)
      return g.Cmt (g.Cc (what) * g.Carg (2), ErrorUnterminatedCall)
    end
  
    local PlainChar = 1 - S"\"\\\n\r"
    local EscapeSequence = (P"\\" * g.C (S"\"\\/bfnrt" + Err "unsupported escape sequence")) / escapechars
    local HexDigit = R("09", "af", "AF")
    local function UTF16Surrogate (match, pos, high, low)
      high, low = tonumber (high, 16), tonumber (low, 16)
      if 0xD800 <= high and high <= 0xDBff and 0xDC00 <= low and low <= 0xDFFF then
        return true, unichar ((high - 0xD800)  * 0x400 + (low - 0xDC00) + 0x10000)
      else
        return false
      end
    end
    local function UTF16BMP (hex)
      return unichar (tonumber (hex, 16))
    end
    local U16Sequence = (P"\\u" * g.C (HexDigit * HexDigit * HexDigit * HexDigit))
    local UnicodeEscape = g.Cmt (U16Sequence * U16Sequence, UTF16Surrogate) + U16Sequence/UTF16BMP
    local Char = UnicodeEscape + EscapeSequence + PlainChar
    local String = P"\"" * (g.Cs (Char ^ 0) * P"\"" + ErrUnterminated "string")
    local Integer = P"-"^(-1) * (P"0" + (R"19" * R"09"^0))
    local Fractal = P"." * R"09"^0
    local Exponent = (S"eE") * (S"+-")^(-1) * R"09"^1
    local Number = (Integer * Fractal^(-1) * Exponent^(-1))/str2num
    local Constant = P"true" * g.Cc (true) + P"false" * g.Cc (false) + P"null" * g.Carg (1)
    local SimpleValue = Number + String + Constant
    local ArrayContent, ObjectContent
  
    -- The functions parsearray and parseobject parse only a single value/pair
    -- at a time and store them directly to avoid hitting the LPeg limits.
    local function parsearray (str, pos, nullval, state)
      local obj, cont
      local start = pos
      local npos
      local t, nt = {}, 0
      repeat
        obj, cont, npos = pegmatch (ArrayContent, str, pos, nullval, state)
        if cont == 'end' then
          return ErrorUnterminatedCall (str, start, "array", state)
        end
        pos = npos
        if cont == 'cont' or cont == 'last' then
          nt = nt + 1
          t[nt] = obj
        end
      until cont ~= 'cont'
      return pos, setmetatable (t, state.arraymeta)
    end
  
    local function parseobject (str, pos, nullval, state)
      local obj, key, cont
      local start = pos
      local npos
      local t = {}
      repeat
        key, obj, cont, npos = pegmatch (ObjectContent, str, pos, nullval, state)
        if cont == 'end' then
          return ErrorUnterminatedCall (str, start, "object", state)
        end
        pos = npos
        if cont == 'cont' or cont == 'last' then
          t[key] = obj
        end
      until cont ~= 'cont'
      return pos, setmetatable (t, state.objectmeta)
    end
  
    local Array = P"[" * g.Cmt (g.Carg(1) * g.Carg(2), parsearray)
    local Object = P"{" * g.Cmt (g.Carg(1) * g.Carg(2), parseobject)
    local Value = Space * (Array + Object + SimpleValue)
    local ExpectedValue = Value + Space * Err "value expected"
    local ExpectedKey = String + Err "key expected"
    local End = P(-1) * g.Cc'end'
    local ErrInvalid = Err "invalid JSON"
    ArrayContent = (Value * Space * (P"," * g.Cc'cont' + P"]" * g.Cc'last'+ End + ErrInvalid)  + g.Cc(nil) * (P"]" * g.Cc'empty' + End  + ErrInvalid)) * g.Cp()
    local Pair = g.Cg (Space * ExpectedKey * Space * (P":" + Err "colon expected") * ExpectedValue)
    ObjectContent = (g.Cc(nil) * g.Cc(nil) * P"}" * g.Cc'empty' + End + (Pair * Space * (P"," * g.Cc'cont' + P"}" * g.Cc'last' + End + ErrInvalid) + ErrInvalid)) * g.Cp()
    local DecodeValue = ExpectedValue * g.Cp ()
  
    jsonlpeg.version = json.version
    jsonlpeg.encode = json.encode
    jsonlpeg.null = json.null
    jsonlpeg.quotestring = json.quotestring
    jsonlpeg.addnewline = json.addnewline
    jsonlpeg.encodeexception = json.encodeexception
    jsonlpeg.using_lpeg = true
  
    function jsonlpeg.decode (str, pos, nullval, ...)
      local state = {}
      state.objectmeta, state.arraymeta = optionalmetatables(...)
      local obj, retpos = pegmatch (DecodeValue, str, pos, nullval, state)
      if state.msg then
        return nil, state.pos, state.msg
      else
        return obj, retpos
      end
    end
  
    -- cache result of this function:
    json.use_lpeg = function () return jsonlpeg end
    jsonlpeg.use_lpeg = json.use_lpeg
  
    return jsonlpeg
  end
  
  if always_use_lpeg then
    return json.use_lpeg()
  end
  
  return json
end
package.preload["fennel-ls.config"] = package.preload["fennel-ls.config"] or function(...)
  local files = require("fennel-ls.files")
  local utils = require("fennel-ls.utils")
  local option_mt = {}
  local function option(default_value)
    local tmp_9_auto = {default_value}
    setmetatable(tmp_9_auto, option_mt)
    return tmp_9_auto
  end
  local default_configuration = {["fennel-path"] = option("./?.fnl;./?/init.fnl;src/?.fnl;src/?/init.fnl"), ["macro-path"] = option("./?.fnl;./?/init-macros.fnl;./?/init.fnl;src/?.fnl;src/?/init-macros.fnl;src/?/init.fnl"), ["lua-version"] = option("lua54"), lints = {["unused-definition"] = option(true), ["unknown-module-field"] = option(true), ["unnecessary-method"] = option(true), ["bad-unpack"] = option(true), ["var-never-set"] = option(true), ["op-with-no-arguments"] = option(true), ["multival-in-middle-of-call"] = option(true)}, libraries = {love2d = option(false), ["tic-80"] = option(false)}, ["extra-globals"] = option("")}
  local function make_configuration_from_template(default, _3fuser, _3fparent)
    if (option_mt == getmetatable(default)) then
      local setting
      local function _283_(...)
        local _284_ = ...
        if (_284_ == nil) then
          local function _285_(...)
            local _286_ = ...
            if (_286_ == nil) then
              return default[1]
            else
              local __87_auto = _286_
              return ...
            end
          end
          local function _289_(...)
            local t_288_ = _3fparent
            if (nil ~= t_288_) then
              t_288_ = t_288_.all
            else
            end
            return t_288_
          end
          return _285_(_289_(...))
        else
          local __87_auto = _284_
          return ...
        end
      end
      setting = _283_(_3fuser)
      assert((type(default[1]) == type(setting)))
      return setting
    elseif ("table" == type(default)) then
      local tbl_16_auto = {}
      for k, _ in pairs(default) do
        local k_17_auto, v_18_auto = nil, nil
        local _293_
        do
          local t_292_ = _3fuser
          if (nil ~= t_292_) then
            t_292_ = t_292_[k]
          else
          end
          _293_ = t_292_
        end
        k_17_auto, v_18_auto = k, make_configuration_from_template(default[k], _293_, _3fuser)
        if ((k_17_auto ~= nil) and (v_18_auto ~= nil)) then
          tbl_16_auto[k_17_auto] = v_18_auto
        else
        end
      end
      return tbl_16_auto
    else
      return error("This is a bug with fennel-ls: default-configuration has a key that isn't a table or option")
    end
  end
  local function make_configuration(_3fc)
    return make_configuration_from_template(default_configuration, _3fc)
  end
  local function choose_position_encoding(init_params)
    _G.assert((nil ~= init_params), "Missing argument init-params on src/fennel-ls\\config.fnl:52")
    local _3fposition_encodings
    do
      local t_297_ = init_params
      if (nil ~= t_297_) then
        t_297_ = t_297_.capabilities
      else
      end
      if (nil ~= t_297_) then
        t_297_ = t_297_.general
      else
      end
      if (nil ~= t_297_) then
        t_297_ = t_297_.positionEncodings
      else
      end
      _3fposition_encodings = t_297_
    end
    local utf8_3f
    if (type(_3fposition_encodings) == "table") then
      local utf_8_3f = false
      for _, encoding in ipairs(_3fposition_encodings) do
        if utf_8_3f then break end
        utf_8_3f = ((encoding == "utf-8") or (encoding == "utf8"))
      end
      utf8_3f = utf_8_3f
    else
      utf8_3f = false
    end
    if utf8_3f then
      return "utf-8"
    else
      return "utf-16"
    end
  end
  local function try_parsing(_303_)
    local text = _303_["text"]
    local uri = _303_["uri"]
    _G.assert((nil ~= uri), "Missing argument uri on src/fennel-ls\\config.fnl:68")
    _G.assert((nil ~= text), "Missing argument text on src/fennel-ls\\config.fnl:68")
    local fennel = require("fennel")
    local ok_3f,_err,result = pcall(fennel.parser(text, uri))
    if ok_3f then
      return result
    else
      return nil
    end
  end
  local function load_config(server)
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\config.fnl:73")
    local function _306_()
      if server["root-uri"] then
        local tmp_3_auto = files["read-file"](server, utils["path->uri"](utils["path-join"](utils["uri->path"](server["root-uri"]), "flsproject.fnl")))
        if (nil ~= tmp_3_auto) then
          return try_parsing(tmp_3_auto)
        else
          return nil
        end
      else
        return nil
      end
    end
    return make_configuration(_306_())
  end
  local function reload(server)
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\config.fnl:81")
    server.configuration = load_config(server)
    return nil
  end
  local function initialize(server, params)
    _G.assert((nil ~= params), "Missing argument params on src/fennel-ls\\config.fnl:84")
    _G.assert((nil ~= server), "Missing argument server on src/fennel-ls\\config.fnl:84")
    server.files = {}
    server.modules = {}
    server["root-uri"] = params.rootUri
    server["position-encoding"] = choose_position_encoding(params)
    reload(server)
    local _308_
    do
      local t_307_ = params
      if (nil ~= t_307_) then
        t_307_ = t_307_.clientInfo
      else
      end
      if (nil ~= t_307_) then
        t_307_ = t_307_.name
      else
      end
      _308_ = t_307_
    end
    server.EGLOT_COMPLETION_QUIRK_MODE = (_308_ == "Eglot")
    return nil
  end
  return {initialize = initialize, reload = reload}
end
package.preload["fennel-ls.formatter"] = package.preload["fennel-ls.formatter"] or function(...)
  local _local_311_ = require("fennel")
  local sym_3f = _local_311_["sym?"]
  local view = _local_311_["view"]
  local function code_block(str)
    _G.assert((nil ~= str), "Missing argument str on src/fennel-ls\\formatter.fnl:9")
    return ("```fnl\n" .. str .. "\n```")
  end
  local function fn_format(special, name, args, docstring)
    local _312_
    if name then
      _312_ = (" " .. tostring(name))
    else
      _312_ = ""
    end
    local function _314_(_241, _242, _243)
      if (_241 == _242) then
        return (": " .. _242 .. _243)
      else
        return nil
      end
    end
    local _316_
    if docstring then
      _316_ = ("\n" .. docstring)
    else
      _316_ = ""
    end
    return (code_block(("(" .. tostring(special) .. _312_ .. (" " .. view(args, {["empty-as-sequence?"] = true, ["one-line?"] = true, ["prefer-colon?"] = true}):gsub(":([%w?_-]+) ([%w?]+)([ }])", _314_)) .. " ...)")) .. _316_)
  end
  local function metadata_format(_318_)
    local binding = _318_["binding"]
    local metadata = _318_["metadata"]
    local function _319_()
      if not metadata["fnl/arglist"] then
        return tostring(binding)
      elseif (0 == #metadata["fnl/arglist"]) then
        return ("(" .. tostring(binding) .. ")")
      else
        return ("(" .. tostring(binding) .. " " .. table.concat(metadata["fnl/arglist"], " ") .. ")")
      end
    end
    return (code_block(_319_()) .. "\n" .. (metadata["fnl/docstring"] or ""))
  end
  local function fn_3f(symbol)
    _G.assert((nil ~= symbol), "Missing argument symbol on src/fennel-ls\\formatter.fnl:39")
    if sym_3f(symbol) then
      local name = tostring(symbol)
      return ((name == "fn") or (name == "\206\187") or (name == "lambda"))
    else
      return nil
    end
  end
  local function analyze_fn(_3fast)
    local and_321_ = ((_G.type(_3fast) == "table") and (nil ~= _3fast[1]) and (nil ~= _3fast[2]) and (nil ~= _3fast[3]) and (nil ~= _3fast[4]) and (nil ~= _3fast[5]))
    if and_321_ then
      local fntype = _3fast[1]
      local name = _3fast[2]
      local arglist = _3fast[3]
      local docstring = _3fast[4]
      local body = _3fast[5]
      and_321_ = (body and fn_3f(fntype) and sym_3f(name) and (type(arglist) == "table") and (type(docstring) == "string"))
    end
    if and_321_ then
      local fntype = _3fast[1]
      local name = _3fast[2]
      local arglist = _3fast[3]
      local docstring = _3fast[4]
      local body = _3fast[5]
      return {fntype = fntype, name = name, arglist = arglist, docstring = docstring}
    else
      local and_323_ = ((_G.type(_3fast) == "table") and (nil ~= _3fast[1]) and (nil ~= _3fast[2]) and (nil ~= _3fast[3]) and (nil ~= _3fast[4]))
      if and_323_ then
        local fntype = _3fast[1]
        local arglist = _3fast[2]
        local docstring = _3fast[3]
        local body = _3fast[4]
        and_323_ = (body and fn_3f(fntype) and (type(arglist) == "table") and (type(docstring) == "string"))
      end
      if and_323_ then
        local fntype = _3fast[1]
        local arglist = _3fast[2]
        local docstring = _3fast[3]
        local body = _3fast[4]
        return {fntype = fntype, arglist = arglist, docstring = docstring}
      else
        local and_325_ = ((_G.type(_3fast) == "table") and (nil ~= _3fast[1]) and (nil ~= _3fast[2]) and (nil ~= _3fast[3]))
        if and_325_ then
          local fntype = _3fast[1]
          local name = _3fast[2]
          local arglist = _3fast[3]
          and_325_ = (fn_3f(fntype) and sym_3f(name) and (type(arglist) == "table"))
        end
        if and_325_ then
          local fntype = _3fast[1]
          local name = _3fast[2]
          local arglist = _3fast[3]
          return {fntype = fntype, name = name, arglist = arglist}
        else
          local and_327_ = ((_G.type(_3fast) == "table") and (nil ~= _3fast[1]) and (nil ~= _3fast[2]))
          if and_327_ then
            local fntype = _3fast[1]
            local arglist = _3fast[2]
            and_327_ = (fn_3f(fntype) and (type(arglist) == "table"))
          end
          if and_327_ then
            local fntype = _3fast[1]
            local arglist = _3fast[2]
            return {fntype = fntype, arglist = arglist}
          else
            return nil
          end
        end
      end
    end
  end
  local function hover_format(result)
    _G.assert((nil ~= result), "Missing argument result on src/fennel-ls\\formatter.fnl:81")
    local _331_
    do
      local _330_ = analyze_fn(result.definition)
      if ((_G.type(_330_) == "table") and true and true and true and true) then
        local _3ffntype = _330_.fntype
        local _3fname = _330_.name
        local _3farglist = _330_.arglist
        local _3fdocstring = _330_.docstring
        _331_ = fn_format(_3ffntype, _3fname, _3farglist, _3fdocstring)
      else
        local _ = _330_
        local _334_
        do
          local tmp_6_auto = result.keys
          if (tmp_6_auto ~= nil) then
            local tmp_6_auto0 = #tmp_6_auto
            if (tmp_6_auto0 ~= nil) then
              _334_ = (0 < tmp_6_auto0)
            else
              _334_ = nil
            end
          else
            _334_ = nil
          end
        end
        if _334_ then
          _331_ = code_block(("ERROR, I don't know how to show this " .. "(. " .. view(result.definition, {["prefer-colon?"] = true}) .. " " .. view(result.keys, {["prefer-colon?"] = true}) .. ")"))
        elseif result.metadata then
          _331_ = metadata_format(result)
        else
          _331_ = code_block(view(result.definition, {["prefer-colon?"] = true}))
        end
      end
    end
    return {kind = "markdown", value = _331_}
  end
  local kinds = {Text = 1, Method = 2, Function = 3, Constructor = 4, Field = 5, Variable = 6, Class = 7, Interface = 8, Module = 9, Property = 10, Unit = 11, Value = 12, Enum = 13, Keyword = 14, Snippet = 15, Color = 16, File = 17, Reference = 18, Folder = 19, EnumMember = 20, Constant = 21, Struct = 22, Event = 23, Operator = 24, TypeParameter = 25}
  local function completion_item_format(label, result)
    _G.assert((nil ~= result), "Missing argument result on src/fennel-ls\\formatter.fnl:105")
    _G.assert((nil ~= label), "Missing argument label on src/fennel-ls\\formatter.fnl:105")
    local tmp_9_auto
    do
      local _340_ = analyze_fn(result.definition)
      if ((_G.type(_340_) == "table") and true) then
        local _ = _340_.fntype
        local _341_
        if label:find(":") then
          _341_ = kinds.Method
        else
          _341_ = kinds.Function
        end
        tmp_9_auto = {label = label, kind = _341_}
      else
        local _ = _340_
        tmp_9_auto = {label = label, kind = kinds.Variable}
      end
    end
    tmp_9_auto["documentation"] = hover_format(result)
    return tmp_9_auto
  end
  return {["hover-format"] = hover_format, ["completion-item-format"] = completion_item_format}
end
dispatch = require("fennel-ls.dispatch")
local json_rpc
package.preload["fennel-ls.json-rpc"] = package.preload["fennel-ls.json-rpc"] or function(...)
  local _local_449_ = require("dkjson")
  local encode = _local_449_["encode"]
  local decode = _local_449_["decode"]
  local function read_header(_in, _3fheader)
    _G.assert((nil ~= _in), "Missing argument in on src/fennel-ls\\json-rpc.fnl:13")
    local header = (_3fheader or {})
    local _450_ = _in:read()
	io.stderr:write("\n---- >>>> Header=" .. tostring(_3fheader) .. "\n")
	io.stderr:write("\n---- READ in: " .. tostring(_450_) .. ", length= " .. tostring(string.len(_450_ or "") .. "\n"))
    if (_450_ == "\13") then
      return header
    elseif (_450_ == nil) then
      return nil
	  	elseif (string.len(_450_) == 0) then
	  return header
    elseif (nil ~= _450_) then
      local header_line = _450_
      local sep = string.find(header_line, ": ")
      local k = string.sub(header_line, 1, (sep - 1))
      --local v = string.sub(header_line, (sep + 2), -2)
	  
	  local v = string.sub(header_line, (sep + 2))
	  v = string.gsub(v, '[ \t]+%f[\r\n%z]', '')
	  io.stderr:write("\n ---- Res V = " .. tostring(v))
      header[k] = v
      return read_header(_in, header)
    else
      return nil
    end
  end
  local function read_n(_in, len, _3fbuffer)
    _G.assert((nil ~= len), "Missing argument len on src/fennel-ls\\json-rpc.fnl:27")
    _G.assert((nil ~= _in), "Missing argument in on src/fennel-ls\\json-rpc.fnl:27")
    local buffer = (_3fbuffer or {})
    io.stderr:write("\n -------- read n=" .. tostring(len) .. "\n")
	if (len <= 0) then
	  io.stderr:write("\n -------- reading A --- \n")
      return table.concat(buffer)
    else
      local _452_ = _in:read(len)
	  io.stderr:write("\n -------- reading B --- read=" .. tostring(_452_) .. ",  length=" .. tostring(string.len(_452_)) .. "\n")
      if (nil ~= _452_) then
        local content = _452_
        local function _453_()
          table.insert(buffer, content)
          return buffer
        end
		 io.stderr:write("\n -------- reading C -- len=" .. tostring(len) .. " #c=" .. tostring(#content) .. ", RETURNING with res=" .. (len - #content))
        return read_n(_in, (len - #content), _453_())
      else
		io.stderr:write("\n -------- reading Z --- nil")
        return nil
      end
    end
  end
  local function read_content(_in, header)
    _G.assert((nil ~= header), "Missing argument header on src/fennel-ls\\json-rpc.fnl:39")
    _G.assert((nil ~= _in), "Missing argument in on src/fennel-ls\\json-rpc.fnl:39")
	io.stderr:write("\n --->>> >>> >>> >>> -- About to read content n=" .. tostring(header["Content-Length"]))
    return read_n(_in, tonumber(header["Content-Length"]))
  end
  local function read(_in)
    _G.assert((nil ~= _in), "Missing argument in on src/fennel-ls\\json-rpc.fnl:43")
    local _3fresult, __3ferr_pos, _3ferr = nil, nil, nil
    do
      local tmp_6_auto = read_header(_in)
      if (tmp_6_auto ~= nil) then
        local tmp_6_auto0 = read_content(_in, tmp_6_auto)
        if (tmp_6_auto0 ~= nil) then
          _3fresult, __3ferr_pos, _3ferr = decode(tmp_6_auto0)
        else
          _3fresult, __3ferr_pos, _3ferr = nil
        end
      else
        _3fresult, __3ferr_pos, _3ferr = nil
      end
    end
    return (_3fresult or _3ferr)
  end
  local function write(out, msg)
    _G.assert((nil ~= msg), "Missing argument msg on src/fennel-ls\\json-rpc.fnl:52")
    _G.assert((nil ~= out), "Missing argument out on src/fennel-ls\\json-rpc.fnl:52")
    local content = encode(msg)
    local msg_stringified = ("Content-Length: " .. #content .. "\13\n\13\n" .. content)
    out:write(msg_stringified)
    if out.flush then
      return out:flush()
    else
      return nil
    end
  end
  return {read = read, write = write}
end
json_rpc = require("fennel-ls.json-rpc")
local function lint(filenames)
  _G.assert((nil ~= filenames), "Missing argument filenames on src/fennel-ls.fnl:5")
  local files = require("fennel-ls.files")
  local lint0 = require("fennel-ls.lint")
  local server
  do
    local tmp_9_auto = {}
    dispatch["handle*"](tmp_9_auto, {id = 1, jsonrpc = "2.0", method = "initialize", params = {capabilities = {general = {positionEncodings = {"utf-8"}}}, clientInfo = {name = "fennel-ls"}, rootUri = "file://."}})
    server = tmp_9_auto
  end
  local should_err_3f = false
  for _, filename in ipairs(filenames) do
    local file = files["get-by-uri"](server, ("file://" .. filename))
    lint0.check(server, file)
    for _0, _459_ in ipairs(file.diagnostics) do
      local message = _459_["message"]
      local _each_460_ = _459_["range"]
      local start = _each_460_["start"]
      print(("%s:%s:%s: %s"):format(filename, ((start.line or 0) + 1), (start.character or "?"), message))
    end
    if file.diagnostics[1] then
      should_err_3f = true
    else
    end
  end
  if should_err_3f then
    return os.exit(1)
  else
    return nil
  end
end

local function dump(o)
   if type(o) == 'table' then
      local s = '{ '
      for k,v in pairs(o) do
         if type(k) ~= 'number' then k = '"'..k..'"' end
         s = s .. '['..k..'] = ' .. dump(v) .. ','
      end
      return s .. '} '
   else
      return tostring(o)
   end
end

local function main_loop(_in, out)
  _G.assert((nil ~= out), "Missing argument out on src/fennel-ls.fnl:32")
  _G.assert((nil ~= _in), "Missing argument in on src/fennel-ls.fnl:32")
  local send
  local function _463_(...)
    --[[
	local ni = 0
	local nk = 0
	local vs = "["
	for i, v in ipairs(...) do
		ni = ni + 1
	end
	for k, v in pairs(...) do
		nk = nk + 1
		vs = vs .. tostring (k) .. " -> " .. tostring(v) ..  ", "
		if type(v) == "table" then
			vs = vs .. "<<<<"
			for kk, vv in pairs(v) do
				vs = vs .. tostring(kk) .. "::" .. tostring(vv) .. ";"
			end
			vs = vs .. ">>>>"
		end
	end
	vs = vs .. "]"
	io.stderr:write("------ RESPONSE: --- counts=" ..  tostring(ni) ..  ", " .. tostring(nk) .. ",  vs=" .. vs .. " ---- \n")
	io.stderr:write("------ RESP TBL: --- ", dump(...))]]
    return json_rpc.write(out, ...)
  end
  send = _463_
  local server = {}
  local iter = 0
  while true do
	io.stderr:write("--------------- ITER START --- i =" .. tostring(iter) .. "\n")
    local msg = json_rpc.read(_in)
    if msg then
	  dispatch.handle(server, send, msg)
	end
	io.stderr:write("--------------- ITER END --- i =" .. tostring(iter) .. "\n")
	iter = iter + 1
  end
  io.stderr:write("--------------- EXIT --- i =" .. tostring(iter) .. "\n")
  return nil
end
local function main()
  if ((_G.type(arg) == "table") and (arg[1] == "--lint")) then
    local filenames = {select(2, (table.unpack or _G.unpack)(arg))}
    return lint(filenames)
  elseif (((_G.type(arg) == "table") and (arg[1] == "--server")) or ((_G.type(arg) == "table") and (arg[1] == nil))) then
    return main_loop(io.input(), io.output())
  else
    local _args = arg
    io.stderr:write("USAGE: fennel-ls [--lint file] [--server]\n")
    return os.exit(1)
  end
end
return main()
