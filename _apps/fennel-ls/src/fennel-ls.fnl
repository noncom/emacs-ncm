(local fennel (require :fennel))
(local dispatch (require :fennel-ls.dispatch))
(local json-rpc (require :fennel-ls.json-rpc))

(λ lint [filenames]
  "non-interactive mode that gets executed from CLI with --lint.
   runs lints on each file, then formats and prints them"
  (local files (require :fennel-ls.files))
  (local lint (require :fennel-ls.lint))
  (let [server (doto {}
                 (dispatch.handle* {:id 1
                                    :jsonrpc "2.0"
                                    :method "initialize"
                                    :params {:capabilities {:general {:positionEncodings [:utf-8]}}
                                             :clientInfo {:name "fennel-ls"}
                                             ; don't think this is a valid URI, but I want to operate in the current directory
                                             :rootUri "file://."}}))]
    (var should-err? false)
    (each [_ filename (ipairs filenames)]
      (let [file (files.get-by-uri server (.. "file://" filename))]
        (lint.check server file)
        (each [_ {: message :range {: start}} (ipairs file.diagnostics)]
          (print (: "%s:%s:%s: %s" :format filename
                    ;; LSP line numbers are zero-indexed, but Emacs and Vim both use
                    ;; 1-indexing for this.
                    (+ (or start.line 0) 1) (or start.character "?") message)))
        (if (. file.diagnostics 1)
          (set should-err? true))))
    (if should-err?
      (os.exit 1))))

(var i 0)

(λ main-loop [in out]
  (io.stderr:write (.. " --------------------- STARTING i=" (tostring i) "------------------------\n"))
  (io.stderr:write (.. " ------ in: " (fennel.view in)))
  (io.stderr:write (.. " ------ out: " (fennel.view out)))
  (io.stderr:write (.. " ------ methods: " (fennel.view io)))
  (local send (partial json-rpc.write out))
  (local server {})
  (while true
	(io.stderr:write (.. "-- Before json-rpc.read -- i=" (tostring i) "\n"))
    (set i (+ i 1))
	(let [msg (json-rpc.read in)]
	  (io.stderr:write " ============================================================ ")
	  (io.stderr:write (.. "\n --------------- in msg=" (fennel.view msg) "\n\n"))
	  (if msg
		(do
			(dispatch.handle server send msg))))))

(λ main []
  (case arg
    ["--lint" & filenames] (lint filenames)
    (where (or ["--server"] [nil])) (main-loop (io.input)
                                               (io.output))
    _args (do (io.stderr:write "USAGE: fennel-ls [--lint file] [--server]\n")
              (os.exit 1))))

(main)
