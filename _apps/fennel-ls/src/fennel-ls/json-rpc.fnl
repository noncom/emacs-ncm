"JSON-RPC
This module implements the parsing and formatting code needed to read/write messages over the language server protocol.
There are only two functions exposed here:

* `read` receives and parses a message from the client.
* `write` serializes and sends a message to the client.

It's probably not compliant yet, because serialization of [] and {} is the same.
Luckily, I'm testing with Neovim, so I can pretend these problems don't exist for now."

(local fennel (require :fennel))
(local {: encode : decode} (require :dkjson))

(λ read-header [in ?header]
  "Reads the header of a JSON-RPC message"
  (let [header (or ?header {})
        in-read (in:read)]
    (io.stderr:write (.. "---- in read=" (tostring in-read) "\n"))
	(case in-read
      "\r" (do (io.stderr:write (.. "--> got A header=" (fennel.view header) "\n")) header) ;; hit an empty line, I'm done reading
      nil (do (io.stderr:write "--> got header nil\n") nil) ;; hit end of stream, return nil
	  "" (do (io.stderr:write (.. "--> got header EMPTY -- return HEADER=" (fennel.view header) "\n")) header)
      ;; reading an actual line
      header-line
      (let [_ (io.stderr:write (.. " ---- header-line: " (tostring header-line) "\n"))
			sep (string.find header-line ": ")
            k (string.sub header-line 1 (- sep 1))
            ;v (string.sub header-line (+ sep 2) -2)] ;; trim off the \r
			v (string.sub header-line (+ sep 2))
			v (string.gsub v "[ \t]+%f[\r\n%z]" "")]
		(io.stderr:write (.. "--> got B header=" (tostring k) " -> " (tostring v) "\n\n"))
		(io.stderr:write (.. " -----> header=" (fennel.view header) "\n\n"))
        (tset header k v)
        (read-header in header)))))

(λ read-n [in len ?buffer]
  "read a string of exactly `len` characters from the `in` stream.
If there aren't enough bytes, return nil"
  (local buffer (or ?buffer []))
  (io.stderr:write (.. "-- read n=" (tostring len) "\n"))
  (if (<= len 0)
    (table.concat buffer)
    (case (in:read len)
      content
      (read-n in
              (- len (length content))
              (doto buffer (table.insert content))))))

(λ read-content [in header]
  "Reads the content of a JSON-RPC message given the header"
  (let [content (read-n in (tonumber header.Content-Length))]
	(io.stderr:write (.. "\n\n-->>> read  HEADER=" (fennel.view header) "  CONTENT=" (fennel.view content) "\n\n\n"))
	content))

(λ read [in]
  "Reads and parses a JSON-RPC message from the input stream
Returns a table with the message if it succeeded, or a string with the parse error if it fails."
  (io.stderr:write "About to read....\n")
  (let [header (read-header in)]
    (if (and header (tonumber header.Content-Length)) 
		(let [(?result _?err-pos ?err)
			  (-?>> header
			  (read-content in)
			   decode)]
			(io.stderr:write (.. "result=" (tostring ?result) ", err=" (tostring ?err)))
			(or ?result ?err)))))

(λ write [out msg]
  "Serializes and writes a JSON-RPC message to the given output stream"
  (let [content (encode msg)
        msg-stringified (.. "Content-Length: " (length content) "\n\n" content)]
	(io.stderr:write (.. "\n\n--- WRITE msg=" msg-stringified "\n\n"))
	(io.stderr:write (.. "\n\n--- OUT=" (fennel.view out) "\n\n"))
    ;(out:write msg-stringified)
    (out:write (.. "Content-Length: " (length content)))
	(out:write "\n\n")
	(out:write content)
	;(out:write "\n")
	(when out.flush
      (out:flush))))

{: read
 : write}
