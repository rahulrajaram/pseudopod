# pseudopod

Common Lisp client SDK for [Moonshot's](https://www.moonshot.ai/) OpenAI-compatible API (Kimi K2.5).

Features: sync and streaming chat completions, tool-call dispatch with bounded step loop, multi-turn conversation state, models/tokenizer APIs, files CRUD, and a typed condition hierarchy.

## Requirements

- [SBCL](http://www.sbcl.org/) (tested on 2.2.9+)
- [Quicklisp](https://www.quicklisp.org/beta/)
- A Moonshot API key ([platform.moonshot.ai](https://platform.moonshot.ai/))

## Installation

Clone the repo and register the system with ASDF:

```bash
git clone https://github.com/rahulrajaram/pseudopod.git
```

Then in your Lisp image:

```lisp
(ql:quickload "dexador")   ; ensure dependencies are available
(ql:quickload "jonathan")

(asdf:load-asd #P"/path/to/pseudopod/pseudopod.asd")
(asdf:load-system "pseudopod")
```

Or symlink into your ASDF source registry:

```bash
ln -s /path/to/pseudopod/ ~/common-lisp/pseudopod
```

Then simply:

```lisp
(asdf:load-system "pseudopod")
```

## API Key Setup

Pseudopod resolves your API key in order:

1. `MOONSHOT_API_KEY` environment variable
2. `~/.moonshotai` file (plain text, key only)
3. Explicit `:api-key` keyword to `make-client`

```bash
export MOONSHOT_API_KEY="sk-..."
```

Or:

```bash
echo "sk-..." > ~/.moonshotai
```

## Quick Start

```lisp
(defparameter *client* (pseudopod:make-client))

;; Non-streaming (returns parsed JSON hash-table)
(pseudopod:chat-completion *client* "Say hello in one sentence.")

;; Non-streaming typed (returns message struct)
(pseudopod:chat-completion* *client* "Say hello in one sentence.")

;; Streaming (prints reasoning + content chunks to stdout)
(pseudopod:print-streamed-completion *client* "Write a haiku about Common Lisp.")
```

## Usage

### Client Configuration

```lisp
(pseudopod:make-client
 :api-key "sk-..."                    ; optional if env/file is set
 :model "kimi-k2.5"                   ; default
 :base-url "https://api.moonshot.ai/v1"
 :temperature 1.0d0
 :max-tokens 32768
 :timeout-seconds 180
 :max-response-bytes (* 64 1024 1024)) ; 64MB streaming limit
```

### Streaming with Callbacks

```lisp
(pseudopod:stream-chat-completion
 *client* "Explain monads."
 :on-reasoning (lambda (chunk) (format t "[think] ~A" chunk))
 :on-content   (lambda (chunk) (write-string chunk))
 :on-tool-call (lambda (tc) (format t "~&Tool: ~A~%" (pseudopod:tool-call-name tc))))
```

### Tool Use

```lisp
(let ((toolset (pseudopod:make-toolset)))
  (pseudopod:register-tool-function
   toolset
   :name "get-weather"
   :description "Get current weather for a city."
   :parameters '{"type":"object","properties":{"city":{"type":"string"}}}
   :fn (lambda (args tool-call)
         (declare (ignore tool-call))
         (format nil "72F in ~A" (gethash "city" args))))

  ;; Single generation (no automatic tool dispatch)
  (pseudopod:generate *client*
                       :user-prompt "What's the weather in SF?"
                       :toolset toolset)

  ;; Automatic tool loop (up to 8 steps by default)
  (pseudopod:step *client*
                   :user-prompt "What's the weather in SF?"
                   :toolset toolset
                   :max-steps 4))
```

Tool functions can accept either `(arguments tool-call)` or just `(arguments)` — the SDK detects arity automatically on SBCL.

### Conversation State

```lisp
(let ((conv (pseudopod:make-conversation
             :client *client*
             :system-prompt "You are a helpful assistant."
             :toolset toolset)))
  ;; Simple completion (appends to history)
  (pseudopod:conversation-add-user conv "Hello!")
  (pseudopod:conversation-complete conv)

  ;; Tool-aware step (runs tool loop, updates history)
  (pseudopod:conversation-step conv
                                :user-prompt "What time is it?"
                                :max-steps 4)

  ;; Inspect history
  (pseudopod:conversation-history conv)

  ;; Reset
  (pseudopod:conversation-clear conv))
```

### Models and Tokens

```lisp
;; List available models
(pseudopod:list-models *client*)

;; Estimate token count
(pseudopod:estimate-tokens *client* :text "How many tokens is this?")
```

### Files API

```lisp
;; Upload
(pseudopod:upload-file *client* #P"data.txt" :purpose "file-extract")

;; CRUD
(pseudopod:list-files *client*)
(pseudopod:get-file *client* "file-abc123")
(pseudopod:file-content *client* "file-abc123")
(pseudopod:delete-file *client* "file-abc123")
```

### Error Handling

Pseudopod signals typed conditions:

```lisp
(handler-case
    (pseudopod:chat-completion *client* "hello")
  (pseudopod:pseudopod-auth-error (e)
    (format t "Auth failed: ~A~%" e))
  (pseudopod:pseudopod-timeout-error (e)
    (format t "Timed out: ~A~%" e))
  (pseudopod:pseudopod-api-error (e)
    (format t "API error ~A: ~A~%"
            (pseudopod:pseudopod-api-error-status-code e) e))
  (pseudopod:pseudopod-parse-error (e)
    (format t "Parse error: ~A~%" e)))
```

Condition hierarchy:

```
pseudopod-error (base)
├── pseudopod-api-error (status-code, body)
│   └── pseudopod-auth-error
├── pseudopod-timeout-error
└── pseudopod-parse-error (payload)
```

## Running Tests

Smoke test (includes live API connectivity check):

```bash
sbcl --script smoke-test.lisp
```

FiveAM unit tests only (no network):

```lisp
(asdf:test-system "pseudopod")
```

## Defaults

| Setting | Default |
|---------|---------|
| Base URL | `https://api.moonshot.ai/v1` |
| Model | `kimi-k2.5` |
| Temperature | `1.0` |
| Max tokens | `32768` |
| Timeout | `180s` |
| Max response size | `64MB` |

## Dependencies

- [dexador](https://github.com/fukamachi/dexador) — HTTP client
- [jonathan](https://github.com/Rudolph-Miller/jonathan) — JSON
- [usocket](https://github.com/usocket/usocket) — socket layer (via dexador)
- [fiveam](https://github.com/lispci/fiveam) — test framework (test-only)

## Package Aliases

The package `:pseudopod` has `:moonshot-common-lisp` as a nickname for backward compatibility.

## License

MIT
