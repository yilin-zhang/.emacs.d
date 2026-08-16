# .emacs.d

A personal, macOS-focused Emacs configuration built around Meow, Org,
Denote, Magit, Eglot, and the Vertico/Consult/Embark completion stack.

This README focuses on everyday usage. Configuration details live in the
`lisp/init-*.el` modules, and locally maintained packages live in `site-lisp/`.

## Installation

```sh
git clone https://github.com/yilin-zhang/.emacs.d.git ~/.emacs.d
```

Emacs installs the Elisp packages on first launch. Install the external
dependencies on macOS:

```sh
brew install coreutils ripgrep enchant pkgconf
brew install --cask font-sarasa-gothic font-open-dyslexic-nerd-font
```

Then run `M-x nerd-icons-install-fonts` inside Emacs.

For spell checking, create `~/.config/enchant/enchant.ordering`:

```text
*:AppleSpell
en_US:AppleSpell
```

Install the language servers you need:

| Language | Server |
| --- | --- |
| Python | `pyright` |
| JavaScript / TypeScript | `typescript-language-server` |
| JSON | `vscode-json-languageserver` |
| Vue | `vue-language-server` |
| Lua | `lua-language-server` |

`eglot-booster` also requires
[emacs-lsp-booster](https://github.com/blahgeek/emacs-lsp-booster).

## Editing with Meow

The configuration uses Meow's QWERTY layout. Press `SPC ?` to open the full
cheatsheet. In normal state, uppercase movement keys generally extend the
selection while moving.

These bindings are added on top of the standard Meow layout:

| Key | Action |
| --- | --- |
| `C-[` | Leave insert state |
| `C-z` | Toggle Meow in the current buffer |
| `/` | Run an `M-x` command |
| `<` / `>` | Shift the line or selection by one indentation step |
| `V` | Start a rectangular selection |
| `'` | Repeat the previous command |

### Leader keys

| Key | Action |
| --- | --- |
| `SPC f` | Find a file |
| `SPC b` | Switch buffers |
| `SPC k` | Kill a buffer |
| `SPC p` | Browse the kill ring |
| `SPC o` | Switch windows |
| `SPC d` | Open Dired at the current file |
| `SPC r` | Search text with ripgrep |
| `SPC s` | Cycle an outline subtree |
| `SPC t` | Open the current directory in a terminal |
| `SPC e` | Open in Finder or the default application |
| `SPC h` | Describe the next key |
| `SPC ?` | Open the Meow cheatsheet |

### Surrounding text

Select some text, press `SPC SPC`, then type a delimiter. For example,
`SPC SPC (` produces `(text)`, while `SPC SPC *` produces `*text*`.

The built-in pairs include `()`, `[]`, `{}`, and `<>`, as well as
`= * + - / _ ~ ' "`. Use `M-x yilin/surround-region` for another character.

## Search and navigation

| Key | Action |
| --- | --- |
| `C-s` | Search the current buffer with Consult |
| `C-x b` | Search and switch buffers |
| `SPC r` | Search a project or directory with ripgrep |
| `C-.` | Run an Embark action on the current target |
| `M-.` | Run Embark's default action |

Vertico displays minibuffer candidates. Type multiple space-separated fragments
to narrow the results with Orderless matching.

## Org and notes

| Key | Action |
| --- | --- |
| `C-c a` | Open Org Agenda |
| `C-c c` | Start Org Capture |
| `C-c b` | Switch Org buffers |

Denote notes whose filenames contain the `_agenda` keyword are added to Org
Agenda automatically. The file list is refreshed whenever Agenda opens, so new
notes appear without restarting Emacs.

## Programming and Git

Eglot provides completion, navigation, and diagnostics in supported major
modes. Corfu displays completion candidates automatically in programming
buffers.

| Key | Action |
| --- | --- |
| `s-b` | Jump to a definition |
| `s-r` | Find references |
| `C-c C-d` | Open Eglot documentation in a separate buffer |
| `C-x g` | Open Magit |
| `C-c g b` | Open the current file on its remote forge |
| `C-c g t` | Browse the current file's history |

## Other useful bindings

| Key | Action |
| --- | --- |
| `s-j` | Show, focus, or hide the bottom terminal |
| `F8` | Open Treemacs |
| `F7` | Toggle the centered writing view |
| `F12` | Toggle fullscreen |
| `C-x \|` | Toggle a two-window split between horizontal and vertical |
| `M-Q` | Join a paragraph into one line |
| `C-x n` | Narrow intelligently, or widen when already narrowed |
| `C-;` | Correct the word at point |
| `C-h f/v/k/x` | Describe a function, variable, key, or command |

## Personal configuration

Put machine-specific paths and personal overrides in the untracked file
`~/.emacs.d/custom/custom-post.el`:

```elisp
;; Regular Org Agenda files.
(setq org-agenda-compose-extra-files
      '("~/agenda.org" "~/work.org"))

;; Default Org Capture destination.
(setq org-default-notes-file "~/inbox.org")

;; Denote directory. Notes with _agenda in the filename enter Org Agenda.
(setq denote-directory "~/notes/denote")

;; Bibliography files used by Citar.
(setq citar-bibliography '("~/references.bib"))
```
