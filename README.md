<img src="./images/snails.png" width="200">

# What is snails?
Snails is modern, easy-to-expand fuzzy search framework.

The goal of this project is to minimize the development threshold for fuzzy search plugins.
If you know how to write filter function, you can write a new search plugin in 5 minutes,
regardless of the complex search framework details.

## Installation
Clone or download this repository (path of the folder is the `<path-to-snails>` used below).

In your `~/.emacs`, add the following two lines:
```Elisp
(add-to-list 'load-path "<path-to-snails>") ; add snails to your load-path
(require 'snails)
```

## Usage
```M-x snails```

<img src="./images/screenshot.png">

### Keymap for view mode

| Key        | Description                                                            |
| :--------: | :----                                                                  |
| C-n        | Select next candidate                                                  |
| C-p        | Select previous candidate                                              |
| C-m        | Confirm                                                                |
| C-g        | Quit snails                                                            |

## How to write new plugin?

Document coming soon, please wait patiently.

## TODO List

* Research async backend and adjust framework interface.
* Add ```fd``` backend to search current project.
* Add elisp library backend.
* Write plugin document.
* Add interactive command to jump next/previous backend.
* Add device to disable window configuration change in poup frame.
* Disable insert return char in input buffer.
* Add backend list sidebar.

Welcome to hacking this framework! ;)
