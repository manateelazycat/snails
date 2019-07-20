<img src="./images/snails.png" width="200">

# What is snails?
Snails is modern, easy-to-expand fuzzy search framework.

Currently still in developing.
Welcome to hacking, not recommand to use for production enviroment.

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
