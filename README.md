<img src="./images/snails.png" width="200">

# What is snails?
Snails is a modern, easy-to-expand fuzzy search framework.

The goal of this project is to minimize the development threshold for fuzzy search plugins.
If you know how to write a filter function, you can write a new search plugin in 5 minutes,
regardless of how complex the search framework is.

## Installation
1. Clone or download this repository (path of the folder is the `<path-to-snails>` used below).
2. Install [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell) dependency.
3. In your `~/.emacs`, add the following two lines:
```elisp
(add-to-list 'load-path "<path-to-snails>") ; add snails to your load-path
(require 'snails)
```

## Usage
```M-x snails```

<img src="./images/screenshot.png">

If you're a big fan of [Ivy](https://github.com/abo-abo/swiper#ivy), you can use a search backend separately in the following way:

### Just Search Buffer
```elisp
(snails '(snails-backend-buffer))
```

### Search with Customized Backends
```elisp
(snails '(snails-backend-buffer snails-backend-buffer))
```

### Keymap

| Key         | Description               |
| :--------:  | :----                     |
| C-n         | Select next candidate     |
| C-p         | Select previous candidate |
| M-n         | Select next candidate     |
| M-p         | Select previous candidate |
| M-j         | Select next backend       |
| M-k         | Select previous backend   |
| C-m         | Confirm                   |
| RET         | Confirm                   |
| C-g         | Quit snails               |
| ESC ESC ESC | Quit snails               |
| M-h         | Quit snails               |

### Search Backend

| Backend                    | Description                                     |
| :--------                  | :----                                           |
| snails-backend-buffer      | Search buffer                                   |
| snails-backend-recentf     | Search recently files                           |
| snails-backend-fd          | Use fd search files in current project          |
| snails-backend-mdfind      | Use mdfind search files in local disk, only Mac |
| snails-backend-rg          | Use ripgrep search content in current project   |
| snails-backend-bookmark    | Switch bookmark                                 |
| snails-backend-awesome-tab | Switch group of awesome-tab                     |

## Architecture Design of Snails

<img src="./images/framework.png">

snails-core.el is framework code, it only do:
1. Monitor user input, generate input ticker and send a search request to the backend.
2. Check backend's search result with input ticker.
3. Render search result if input ticker is newest.

Sync backend search action is trigger by framework when user type new character.
Async backend search action only trigger by framework when user release keyboard key.

Input ticker is the label of the input event，backend's input ticker will expired when user type new character in input buffer.

When backend search finish, framework will drop search result if input ticker is expired.

## How to Write a New Plugin?

Writing a plugin for snails is very simple.
As long as you have basic knowledge of elisp, you can write a plugin in 5 minutes!

Snails plugins fall into two categories: sync plugins and asynchronous plugins.

Sync plugins are plugins that get the completion results immediately, such as buffers, recent files, etc.

Asynchronous plugins are plugins that take time to get completion results and are usually placed in child processes for calculation, such as find file, grep file, etc.

### Writing a Sync Plugin
Let's take the example of ```snails-backend-recentf``` plugin:

```elisp
(require 'snails-core)
(require 'recentf)

(recentf-mode 1)

(snails-create-sync-backend
 :name
 "RECENTF"

 :candidate-filter
 (lambda (input)
   (let (candidates)
     (dolist (file recentf-list)
       (when (or
              (string-equal input "")
              (string-match-p (regexp-quote input) file))
         (add-to-list 'candidates
                      (list
                       (snails-wrap-file-icon file)
                       file)
                      t)))
     candidates))

 :candiate-do
 (lambda (candidate)
   (find-file candidate)))

(provide 'snails-backend-recentf)
```

* :name parameter is the name of your plugin, and it must be unique. Snails distinguishes the results of different plugins based on the plugin name.

* :candidate-filter is the filter function. ```input``` is user input content, and you need to return a candidate list to the snails framework, where elements of the candidate list are formatted as ```(list display-name candidate-content)```. The first candidate element ```display-name``` is the string presented to the user, and the second candidate element ```candidate-content``` is the string passed to the ```candidate-do``` callback below. If nothing was found, please return nil, and snails will hide the backend result.

* :candidate-do is the function to confirm the candidate, and it can be any code you want.

Taking the above plug-in as an example, when the user does not input anything, all the recently viewed files are displayed, and when the user does input something, the recently viewed files are filtered according to the input content. When the user confirms, use the find-file command to open the file.

### Writing an Async Plugin
Let's take the example of ```snails-backend-mdfind``` plugin:

```elisp
(require 'snails-core)

(snails-create-async-backend
 :name
 "MDFIND"

 :build-command
 (lambda (input)
   (when (and (featurep 'cocoa)
              (> (length input) 5))
     (list "mdfind" (format "'%s'" input))))

 :candidate-filter
 (lambda (candidate-list)
   (let (candidates)
     (dolist (candidate candidate-list)
       (add-to-list 'candidates
                    (list
                     (snails-wrap-file-icon candidate)
                     candidate)
                    t))
     candidates))

 :candiate-do
 (lambda (candidate)
   (find-file candidate)))

(provide 'snails-backend-mdfind)
```

* :name parameter is the name of your plugin, and it must be unique. Snails distinguishes the results of different plugins based on the plugin name.

* :build-command is the function to build a command with user input. ```input``` is the user input content, and you need to  return a list of strings where the first string is a shell command, and the rest of the strings are arguments to pass to the shell command. If you don't want the search to continue, please return nil.

* :candidate-filter is the filter function, ```candidate-list``` is a list of strings returned by the shell command, and you need to return a candidate list to the snails framework, where elements of the candidate list are formatted as ```(list display-name candidate-content)```. The first candidate element ```display-name``` is the string presented to the user, and the second candidate element ```candidate-content``` is the string passed to the ```candidate-do``` callback below. If nothing was found, please return nil, and snails will hide the backend result.

* :candidate-do is the function to confirm the candiate, and it can be any code you want.

Taking the above plug-in as an example, when the user inputs "multi-term", build-command will check the input length, and the search will only start after more than 5 characters have been entered. Then, the build-command function will build commands ```(list "mdfind" "'multi-term'")``` to pass to the async subprocess. When the async subprocess finishes, it will return a list of strings to the candidate-fitler callback, and the candiate-filter function will wrap the shell result as a candidate list. When the user confirms, use the find-file command to open the file.

Snails is very smart; it will manage subprocesses of the async backend. When the user modifies the input, the snails framework automatically creates a new subprocess to search for the results, while automatically killing the old running process. No matter how fast the user enters, it won't block Emacs.

### FAQ

#### Why doesn't snails frame work when I open a fullscreen Emacs on Mac?
Mac will force the fullscreen Emacs window to a separate workspace, and then any new frame created by ```make-frame``` will not float above the Emacs window as expected.

If you start Emacs with fullscreen mode, you can use my workaround code to fix this problem:

```elisp
(if (featurep 'cocoa)
    (progn
      (setq ns-use-native-fullscreen nil)
      (setq ns-use-fullscreen-animation nil)

      (set-frame-parameter (selected-frame) 'fullscreen 'maximized)

      (run-at-time "2sec" nil
                   (lambda ()
                     (toggle-frame-fullscreen)
                     )))
  (require 'fullscreen)
  (fullscreen))
```

## TODO List

* Use better fuzz match algorithm.
* Highlight match keyword in candidate line.
* Disable insert return char in input buffer.
* Add device to disable window configuration change in popup frame.
* Display ansi color string from async process, such as fd, rg, mdfind.

Welcome to hacking this framework! ;)
