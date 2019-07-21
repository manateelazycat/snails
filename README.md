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

| Key         | Description               |
| :--------:  | :----                     |
| C-n         | Select next candidate     |
| C-p         | Select previous candidate |
| M-n         | Select next candidate     |
| M-p         | Select previous candidate |
| M-j         | Select next backend       |
| M-k         | Select previous backend   |
| C-m         | Confirm                   |
| Renturn     | Confirm                   |
| C-g         | Quit snails               |
| ESC ESC ESC | Quit snails               |

## How to write new plugin?

Write the plugin of snails is very simple.
As long as you have the basic knowledge of elisp, you can write a plugin in 5 minutes!

Snails plugins fall into two categories: sync plugins and asynchronous plugins.

The sync plugin mean you can get the completion result immediately, such as, buffer, recently files etc.

Asynchronous plugins are plugins that take time to get completion results and are usually placed in child processes for calculation，such as, find file, grep file etc.

### Write sync plugin
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

* :name parameter is the name of your plugin, you must use a unqie name. Snails distinguishes the results of different plugins based on the plugin name.

* :candidate-filter is fitler function, ```input``` is user input content, you need return candidate list to snails framework, element of candidate list is format as ```(list display-name candidate-content)```, first of candidate element ```display-name``` is the string presented to the user, second of candidate elemenet ```candidate-content``` is the string pass to ```candidate-do``` callback at below. If nothing search, please renturn nil, snails will hide the backend result.

* :candidate-do is function to confirm candiate, it can be any code you want to do.

Taking the above plug-in as an example, when the user does not input, all the recently viewed files are displayed, and when the user input something, the recently files filtered according to the input content. When the user confirms, use the find-file command to open the file.

### Write Async plugin
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

* :name parameter is the name of your plugin, you must use a unqie name. Snails distinguishes the results of different plugins based on the plugin name.

* :build-command is function to build command with user input, ```input``` is user input content, you need return list string that first string is shell command, rest string is argument that pass to shell command. If you don't want search continue, please return nil.

* :candidate-filter is fitler function, ```candidate-list``` is list string return by shell command, you need return candidate list to snails framework, element of candidate list is format as ```(list display-name candidate-content)```, first of candidate element ```display-name``` is the string presented to the user, second of candidate elemenet ```candidate-content``` is the string pass to ```candidate-do``` callback at below. If nothing search, please renturn nil, snails will hide the backend result.

* :candidate-do is function to confirm candiate, it can be any code you want to do.

Taking the above plug-in as an example, when the user input "multi-term", build-command will check input length, search will only start after more than 5 characters have been entered， after then, build-command function will build commands ```(list "mdfind" "'multi-term'")``` pass to async subprocess, when async subprocess finish, it will return list string to candidate-fitler callback, candiate-filter function wrap shell result as candidate list. When the user confirms, use the find-file command to open the file.

Snails is very smart, it will manage subprocess of async backend, When the user modifies the input, the snails framework automatically creates a new subprocess to search for the results, while automatically killing the old running process. No matter how fast the user enters, it won't block Emacs.


## TODO List

* Add backend list sidebar.
* Make input buffer same height with font size, not fixed height.
* Disable insert return char in input buffer.
* Add device to disable window configuration change in poup frame.

Welcome to hacking this framework! ;)
