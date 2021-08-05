# Coq IDE for macOS

[![Build Status](https://github.com/juniorxxue/CoqIDE/workflows/CI/badge.svg?branch=main)](https://github.com/juniorxxue/CoqIDE/actions)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](LICENSE)

An out-of-the-box Emacs configuration of Proof General.

![Screenshot](https://github.com/juniorxxue/xcode-theme/blob/main/images/xcode-dark-theme.png)


## Installation

### Step 1

Download the latest Emacs from [Emacs for macOS](https://emacsformacosx.com) and make sure your emacs version is 27.x

### Step 2

Make sure `coqtop` is in your $PATH

```
which coqtop
```

otherwise, install it from homebrew

```
brew install coq
```

### Step 3

clone this repo in your $HOME directory

```
git clone https://github.com/juniorxxue/CoqIDE.git ~/.emacs.d
```

### Step 4

Download and install the recommended font [Fira Code](https://fonts.google.com/specimen/Fira+Code) for the best

### Step 5

Run your emacs and wait for a few minutes until the welcome page shows up

## Usage

In Emacs's universe, `C-x` stands for holding `Control` and then press `X`, `M-x` stands for holding Meta key `Cmd` and then press `X`.

### Frequent Editing Commands

| Shortcuts | Functions                                                    |
| --------- | ------------------------------------------------------------ |
| M-w       | copy                                                         |
| C-y       | paste                                                        |
| C-a       | move cursor to start of the line                             |
| C-e       | move cursor to end of the line                               |
| C-k       | delete line of current content to cursor                     |
| C-p       | move cursor to previous line                                 |
| C-n       | move cursor to next line                                     |
| C-f       | forward cursor one char                                      |
| C-b       | backward cursor one char                                     |
| C-s       | search                                                       |
| C-x C-f   | open file                                                    |
| C-x p     | open file in current project (don't use it in home directory) |

### Selection Form (Completion, Minibuffer)

| Shortcuts    | Functions                  |
|--------------|----------------------------|
| C-p | previous candidate |
| C-n       | next candidate |
| Enter | confirm |
| C-g | quit (I don't want to select any) |

### Proof General

| Shortcuts    | Functions                  |
|--------------|----------------------------|
| C-c C-return | run code to this point |
| M-p          | run previous command       |
| M-n          | run next command           |
