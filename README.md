# Coq IDE for macOS

Throw away your CoqIDE/VSCoq, and use tailored Proof General to maximize your productivity!

![Screenshot](https://user-images.githubusercontent.com/31076337/125457209-3f5b9e1e-31b9-43eb-a1fc-6dadaa335530.png)


## Installation

### Step 1

Download the latest Emacs from [Emacs for macOS](https://emacsformacosx.com)

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

Download and install the recommended font [Source Code Pro](https://fonts.google.com/specimen/Source+Code+Pro) for the best

### Step 5

Run your emacs and wait for a few minutes until the welcome page shows up

## Usage

### Shortcuts

In Emacs's universe, `C-x` stands for holding `Control` and then press `X`, `M-x` stands for holding Meta key `Cmd` and then press `X`.

I've customized some shortcuts for my personal usage.

| Shortcuts    | Functions                  |
|--------------|----------------------------|
| C-c C-return | run code to this point |
| M-p          | run previous command       |
| M-n          | run next command           |
