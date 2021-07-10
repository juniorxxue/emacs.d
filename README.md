# Coq IDE for macOS

Throw away your CoqIDE/VSCoq, and use Proof General to maximize your productivity!

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

Run your emacs and wait for a few seconds until the welcome page shows up

## Usage

### Shortcuts

In Emacs's universe, `C-x` stands for holding `Control` and then press `X`, `M-x` stands for holding Meta key `Cmd` and then press `X`.

I've customized some shortcuts for my personal usage.

| Shortcuts    | Functions                  |
|--------------|----------------------------|
| C-c C-Return | run code to this point |
| M-p          | run previous command       |
| M-n          | run next command           |
