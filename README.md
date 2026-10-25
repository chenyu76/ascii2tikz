# ASCII 2 TikZ

This repo is my solution to quickly draw some simple TikZ graph in ASCII.

For instance, the [LaTeX code](./beamer-example.tex)

```latex
\begin{frame}[fragile]{ASCII 2 TikZ}
  \centering
  \begin{asciitikz}
    ┌─────────┐   ┌─────────┐   ┌────────┐
    │  Input  │──→│ Process │──→│ Output │
    └─────────┘   └─────────┘   └────────┘
  \end{asciitikz}

  \medskip
  Draw ASCII diagram in the \texttt{asciitikz} environment, and TikZ will be
  generated automatically.
\end{frame}
```

now can be compile to the PDF

<img width="1920" height="1080" alt="beamer-example" src="https://github.com/user-attachments/assets/970852d7-3ad9-4a2a-8cb4-8955870d65ef" />

The ASCII graph can be drawn with my neovim plugin [draftsman.nvim](https://github.com/chenyu76/draftsman.nvim).

## Quick start

NOTE: This package only works on Linux now.

1. Install [GHC](https://www.haskell.org/ghc/). Make sure `ghc` is in your `PATH`.
2. Clone this repository.
3. Compile this LaTeX example:
   ```sh
   xelatex -shell-escape beamer-example.tex
   ```

## Usage

### Use in CLI

Requires GHC, with runghc available in PATH.
Read a diagram from stdin and write a TikZ picture to stdout:

```sh
runghc ascii2tikz.hs < diagram.txt > diagram.tex
```

An optional argument is appended verbatim to the tikzpicture options:

```sh
runghc ascii2tikz.hs 'scale=1.5,draw=blue' < diagram.txt > diagram.tex
```

Piped input is also supported:

```sh
cat diagram.txt | runghc ascii2tikz.hs > diagram.tex
```

To compile the script into an executable:

```sh
ghc -O2 ascii2tikz.hs -o ascii2tikz
```

### Use directly in LaTeX:

First, Place `ascii2tikz.hs` and `ascii2tikz.sty` beside the .tex file,
or add this repository as a submodule. The package calls the executable beside
its own `.sty` file.

#### In preamble:

add

```latex
\usepackage{ascii2tikz}
```

to the preamble.

For a submodule stored in `ascii2tikz/`:

```latex
\usepackage{ascii2tikz/ascii2tikz}
```

#### Use in document:

```latex
\begin{asciitikz}
┌───────────┐
│hello world│
└───────────┘
\end{asciitikz}
```

Pass extra TikZ options through the environment's optional argument:

```latex
\begin{asciitikz}[scale=1.5, transform shape, draw=blue]
┌───────────┐
│hello world│
└───────────┘
\end{asciitikz}
```

#### Compile:

Loading `ascii2tikz.sty` builds the executable beside the `.sty` file.
GHC must be in PATH. Enable shell escape:

```sh
xelatex -shell-escape main.tex
```

This repository's .latexmkrc already selects XeLaTeX and enables shell escape,
so the bundled example can be compiled with `latexmk beamer-example.tex`.

#### Tips:

- In Beamer, frames containing asciitikz must use `\begin{frame}[fragile]`.

- If you format LaTeX files with tex-fmt, add the following to tex-fmt.toml
  in the project directory to preserve the contents of asciitikz environments:

  ```toml
  verbatims = ["asciitikz"]
  ```

Check [ascii2tikz.hs](./ascii2tikz.hs) for more information.
