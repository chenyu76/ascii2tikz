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

Check [ascii2tikz.hs](./ascii2tikz.hs) for more information.
