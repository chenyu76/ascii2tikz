# ASCII 2 TikZ

This project is my solution to quickly draw some simple TikZ graph in ASCII.

For instance, the LaTeX code

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

PLACEHOLDER

The ASCII graph can be drawn with my neovim plugin [draftsman.nvim](https://github.com/chenyu76/draftsman.nvim).

Check [ascii2tikz.hs](./ascii2tikz.hs) for more information.
