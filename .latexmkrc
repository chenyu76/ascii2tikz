# Set default engine to XeLaTeX (pdf_mode = 5)
$pdf_mode = 5;
$dvi_mode = 0;
$postscript_mode = 0;

# Define xelatex command with necessary flags
# -synctex=1: Enable forward/reverse search in PDF viewers
# --shell-escape: Allow execution of external programs (required by minted, etc.)
# -interaction=nonstopmode: Don't pause on errors
$xelatex = 'xelatex %O -synctex=1 --shell-escape -interaction=nonstopmode %S';
