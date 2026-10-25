# Build the converter before TeX; incremental runs reuse the executable.
if (-f 'ascii2tikz.hs'
    && (!-x 'ascii2tikz' || -M 'ascii2tikz.hs' < -M 'ascii2tikz')) {
    system('ghc', '-O2', '-outputdir', '.ghc-build',
           'ascii2tikz.hs', '-o', 'ascii2tikz') == 0
        or die "Failed to compile ascii2tikz.hs\n";
    # Piped command dependencies are not recorded by TeX's recorder.
    $go_mode = 3;
}

# Set default engine to XeLaTeX (pdf_mode = 5)
$pdf_mode = 5;
$dvi_mode = 0;
$postscript_mode = 0;

# Define xelatex command with necessary flags
# -synctex=1: Enable forward/reverse search in PDF viewers
# --shell-escape: Allow execution of external programs (required by minted, etc.)
# -interaction=nonstopmode: Don't pause on errors
$xelatex = 'xelatex %O -synctex=1 --shell-escape -interaction=nonstopmode %S';
