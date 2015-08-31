Building the system

If you want to rebuild the PDF you'll need to install
TexLive and make sure that pdflatex works.

I've written this in org mode in emacs with
a little Erlang program to convert org mode to LaTeX.

The conventions I've used are as follows:

+ Org mode * means a new chapter
+ Org mode ** means a new section
+ Preformed starts with 4 blanks
+ Line starting !! and > are expanded by an erlang program
+  Most sections are just LaTeX

I don't use org mode ``export to LaTeX'' but do the translation myself.

The resulting PDF should be readable. The filenames and
code is a bit of a mess and be be refactored.

To build

   > cd src
   > make

When you're happy do

   > make publish

Check ./crypto_tutorial.pdf

If you're happy do

   > make clean

And *then* push changes in ./src to me

make_publish deletes the tempory files and creates a top level PDF
This is ``bad practise'' since we should only commit source code and
not generated outsput to the repository. BUT it's there so people can read
the result without having to build everything for themselves.

If you a re suggesting change don't commit the crypto_tutorial.pdf
to the archive.