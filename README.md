This is a modified mirror of the source code accompanying [David Cope's][cope] book

* [*Computer Models of Musical Creativity*.][cmmc] Cambridge, MA: MIT Press. 2006.

The original source code can be found here:

*  [http://artsites.ucsc.edu/faculty/cope/cmmc.html][1]

It was originally intended to be run with [Macintosh Common LISP][mac-clisp].

----

I have performed some anti-bitrot maintenance and tried to make the code more compatible across platforms:

* README files in plain .txt format.
* Folder names have been changed to avoid the '/' and ' ' characters.

In all folders, except for `sorcerer-chapter-5`, I have also performed the following adjustments:

* Line endings are LF.
* Character encoding is UTF-8.
* File names have been changed to add '.lisp' extension and avoid spaces.

As far as I can tell, all these changes seem to be compatible with Macintosh Common LISP. (You may have to drag the `init.lisp` file onto the `RMCL` Application instead of opening it with the "Open" menu command.)


  [mac-clisp]: http://en.wikipedia.org/wiki/Macintosh_Common_Lisp
  [cope]: http://artsites.ucsc.edu/faculty/cope/
  [cmmc]: http://books.google.de/books?id=rnEJAQAAMAAJ
  [1]: http://artsites.ucsc.edu/faculty/cope/cmmc.html
