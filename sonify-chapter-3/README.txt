
    Sonify/Chapter 3

Sonify can be used on any computer which supports a Common Lisp (which virtually means any computer). To run Sonify, open the file from within the Common Lisp you've chosen and carefully read the instructions in the header to the file. In most cases you'll have to cut and paste an instruction such as "(sonify random-data)" or "(sonify CASSIOPEIA-A)" with parentheses into a Listener-type window and press return. Most Common Lisps (even free ones) have associated manuals or help menus which will also be of use. 

Sonify can also be used on a Macintosh computer using System X (non-Intel based) with Macintosh Common Lisp 5.0 available from digitool.com by also loading the "play-midi-QT.lisp" file. To load the Mac version of Sonify, place the sonify-Chapter-3 folder alongside your MCL 5.0 application and then load the Sonify file from that folder from within MCL ("Load File" from the "File" menu) and then the "play-midi-QT.lisp" file in the same way. Then run (play-events (make-events (sonify random-data))) in the Listener window as described in the "play-midi-QT.lisp" file.
