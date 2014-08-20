

                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;       Poet Function/Chapter 2       ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;       simple code to run poet       ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;

#| (poet '(life love angel mercy patience) *choices*) |#

;;;  In order to run the poet function you must load this file and then use the function "poet"
;;;  with some arguments as in (poet '(life love angel mercy patience) *choices*) as shown in the 
;;;  example for the top-level below.
;;;  Note that the words used in the first argument must be in the database somewhere.

(defVar *RS* (make-random-state t) "Variable for storing the current random state.")
(defVar *CHOICES* '(kepler emerson alice turing hamlet-5 war-1 newton) "Variable for storing the texts.")

;;;;;
#|? (poet '(life love angel mercy patience) *choices*)
flourishing overhead before it descended
and smote me out of
life for life to go
on in the common way
i concede that if he
should know no use if
not in every day life
or draw off from his
veins the black drop which
he drew from his fathers
or his mothers life make
some one fall in love
with it learn from experience
to laertes strengthen your patience
in our last nights speech
but if the shell moves
the kernel will also move
|#
;;;;;

(defun POET (keywords choices)
  "Keywords should be a list of words the user wishes to poem be about; choices is a list of 
      the loaded data names. Note that the result poem has no punctuation or real formatting."
  (format-output (apply #'append (create-poem (mix keywords)(mix choices)))))

;;;;;
#|? (create-poem '(patience angel mercy life love)
                '(kepler hamlet-5 newton war-1 alice turing emerson))
((who said he did not know any greater good of geometry than the arts
                        which are necessary to life)
                       (shortens not . . . .|#
;;;;;

(defun CREATE-POEM (keywords mixed-choices)
  "Top-level function for Poet - actually creates the poem."
  (if (null mixed-choices)()
      (let ((test (assoc-list keywords (eval (first mixed-choices)))))
        (if test (cons test (create-poem (mix keywords) (rest mixed-choices)))
            (let ((reserve-test (choose-one (first mixed-choices))))
              (cons reserve-test (create-poem reserve-test (rest mixed-choices))))))))

;;;;;
#|? (assoc-list '(hello goodbye) '((goodbye)(hello there)))
(hello there)|#
;;;;;

(defun ASSOC-LIST (arrow target)
  "Runs a full search of its arrows in target."
  (if (null arrow)(choose-one target)
      (let ((test (full-search (first arrow) (mix target))))
        (if test test
            (assoc-list (rest arrow) target)))))
               
;;;;;
#|? (full-search 'hello '((goodbye)(hello there)))
(hello there)|#
;;;;;

(defun FULL-SEARCH (word lists)
  "Associates the word arg with the first encounter in its lists arg."
  (cond ((null lists)())
        ((assoc word (mapcar #'list (first lists)) :test #'equal)
         (first lists))
        (t (full-search word (Rest lists)))))
      
;;;;;
#|? (choose-one '(1 2 3 4))
   3|#
;;;;;

(defun CHOOSE-ONE (list)
  "Aritrarily chooses one of its list arg."
  (nth (random (length list) *rs*)
       list))

;;;;;
#|? (mix '(1 2 3 4))
   (1 3 4 2)|#
;;;;;

(defun MIX (list)
  "Arbitrarily mixes its arg."
  (let ((choice ()))
    (loop until (null list)
          do (setf choice (choose-one list))
          collect choice 
          do (setf list (remove choice list :count 1)))))

;;;;;
#|? (format-output (apply #'append (poet '(life love angel mercy patience) *choices*)))
and the moral of that
is oh tis love tis
love that makes the world
go round and if there|#
;;;;;

(defun FORMAT-OUTPUT (output)
  "Simple formating function to work on very large lists."
  (cond ((null output) t)
        ((< (length output) 5)(format-string output))
        (t (progn (format t "~A~A~A~A~A~A~A~A~A~&" (first output)" " (second output)" " 
                          (third output)" " (fourth output)" " (fifth output))
                  (format-output (nthcdr 5 output))))))
;;;;;
#|? (format-string '(hello there))
hello there |#
;;;;;

(defun FORMAT-STRING (list)
  "Simple formating function to work on lists."
    (if (null list) t
        (progn (format t "~A~A" (first list) " ")
               (format-string (rest list)))))

(defvar alice '(
  (Alice was beginning to get very tired)
 
( sitting by her sister
on the bank )
 
(  and of having nothing to do)
 
( once or twice she had
peeped into the book)
 
( her sister was reading)
 
( but it had no
pictures or conversations in it)
 
( and what is the use of a book)
 
( 
thought Alice)
 
( without pictures or conversation)
 
( 

  So she was considering in her own mind)
 
( as well as she could)
 
( 
for the hot day made her feel very sleepy)
 
(  whether
the pleasure of making a daisy-chain)
 
( would be worth the trouble)
  
( getting up and picking the daisies)
 
( suddenly a White
Rabbit with pink eyes )
 
( ran close by her)
 
( 

  nothing so VERY remarkable)
 
( nor did Alice
think it so VERY much)
 
( out of the way to hear the Rabbit say to
itself)
 
 
( I shall be late)
 
( when she thought
it over afterwards)
 
( it occurred to her that she ought to)
 
(  have
wondered at this)
 
( but at the time it all seemed)
 
(
but when the Rabbit actually )
 
( TOOK A WATCH OUT OF ITS WAISTCOAT-
POCKET)
 
( and looked at it)
 
( and then hurried on)
 
( Alice started to
her feet)
 
( it flashed across her mind)
 
(  that she had never
before seen a rabbit )
 
( with either a waistcoat-pocket)
 
(  or a watch to
take out of it)
 
( and burning with curiosity)
 
( she ran across the
field after it)
 
( and fortunately )
 
( just in time to see it pop)
 
( 
down a large rabbit-hole under the hedge)
 
( 

  In another moment )
 
( down went Alice after it)
 
( never once
considering )
 
( how in the world )
 
( she was to get out again)
 
( 

  The rabbit-hole went straight)
 
(  like a tunnel)
 
( and then dipped suddenly down)
 
( so suddenly that Alice )
 
( had not a
moment to think )
 
( about stopping herself )
 
( before she found herself)
 
( 
falling down a very deep well)
 
( 

  Either the well was very deep)
 
( or she fell very slowly)
 
( for she
had plenty of time )
 
( as she went down to look )
 
( 
wonder what was going to happen next)
 
( First she tried to look
down )
 
( and make out what she )
 
(  but it was too dark to
see anything)
 
( then she looked at the sides of the well)
 
( and
noticed that they were filled)
 
(  with cupboards and book-shelves)
 
( 
here and there she saw )
 
( maps and pictures hung upon pegs)
 
( She
took down a jar )
 
( from one of the shelves )
 
( as she passed)
 
( it was
labelled ORANGE MARMALADE)
 
(  but to her great disappointment )
 
( it
was empty)
 
( she did not like to drop the jar )
 
( for fear of killing
somebody)
 
( so managed to put it )
 
( into one of the cupboards )
 
( as she
fell past it)
 
( 

  )
 
( Well)
 
(  thought Alice to herself)
 
( after such a fall as this)
 
( I
shall think nothing of tumbling down stairs)
 
( How brave they will
all think me)
 
( wouldnt say anything about it)
 
( 
even if I fell off the top of the house)
 
( Which was very likely
true)
 
( 

  Down)
 
( Would the fall NEVER )
 
( come to an end)
 
( I
wonder how many miles)
 
(  I have fallen by this time)
 
( I must be getting somewhere )
 
( near the centre of the earth)
 
( Let
me see)
 
( that would be four thousand miles )
 
( for Alice had learnt several things )
 
( in her
lessons in the schoolroom)
 
( and though this )
 
( not a VERY good
opportunity)
 
(  for showing off her knowledge)
 
( as there was no one to
listen to her)
 
(  still it was good practice to say it over)
 
( yes
that is about the right distance)
 
( but then I wonder what Latitude
)
 
( Alice had no idea what Latitude )
 
 
( thought they were nice grand words to
say)
 
( 

  Presently she began again)
 
( I wonder if I shall fall right
THROUGH the earth)
 
( How funny it will seem )
 
( to come out among the
people that walk)
 
(  with their heads downward)
 
( The Antipathies)
 
(she was rather glad )
 
( there WAS no one listening)
 
( this
time)
 
(  as it didnt sound at all the right word)
 
( but I shall
have to ask them )
 
( what the name of the country is)
 
( 
Please is this New Zealand )
 
( and she tried
to curtsey as she spoke)
 
( fancy CURTSEYING as youre falling
through the air)
 
( Do you think you could manage it)
 
(   And what
an ignorant little girl)
 
(  she will think me for asking)
 
( No it will
never do to ask)
 
( perhaps I shall see it written up somewhere)
 
( 

  Down)
 
( there was nothing else to do)
 
( so Alice soon
began talking again)
 
( miss me very much to-night)
 
( I
should think)
 
( I hope they will remember
her saucer of milk)
 
( I wish you were)
 
( 
down here with me)
 
( There are no mice in the air)
 
( but
you might catch a bat)
 
( and that is very like a mouse)
 
( you know)
 
( 
But do cats eat bats)
 
( And here Alice began to get
rather sleepy)
 
(  went on saying to herself)
 
( in a dreamy sort of
way)
 
( and sometimes)
 
( you see)
 
( as she could not answer either
question)
 
( it did not much matter which way she put it)
 
( She felt
that she was dozing )
 
( and had just begun to dream )
 
( that she
was walking hand in hand )
 
( and saying to her very
earnestly)
 
( tell me the truth)
 
( did you ever eat a
bat)
 
( suddenly)
 
( down she came upon a heap of
sticks )
 
( and dry leaves)
 
( and the fall was over)
 
( 

  Alice was not a bit hurt)
 
( and she jumped up on to her feet )
 
( she looked up)
 
( but it was all dark overhead)
 
(  before her
was another long passage)
 
( the White Rabbit was still in
sight)
 
( there was not a moment to be lost)
 
( 
away went Alice like the wind)
 
( and was just in time to hear it
say)
 
( as it turned a corner)
 
( Oh my ears and whiskers)
 
( how late
its getting)
 
( She was close behind it )
 
( when she turned the
corner)
 
( but the Rabbit was no longer to be seen)
 
( she found
herself in a long low hall)
 
( which was lit up by a row of lamps)
 
( 
hanging from the roof)
 
( 

  There were doors all round the hall)
 
( but they were all locked)
 
( 
and when Alice had been all the way down one side)
 
( trying every door)
 
( she walked sadly down the middle)
 
( 
wondering how she was ever to get out again)
 
( 

  Suddenly she came upon a little three-legged table)
 
( all made of
solid glass)
 
( there was nothing on it )
 
( except a tiny golden key)
 
( 
Alices first thought )
 
( it might belong to one of the
doors of the hall)
 
( either the locks were too large)
 
(  or
the key was too small)
 
(  but at any rate it would not open any of
them)
 
( on the second time round)
 
( she came upon a low
curtain)
 
(  she had not noticed before)
 
( behind it was a little
door about fifteen inches high)
 
(  she tried the little golden key)
 
( 
and to her great delight it fitted)
 
( 

  Alice opened the door )
 
( and found that it led into a small
passage)
 
( not much larger than a rat-hole)
 
( she knelt down and
looked along the passage )
 
( into the loveliest garden you ever saw)
 
( 
How she longed to get out of that dark hall)
 
( and wander about
among those beds of bright flowers)
 
(  and those cool fountains)
 
(  but
she could not even get her head though the doorwa)
 
( and even if
my head would go through)
 
( it would be of
very little use without my shoulders)
 
( Oh how I wish
I could shut up like a telescope)
 
( I think I could)
 
( if I only
know how to begin)
 
( so many out-of-the-way things)
 
( 
had happened lately)
 
( that Alice had begun to think)
 
(  that very few
things indeed were really impossible)
 
( 

  There seemed to be no use in waiting by the little door)
 
(  so she
went back to the table)
 
( half hoping she might find another key on
it)
 
(  or at any rate a book of rules for shutting people up)
 
(  this time she found a little bottle on it)
 
( which
certainly was not here before)
 
( and round the neck
of the bottle was a paper label)
 
( DRINK ME)
 
( 
beautifully printed on it in large letters)
 
( 

  It was all very well to say)
 
( but the wise little
Alice was not going to do THAT in a hurry)
 
( for she had read several nice little histories about children )
 
( who
had got burnt)
 
( and eaten up by wild beasts)
 
(  and other unpleasant
things)
 
( all because they WOULD not remember the simple rules)
 
( 
their friends had taught them)
 
( a red-hot poker
will burn you)
 
(  if you hold it too long)
 
( and that if you cut your
finger VERY deeply with a knife)
 
(  it usually bleeds)
 
( and she had
never forgotten that)
 
( if you drink much from a bottle)
 
( it is almost certain to disagree with you)
 
( 
  However this bottle was NOT marked )
 
( so Alice ventured
to taste it)
 
( finding it very nice)
 
( a sort
of mixed flavour of cherry-tart custard pine-apple roast
turkey toffee and hot buttered toast)
 
(  she very soon finished
it off)
 
(   And so it was indeed)
 
( she was now only ten inches high)
 
( and
her face brightened up at the thought )
 
( that she was now the right
size for going though the little door into that lovely garden)
 
( 
she waited for a few minutes )
 
( to see if she was
going to shrink any further)
 
( she felt a little nervous about
this)
 
( for it might end)
 
( I wonder what I should be
like then)
 
( she tried to fancy what the flame of a candle)
 
( after the candle is blown out)
 
( for she could not remember)
 
( 
ever having seen such a thing)
 
( 

  After a while)
 
( finding that nothing more happened)
 
( she decided
on going into the garden)
 
( alas for poor Alice)
 
( when
she got to the door)
 
(  she found he had forgotten the little golden
key)
 
( and when she went back to the table for it)
 
( she found she
could not possibly reach it)
 
( she could see it quite plainly
through the glass)
 
( and she tried her best to climb up one of the
legs of the table)
 
(  but it was too slippery)
 
( and when she had
tired herself out with trying)
 
( the poor little thing sat down and
cried)
 
( no use in crying like that)
 
( I advise you to leave off this minute)
 
( She generally gave herself very good advice)
 
( she very
seldom followed it)
 
( sometimes she scolded herself so
severely as to bring tears into her eyes)
 
(  once she remembered
trying to box her own ears)
 
(  for having cheated herself in a game
of croquet )
 
( she was playing against herself)
 
( for this curious
child was very fond of pretending to be two people)

 
( to pretend to be two people)
 
( hardly enough of me left )
 
( to make ONE respectable
person)
 
( 

  Soon her eye fell on a little glass box )
 
( that was lying under
the table)
 
( she opened it)
 
( and found in it a very small cake)
 
( on
which the words EAT ME)
 
(  were beautifully marked in currants)
 
( if it makes me grow larger)
 
( 
I can reach the key)
 
( and if it makes me grow smaller)
 
(  I can creep
under the door)
 
(  so either way Ill get into the garden)
 
(  and I
do not care which happens)
 
( 

  She ate a little bit)
 
(  and said anxiously to herself)
 
( Which
way)
 
( holding her hand on the top of her head )
 
( to
feel which way it was growing)
 
(  and she was quite surprised to
find )
 
( that she remained the same size)
 
(  to be sure)
 
( this generally
happens when one eats cake)
 
( but Alice had got so much into the
way of expecting nothing )
 
( out-of-the-way things to happen)
 
( 
that it seemed quite dull and stupid )
 
( for life to go on in the
common way)
 
( 

  So she set to work)
 
(  and very soon finished off the cake)
 
( Curiouser and curiouser)
 
( she was so much
surprised)
 
( that for the moment she quite forgot how to speak )
 
( now I am opening out like the largest telescope that
ever was)
 
( for when she looked down at her
feet)
 
(  they seemed to be almost out of sight)
 
(  they were getting so
far off)
 
( Oh my poor little feet)
 
(  I wonder who will put on
your shoes and stockings for you now)
 
( I shall be a great deal too far off to trouble myself
about you)
 
( you must manage the best way you can)
 
( but I must be
kind to them)
 
( or perhaps they will not walk the
way I want to go)
 
( I will give them a new pair of
boots every Christmas)
 
( 
  And she went on planning to herself )
 
( how she would manage it)
 
 
( how funny it will
seem)
 
(  sending presents to ones own feet)
 
(  And how odd the
directions will look)
 
( 

Oh dear what nonsense I am talking )
 
( 

  Just then her head struck against the roof of the hall )
 
( in
fact she was now more than nine feet high )
 
(  and she at once took
up the little golden key  )
 
( and hurried off to the garden door )
 
( 

  Poor Alice )
 
( It was as much as she could do )
 
( lying down on one
side)
 
( to look through into the garden with one eye )
 
( but to get
through was more hopeless than ever )
 
( she sat down and began to
cry again )
 
( 

  You ought to be ashamed of yourself )
 
( a great
girl like you )
 
( she might well say this )
 
( to go on crying in
this way )
 
( Stop this moment I tell you )
 
( But she went on all
the same )
 
( shedding gallons of tears )
 
(  until there was a large pool
all round her )
 
( about four inches deep  )
 
( and reaching half down the
hall )
 
( 

  After a time she heard a little pattering of feet in the
distance )
 
(  and she hastily dried her eyes to see what was coming )
 
( 
It was the White Rabbit returning )
 
(  splendidly dressed with a
pair of white kid gloves in one hand  )
 
( and a large fan in the
other )
 
( he came trotting along in a great hurry )
 
( muttering to
himself as he came )
 
( the Duchess the Duchess Oh wont she
be savage if Ive kept her waiting )
 
( Alice felt so desperate
that she was ready to ask help of any one )
 
(  when the Rabbit
came near her )
 
(  she began in a low )
 
(  timid voice )
 
( If you please )
 
( The Rabbit started violently dropped the white kid
gloves and the fan )
 
(  and skurried away into the darkness as hard
as he could go )
 
( 

  Alice took up the fan and gloves )
 
(  as the hall was very
hot )
 
(  she kept fanning herself all the time she went on talking )

 
( How queer everything is to-day )
 
( And yesterday
things went on just as usual. )
 
( I wonder if I have been changed in
the night )
 
(  Let me think )
 
(  was I the same when I got up this
morning )
 
(  I almost think I can remember feeling a little
different )
 
( But if I am not the same )
 
(  the next question is )
 
( Who in
the world am I )
 
( THAT iS the great puzzle )
 
( And she began
thinking over all the children she knew t )
 
( hat were of the same age
as herself )
 
( to see if she could have been changed for any of
them )
 
( 

  I am sure I am not Ada )
 
( for her hair goes in such
long ringlets )
 
( and mine does not go in ringlets at all )
 
(  and I am
sure I cannot be Mabel )
 
( for I know all sorts of things )
 
( and she knows such a very little )
 
( Besides SHE iS she and I am I )
 
( 
and oh dear how puzzling it all is )
 
( I will try if I know all the
things I used to know )
 
(  Let me see )
 
(  four times five is twelve )
 
( 
and four times six is thirteen )
 
( and four times seven is )
 
( oh dear
I shall never get to twenty at that rate )
 
( However the
Multiplication Table does not signify )
 
(   let us try Geography )
 
( 
London is the capital of Paris )
 
(  and Paris is the capital of Rome )
 
( 
and Rome THAT iS all wrong )
 
( I must have been
changed for Mabel )
 
(
and she crossed her hands on her lap )
 
( as if she were saying lessons )
 
( 
and began to repeat it )
 
( but her voice sounded hoarse and
strange )
 
(  and the words did not come the same as they used to do )
 
( 

            How doth the little crocodile
              Improve his shining tail )
 
( 
            And pour the waters of the Nile
              On every golden scale )
 
( 

            How cheerfully he seems to grin )
 
( 
              How neatly spread his claws )
 
( 
            And welcome little fishes in
              With gently smiling jaws )
 
 
( and
her eyes filled with tears again as she went on )
 
( I must be Mabel
after all )
 
(  and I shall have to go and live in that poky little
house )
 
(  and have next to no toys to play with )
 
( ever so
many lessons to learn )
 
( No I have made up my mind about it )
 
(  if I am
Mabel I will stay down here )
 
( no use their putting their
heads down and saying  )
 
( Come up again dear )
 
( I shall only look
up and say Who am I then )
 
(  Tell me that first )
 
(  and then if I
like being that person )
 
( I will come up )
 
(  if not I will stay down
here till I am somebody else )
 
( with a
sudden burst of tears )
 
( I do wish they WOULD put their heads
down )
 
(  I am so VERY tired of being all alone here )
 
( 

  As she said this she looked down at her hands )
 
(  and was
surprised to see that she had put on one of the Rabbits little
white kid gloves while she was talking )
 
(  How CAN I have done
that )
 
( I must be growing small again )
 
(   She got up
and went to the table to measure herself by it )
 
( and found that
as nearly as she could guess )
 
( she was now about two feet high )
 
( 
and was going on shrinking rapidly )
 
( she soon found out that the
cause of this was the fan she was holding )
 
( and she dropped it
hastily )
 
( just in time to avoid shrinking away altogether )
 
( 

That WAS a narrow escape)
 
(a good deal frightened at
the sudden change)
 
( but very glad to find herself still in
existence)
 
(and now for the garden)
 
( and she ran with all speed
back to the little door)
 
(  but alas the little door was shut
again)
 
( and the little golden key was lying on the glass table as
before)
 
( and things are worse than ever thought the poor child)
 
(
for I never was so small as this before never)
 
(And I declare
its too bad that it is)
 
(

  As she said these words her foot slipped)
 
( and in another
moment splash)
 
( she was up to her chin in salt water)
 
(  He first
idea was that she had somehow fallen into the sea)
 
(and in that
case I can go back by railway)
 
( she said to herself)
 
(Alice had
been to the seaside once in her life)
 
( and had come to the general
conclusion that wherever you go to on the English coast )
 
(you find
a number of bathing machines in the sea)
 
( some children digging in
the sand with wooden spades)
 
( then a row of lodging houses and
behind them a railway station)
 
(  However she soon made out that
she was in the pool of tears)
 
( which she had wept when she was nine
feet high)
 
(

  I wish I hadnot cried so much)
 
( as she swam about
trying to find her way out)
 
(I shall be punished for it now)
 
(by being drowned in my own tears)
 
(  That WILL be a queer
thing to be sure)
 
(  However everything is queer to-day)
 
(

  Just then she heard something splashing about in the pool a
little way off)
 
( and she swam nearer to make out what it was)
 
(  at
first she thought it must be a walrus or hippopotamus)
 
( but then
she remembered how small she was now)
 
( and she soon made out that
it was only a mouse)
 
( that had slipped in like herself)
 
(

  Would it be of any use now)
 
(to speak to this
mouse)
 
(  Everything is so out-of-the-way down here)
 
( that I should
think very likely it can talk)
 
(  at any rate there is no harm in
trying.)
 
(O Mouse do you know the way out of
this pool)
 
(  I am very tired of swimming about here O Mouse)
 
(
Alice thought this must be the right way of speaking to a mouse)
 
(
she had never done such a thing before)
 
( but she remembered having
seen in her brothers Latin Grammar)
 
(A mouse--of a mouse--to a
mouse--a mouse--O mouse)
 
(  The Mouse looked at her rather
inquisitively)
 
( and seemed to her to wink with one of its little
eyes but it said nothing)
 
(

   Perhaps it doesn t understand English)
 
(I
daresay it s a French mouse  come over with William the
Conqueror)
 
(For  with all her knowledge of history  )
 
(Alice had
no very clear notion how long ago anything had happened )
 
(  So she
began again    Ou est ma chatte)
 
( which was the first sentence in
her French lesson-book   )
 
(The Mouse gave a sudden leap out of the
water  and seemed to quiver all over with fright    )
 
(Oh  I beg
your pardon  )
 
( cried Alice hastily  afraid that she had hurt the
poor animal s feelings   )
 
( I quite forgot you didn t like cats  

   Not like cats   cried the Mouse  in a shrill  passionate
voice    )
 
(Would YOU like cats if you were me)
 
(

   Well  perhaps not   said Alice in a soothing tone  )
 
(  don t be
angry about it  )
 
( And yet I wish I could show you our cat Dinah )
 
(
I think you d take a fancy to cats if you could only see her )
 
(
She is such a dear quiet thing   )
 
(Alice went on  half to herself 
as she swam lazily about in the pool   )
 
(and she sits purring so
nicely by the fire  )
 
(licking her paws and washing her face and
she is such a nice soft thing to nurse )
 
(and she s such a capital
one for catching mice oh )
 
( I beg your pardon  )
 
( cried Alice again 
for this time the Mouse was bristling all over  and she felt
certain it must be really offended  )
 
(  We won t talk about her any
more if you d rather not  )
 
(

   We indeed   cried the Mouse  who was trembling down to the end
of his tail    )
 
(As if I would talk on such a subject   )
 
(Our family
always HATED cats   )
 
(nasty  low  vulgar things  )
 
( Don t let me hear
the name again  )
 
(

   I won t indeed   said Alice  in a great hurry to change the
subject of conversation    )
 
(Are you are you fond of of dogs)
 
(
The Mouse did not answer  so Alice went on eagerly    )
 
(There is
such a nice little dog near our house I should like to show you 
A little bright-eyed terrier  you know  )
 
(with oh  such long curly
brown hair   )
 
(And it ll fetch things when you throw them  and
it ll sit up and beg for its dinner  )
 
(and all sorts of things I
can t remember half of them and it belongs to a farmer  you
know  )
 
(and he says it s so useful  it s worth a hundred pounds )
 
(
He says it kills all the rats and oh dear  )
 
( cried Alice in a
sorrowful tone   )
 
(I m afraid I ve offended it again   )
 
( For the
Mouse was swimming away from her as hard as it could go  )
 
(and
making quite a commotion in the pool as it went )
 
(

  So she called softly after it   )
 
(Mouse dear   Do come back
again  and we won t talk about cats or dogs either  if you don t
like them   )
 
( When the Mouse heard this  it turned round and swam
slowly back to her   )
 
(its face was quite pale with passion  Alice
thought)
 
(  and it said in a low trembling voice   )
 
(Let us get to
the shore  and then I ll tell you my history  )
 
(and you ll
understand why it is I hate cats and dogs  )
 
(

  It was high time to go  )
 
(for the pool was getting quite crowded
with the birds and animals that had fallen into it  )
 
( there were a
Duck and a Dodo  a Lory and an Eaglet  and several other curious
creatures   )
 
(Alice led the way  and the whole party swam to the
shore )
 
(


  They were indeed a queer-looking party that assembled on the
bank the birds with draggled feathers  )
 
(the animals with their
fur clinging close to them  )
 
(and all dripping wet  cross  and
uncomfortable )
 
(

  The first question of course was  how to get dry again  )
 
( they
had a consultation about this  and after a few minutes it seemed
quite natural to Alice)
 
( to find herself talking familiarly with
them )
 
( as if she had known them all her life   )
 
(Indeed  she had
quite a long argument with the Lory  )
 
(who at last turned sulky 
and would only say   )
 
(I am older than you  and must know better  
and this )
 
(Alice would not allow without knowing how old it was 
and  as the Lory positively refused to tell its age  )
 
(there was no
more to be said )
 
(

  At last the Mouse  )
 
(who seemed to be a person of authority among
them  called out  )
 
( Sit down  all of you  and listen to me  )
 
( I LL
soon make you dry enough    )
 
(They all sat down at once  in a large
ring  with the Mouse in the middle   )
 
()
 
(Alice kept her eyes
anxiously fixed on it  )
 
(for she felt sure she would catch a bad
cold if she did not get dry very soon )
 
(

   Ahem   said the Mouse with an important air   )
 
(are you all ready)
 
(
This is the driest thing I know  )
 
( Silence all round  if you please 
 William the Conqueror  )
 
(whose cause was favoured by the pope  was
soon submitted to by the English  )
 
(who wanted leaders  )
 
(and had been
of late much accustomed to usurpation and conquest   )
 
(Edwin and
Morcar  the earls of Mercia and Northumbria   )
 
(

   Ugh   said the Lory  with a shiver )
 
(

   I beg your pardon   said the Mouse  frowning  but very
politely    Did you speak)
 
(

   Not I   said the Lory hastily )
 
(

   I thought you did   said the Mouse    )
 
( I proceed   )
 
( Edwin and
Morcar  the earls of Mercia and Northumbria )
 
( declared for him 
and even Stigand )
 
( the patriotic archbishop of Canterbury  found
it advisable   )
 
(

   Found WHAT  said the Duck )
 
(

   Found IT   the Mouse replied rather crossly   )
 
( of course you
know what  it  means  )
 
(

   I know what  it  means well enough  )
 
(when I find a thing   said
the Duck  )
 
(  it s generally a frog or a worm  )
 
( The question is 
what did the archbishop find)
 
(

  The Mouse did not notice this question )
 
( but hurriedly went on 
   found it advisable to go with Edgar Atheling to meet William
and offer him the crown )
 
(  William s conduct at first was
moderate )
 
(  But the insolence of his Normans    )
 
(How are you
getting on now  my dear)
 
(  it continued  turning to Alice as it
spoke )
 
(

   As wet as ever   said Alice in a melancholy tone  )
 
( it doesn t
seem to dry me at all  )
 
(

   In that case   said the Dodo solemnly  rising to its feet  )
 
( I
move that the meeting adjourn  )
 
(for the immediate adoption of more
energetic remedies  )
 
(

   Speak English   said the Eaglet    )
 
(I don t know the meaning of
half those long words  and  what s more )
 
( I don t believe you do
either    )
 
(And the Eaglet bent down its head to hide a smile 
some of the other birds tittered audibly )
 
(

   What I was going to say   )
 
(said the Dodo in an offended tone 
 was  that the best thing to get us dry would be a Caucus-race  )
 
(

   What IS a Caucus-race)
 
( said Alice  not that she wanted much
to know  but the Dodo had paused )
 
(as if it thought that SOMEBODY
ought to speak )
 
( and no one else seemed inclined to say anything )
 
(

   Why   said the Dodo)
 
(   the best way to explain it is to do it  
)
 
(And  as you might like to try the thing yourself )
 
( some winter
day  I will tell you how the Dodo managed it )
 
(

  First it marked out a race-course  )
 
(in a sort of circle  )
 
( the
exact shape doesn t matter   it said )
 
(and then all the party
were placed along the course  here and there  )
 
( There was no  One 
two  three  and away  )
 
( but they began running when they liked 
and left off when they liked )
 
( so that it was not easy to know
when the race was over   )
 
(However  when they had been running half
an hour or so  and were quite dry again )
 
( the Dodo suddenly called
out  )
 
(The race is over   )
 
(and they all crowded round it  panting 
and asking  )
 
( But who has won)
 
(

  This question the Dodo could not answer without a great deal of
thought  )
 
(and it sat for a long time with one finger pressed upon
its forehead )
 
(the position in which you usually see Shakespeare 
in the pictures of him)
 
(  while the rest waited in silence)
 
(   At
last the Dodo said   )
 
(EVERYBODY has won  and all must have
prizes  )
 
(

   But who is to give the prizes)
 
(  quite a chorus of voices
asked )
 
(

   Why  SHE  of course  )
 
( said the Dodo  pointing to Alice with
one finger  )
 
(and the whole party at once crowded round her 
calling out in a confused way   )
 
(Prizes  Prizes  

  Alice had no idea what to do  and in despair she put her hand
in her pocket  )
 
(and pulled out a box of comfits  )
 
(luckily the salt
water had not got into it)
 
(  and handed them round as prizes 
There was exactly one a-piece all round )
 
(

   But she must have a prize herself  you know   )
 
(said the Mouse 

   Of course  )
 
( the Dodo replied very gravely    )
 
(What else have
you got in your pocket)
 
(  he went on  turning to Alice )
 
(

   Only a thimble   said Alice sadly )
 
(

   Hand it over here   said the Dodo )
 
(

  Then they all crowded round her once more  )
 
(while the Dodo
solemnly presented the thimble  saying )
 
( We beg your acceptance of
this elegant thimble  )
 
( and  when it had finished this short
speech  they all cheered )
 
(

  Alice thought the whole thing very absurd )
 
( but they all looked
so grave that she did not dare to laugh  )
 
(and  as she could not
think of anything to say  she simply bowed  )
 
(and took the thimble 
looking as solemn as she could )
 
(

  The next thing was to eat the comfits   )
 
(this caused some noise
and confusion  )
 
(as the large birds complained that they could not
taste theirs )
 
( and the small ones choked and had to be patted on
the back  )
 
( However  it was over at last  and they sat down again
in a ring  )
 
(and begged the Mouse to tell them something more )
 
(

   You promised to tell me your history  you know   )
 
(said Alice 
 and why it is you hate C and D   )
 
(she added in a whisper  half
afraid that it would be offended again )
 
(

   Mine is a long and a sad tale   )
 
(said the Mouse  turning to
Alice  and sighing )
 
(

   It IS a long tail  certainly   )
 
(said Alice  looking down with
wonder at the Mouse s tail   but why do you call it sad)
 
(   And
she kept on puzzling about it while the Mouse was speaking )
 
( so
that her idea of the tale was something like this  )
 
(

                     Fury said to a
                   mouse )
 
( That he
                 met in the
               house )
 
(
             Let us
              both go to
                law  )
 
( I will
                  prosecute
                    YOU    )
 
(Come 
                       I ll take no
                        denial  )
 
(We
                     must have a
                 trial  )
 
(For
              really this
           morning I ve
          nothing
         to do  )
 
(
           Said the
             mouse to the
               cur  )
 
( Such
                 a trial 
                   dear Sir )
 
(
                         With
                     no jury
                  or judge 
                would be
              wasting
             our
              breath  )
 
(
                I ll be
                 judge  I ll
                   be jury  )
 
(
                         Said
                    cunning
                      old Fury )
 
(
                      I ll
                      try the
                         whole
                          cause 
                             and
                        condemn
                       you
                      to
                       death   )
 
(


   You are not attending  )
 
( said the Mouse to Alice severely 
 What are you thinking of)
 
(

   I beg your pardon   )
 
(said Alice very humbly    )
 
(you had got to
the fifth bend  I think)
 
(

   I had NOT  )
 
( cried the Mouse  sharply and very angrily )
 
(

   A knot   said Alice  )
 
(always ready to make herself useful  and
looking anxiously about her   )
 
( Oh  do let me help to undo it  

   I shall do nothing of the sort   said the Mouse  )
 
(getting up
and walking away    )
 
(You insult me by talking such nonsense  

   I didn t mean it   pleaded poor Alice )
 
(   But you re so easily
offended  you know  )
 
(

  The Mouse only growled in reply 
)
 
(
   Please come back and finish your story  )
 
( Alice called after
it  and the others all joined in chorus   )
 
(Yes  please do   but
the Mouse only shook its head impatiently  and walked a little
quicker )
 
(

   What a pity it wouldn t stay   )
 
(sighed the Lory  as soon as it
was quite out of sight  )
 
(and an old Crab took the opportunity of
saying to her daughter  )
 
(Ah  my dear   Let this be a lesson to you
never to lose YOUR temper    )
 
( Hold your tongue  Ma   said the
young Crab  a little snappishly   )
 
( You re enough to try the
patience of an oyster  )
 
(

   I wish I had our Dinah here  I know I do  )
 
( said Alice aloud 
addressing nobody in particular   )
 
( She d soon fetch it back  

   And who is Dinah )
 
( if I might venture to ask the question 
said the Lory )
 
(

  Alice replied eagerly  for she was always ready to talk about
her pet )
 
(   Dinah s our cat   )
 
(And she s such a capital one for
catching mice you can t think   )
 
(And oh  I wish you could see her
after the birds   )
 
(Why  she ll eat a little bird as soon as look
at it  )
 
(

  This speech caused a remarkable sensation among the party )
 
(
Some of the birds hurried off at once   )
 
(one the old Magpie began
wrapping itself up very carefully  remarking   )
 
(I really must be
getting home  the night-air doesn t suit my throat  )
 
( and a Canary
called out in a trembling voice to its children  )
 
( Come away  my
dears   It s high time you were all in bed   )
 
( On various pretexts
they all moved off  and Alice was soon left alone )
 
(

   I wish I hadn t mentioned Dinah  )
 
( she said to herself in a
melancholy tone   )
 
( Nobody seems to like her  down here  and I m
sure she s the best cat in the world )
 
(  Oh  my dear Dinah  )
 
( I
wonder if I shall ever see you any more    )
 
(And here poor Alice
began to cry again)
 
(  for she felt very lonely and low-spirited 
In a little while  )
 
(however  she again heard a little pattering of
footsteps in the distance  )
 
(and she looked up eagerly  half hoping
that the Mouse had changed his mind  )
 
(and was coming back to
finish his story )
 
(

  It was the White Rabbit )
 
(trotting slowly back again )
 
( and
looking anxiously about as it went )
 
( as if it had lost something 
and she heard it muttering to itself  The Duchess  )
 
( The Duchess 
Oh my dear paws  )
 
( Oh my fur and whiskers  )
 
( She ll get me
executed  as sure as ferrets are ferrets   )
 
(Where CAN I have
dropped them  I wonder   )
 
(Alice guessed in a moment that it was
looking for the fan and the pair of white kid gloves  )
 
(and she
very good-naturedly began hunting about for them )
 
( but they were
nowhere to be seen everything seemed to have changed since her
swim in the pool )
 
( and the great hall )
 
( with the glass table and
the little door)
 
(  had vanished completely )
 
(

  Very soon the Rabbit noticed Alice )
 
( as she went hunting about 
and called out to her in an angry tone   )
 
(Why  Mary Ann  what ARE
you doing out here)
 
(  Run home this moment  and fetch me a pair of
gloves and a fan )
 
(  Quick  now  )
 
(  And Alice was so much frightened
that she ran off at once in the direction it pointed to )
 
( without
trying to explain the mistake it had made )
 
(

   He took me for his housemaid  )
 
( she said to herself as she ran 
 How surprised he ll be when he finds out who I am )
 
(  But I d
better take him his fan and gloves that is )
 
( if I can find them  
As she said this )
 
( she came upon a neat little house  on the door
of which was a bright brass plate with the name  )
 
(W  RABBIT 
engraved upon it  )
 
( She went in without knocking  and hurried
upstairs )
 
( in great fear lest she should meet the real Mary Ann 
and)
 
( be turned out of the house before she had found the fan and
gloves )
 
(

   How queer it seems  )
 
( Alice said to herself  )
 
( to be going
messages for a rabbit )
 
(  I suppose Dinah ll be sending me on
messages next  )
 
(  And she began fancying the sort of thing that
would happen    )
 

 
(  Come here directly  and get ready
for your walk )
 
(   Coming in a minute  nurse  )
 
( But I ve got to see
that the mouse doesn t get out   )
 
( Only I don t think )
 
(  Alice went
on  )
 
( that they d let Dinah stop in the house if it began ordering
people about like that  )
 
(

  By this time she had found her way into a tidy little room with
a table in the window )
 
( and on it as she had hoped)
 
( a fan and two
or three pairs of tiny white kid gloves  )
 
( she took up the fan and
a pair of the gloves  )
 
(and was just going to leave the room )
 
( when
her eye fell upon a little bottle that stood near the looking-
glass )
 
(  There was no label this time with the words  )
 
(DRINK ME  
but nevertheless she uncorked it and put it to her lips   )
 
( I know
SOMETHING interesting is sure to happen  )
 
( she said to herself 
 whenever I eat or drink anything )
 
( so I ll just see what this
bottle does )
 
(  I do hope it ll make me grow large again  )
 
(for
really I m quite tired of being such a tiny little thing  )
 
(

  It did so indeed  )
 
(and much sooner than she had expected 
before she had drunk half the bottle )
 
( she found her head pressing
against the ceiling  )
 
(and had to stoop to save her neck from being
broken )
 
(  She hastily put down the bottle  )
 
(saying to herself)
 
(
 That s quite enough I hope I shan t grow any more As it is  I
can t get out at the door I do wish I hadn t drunk quite so
much  )
 
(

  Alas  it was too late to wish that   )
 
(She went on growing )
 
( and
growing  and very soon had to kneel down on the floor )
 
(  in
another minute there was not even room for this )
 
( and she tried
the effect of lying down with one elbow against the door )
 
( and the
other arm curled round her head   )
 
(Still she went on growing  and 
as a last resource )
 
(she put one arm out of the window  and one
foot up the chimney  and said to herself  )
 
(Now I can do no more 
whatever happens  )
 
( What WILL become of me)
 
(

  Luckily for Alice  )
 
(the little magic bottle had now had its full
effect  and she grew no larger  )
 
( still it was very uncomfortable 
and  as there seemed to be no sort of chance of her ever getting
out of the room again  )
 
(no wonder she felt unhappy )
 
(

   It was much pleasanter at home   )
 
(thought poor Alice  )
 
( when one
wasn t always growing larger and smaller )
 
( and being ordered about
by mice and rabbits  )
 
( I almost wish I hadn t gone down that
rabbit-hole and yet and yet it s rather curious )
 
( you know 
this sort of life )
 
(  I do wonder what CAN have happened to me 
When I used to read fairy-tales  )
 
(I fancied that kind of thing
never happened )
 
( and now here I am in the middle of one  )
 
( There
ought to be a book written about me  that there ought  )
 
( And when
I grow up  I ll write one but I m grown up now  )
 
( she added in a
sorrowful tone )
 
(  at least there s no room to grow up any more
HERE  )
 
(

   But then   thought Alice  )
 
( shall I NEVER get any older than I
am now)
 
(  That ll be a comfort  one way never to be an old woman-
-but then always to have lessons to learn )
 
(  Oh  I shouldn t like
THAT  )
 
(

   Oh  you foolish Alice   )
 
(she answered herself   )
 
( How can you
learn lessons in here)
 
(  Why  there s hardly room for YOU )
 
( and no
room at all for any lesson-books  )
 
(

  And so she went on )
 
( taking first one side and then the other 
and making quite a conversation of it altogether )
 
( but after a few
minutes she heard a voice outside  )
 
(and stopped to listen )
 
(

   Mary Ann   Mary Ann   said the voice )
 
(   Fetch me my gloves
this moment    )
 
(Then came a little pattering of feet on the
stairs   )
 
(Alice knew it was the Rabbit coming to look for her )
 
( and
she trembled till she shook the house  )
 
(quite forgetting that she
was now about a thousand times as large as the Rabbit )
 
( and had no
reason to be afraid of it )
 
(

  Presently the Rabbit came up to the door  )
 
(and tried to open it 
but  as the door opened inwards )
 
( and Alice s elbow was pressed
hard against it )
 
( that attempt proved a failure  )
 
( Alice heard it
say to itself  )
 
(Then I ll go round and get in at the window  )
 
(

   THAT you won t  thought Alice )
 
( and  after waiting till she
fancied she heard the Rabbit just under the window  )
 
(she suddenly
spread out her hand )
 
( and made a snatch in the air  )
 
( She did not
get hold of anything  but she heard a little shriek and a fall 
and a crash of broken glass )
 
( from which she concluded that it was
just possible it had fallen into a cucumber-frame )
 
( or something
of the sort )
 
(

  Next came an angry voice the Rabbit s  Pat  Pat  )
 
( Where are
you)
 
(   And then a voice she had never heard before )
 
(  Sure then
I m here   )
 
(Digging for apples  yer honour  )
 
(

   Digging for apples  indeed  )
 
( said the Rabbit angrily   )
 
( Here 
Come and help me out of THIS   )
 
( Sounds of more broken glass )
 
(

   Now tell me  Pat  what s that in the window)
 
(

   Sure  it s an arm  yer honour  )
 
(

   An arm  you goose  )
 
(  Who ever saw one that size)
 
(  Why  it
fills the whole window  )
 
(

   Sure  it does  yer honour  )
 
( but it s an arm for all that  )
 
(

   Well  it s got no business there  )
 
(at any rate   go and take it
away  )
 
(

  There was a long silence after this )
 
(and Alice could only hear
whispers now and then  such as  )
 
( Sure  I don t like it  yer
honour  at all  at all     )
 
(Do as I tell you  you coward  )
 
( and at
last she spread out her hand again  )
 
(and made another snatch in
the air  )
 
( This time there were TWO little shrieks )
 
( and more
sounds of broken glass )
 
(   What a number of cucumber-frames there
must be   thought Alice  )
 
(  I wonder what they ll do next   )
 
(As for
pulling me out of the window )
 
( I only wish they COULD   )
 
(I m sure I
don t want to stay in here any longer  )
 
(

  She waited for some time without hearing anything more   )
 
(at
last came a rumbling of little cartwheels )
 
( and the sound of a
good many voice all talking together   )
 
(she made out the words 
 Where s the other ladder)
 
( Why  I hadn t to bring but one 
Bill s got the other Bill)
 
(  fetch it here  lad  Here  put  em up
at this corner)
 
( No  tie  em together first they don t reach half
high enough yet Oh  )
 
(they ll do well enough )
 
( don t be particular-
-Here  Bill  catch hold of this rope Will the roof bear)
 
( Mind
that loose slate Oh  it s coming down  )
 
( Heads below  a loud
crash)
 
( Now  who did that)
 
( It was Bill  I fancy Who s to go
down the chimney)
 

 
 
(Bill s to go down Here )
 
( Bill  the master says you re to
go down the chimney  )
 
(

   Oh  So Bill s got to come down the chimney  has he  said
Alice to herself   )
 
( Shy  they seem to put everything upon Bill 
I wouldn t be in Bill s place for a good deal  )
 
( this fireplace is
narrow  to be sure  but I THINK I can kick a little  )
 
(

  She drew her foot as far down the chimney as she could )
 
( and
waited till she heard a little animal )
 
(she couldn t guess of what
sort it was)
 
( scratching and scrambling about in the chimney close
above her  )
 
( then  saying to herself )
 
( This is Bill )
 
(  she gave one
sharp kick )
 
( and waited to see what would happen next )
 
(

  The first thing she heard was a general chorus of)
 
( There goes
Bill   then the Rabbit s voice along  )
 
(Catch him )
 
( you by the
hedge  )
 
( then silence  )
 
(and then another confusion of voices  )
 
(Hold
up his head Brandy now Don t choke him How was it  )
 
(old fellow
What happened to you)
 
(  Tell us all about it  )
 
(

  Last came a little feeble  squeaking voice )
 
( That s Bill  
thought Alice)
 
(  Well  I hardly know No more  )
 
(thank ye  I m
better now but I m a deal too flustered to tell you all I know)
 
(
is  something comes at me like a Jack-in-the-box  )
 
(and up I goes
like a sky-rocket  )
 
(

   So you did  old fellow   said the others )
 
(

   We must burn the house down   )
 
(said the Rabbit s voice  and
Alice called out as loud as she could  )
 
( If you do   I ll set
Dinah at you  )
 
(

  There was a dead silence instantly  )
 
(and Alice thought to
herself  )
 
( I wonder what they WILL do next  )
 
( If they had any
sense  they d take the roof off    )
 
(After a minute or two  )
 
(they
began moving about again  )
 
(and Alice heard the Rabbit say   )
 
(A
barrowful will do  to begin with  )
 
(

   A barrowful of WHAT)
 
(  thought Alice )
 
( but she had not long to
doubt )
 
( for the next moment a shower of little pebbles came
rattling in at the window)
 
(  and some of them hit her in the face 
 I ll put a stop to this  )
 
( she said to herself  and shouted out )
 
(
 You d better not do that again )
 
(  which produced another dead
silence )
 
(

  Alice noticed with some surprise that the pebbles were all
turning into little cakes as they lay on the floor )
 
( and a bright
idea came into her head   )
 
( If I eat one of these cakes  )
 
( she
thought )
 
(  it s sure to make SOME change in my size)
 
(  and as it
can t possibly make me larger )
 
( it must make me smaller  I
suppose  )
 
(

  So she swallowed one of the cakes  )
 
(and was delighted to find
that she began shrinking directly  )
 
( As soon as she was small
enough to get through the door  )
 
(she ran out of the house  and
found quite a crowd of little animals and birds waiting outside 
The poor little Lizard  )
 
(Bill  was in the middle  being held up by
two guinea-pigs  )
 
(who were giving it something out of a bottle )
 
(
They all made a rush at Alice the moment she appeared )
 
( but she
ran off as hard as she could  and soon found herself safe in a
thick wood )
 
(

   The first thing I ve got to do )
 
(  said Alice to herself  as she
wandered about in the wood  )
 
( is to grow to my right size again )
 
(
and the second thing is to find my way into that lovely garden )
 
(
I think that will be the best plan  )
 
(

  It sounded an excellent plan )
 
( no doubt  and very neatly and
simply arranged  the only difficulty was  )
 
(that she had not the
smallest idea how to set about it  )
 
(and while she was peering
about anxiously among the trees )
 
( a little sharp bark just over
her head made her look up in a great hurry )
 
(

  An enormous puppy was looking down at her with large round
eyes )
 
( and feebly stretching out one paw  )
 
(trying to touch her 
 Poor little thing  )
 
( said Alice  in a coaxing tone  )
 
(and she tried
hard to whistle to it )
 
( but she was terribly frightened all the
time at the thought that it might be hungry )
 
( in which case it
would be very likely to eat her up in spite of all her coaxing )
 
(

  Hardly knowing what she did )
 
( she picked up a little bit of
stick )
 
( and held it out to the puppy  whereupon the puppy jumped
into the air off all its feet at once )
 
( with a yelp of delight 
and rushed at the stick  )
 
(and made believe to worry it )
 
( then Alice
dodged behind a great thistle  )
 
(to keep herself from being run
over  )
 
(and the moment she appeared on the other side  )
 
(the puppy
made another rush at the stick )
 
( and tumbled head over heels in
its hurry to get hold of it )
 
( then Alice  thinking it was very
like having a game of play with a cart-horse )
 
( and expecting every
moment to be trampled under its feet )
 
( ran round the thistle
again )
 
( then the puppy began a series of short charges at the
stick  )
 
(running a very little way forwards each time and a long
way back  )
 
(and barking hoarsely all the while  )
 
(till at last it sat
down a good way off  panting  )
 
(with its tongue hanging out of its
mouth )
 
( and its great eyes half shut )
 
(

  This seemed to Alice a good opportunity )
 
(for making her escape 
so she set off at once )
 
( and ran till she was quite tired and out
of breath  )
 
(and till the puppy s bark sounded quite faint in the
distance )
 
(

   And yet what a dear little puppy it was  )
 
( said Alice  )
 
(as she
leant against a buttercup to rest herself  )
 
(and fanned herself
with one of the leaves   )
 
( I should have liked teaching it tricks
very much )
 
( if if I d only been the right size to do it  )
 
( Oh
dear )
 
(  I d nearly forgotten that I ve got to grow up again)
 
(   Let
me see how IS it to be managed)
 
( I suppose I ought to eat or
drink something or other )
 
( but the great question is  what)
 
(

  The great question certainly was  what)
 
(  Alice looked all round
her at the flowers and the blades of grass )
 
( but she did not see
anything that looked like the right thing to eat or drink )
 
(under
the circumstances   )
 
(There was a large mushroom growing near her 
about the same height as herself  )
 
(and when she had looked under
it  and on both sides of it )
 
(and behind it )
 
( it occurred to her
that she might as well look and see what was on the top of it )
 
(

  She stretched herself up on tiptoe  )
 
(and peeped over the edge of
the mushroom)
 
(  and her eyes immediately met those of a large
caterpillar  )
 
(that was sitting on the top with its arms folded )
 
(
quietly smoking a long hookah )
 
( and taking not the smallest notice
of her or of anything else )
 
(


  The Caterpillar and Alice looked at each other for some time in
silence  )
 
( at last the Caterpillar took the hookah out of its
mouth  )
 
(and addressed her in a languid  sleepy voice )
 
(

   Who are YOU said the Caterpillar )
 
(

  This was not an encouraging opening for a conversation )
 
(  Alice
replied  rather shyly )
 
(   I hardly know  sir  just at present 
at least I know who I WAS )
 
(when I got up this morning  but I think
I must have been changed several times since then  )
 
(

   What do you mean by that)
 
(  said the Caterpillar sternly 
 Explain yourself  )
 
(

   I can t explain MYSELF )
 
( I m afraid  sir  said Alice  )
 
( because
I m not myself  you see  )
 
(

   I don t see   said the Caterpillar )
 
(

   I m afraid I can t put it more clearly  )
 
( Alice replied very
politely  )
 
( for I can t understand it myself to begin with  )
 
(and
being so many different sizes in a day is very confusing  )
 
(

   It isn t   said the Caterpillar )
 
(

   Well  perhaps you haven t found it so yet   )
 
(said Alice   but
when you have to turn into a chrysalis you will some day  you
know and then after that into a butterfly )
 
( I should think you ll
feel it a little queer  won t you)
 
(

   Not a bit   said the Caterpillar )
 
(

   Well  perhaps your feelings may be different  )
 
( said Alice 
 all I know is  it would feel very queer to ME  )
 
(

   You   said the Caterpillar contemptuously  )
 
(  Who are YOU)
 
( 

  Which brought them back again to the beginning of the
conversation   )
 
(Alice felt a little irritated at the Caterpillar s
making such VERY short remarks  and she drew herself up and said 
very gravely  )
 
( I think  you ought to tell me who YOU are  first  

   Why  said the Caterpillar )
 
(

  Here was another puzzling question  )
 
(and as Alice could not
think of any good reason  )
 
(and as the Caterpillar seemed to be in
a VERY unpleasant state of mind )
 
( she turned away )
 
(

   Come back  )
 
( the Caterpillar called after her )
 
(   I ve something
important to say  )
 
(

  This sounded promising  certainly  )
 
( Alice turned and came back
again )
 
(

   Keep your temper   said the Caterpillar )
 
(

   Is that all  said Alice  )
 
(swallowing down her anger as well as
she could )
 
(

   No   said the Caterpillar )
 
(

  Alice thought she might as well wait  )
 
(as she had nothing else
to do )
 
( and perhaps after all it might tell her something worth
hearing  )
 
( For some minutes it puffed away without speaking  )
 
(but
at last it unfolded its arms  took the hookah out of its mouth
again  and said  )
 
( So you think you re changed  do you)
 
(

   I m afraid I am  sir   said Alice )
 
(  I can t remember things as
I used and I don t keep the same size for ten minutes together  )
 
(

   Can t remember WHAT things)
 
(  said the Caterpillar )
 
(

   Well  I ve tried to say  )
 
(HOW DOTH THE LITTLE BUSY BEE   )
 
(but it
all came different   )
 
(Alice replied in a very melancholy voice )
 
(

   Repeat   YOU ARE OLD  FATHER WILLIAM )
 
(   said the Caterpillar 

  Alice folded her hands  and began  )
 
(

     You are old  Father William )
 
(  the young man said )
 
(
       And your hair has become very white )
 
(
    And yet you incessantly stand on your head )
 
(
      Do you think  at your age  it is right)
 
(

     In my youth   Father William replied to his son )
 
(
       I feared it might injure the brain )
 
(
    But  now that I m perfectly sure I have none )
 
(
      Why  I do it again and again  )
 
(

     You are old   said the youth   as I mentioned before )
 
(
      And have grown most uncommonly fat )
 
(
    Yet you turned a back-somersault in at the door )
 
(
      Pray  what is the reason of that)
 

 
(
     In my youth   said the sage  as he shook his grey locks 
       I kept all my limbs very supple)
 
(
    By the use of this ointment one shilling the box )
 
(
      Allow me to sell you a couple)
 
(

     You are old   said the youth )
 
(  and your jaws are too weak)
 
(
      For anything tougher than suet )
 
(
    Yet you finished the goose )
 
( with the bones and the beak )
 
(
      Pray how did you manage to do it)
 
(

     In my youth   said his father )
 
(  I took to the law )
 
(
      And argued each case with my wife )
 
(
    And the muscular strength  which it gave to my jaw )
 
(
      Has lasted the rest of my life  )
 
(

     You are old   said the youth  )
 
( one would hardly suppose)
 
(
      That your eye was as steady as ever )
 
(
    Yet you balanced an eel on the end of your nose )
 
(
      What made you so awfully clever)
 
(

     I have answered three questions  and that is enough  )
 
(
      Said his father   don t give yourself airs )
 
(
    Do you think I can listen all day to such stuff)
 
(
      Be off  or I ll kick you down stairs  )
 
(


   That is not said right   said the Caterpillar )
 
(

   Not QUITE right  I m afraid   )
 
(said Alice  timidly   some of the
words have got altered  )
 
(

   It is wrong from beginning to end  )
 
( said the Caterpillar
decidedly  )
 
(and there was silence for some minutes )
 
(

  The Caterpillar was the first to speak )
 
(

   What size do you want to be  it asked )
 
(

   Oh  I m not particular as to size   )
 
(Alice hastily replied 
 only one doesn t like changing so often  you know  )
 
(

   I DON T know   said the Caterpillar )
 
(

  Alice said nothing   )
 
(she had never been so much contradicted in
her life before  )
 
(and she felt that she was losing her temper )
 
(

   Are you content now  said the Caterpillar )
 
(

   Well  I should like to be a LITTLE larger  sir  )
 
(if you
wouldn t mind   said Alice  )
 
(  three inches is such a wretched
height to be  

   It is a very good height indeed  )
 
( said the Caterpillar
angrily  rearing itself upright as it spoke )
 
(it was exactly three
inches high)
 
(

   But I m not used to it   )
 
(pleaded poor Alice in a piteous tone 
And she thought of herself  )
 
( I wish the creatures wouldn t be so
easily offended  )
 
(

   You ll get used to it in time  )
 
( said the Caterpillar  and it
put the hookah into its mouth and began smoking again )
 
(

  This time Alice waited patiently until it chose to speak again )
 
(
In a minute or two the Caterpillar took the hookah out of its)
 
(
mouth and yawned once or twice  and shook itself )
 
(  Then it got
down off the mushroom  )
 
(and crawled away in the grass )
 
( merely
remarking as it went  )
 
(One side will make you grow taller)
 
(  and
the other side will make you grow shorter  )
 
(

   One side of WHAT)
 
(  The other side of WHAT)
 
(  thought Alice to
herself )
 
(

   Of the mushroom  )
 
( said the Caterpillar  )
 
(just as if she had
asked it aloud  and in another moment it was out of sight )
 
(

  Alice remained looking thoughtfully at the mushroom for a
minute )
 
( trying to make out which )
 
(were the two sides of it  )
 
(and as
it was perfectly round  )
 
(she found this a very difficult question 
However  )
 
(at last she stretched her arms round it as far as they
would go  )
 
(and broke off a bit of the edge with each hand )
 
(

   And now which is which)
 
(  she said to herself )
 
( and nibbled a
little of the right-hand bit to try the effect  )
 
( the next moment
she felt a violent blow underneath her chin   )
 
(it had struck her
foot )
 
(

  She was a good deal frightened by this very sudden change  )
 
(but
she felt that there was no time to be lost  )
 
(as she was shrinking
rapidly  )
 
(so she set to work at once to eat some of the other bit )
 
(
Her chin was pressed so closely against her foot )
 
( that there was
hardly room to open her mouth )
 
( but she did it at last  )
 
(and
managed to swallow a morsel of the lefthand bit )
 
(

   Come  my head s free at last   )
 
(said Alice in a tone of
delight  )
 
(which changed into alarm in another moment )
 
(when she
found that her shoulders were nowhere to be found  )
 
( all she could
see  when she looked down )
 
( was an immense length of neck )
 
( which
seemed to rise like a stalk out of a sea of green leaves that lay
far below her )
 
(

   What CAN all that green stuff be  said Alice )
 
(   And where
HAVE my shoulders got to)
 
(  And oh  my poor hands  how is it I
can t see you)
 
(  She was moving them about as she spoke  )
 
(but no
result seemed to follow )
 
( except a little shaking among the
distant green leaves )
 
(

  As there seemed to be no chance of getting her hands up to her
head  )
 
(she tried to get her head down to them )
 
( and was delighted
to find that her neck )
 
(would bend about easily in any direction 
like a serpent  )
 
( She had just succeeded in curving it down into a
graceful zigzag )
 
( and was going to dive in among the leaves )
 
( which
she found to be nothing but the tops of the trees )
 
(under which she
had been wandering  )
 
(when a sharp hiss made her draw back in a
hurry  )
 
( a large pigeon had flown into her face )
 
( and was beating
her violently with its wings )
 
(

   Serpent   screamed the Pigeon )
 
(

   I m NOT a serpent   )
 
(said Alice indignantly   )
 
( Let me alone  )
 
(

   Serpent  I say again   )
 
(repeated the Pigeon  but in a more
subdued tone )
 
( and added with a kind of sob )
 
(  I ve tried every
way  and nothing seems to suit them  )
 
(

   I haven t the least idea )
 
(what you re talking about   said
Alice )
 
(

   I ve tried the roots of trees  )
 
(and I ve tried banks  and I ve
tried hedges   )
 
(the Pigeon went on  without attending to her   but
those serpents   )
 
(There s no pleasing them  

  Alice was more and more puzzled  )
 
(but she thought there was no
use in saying anything more till the Pigeon had finished )
 
(

   As if it wasn t trouble enough hatching the eggs )
 
(  said the
Pigeon  )
 
( but I must be on the look-out for serpents night and
day   )
 
(Why  I haven t had a wink of sleep these three weeks  )
 
(

   I m very sorry you ve been annoyed  )
 
( said Alice  who was
beginning to see its meaning )
 
(

   And just as I d taken the highest tree in the wood )
 
(  continued
the Pigeon  raising its voice to a shriek  )
 
( and just as I was
thinking I should be free of them at last  )
 
(they must needs come
wriggling down from the sky  )
 
( Ugh  Serpent  )
 
(

   But I m NOT a serpent  I tell you  )
 

 
(

   Well   WHAT are you)
 
(  said the Pigeon)
 
(    I can see you re
trying to invent something  )
 
(

   I I m a little girl   said Alice)
 
( rather doubtfully  as she
remembered the number of changes she had gone through that day )
 
(

   A likely story indeed   said the Pigeon)
 
( in a tone of the
deepest contempt   )
 
( I ve seen a good many little girls in my
time )
 
( but never ONE with such a neck as that  )
 
( No  no   You re a
serpent )
 
( and there s no use denying it  )
 
( I suppose you ll be
telling me next that you never tasted an egg  )
 
(

   I HAVE tasted eggs  )
 
(certainly   said Alice  who was a very
truthful child  )
 
( but little girls eat eggs quite as much as
serpents do  you know  )
 
(

   I don t believe it   said the Pigeon   )
 
(but if they do  why
then they re a kind of serpent  that s all I can say  )
 
(

  This was such a new idea to Alice )
 
( that she was quite silent
for a minute or two  which gave the Pigeon the opportunity of
adding  )
 
( You re looking for eggs  I know THAT well enough  )
 
(and
what does it matter to me )
 
(whether you re a little girl or a
serpent)
 
(

   It matters a good deal to ME   )
 
(said Alice hastily   )
 
(but I m
not looking for eggs  )
 
(as it happens  and if I was  I shouldn t
want YOURS  )
 
( I don t like them raw  )
 
(

   Well  be off  then   )
 
(said the Pigeon in a sulky tone  as it
settled down again into its nest  )
 
( Alice crouched down among the
trees as well as she could  for her neck kept getting entangled
among the branches  )
 
(and every now and then she had to stop and
untwist it  )
 
( After a while she remembered that she still held the
pieces of mushroom in her hands )
 
( and she set to work very
carefully  )
 
(nibbling first at one and then at the other )
 
( and
growing sometimes taller and sometimes shorter )
 
( until she had
succeeded in bringing herself down to her usual height )
 
(

  It was so long since she had been anything near the right size )
 
(
that it felt quite strange at first  but she got used to it in a
few minutes  )
 
(and began talking to herself  as usual   )
 
( Come 
there s half my plan done now  )
 
( How puzzling all these changes
are  )
 
( I m never sure what I m going to be  )
 
(from one minute to
another  )
 
( However  I ve got back to my right size  )
 
( the next
thing is  )
 
(to get into that beautiful garden how IS that to be
done  I wonder)
 
(   As she said this  she came suddenly upon an
open place)
 
(  with a little house in it about four feet high 
 Whoever lives there   thought Alice )
 
(  it ll never do to come
upon them THIS size )
 
(  why  I should frighten them out of their
wits  )
 
(  So she began nibbling at the righthand bit again  )
 
(and did
not venture to go near the house till she had brought herself
down to nine inches high )
 
(



  For a minute or two she stood looking at the house  )
 
(and
wondering what to do next )
 
( when suddenly a footman in livery came
running out of the wood )
 
(she considered him to be a footman
because he was in livery   )
 
(otherwise  judging by his face only 
she would have called him a fish)
 
( and rapped loudly at the door
with his knuckles  )
 
( It was opened by another footman in livery 
with a round face  and large eyes like a frog )
 
( and both footmen 
Alice noticed  )
 
(had powdered hair that curled all over their
heads  )
 
( She felt very curious to know what it was all about  )
 
(and
crept a little way out of the wood to listen )
 
(

  The Fish-Footman began by producing from under his arm a great
letter  nearly as large as himself  )
 
(and this he handed over to
the other )
 
( saying  in a solemn tone   )
 
(For the Duchess   )
 
(An
invitation from the Queen to play croquet  )
 
(  The Frog-Footman
repeated )
 
( in the same solemn tone  only changing the order of the
words a little )
 
(  From the Queen  )
 
( An invitation for the Duchess
to play croquet  )
 
(

  Then they both bowed low  )
 
(and their curls got entangled
together )
 
(

  Alice laughed so much at this  )
 
(that she had to run back into
the wood for fear of their hearing her)
 
(  and when she next peeped
out the Fish-Footman was gone  )
 
(and the other was sitting on the
ground near the door )
 
( staring stupidly up into the sky )
 
(

  Alice went timidly up to the door  )
 
(and knocked )
 
(

   There s no sort of use in knocking )
 
(  said the Footman   and
that for two reasons   )
 
(First  because I m on the same side of the
door as you are  )
 
(secondly  because they re making such a noise
inside  )
 
(no one could possibly hear you   )
 
( And certainly there was
a most extraordinary noise going on )
 
(within a constant howling
and sneezing  )
 
(and every now and then a great crash  )
 
(as if a dish
or kettle had been broken to pieces )
 
(

   Please  then   said Alice   how am I to get in)
 
(

   There might be some sense in your knocking   )
 
(the Footman went
on without attending to her   )
 
(if we had the door between us   )
 
(For
instance )
 
( if you were INSIDE  you might knock )
 
( and I could let
you out  you know    )
 
(He was looking up into the sky all the time
he was speaking  and this Alice thought decidedly uncivil    )
 
(But
perhaps he can t help it  )
 
( she said to herself  )
 
( his eyes are so
VERY nearly at the top of his head )
 
(  But at any rate he might
answer questions  How am I to get in)
 
(  she repeated  aloud )
 
(

   I shall sit here   )
 
(the Footman remarked   )
 
(till tomorrow  )
 
(

  At this moment the door of the house opened  )
 
(and a large plate
came skimming out  straight at the Footman s head  )
 
( it just
grazed his nose  )
 
(and broke to pieces against one of the trees
behind him )
 
(

    or next day  maybe  )
 
( the Footman continued in the same tone 
exactly as if nothing had happened )
 
(

   How am I to get in)
 
(  asked Alice again)
 
(  in a louder tone )
 
(

   ARE you to get in at all)
 
(  said the Footman   )
 
( That s the
first question )
 
( you know  )
 
(

  It was  no doubt  )
 
( only Alice did not like to be told so )
 
(
 It s really dreadful  )
 
( she muttered to herself  )
 
( the way all the
creatures argue  )
 
( It s enough to drive one crazy  )
 
(

  The Footman seemed to think this a good opportunity for
repeating his remark  with variations   )
 
( I shall sit here   he
said   on and off  for days and days  )
 
(

   But what am I to do  said Alice )
 
(

   Anything you like )
 
(  said the Footman  and began whistling )
 
(

   Oh  there s no use in talking to him )
 
(  said Alice desperately 
 he s perfectly idiotic   )
 
( And she opened the door and went in )
 
(

  The door led right into a large kitchen  )
 
(which was full of
smoke from one end to the other  )
 
(the Duchess was sitting on a
three-legged stool in the middle )
 
( nursing a baby  )
 
(the cook was
leaning over the fire )
 
( stirring a large cauldron which seemed to
be full of soup )
 
(

   There s certainly too much pepper in that soup  )
 
( Alice said to
herself  as well as she could for sneezing )
 
(

  There was certainly too much of it in the air  )
 
( Even the
Duchess sneezed occasionally )
 
( and as for the baby  )
 
(it was
sneezing and howling alternately without a moment s pause  )
 
( The
only things in the kitchen that did not sneeze  )
 
(were the cook 
and a large cat which was sitting on the hearth and grinning from
ear to ear )
 
(

   Please would you tell me )
 
(  said Alice  a little timidly  for
she was not quite sure whether it was good manners for her to
speak first )
 
(  why your cat grins like that)
 
(

   It s a Cheshire cat)
 
(   said the Duchess  )
 
( and that s why 
Pig  )
 
(

  She said the last word with such sudden violence that Alice
quite jumped )
 
( but she saw in another moment that it was addressed
to the baby  )
 
(and not to her  so she took courage  )
 
(and went on
again  )
 
(

   I didn t know that Cheshire cats always grinned  )
 
(in fact  I
didn t know that cats COULD grin  )
 
(

   They all can   said the Duchess  )
 
( and most of  em do  )
 
(

   I don t know of any that do  )
 
( Alice said very politely 
feeling quite pleased to have got into a conversation )
 
(

   You don t know much  )
 
( said the Duchess  )
 
( and that s a fact  )
 
(

  Alice did not at all like the tone of this remark  )
 
(and thought
it would be as well to introduce some other subject of
conversation   )
 
(While she was trying to fix on one )
 
( the cook took
the cauldron of soup off the fire  )
 
(and at once set to work
throwing everything within her reach)
 
(at the Duchess and the baby
 the fire-irons came first  )
 
(then followed a shower of saucepans 
plates  and dishes  )
 
( The Duchess took no notice of them even when
they hit her  )
 
(and the baby was howling so much already  )
 
(that it
was quite impossible to say whether the blows hurt it or not )
 
(

   Oh  PLEASE mind what you re doing   cried Alice )
 
( jumping up
and down in an agony of terror   )
 
( Oh  there goes his PRECIOUS
nose  )
 
( as an unusually large saucepan flew close by it )
 
( and very
nearly carried it off )
 
(

   If everybody minded their own business   )
 
(the Duchess said in a
hoarse growl   )
 
(the world would go round a deal faster than it
does  )
 
(

   Which would NOT be an advantage  )
 
( said Alice  who felt very
glad to get an opportunity of showing off a little of her
knowledge  )
 
(  Just think of what work it would make with the day
and night   )
 
(You see the earth takes twenty-four hours to turn
round on its axis  )
 
(

   Talking of axes   said the Duchess )
 
(  chop off her head  )
 
(

  Alice glanced rather anxiously at the cook  to see if she meant
to take the hint  )
 
(but the cook was busily stirring the soup  and
seemed not to be listening  so she went on again    )
 
(Twenty-four
hours  I THINK  or is it twelve)
 
(

   Oh  don t bother ME   said the Duchess )
 
(  I never could abide
figures   )
 
( And with that she began nursing her child again )
 
(
singing a sort of lullaby to it as she did so )
 
( and giving it a
violent shake at the end of every line )
 
(

         Speak roughly to your little boy )
 
(
          And beat him when he sneezes )
 
(
        He only does it to annoy )
 
(
          Because he knows it teases  )
 
(
  While the Duchess sang the second verse of the song  )
 
(she kept
tossing the baby violently up and down  )
 
(and the poor little thing
howled so )
 
( that Alice could hardly hear the words  )
 
(

         I speak severely to my boy )
 
(
          I beat him when he sneezes )
 
(
        For he can thoroughly enjoy)
 
(
          The pepper when he pleases  )
 
(

   Here  you may nurse it a bit  if you like  )
 
( the Duchess said
to Alice )
 
( flinging the baby at her as she spoke   )
 
( I must go and
get ready to play croquet with the Queen  )
 
( and she hurried out of
the room   )
 
(The cook threw a frying-pan after her as she went out 
but it just missed her )
 
(

  Alice caught the baby with some difficulty  )
 
(as it was a queer-
shaped little creature  )
 
(and held out its arms and legs in all
directions  )
 
( just like a star-fish   thought Alice )
 
(  The poor
little thing was snorting like a steam-engine when she caught it )
 
(
and kept doubling itself up and straightening itself out again 
so that altogether  )
 
(for the first minute or two )
 
( it was as much
as she could do to hold it )
 
(

  As soon as she had made out the proper way of nursing it 
)
 
(which was to twist it up into a sort of knot  )
 
(and then keep
tight hold of its right ear )
 
(and left foot  )
 
(so as to prevent its
undoing itself )
 
(she carried it out into the open air  )
 
(  IF I
don t take this child away with me   thought Alice )
 
(  they re sure
to kill it in a day or two   )
 
(wouldn t it be murder to leave it
behind)
 
(   She said the last words out loud  )
 
(and the little thing
grunted in reply )
 
(it had left off sneezing by this time)
 
(    Don t
grunt   said Alice  )
 
( that s not at all a proper way of expressing
yourself  )
 
(

  The baby grunted again  )
 
(and Alice looked very anxiously into
its face to see what was the matter with it  )
 
( There could be no
doubt that it had a VERY turn-up nose )
 
( much more like a snout
than a real nose)
 
(  also its eyes were getting extremely small for
a baby   )
 
(altogether Alice did not like the look of the thing at
all    )
 
(But perhaps it was only sobbing   )
 
(she thought  and looked
into its eyes again  )
 
(to see if there were any tears )
 
(

  No  there were no tears   )
 
( If you re going to turn into a pig 
my dear )
 
(  said Alice  seriously )
 
(  I ll have nothing more to do
with you  )
 
( Mind now    )
 
(The poor little thing sobbed again )
 
(or
grunted  it was impossible to say which)
 
( and they went on for
some while in silence )
 
(

  Alice was just beginning to think to herself   )
 
(Now  what am I
to do with this creature when I get it home)
 
(  when it grunted
again  so violently  )
 
(that she looked down into its face in some
alarm  )
 
( This time there could be NO mistake about it )
 
(  it was
neither more nor less than a pig )
 
( and she felt that it would be
quite absurd for her to carry it further )
 
(

  So she set the little creature down )
 
( and felt quite relieved to
see it trot away quietly into the wood  )
 
(  If it had grown up  
she said to herself  )
 
( it would have made a dreadfully ugly child 
but it makes rather a handsome pig  I think  )
 
(  And she began
thinking over other children she knew  )
 
(who might do very well as
pigs  and was just saying to herself  )
 
( if one only knew the right
way to change them  )
 
( when she was a little startled by seeing
the Cheshire Cat sitting on a bough of a tree a few yards off )
 
(

  The Cat only grinned when it saw Alice  )
 
( It looked good-
natured  she thought   )
 
(still it had VERY long claws and a great
many teeth )
 
( so she felt that it ought to be treated with respect )
 
(

   Cheshire Puss )
 
(  she began  rather timidly )
 
( as she did not at
all know whether it would like the name )
 
(  however  it only
grinned a little wider  )
 
(  Come  it s pleased so far )
 
(  thought
Alice  and she went on  )
 
(  Would you tell me  please )
 
( which way I
ought to go from here)
 
(

   That depends a good deal on where you want to get to   said
the Cat )
 
(

   I don t much care where   said Alice )
 
(

   Then it doesn t matter which way you go   said the Cat )
 
(

    so long as I get SOMEWHERE  )
 
( Alice added as an explanation )
 
(

   Oh  you re sure to do that   said the Cat )
 
(  if you only walk
long enough  )
 
(

  Alice felt that this could not be denied)
 
(  so she tried another
question   )
 
( What sort of people live about here)
 
(

   In THAT direction   the Cat said)
 
(  waving its right paw round 
 lives a Hatter  )
 
( and in THAT direction   )
 
(waving the other paw 
 lives a March Hare  )
 
( Visit either you like )
 
(  they re both mad  )
 
(

   But I don t want to go among mad people   Alice remarked )
 
(

   Oh  you can t help that   said the Cat  )
 
(  we re all mad here 
I m mad )
 
(  You re mad  )
 
(

   How do you know I m mad  said Alice )
 
(

   You must be   said the Cat )
 
(  or you wouldn t have come here  )
 
(

  Alice didn t think that proved it at all )
 
(however  she went on
 And how do you know that you re mad)
 
(

   To begin with   said the Cat  )
 
( a dog s not mad   )
 
(You grant
that)
 
(

   I suppose so   said Alice )
 
(

   Well  then   the Cat went on  )
 
( you see  a dog growls when it s
angry )
 
( and wags its tail when it s pleased   )
 
(Now I growl when I m
pleased  )
 
(and wag my tail when I m angry   )
 
(Therefore I m mad  )
 
(

   I call it purring  not growling   said Alice )
 
(

   Call it what you like   said the Cat   )
 
( Do you play croquet
with the Queen to-day)
 
(

   I should like it very much   said Alice  )
 
( but I haven t been
invited yet  )
 
(

   You ll see me there   said the Cat  and vanished )
 
(

  Alice was not much surprised at this  )
 
(she was getting so used
to queer things happening )
 
(  While she was looking at the place
where it had been )
 
( it suddenly appeared again )
 
(

   By-the-bye  what became of the baby  said the Cat )
 
(   I d
nearly forgotten to ask  )
 
(

   It turned into a pig  )
 
( Alice quietly said  just as if it had
come back in a natural way )
 
(

   I thought it would   said the Cat  and vanished again )
 
(

  Alice waited a little  half expecting to see it again )
 
( but it
did not appear )
 
( and after a minute or two she walked on in the
direction in which the March Hare was said to live )
 
(   I ve seen
hatters before   she said to herself   )
 
(the March Hare will be
much the most interesting )
 
( and perhaps as this is May it won t be
raving mad at least not so mad as it was in March)
 
(    As she said
this  she looked up )
 
( and there was the Cat again  sitting on a
branch of a tree )
 
(

   Did you say pig  or fig  said the Cat )
 
(

   I said pig   replied Alice   )
 
(and I wish you wouldn t keep
appearing and vanishing so suddenly   you make one quite giddy  )
 
(

   All right   said the Cat )
 
( and this time it vanished quite
slowly  beginning with the end of the tail  )
 
(and ending with the
grin  which remained some time after the rest of it had gone )
 
(

   Well   I ve often seen a cat without a grin )
 
(  thought Alice 
 but a grin without a cat )
 
(  It s the most curious thing I ever
say in my life  )
 
(

  She had not gone much farther before she came in sight of the
house of the March Hare  )
 
( she thought it must be the right house 
because the chimneys were shaped like ears and the roof was
thatched with fur  )
 
( It was so large a house  that she did not
like to go nearer till she had nibbled some more of the lefthand
bit of mushroom  )
 
(and raised herself to about two feet high  )
 
( even
then she walked up towards it rather timidly  )
 
(saying to herself
 Suppose it should be raving mad after all   )
 
(I almost wish I d
gone to see the Hatter instead  )
 
(


  There was a table set out under a tree in front of the house )
 
(
and the March Hare and the Hatter were having tea at it  )
 
( a
Dormouse was sitting between them  fast asleep )
 
(and the other two
were using it as a cushion  resting their elbows on it )
 
( and the
talking over its head   )
 
( Very uncomfortable for the Dormouse  
thought Alice  )
 
( only  as it s asleep  I suppose it doesn t mind  )
 
(

  The table was a large one  but the three were all crowded
together at one corner of it  )
 
(  No room   No room   they cried)
 
(
out when they saw Alice coming    There s PLENTY of room   said
Alice indignantly )
 
(and she sat down in a large arm-chair at one
end of the table )
 
(

   Have some wine   the March Hare said in an encouraging tone )
 
(

  Alice looked all round the table  but there was nothing on it
but tea  )
 
(  I don t see any wine   she remarked )
 
(

   There isn t any   said the March Hare )
 
(

   Then it wasn t very civil of you to offer it   said Alice)
 
(
angrily 

   It wasn t very civil of you to sit down without being
invited   said the March Hare )
 
(

   I didn t know it was YOUR table   said Alice   it s laid for a
great many more than three  )
 
(

   Your hair wants cutting   said the Hatter )
 
(  He had been
looking at Alice for some time with great curiosity  and this was
his first speech )
 
(

   You should learn not to make personal remarks   )
 
(Alice said
with some severity   it s very rude  )
 
(

  The Hatter opened his eyes very wide on hearing this  but all
he SAID was  )
 
( Why is a raven like a writing-desk)
 
(

   Come  we shall have some fun now   thought Alice   )
 
( I m glad
they ve begun asking riddles  I believe I can guess that   she
added aloud )
 
(

   Do you mean that you think you can find out the answer to it 
said the March Hare )
 
(

   Exactly so   said Alice )
 
(

   Then you should say what you mean )
 
(  the March Hare went on )
 
(

   I do   Alice hastily replied  )
 
( at least at least I mean what
I say that s the same thing  you know  )
 
(

   Not the same thing a bit   said the Hatter   )
 
( You might just
as well say that  I see what I eat  is the same thing as  I eat
what I see   )
 
(

   You might just as well say  )
 
(added the March Hare   that  I
like what I get  is the same thing as  I get what I like   )
 
(

   You might just as well say   added the Dormouse  )
 
(who seemed to
be talking in his sleep   )
 
(that  I breathe when I sleep  is the
same thing as  I sleep when I breathe   )
 
(

   It IS the same thing with you   said the Hatter )
 
( and here the
conversation dropped )
 
( and the party sat silent for a minute 
while Alice thought over all she could remember about ravens and
writing-desks  which wasn t much )
 
(

  The Hatter was the first to break the silence   )
 
( What day of
the month is it  he said  turning to Alice )
 
(  he had taken his
watch out of his pocket  and was looking at it uneasily  shaking
it every now and then )
 
( and holding it to his ear )
 
(

  Alice considered a little  and then said  The fourth  )
 
(

   Two days wrong   sighed the Hatter   )
 
( I told you butter
wouldn t suit the works   )
 
(he added looking angrily at the March
Hare )
 
(

   It was the BEST butter   the March Hare meekly replied )
 
(

   Yes  but some crumbs must have got in as well  )
 
( the Hatter
grumbled    you shouldn t have put it in with the bread-knife  )
 
(

  The March Hare took the watch and looked at it gloomily  )
 
( then
he dipped it into his cup of tea  and looked at it again  )
 
( but he
could think of nothing better to say than his first remark  )
 
( It
was the BEST butter  you know  )
 
(

  Alice had been looking over his shoulder with some curiosity )
 
(
 What a funny watch   she remarked   )
 
( It tells the day of the
month  and doesn t tell what o clock it is  )
 
(

   Why should it  muttered the Hatter    )
 
(Does YOUR watch tell
you what year it is 
)
 
(
   Of course not   Alice replied very readily   )
 
( but that s
because it stays the same year for such a long time together  )
 
(

   Which is just the case with MINE   said the Hatter )
 
(

  Alice felt dreadfully puzzled  )
 
( The Hatter s remark seemed to
have no sort of meaning in it  and yet it was certainly English 
 I don t quite understand you  )
 
( she said  as politely as she
could )
 
(

   The Dormouse is asleep again  )
 
( said the Hatter  and he poured
a little hot tea upon its nose )
 
(

  The Dormouse shook its head impatiently  and said  without
opening its eyes  )
 
( Of course  of course  just what I was going to
remark myself  )
 
(

   Have you guessed the riddle yet)
 
(  the Hatter said  turning to
Alice again )
 
(

   No  I give it up   Alice replied    what s the answer)
 
(

   I haven t the slightest idea   said the Hatter )
 
(

   Nor I   said the March Hare )
 
(

  Alice sighed wearily   )
 
( I think you might do something better
with the time   she said   than waste it in asking riddles that
have no answers  )
 
(

   If you knew Time as well as I do   said the Hatter  )
 
( you
wouldn t talk about wasting IT   It s HIM  )
 
(

   I don t know what you mean   said Alice )
 
(

   Of course you don t   the Hatter said  tossing his head
contemptuously  )
 
(  I dare say you never even spoke to Time  )
 
(

   Perhaps not   Alice cautiously replied    but I know I have to
beat time when I learn music  )
 

 
(
   Ah  that accounts for it   said the Hatter    He won t stand
beating   )
 
(Now  if you only kept on good terms with him  he d do
almost anything you liked with the clock   )
 
(For instance  suppose
it were nine o clock in the morning  )
 
(just time to begin lessons 
you d only have to whisper a hint to Time  )
 
(and round goes the
clock in a twinkling )
 
( Half-past one  time for dinner  

 I only wish it was   the March Hare said to itself in a
whisper )
 
(

   That would be grand  certainly   said Alice thoughtfully )
 
(
 but then I shouldn t be hungry for it  you know  )
 
(

   Not at first  perhaps   said the Hatter  )
 
(  but you could keep
it to half-past one as long as you liked  )
 
(

   Is that the way YOU manage  Alice asked )
 
(

  The Hatter shook his head mournfully   )
 
( Not I   he replied )
 
(
 We quarrelled last March just before HE went mad  you know  
)
 
(pointing with his tea spoon at the March Hare)
 
(  it was at the
great concert given by the Queen of Hearts  and I had to sing)
 
(

             Twinkle  twinkle  little bat )
 
(
            How I wonder what you re at  )
 
(

You know the song  perhaps)
 
(

   I ve heard something like it   said Alice )
 
(

   It goes on  you know   the Hatter continued   in this way  )
 
(

             Up above the world you fly )
 
(
            Like a tea-tray in the sky )
 
(
                    Twinkle  twinkle   )
 
(

Here the Dormouse shook itself  and began singing in its sleep)
 
(
 Twinkle  twinkle  twinkle  twinkle  )
 
( and went on so long that
they had to pinch it to make it stop )
 
(

   Well  I d hardly finished the first verse   said the Hatter )
 
(
 when the Queen jumped up and bawled out   )
 
(He s murdering the
time   Off with his head   )
 
(

   How dreadfully savage   exclaimed Alice )
 
(

   And ever since that   the Hatter went on in a mournful tone )
 
(
 he won t do a thing I ask   It s always six o clock now  )
 
(

  A bright idea came into Alice s head  )
 
(  Is that the reason so
many tea-things are put out here  she asked )
 
(

   Yes  that s it   said the Hatter with a sigh   )
 
( it s always
tea-time  and we ve no time to wash the things between whiles  )
 
(

   Then you keep moving round  I suppose  said Alice )
 
(

   Exactly so   said the Hatter   )
 
( as the things get used up  )
 
(

   But what happens when you come to the beginning again Alice
ventured to ask )
 
(

   Suppose we change the subject   the March Hare interrupted )
 
(
yawning    I m getting tired of this  )
 
( I vote the young lady
tells us a story  )
 
(

   I m afraid I don t know one   said Alice  )
 
(rather alarmed at
the proposal )
 
(

   Then the Dormouse shall   they both cried   )
 
( Wake up 
Dormouse    And they pinched it on both sides at once )
 
(

  The Dormouse slowly opened his eyes  )
 
(  I wasn t asleep   he
said in a hoarse  feeble voice   )
 
( I heard every word you fellows
were saying  )
 
(

   Tell us a story   said the March Hare )
 
(

   Yes  please do   pleaded Alice )
 
(

   And be quick about it   added the Hatter  )
 
( or you ll be asleep
again before it s done  )
 
(

   Once upon a time there were three little sisters   )
 
(the
Dormouse began in a great hurry   )
 
(and their names were Elsie 
Lacie  and Tillie  and they lived at the bottom of a well  )
 
(

   What did they live on  said Alice  )
 
(who always took a great
interest in questions of eating and drinking )
 
(

   They lived on treacle   said the Dormouse  after thinking a
minute or two )
 
(

   They couldn t have done that  you know   Alice gently
remarked   they d have been ill  )
 
(

   So they were   said the Dormouse   VERY ill  )
 
(

  Alice tried to fancy to herself what such an extraordinary ways
of living would be like  but it puzzled her too much  )
 
(so she went
on    But why did they live at the bottom of a well)
 
(

   Take some more tea   the March Hare said to Alice  very
earnestly )
 
(

   I ve had nothing yet   Alice replied in an offended tone   so
I can t take more  )
 
(

   You mean you can t take LESS   said the Hatter  )
 
(  it s very
easy to take MORE than nothing  )
 
(

   Nobody asked YOUR opinion   said Alice )
 
(

   Who s making personal remarks now the Hatter asked
triumphantly )
 
(

  Alice did not quite know what to say to this   so she helped
herself to some tea and bread-and-butter )
 
( and then turned to the
Dormouse  and repeated her question  )
 
(  Why did they live at the
bottom of a well)
 
(

  The Dormouse again took a minute or two to think about it  and
then said   It was a treacle-well  )
 
(

   There s no such thing    Alice was beginning very angrily  but
the Hatter and the March Hare went  )
 
(Sh  sh   and the Dormouse
sulkily remarked  )
 
( If you can t be civil  you d better finish the
story for yourself  )
 
(

   No  please go on   Alice said very humbly  )
 
( I won t interrupt
again   I dare say there may be ONE  )
 
(

   One  indeed   said the Dormouse indignantly   )
 
(However  he
consented to go on    )
 
(And so these three little sisters they
were learning to draw  you know  )
 
(

   What did they draw  said Alice  quite forgetting her promise )
 
(

   Treacle   said the Dormouse  without considering at all this
time )
 
(

   I want a clean cup   interrupted the Hatter    )
 
(let s all move
one place on  )
 
(

  He moved on as he spoke  )
 
(and the Dormouse followed him  )
 
( the
March Hare moved into the Dormouse s place )
 
( and Alice rather
unwillingly took the place of the March Hare  )
 
( The Hatter was the
only one who got any advantage from the change   )
 
(and Alice was a
good deal worse off than before  )
 
(as the March Hare had just upset
the milk-jug into his plate )
 
(

  Alice did not wish to offend the Dormouse again )
 
( so she began
very cautiously    )
 
(But I don t understand   )
 
(Where did they draw
the treacle from)
 
(

   You can draw water out of a water-well  )
 
( said the Hatter   so
I should think you could draw treacle out of a treacle-well eh 
stupid)
 
(

   But they were IN the well  )
 
( Alice said to the Dormouse  not
choosing to notice this last remark )
 
(

   Of course they were   said the Dormouse    well in  )
 
(

  This answer so confused poor Alice  )
 
(that she let the Dormouse
go on for some time without interrupting it )
 
(

   They were learning to draw  )
 
( the Dormouse went on  yawning and
rubbing its eyes  for it was getting very sleepy   )
 
(and they drew
all manner of things everything that begins with an M  )
 
(

   Why with an M  said Alice )
 
(

   Why not  said the March Hare )
 
(

  Alice was silent )
 
(

  The Dormouse had closed its eyes by this time )
 
( and was going
off into a doze  but  on being pinched by the Hatter  )
 
(it woke up
again with a little shriek  and went on    )
 
( that begins with an
M  such as mouse-traps  and the moon  )
 
(and memory  and muchness 
you know you say things are  much of a muchness  )
 
(did you ever
see such a thing as a drawing of a muchness)
 
(

   Really  now you ask me   said Alice  very much confused   I
don t think  )
 
(

   Then you shouldn t talk   said the Hatter )
 
(

  This piece of rudeness was more than Alice could bear  )
 
( she got
up in great disgust  and walked off  the Dormouse fell asleep
instantly )
 
( and neither of the others took the least notice of her
going  though she looked back once or twice  half hoping that
they would call after her  )
 
( the last time she saw them  they were
trying to put the Dormouse into the teapot )
 
(

   At any rate I ll never go THERE again   said Alice as she
picked her way through the wood )
 
(   It s the stupidest tea-party I
ever was at in all my life  )
 
(

  Just as she said this  she noticed that one of the trees had a
door leading right into it  )
 
(  That s very curious   she thought 
 But everything s curious today  )
 
( I think I may as well go in at
once    And in she went )
 
(

  Once more she found herself in the long hall )
 
( and close to the
little glass table    )
 
(Now  I ll manage better this time   )
 
(she
said to herself  and began by taking the little golden key )
 
( and
unlocking the door that led into the garden )
 
(  Then she went to
work nibbling at the mushroom )
 
(she had kept a piece of it in her
pockey)
 
( till she was about a foot high  )
 
( then she walked down the
little passage )
 
(  and THEN she found herself at last in the
beautiful garden  among the bright flower-beds and the cool
fountains )
 
(

  A large rose-tree stood near the entrance of the garden  )
 
( the
roses growing on it were white  but there were three gardeners at
it  busily painting them red  )
 
( Alice thought this a very curious
thing  and she went nearer to watch them  )
 
(and just as she came up
to them she heard one of them say  )
 
( Look out now  Five   Don t go
splashing paint over me like that  )
 
(

   I couldn t help it   said Five  in a sulky tone   Seven jogged
my elbow  )
 
(

  On which Seven looked up and said   That s right  Five   Always
lay the blame on others  )
 
(

   YOU D better not talk   said Five    I heard the Queen say only
yesterday you deserved to be beheaded  )
 
(

   What for  said the one who had spoken first )
 
(

   That s none of YOUR business  Two   said Seven )
 
(

   Yes  it IS his business   said Five   and I ll tell him it)
 
(
was for bringing the cook tulip-roots instead of onions  )
 
(

  Seven flung down his brush  and had just begun  )
 
(Well  of all
the unjust things   when his eye chanced to fall upon Alice  )
 
(as
she stood watching them  and he checked himself suddenly )
 
(  the
others looked round also  and all of them bowed low )
 
(

   Would you tell me   said Alice  a little timidly   why you are
painting those roses)
 
(

  Five and Seven said nothing  but looked at Two )
 
(  Two began in a
low voice )
 
(  Why the fact is  you see  Miss  this here ought to
have been a RED rose-tree )
 
( and we put a white one in by mistake 
and if the Queen was to find it out  )
 
(we should all have our heads
cut off  you know  )
 
( So you see  Miss  we re doing our best  afore
she comes  to    )
 
(At this moment Five  who had been anxiously
looking across the garden  called out  The Queen  )
 
(The Queen  
and the three gardeners instantly threw themselves flat upon
their faces )
 
(  There was a sound of many footsteps  and Alice
looked round  eager to see the Queen )
 
(

  First came ten soldiers carrying clubs )
 
( these were all shaped
like the three gardeners  oblong and flat  )
 
(with their hands and
feet at the corners  )
 
( next the ten courtiers  these were
ornamented all over with diamonds  )
 
(and walked two and two  as the
soldiers did   )
 
(After these came the royal children  )
 
(there were
ten of them  and the little dears came jumping merrily along hand
in hand  in couples  )
 
( they were all ornamented with hearts   Next
came the guests  mostly Kings and Queens  and among them Alice
recognised the White Rabbit  )
 
( it was talking in a hurried nervous
manner  smiling at everything that was said )
 
( and went by without
noticing her )
 
(  Then followed the Knave of Hearts  carrying the
King s crown on a crimson velvet cushion )
 
( and  last of all this
grand procession  came THE KING AND QUEEN OF HEARTS )
 
(

  Alice was rather doubtful whether she ought not to lie down on
her face like the three gardeners )
 
( but she could not remember
every having heard of such a rule at processions  )
 
( and besides 
what would be the use of a procession   thought she )
 
(  if people
had all to lie down upon their faces )
 
( so that they couldn t see
it   So she stood still where she was  and waited )
 
(

  When the procession came opposite to Alice )
 
( they all stopped
and looked at her  and the Queen said severely  Who is this)
 
(
She said it to the Knave of Hearts  who only bowed and smiled in
reply )
 
(

   Idiot   said the Queen  )
 
(tossing her head impatiently  and 
turning to Alice  she went on  )
 
( What s your name  child)
 
( 

   My name is Alice  so please your Majesty   said Alice )
 
(very
politely  but she added  to herself  )
 
( Why  they re only a pack of
cards  after all   I needn t be afraid of them  )
 
(

   And who are THESE  said the Queen  p)
 
(ointing to the three
gardeners who were lying round the rosetree  )
 
(for  you see  as
they were lying on their faces  and the pattern on their backs
was the same as the rest of the pack  )
 
(she could not tell whether
they were gardeners  or soldiers  or courtiers  or three of her
own children )
 
(

   How should I know said Alice )
 
( surprised at her own courage 
 It s no business of MINE  )
 
(

  The Queen turned crimson with fury  )
 
(and  after glaring at her
for a moment like a wild beast  screamed )
 
( Off with her head 
Off  )
 
(

   Nonsense   said Alice  very loudly and decidedly  and the
Queen was silent )
 
(

  The King laid his hand upon her arm  )
 
(and timidly said
 Consider  my dear   she is only a child  )
 
(

  The Queen turned angrily away from him )
 
( and said to the Knave
 Turn them over  )
 
(

  The Knave did so  very carefully  with one foot )
 
(

   Get up   said the Queen)
 
(  in a shrill  loud voice  and the
three gardeners instantly jumped up  and began bowing to the
King  the Queen )
 
( the royal children  and everybody else )
 
(

   Leave off that   screamed the Queen  )
 
(  You make me giddy  
And then  turning to the rose-tree  she went on  )
 
( What HAVE you
been doing here)
 
(

   May it please your Majesty   said Two )
 
( in a very humble tone 
going down on one knee as he spoke   we were trying  )
 
(

   I see   said the Queen  who had meanwhile been examining the
roses   )
 
( Off with their heads )
 
(  and the procession moved on 
three of the soldiers remaining behind to execute the unfortunate
gardeners  )
 
(who ran to Alice for protection )
 
(

   You shan t be beheaded   said Alice  )
 
(and she put them into a
large flower-pot that stood near   )
 
(The three soldiers wandered
about for a minute or two  looking for them )
 
( and then quietly
marched off after the others )
 
(

   Are their heads off  shouted the Queen )
 
(

   Their heads are gone  if it please your Majesty   the soldiers
shouted in reply )
 
(

   That s right   shouted the Queen    Can you play croquet)
 
(

  The soldiers were silent  and looked at Alice)
 
(  as the question
was evidently meant for her )
 
(

   Yes   shouted Alice )
 
(

   Come on  then   roared the Queen  )
 
(and Alice joined the
procession )
 
( wondering very much what would happen next )
 
(

   It s it s a very fine day   said a timid voice at her side )
 
(
She was walking by the White Rabbit )
 
( who was peeping anxiously
into her face )
 
(

   Very   said Alice     where s the Duchess)
 
(

   Hush   Hush   said the Rabbit in a low  hurried tone  )
 
( He
looked anxiously over his shoulder as he spoke )
 
( and then raised
himself upon tiptoe)
 
(  put his mouth close to her ear  and
whispered  She s under sentence of execution  )
 
(

   What for  said Alice )
 
(

   Did you say  What a pity    the Rabbit asked )
 
(

   No  I didn t   said Alice  )
 
(  I don t think it s at all a pity 
I said  What for)
 
(

   She boxed the Queen s ears   the Rabbit began   )
 
(Alice gave a
little scream of laughter   )
 
( Oh  hush   the Rabbit whispered in a
frightened tone   )
 
( The Queen will hear you   )
 
(You see  she came
rather late  and the Queen said  )
 
(

   Get to your places   shouted the Queen in a voice of thunder 
and people began running about in all directions )
 
( tumbling up
against each other )
 
( however  they got settled down in a minute or
two  and the game began  )
 
( Alice thought she had never seen such a
curious croquet-ground in her life )
 
( it was all ridges and
furrows  )
 
(the balls were live hedgehogs  the mallets live
flamingoes  )
 
(and the soldiers had to double themselves up and to
stand on their hands and feet  to make the arches )
 
(

  The chief difficulty Alice found at first was in managing her
flamingo )
 
(  she succeeded in getting its body tucked away 
comfortably enough  )
 
(under her arm  with its legs hanging down 
but generally  )
 
(just as she had got its neck nicely straightened
out  and was going to give the hedgehog a blow with its head )
 
( it
WOULD twist itself round and look up in her face)
 
(  with such a
puzzled expression that she could not help bursting out laughing 
and when she had got its head down )
 
( and was going to begin again 
it was very provoking to find that the hedgehog had unrolled
itself  and was in the act of crawling away  )
 
( besides all this 
there was generally a ridge or furrow in the way wherever she
wanted to send the hedgehog to )
 
( and  as the doubled-up soldiers
were always getting up and walking off to other parts of the
ground )
 
( Alice soon came to the conclusion that it was a very
difficult game indeed )
 
(

  The players all played at once without waiting for turns 
quarrelling all the while  and fighting for the hedgehogs )
 
( and in
a very short time the Queen was in a furious passion)
 
(  and went
stamping about  and shouting  Off with his head  )
 
( or  Off with
her head   about once in a minute )
 
(

  Alice began to feel very uneasy )
 
(  to be sure  she had not as
yet had any dispute with the Queen  )
 
(but she knew that it might
happen any minute   and then   thought she )
 
(  what would become of
me  They re dreadfully fond of beheading people here )
 
( the great
wonder is  that there s any one left alive  )
 
(

  She was looking about for some way of escape )
 
( and wondering
whether she could get away without being seen )
 
( when she noticed a
curious appearance in the air  )
 
( it puzzled her very much at
first  but  after watching it a minute or two  )
 
(she made it out to
be a grin  and she said to herself  It s the Cheshire Cat   )
 
(now I
shall have somebody to talk to  )
 
(

   How are you getting on  said the Cat  )
 
(as soon as there was
mouth enough for it to speak with )
 
(

  Alice waited till the eyes appeared  and then nodded  )
 
(  It s no
use speaking to it   she thought  )
 
( till its ears have come  or at
least one of them)
 
(    In another minute the whole head appeared 
and then Alice put down her flamingo )
 
( and began an account of the
game  feeling very glad she had someone to listen to her  )
 
(The
Cat seemed to think that there was enough of it now in sight  )
 
(and
no more of it appeared )
 
(

   I don t think they play at all fairly   )
 
(Alice began  in rather
a complaining tone)
 
(   and they all quarrel so dreadfully one can t
hear oneself speak and they don t seem to have any rules in
particular )
 
( at least  if there are  nobody attends to them and
you ve no idea how confusing it is all the things being alive )
 
(
for instance  there s the arch I ve got to go through next
walking about at the other end of the ground )
 
(and I should have
croqueted the Queen s hedgehog just now  )
 
(only it ran away when it
saw mine coming  )
 
(

   How do you like the Queen  said the Cat in a low voice )
 
(

   Not at all   said Alice    she s so extremely  )
 
(  Just then
she noticed that the Queen was close behind her  listening   so
she went on    )
 
(likely to win  that it s hardly worth while
finishing the game  )
 
(

  The Queen smiled and passed on )
 
(
   Who ARE you talking to  said the King)
 
( going up to Alice  and
looking at the Cat s head with great curiosity )
 
(

   It s a friend of mine a Cheshire Cat   said Alice )
 
(   allow me
to introduce it  )
 
(

   I don t like the look of it at all   said the King   )
 
( however 
it may kiss my hand if it likes  )
 
(

   I d rather not   the Cat remarked )
 
(

   Don t be impertinent   said the King   )
 
(and don t look at me
like that  )
 
(  He got behind Alice as he spoke )
 
(

   A cat may look at a king   said Alice  )
 
(  I ve read that in
some book  but I don t remember where  )
 
(

   Well  it must be removed   said the King very decidedly  )
 
(and
he called the Queen  who was passing at the moment  )
 
( My dear   I
wish you would have this cat removed  )
 
(

  The Queen had only one way of settling all difficulties  great
or small  )
 
(  Off with his head   she said  without even looking
round )
 
(

   I ll fetch the executioner myself   said the King eagerly )
 
( and
he hurried off )
 
(

  Alice thought she might as well go back  )
 
(and see how the game
was going on )
 
( as she heard the Queen s voice in the distance 
screaming with passion )
 
(  She had already heard her sentence three
of the players to be executed for having missed their turns  and
she did not like the look of things at all )
 
( as the game was in
such confusion that she never knew whether it was her turn or
not  )
 
(So she went in search of her hedgehog )
 
(

  The hedgehog was engaged in a fight with another hedgehog )
 
(
which seemed to Alice an excellent opportunity for croqueting one
of them with the other  )
 
( the only difficulty was  that her
flamingo was gone across to the other side of the garden )
 
( where
Alice could see it trying in a helpless sort of way to fly up)
 
(
into a tree )
 
(

  By the time she had caught the flamingo and brought it back 
the fight was over )
 
( and both the hedgehogs were out of sight 
 but it doesn t matter much   thought Alice )
 
(  as all the arches
are gone from this side of the ground   )
 
( So she tucked it away
under her arm  that it might not escape again )
 
( and went back for
a little more conversation with her friend )
 
(

  When she got back to the Cheshire Cat  she was surprised to
find quite a large crowd collected round it )
 
(  there was a dispute
going on between the executioner  the King  )
 
(and the Queen  who
were all talking at once  )
 
(while all the rest were quite silent 
and looked very uncomfortable )
 
(

  The moment Alice appeared  she was appealed to by all three to
settle the question  )
 
(and they repeated their arguments to her 
though  )
 
(as they all spoke at once  she found it very hard indeed
to make out exactly what they said )
 
(

  The executioner s argument was  that you couldn t cut off a
head unless there was a body to cut it off from )
 
(  that he had
never had to do such a thing before)
 
(  and he wasn t going to begin
at HIS time of life )
 
(

  The King s argument was  that anything that had a head could be
beheaded  and that you weren t to talk nonsense )
 
(

  The Queen s argument was  that if something wasn t done about
it in less than no time she d have everybody executed  )
 
(all round 
)
 
(It was this last remark that had made the whole party look so
grave and anxious )
 
(

  Alice could think of nothing else to say but  )
 
(It belongs to the
Duchess   you d better ask HER about it  )
 
(

   She s in prison   the Queen said to the executioner  )
 
(  fetch
her here    And the executioner went off like an arrow )
 
(

   The Cat s head began fading away the moment he was gone  and 
by the time he had come back with the Dutchess)
 
(  it had entirely
disappeared  so the King and the executioner ran wildly up and
down looking for it )
 
( while the rest of the party went back to the game )
 
(


   You can t think how glad I am to see you again  you dear old
thing   said the Duchess )
 
( as she tucked her arm affectionately
into Alice s  and they walked off together )
 
(

  Alice was very glad to find her in such a pleasant temper )
 
( and
thought to herself that perhaps it was only the pepper that had
made her so savage when they met in the kitchen )
 
(

   When I M a Duchess   she said to herself  )
 
(not in a very
hopeful tone though)
 
(  I won t have any pepper in my kitchen AT
ALL  )
 
( Soup does very well without Maybe it s always pepper that
makes people hot-tempered   she went on )
 
( very much pleased at
having found out a new kind of rule  )
 
( and vinegar that makes them
sour and camomile that makes them bitter and and barley-sugar
and such things that make children sweet-tempered  )
 
( I only wish
people knew that )
 
(  then they wouldn t be so stingy about it  you
know  )
 
(

  She had quite forgotten the Duchess by this time)
 
(  and was a
little startled when she heard her voice close to her ear )
 
(
 You re thinking about something  my dear )
 
( and that makes you
forget to talk)
 
(   I can t tell you just now what the moral of that
is  but I shall remember it in a bit  )
 
(

   Perhaps it hasn t one   Alice ventured to remark )
 
(

   Tut  tut  child   said the Duchess    )
 
(Everything s got a
moral  if only you can find it   )
 
( And she squeezed herself up
closer to Alice s side as she spoke )
 
(

  Alice did not much like keeping so close to her   first 
because the Duchess was VERY ugly  )
 
(and secondly  because she was
exactly the right height to rest her chin upon Alice s shoulder 
and it was an uncomfortably sharp chin  )
 
(However  she did not
like to be rude  so she bore it as well as she could )
 
(

   The game s going on rather better now   she said )
 
( by way of
keeping up the conversation a little )
 
(

    Tis so   said the Duchess  )
 
(  and the moral of that is  Oh 
 tis love   tis love  that makes the world go round   )
 
(

   Somebody said   Alice whispered  )
 
( that it s done by everybody
minding their own business  )
 
(

   Ah  well   It means much the same thing   said the Duchess )
 
(
digging her sharp little chin into Alice s shoulder as she added )
 
(
 and the moral of THAT is  )
 
(Take care of the sense  and the
sounds will take care of themselves   )
 
(

   How fond she is of finding morals in things   )
 
(Alice thought to
herself )
 
(

   I dare say you re wondering why I don t put my arm round your
waist   the Duchess said after a pause  )
 
(  the reason is  that I m
doubtful about the temper of your flamingo   )
 
(Shall I try the
experiment)
 
(

   HE might bite   Alice cautiously replied  )
 
(not feeling at all
anxious to have the experiment tried )
 
(

   Very true   said the Duchess )
 
(   flamingoes and mustard both
bite   And the moral of that is )
 
( Birds of a feather flock
together   )
 
(

   Only mustard isn t a bird   Alice remarked )
 
(

   Right  as usual   said the Duchess  )
 
(  what a clear way you
have of putting things  )
 
(

   It s a mineral  I THINK   said Alice )
 
(

   Of course it is   said the Duchess  )
 
(who seemed ready to agree
to everything that Alice said )
 
(  there s a large mustard-mine near
here   And the moral of that is  )
 
(The more there is of mine  the
less there is of yours   )
 
(

   Oh  I know   exclaimed Alice  )
 
(who had not attended to this
last remark )
 
(  it s a vegetable   It doesn t look like one  but it
is  )
 
(

   I quite agree with you   said the Duchess  )
 
( and the moral of
that is  )
 
(Be what you would seem to be  or if you d like it put
more simply  )
 
(Never imagine yourself not to be otherwise than
what it might appear to others that what you were )
 
(or might have
been was not otherwise than what you had been )
 
(would have appeared
to them to be otherwise   )
 
(

   I think I should understand that better   Alice said very
politely  )
 
( if I had it written down )
 
(  but I can t quite follow it
as you say it  )
 
(

   That s nothing to what I could say if I chose   the Duchess
replied  in a pleased tone )
 
(

   Pray don t trouble yourself to say it any longer than that  
said Alice )
 
(

   Oh  don t talk about trouble   said the Duchess  )
 
(  I make you
a present of everything I ve said as yet  )
 
(

   A cheap sort of present   thought Alice   )
 
( I m glad they don t
give birthday presents like that   )
 
( But she did not venture to
say it out loud )
 
(

   Thinking again  the Duchess asked  )
 
(with another dig of her
sharp little chin )
 
(

   I ve a right to think   said Alice sharply  for she was
beginning to feel a little worried )
 
(

   Just about as much right   said the Duchess   as pigs have to
fly  and the m  )
 
(

  But here  to Alice s great surprise  the Duchess s voice died)
 
(
away  even in the middle of her favourite word  moral   )
 
(and the
arm that was linked into hers began to tremble  )
 
( Alice looked up 
and there stood the Queen in front of them )
 
( with her arms folded 
frowning like a thunderstorm )
 
(

   A fine day  your Majesty   the Duchess began in a low  weak
voice )
 
(

   Now  I give you fair warning   shouted the Queen )
 
( stamping on
the ground as she spoke )
 
(  either you or your head must be off 
and that in about half no time   )
 
(Take your choice )
 
( 

  The Duchess took her choice  and was gone in a moment )
 
(

   Let s go on with the game   the Queen said to Alice )
 
( and Alice
was too much frightened to say a word )
 
( but slowly followed her
back to the croquet-ground )
 
(

  The other guests had taken advantage of the Queen s absence 
and were resting in the shade   )
 
(however  the moment they saw her 
they hurried back to the game )
 
( the Queen merely remarking that a
moment s delay would cost them their lives )
 
(

  All the time they were playing the Queen never left off
quarrelling with the other players  and shouting )
 
( Off with his
head   or  Off with her head   )
 
( Those whom she sentenced were
taken into custody by the soldiers  )
 
(who of course had to leave
off being arches to do this )
 
( so that by the end of half an hour
or so there were no arches left  and all the players  except the
King  the Queen )
 
( and Alice  were in custody and under sentence of
execution )
 
(

  Then the Queen left off  quite out of breath )
 
( and said to
Alice   Have you seen the Mock Turtle yet)
 
(

   No   said Alice    I don t even know what a Mock Turtle is  )
 
(

   It s the thing Mock Turtle Soup is made from   said the Queen )
 
(

   I never saw one  or heard of one   said Alice )
 
(

   Come on  then   said the Queen   and he shall tell you his
history  )
 
(

  As they walked off together  Alice heard the King say in a low
voice  to the company generally  )
 
( You are all pardoned    )
 
( Come 
THAT S a good thing   )
 
(she said to herself  for she had felt quite
unhappy at the number of executions the Queen had ordered )
 
(

  They very soon came upon a Gryphon )
 
( lying fast asleep in the
sun  )
 
(IF you don t know what a Gryphon is  look at the picture)
 
(
 Up  lazy thing   said the Queen )
 
(  and take this young lady to
see the Mock Turtle  and to hear his history  )
 
( I must go back and
see after some executions I have ordered  )
 
( and she walked off 
leaving Alice alone with the Gryphon  )
 
( Alice did not quite like
the look of the creature  but on the whole she thought it would
be quite as safe to stay with it as to go after that savage
Queen   so she waited )
 
(

  The Gryphon sat up and rubbed its eyes )
 
(  then it watched the
Queen till she was out of sight   then it chuckled    )
 
(What fun  
said the Gryphon  half to itself  half to Alice )
 
(

   What IS the fun  said Alice )
 
(

   Why  SHE   said the Gryphon  )
 
(  It s all her fancy  that   they
never executes nobody  you know   Come on  )
 
(

   Everybody says  come on   here   thought Alice )
 
( as she went
slowly after it    )
 
(I never was so ordered about in all my life 
never  )
 
(

  They had not gone far before they saw the Mock Turtle in the
distance  )
 
(sitting sad and lonely on a little ledge of rock  and 
as they came nearer )
 
( Alice could hear him sighing as if his heart
would break  )
 
( She pitied him deeply   )
 
( What is his sorrow)
 
( she
asked the Gryphon  and the Gryphon answered )
 
( very nearly in the
same words as before  )
 
( It s all his fancy  that   he hasn t got
no sorrow  you know   Come on  )
 
(

  So they went up to the Mock Turtle )
 
( who looked at them with
large eyes full of tears  but said nothing )
 
(

   This here young lady   said the Gryphon  )
 
( she wants for to
know your history  she do  )
 
(

   I ll tell it her   said the Mock Turtle in a deep  hollow
tone  )
 
(  sit down  both of you  and don t speak a word till I ve
finished  )
 
(

  So they sat down  and nobody spoke for some minutes   Alice
thought to herself )
 
(  I don t see how he can EVEN finish  if he
doesn t begin    But she waited patiently )
 
(

   Once   said the Mock Turtle at last  with a deep sigh   I was
a real Turtle  )
 
(

  These words were followed by a very long silence  broken only
by an occasional exclamation of  Hjckrrh )
 
(  from the Gryphon  and
the constant heavy sobbing of the Mock Turtle  )
 
( Alice was very
nearly getting up and saying   )
 
(Thank you  sir  for your
interesting story   but she could not help thinking there MUST be
more to come)
 
(  so she sat still and said nothing )
 
(

   When we were little  )
 
( the Mock Turtle went on at last  more
calmly  though still sobbing a little now and then  )
 
( we went to
school in the sea )
 
(  The master was an old Turtle we used to call
him Tortoise  )
 
(

   Why did you call him Tortoise  if he wasn t one  Alice asked )
 
(

   We called him Tortoise because he taught us   said the Mock
Turtle angrily  )
 
(  really you are very dull  )
 
(

   You ought to be ashamed of yourself for asking such a simple
question  )
 
( added the Gryphon  and then they both sat silent and
looked at poor Alice  )
 
(who felt ready to sink into the earth )
 
( At
last the Gryphon said to the Mock Turtle  )
 
( Drive on  old fellow 
Don t be all day about it   and he went on in these words )
 
(

   Yes  we went to school in the sea  though you mayn t believe
it  )
 
(

   I never said I didn t   interrupted Alice )
 
(

   You did   said the Mock Turtle )
 
(

   Hold your tongue   added the Gryphon  )
 
(before Alice could speak
again   The Mock Turtle went on )
 
(

   We had the best of educations in fact  we went to school
every day  )
 
(

   I VE been to a day-school  too   said Alice   )
 
(you needn t be
so proud as all that  )
 
(

   With extras  asked the Mock Turtle a little anxiously )
 
(

   Yes   said Alice   we learned French and music  )
 
(

   And washing  said the Mock Turtle )
 
(

   Certainly not   said Alice indignantly )
 
(

   Ah  then yours wasn t a really good school   )
 
(said the Mock
Turtle in a tone of great relief  )
 
(  Now at OURS they had at the
end of the bill  )
 
( French  music  AND WASHING extra   )
 
(

   You couldn t have wanted it much   said Alice  )
 
( living at the
bottom of the sea  )
 
(

   I couldn t afford to learn it   said the Mock Turtle )
 
(with a
sigh    I only took the regular course  )
 
(

   What was that  inquired Alice )
 
(

   Reeling and Writhing  of course  to begin with   the Mock
Turtle replied  )
 
( and then the different branches of Arithmetic 
Ambition  Distraction  Uglification  and Derision  )
 
(

   I never heard of  Uglification    Alice ventured to say    What
is it)
 
(

  The Gryphon lifted up both its paws in surprise  )
 
( What   Never
heard of uglifying   it exclaimed   )
 
( You know what to beautify
is  I suppose)
 
(

   Yes   said Alice doubtfully    it means to make anything 
prettier  )
 
(

   Well  then   the Gryphon went on   if you don t know what to
uglify is  you ARE a simpleton  )
 
(

  Alice did not feel encouraged to ask any more questions about
it  so she turned to the Mock Turtle  and said  )
 
(What else had you
to learn)
 
(

   Well  there was Mystery   the Mock Turtle replied  counting
off the subjects on his flappers  )
 
(  Mystery  ancient and modern 
with Seaography )
 
(  then Drawling the Drawling-master was an old
conger-eel  that used to come once a week )
 
(  HE taught us
Drawling  Stretching  and Fainting in Coils  )
 
(

   What was THAT like  said Alice )
 
(

   Well  I can t show it you myself   the Mock Turtle said   )
 
( I m
too stiff   And the Gryphon never learnt it  )
 
(

   Hadn t time   said the Gryphon  )
 
(  I went to the Classics
master  though   He was an old crab  HE was  )
 
(

   I never went to him   the Mock Turtle said with a sigh  )
 
(  he
taught Laughing and Grief  they used to say  )
 
(

   So he did  so he did   )
 
(said the Gryphon  sighing in his turn 
and both creatures hid their faces in their paws )
 
(

   And how many hours a day did you do lessons  said Alice  in a
hurry to change the subject )
 
(

   Ten hours the first day   said the Mock Turtle   nine the
next  and so on  )
 
(

   What a curious plan   exclaimed Alice )
 
(

   That s the reason they re called lessons   the Gryphon
remarked  )
 
(  because they lessen from day to day  )
 
(

  This was quite a new idea to Alice )
 
( and she thought it over a
little before she made her next remark    )
 
(Then the eleventh day
must have been a holiday)
 
(

   Of course it was   said the Mock Turtle )
 
(

   And how did you manage on the twelfth Alice went on eagerly )
 
(
   That s enough about lessons )
 
(  the Gryphon interrupted in a
very decided tone )
 
(   tell her something about the games now  )
 
(


  The Mock Turtle sighed deeply )
 
( and drew the back of one flapper
across his eyes )
 
(  He looked at Alice  and tried to speak )
 
( but for
a minute or two sobs choked his voice   )
 
( Same as if he had a bone
in his throat   said the Gryphon  )
 
( and it set to work shaking him
and punching him in the back )
 
(  At last the Mock Turtle recovered
his voice )
 
( and  with tears running down his cheeks  he went on
again  )
 
(

   You may not have lived much under the sea )
 
( I haven t  
said Alice)
 
( and perhaps you were never even introduced to a lobster  
)
 
(Alice began to say  I once tasted   but checked herself hastily 
and said  No  never)
 
(   so you can have no idea what a delightful
thing a Lobster Quadrille is  )
 
(

   No  indeed   said Alice    What sort of a dance is it)
 
(

   Why   said the Gryphon   you first form into a line along the
sea-shore  )
 
(

   Two lines   cried the Mock Turtle   )
 
( Seals  turtles  salmon 
and so on  then  when you ve cleared all the jelly-fish out of
the way  )
 
(

   THAT generally takes some time   interrupted the Gryphon )
 
(

    you advance twice  )
 
(

   Each with a lobster as a partner   cried the Gryphon )
 
(

   Of course   the Mock Turtle said    advance twice  set to
partners  )
 
(

    change lobsters  and retire in same order   continued the
Gryphon )
 
(

   Then  you know   the Mock Turtle went on   you throw the  )
 
(

   The lobsters   shouted the Gryphon  with a bound into the air )
 
(

    as far out to sea as you can  )
 
(

   Swim after them   screamed the Gryphon )
 
(

   Turn a somersault in the sea   cried the Mock Turtle 
capering wildly about )
 
(

   Back to land again  and that s all the first figure   said the
Mock Turtle )
 
( suddenly dropping his voice  and the two creatures 
who had been jumping about like mad things all this time )
 
( sat
down again very sadly and quietly  and looked at Alice )
 
(

   It must be a very pretty dance   said Alice timidly )
 
(

   Would you like to see a little of it  said the Mock Turtle )
 
(

   Very much indeed   said Alice )
 
(

   Come  let s try the first figure   said the Mock Turtle to the
Gryphon    We can do without lobsters  you know  )
 
( Which shall
sing)
 
(

   Oh  YOU sing   said the Gryphon    I ve forgotten the words  )
 
(

  So they began solemnly dancing round and round Alice )
 
( every now
and then treading on her toes when they passed too close )
 
( and
waving their forepaws to mark the time )
 
( while the Mock Turtle
sang this  very slowly and sadly  )
 
(


  Will you walk a little faster  said a whiting to a snail )
 
(
 There s a porpoise close behind us  and he s treading on my
 tail )
 
(
See how eagerly the lobsters and the turtles all advance )
 
(
They are waiting on the shingle will you come and join the
dance)
 
(

Will you  won t you  will you  won t you  will you join the
dance)
 
(
Will you  won t you  will you  won t you  won t you join the
dance)
 
(


 You can really have no notion how delightful it will be)
 
(
When they take us up and throw us  with the lobsters  out to
                                                      sea  )
 
(
But the snail replied  Too far  too far   and gave a look
                                                       askance )
 
(
Said he thanked the whiting kindly  but he would not join the
   dance )
 
(
    Would not  could not  would not  could not  would not join
        the dance )
 
(
    Would not  could not  would not  could not  could not join
        the dance )
 
(

  What matters it how far we go)
 
(  his scaly friend replied 
 There is another shore  you know  )
 
(upon the other side 
The further off from England the nearer is to France )
 
(
Then turn not pale  beloved snail  but come and join the dance )
 
(

    Will you  won t you  will you  won t you  will you join the
         dance)
 
(
    Will you  won t you  will you  won t you  won t you join the
         dance)
 
(



   Thank you  it s a very interesting dance to watch   said
Alice  )
 
(feeling very glad that it was over at last    )
 
(and I do so
like that curious song about the whiting  )
 
(

   Oh  as to the whiting   said the Mock Turtle   )
 
(they you ve
seen them  of course)
 
(

   Yes   said Alice   I ve often seen them at dinn  )
 
( she
checked herself hastily )
 
(

   I don t know where Dinn may be   said the Mock Turtle )
 
(  but
if you ve seen them so often  of course you know what they re
like  )
 
(

   I believe so   Alice replied thoughtfully )
 
(   They have their
tails in their mouths and they re all over crumbs  )
 
(

   You re wrong about the crumbs   said the Mock Turtle )
 
(
 crumbs would all wash off in the sea   )
 
(But they HAVE their tails
in their mouths  and the reason is )
 
(  here the Mock Turtle
yawned and shut his eyes  )
 
( Tell her about the reason and all
that   he said to the Gryphon )
 
(

   The reason is   said the Gryphon )
 
(  that they WOULD go with
the lobsters to the dance   )
 
(So they got thrown out to sea   So
they had to fall a long way  )
 
( So they got their tails fast in
their mouths  )
 
( So they couldn t get them out again   That s all  )
 
(

   Thank you   said Alice   it s very interesting )
 
(  I never knew
so much about a whiting before  )
 
(

   I can tell you more than that  if you like   said the
Gryphon  )
 
(  Do you know why it s called a whiting)
 
(

   I never thought about it   said Alice    Why)
 
(

   IT DOES THE BOOTS AND SHOES   the Gryphon replied very
solemnly )
 
(

  Alice was thoroughly puzzled  )
 
( Does the boots and shoes   she
repeated in a wondering tone )
 
(

   Why  what are YOUR shoes done with  said the Gryphon)
 
(    I
mean  what makes them so shiny)
 
(

  Alice looked down at them  and considered a little before she
gave her answer  )
 
(  They re done with blacking  I believe  )
 
(

   Boots and shoes under the sea  )
 
( the Gryphon went on in a deep
voice   are done with a whiting   Now you know  )
 
(

   And what are they made of)
 
(  Alice asked in a tone of great
curiosity )
 
(

   Soles and eels  of course   the Gryphon replied rather
impatiently )
 
(   any shrimp could have told you that  )
 
(

   If I d been the whiting   said Alice  )
 
(whose thoughts were
still running on the song )
 
(  I d have said to the porpoise  )
 
( Keep
back  please   we don t want YOU with us   )
 
(

   They were obliged to have him with them   the Mock Turtle
said )
 
(   no wise fish would go anywhere without a porpoise )
 
( 

   Wouldn t it really  said Alice in a tone of great surprise )
 
(

   Of course not   said the Mock Turtle   )
 
( why  if a fish came
to ME  and told me he was going a journey  I should say  With
what porpoise)
 
(

   Don t you mean  purpose   said Alice )
 
(

   I mean what I say   the Mock Turtle replied in an offended
tone   )
 
(And the Gryphon added  Come  let s hear some of YOUR
adventures  )
 
(

   I could tell you my adventures beginning from this morning  
said Alice a little timidly   )
 
( but it s no use going back to
yesterday  because I was a different person then )
 
( 

   Explain all that   said the Mock Turtle )
 
(

   No  no   The adventures first   said the Gryphon in an
impatient tone  )
 
(  explanations take such a dreadful time  )
 
(

  So Alice began telling them her adventures from the time when
she first saw the White Rabbit   )
 
(She was a little nervous about
it just at first )
 
( the two creatures got so close to her  one on
each side  and opened their eyes and mouths so VERY wide  but she
gained courage as she went on )
 
(  Her listeners were perfectly
quiet till she got to the part about her repeating  YOU ARE OLD 
FATHER WILLIAM   to the Caterpillar  )
 
(and the words all coming
different  and then the Mock Turtle drew a long breath  and said
 That s very curious  )
 
(

   It s all about as curious as it can be   said the Gryphon )
 
(

   It all came different   the Mock Turtle repeated)
 
(
thoughtfully    I should like to hear her try and repeat
something now )
 
(  Tell her to begin    He looked at the Gryphon as
if he thought it had some kind of authority over Alice )
 
(

   Stand up and repeat   TIS THE VOICE OF THE SLUGGARD    said
the Gryphon )
 
(

   How the creatures order one about  and make one repeat
lessons   thought Alice   )
 
(I might as well be at school at once  
However  she got up  and began to repeat it  but her head was so
full of the Lobster Quadrille  )
 
(that she hardly knew what she was
saying  and the words came very queer indeed  )
 
(

      Tis the voice of the Lobster  I heard him declare )
 
(
     You have baked me too brown  I must sugar my hair  )
 
(
    As a duck with its eyelids  so he with his nose)
 
(
    Trims his belt and his buttons  and turns out his toes  )
 
(

              later editions continued as follows)
 
(
    When the sands are all dry  he is gay as a lark )
 
(
    And will talk in contemptuous tones of the Shark )
 
(
    But  when the tide rises and sharks are around )
 
(
    His voice has a timid and tremulous sound)
 
(

   That s different from what I used to say when I was a child  
said the Gryphon )
 
(

   Well  I never heard it before   said the Mock Turtle)
 
(   but it
sounds uncommon nonsense  )
 
(

  Alice said nothing  she had sat down with her face in her
hands )
 
( wondering if anything would EVER happen in a natural way
again )
 
(

   I should like to have it explained   said the Mock Turtle )
 
(

   She can t explain it   said the Gryphon hastily    Go on with
the next verse  )
 
(

   But about his toes  the Mock Turtle persisted  )
 
(  How COULD
he turn them out with his nose  you know)
 
(

   It s the first position in dancing   Alice said  but was
dreadfully puzzled by the whole thing  )
 
(and longed to change the
subject )
 
(

   Go on with the next verse   the Gryphon repeated impatiently 
 it begins  I passed by his garden  )
 
( 

  Alice did not dare to disobey  though she felt sure it would
all come wrong  and she went on in a trembling voice  )
 
(

     I passed by his garden  and marked  with one eye )
 
(
    How the Owl and the Panther were sharing a pie  )
 
(

    The Panther took pie-crust  and gravy  and meat )
 
(
    While the Owl had the dish as its share of the treat )
 
(
    When the pie was all finished  the Owl  as a boon )
 
(
    Was kindly permitted to pocket the spoon )
 
(
    While the Panther received knife and fork with a growl )
 
(
    And concluded the banquet)
 
(

   What IS the use of repeating all that stuff   the Mock Turtle)
 
(
interrupted   if you don t explain it as you go on)
 
(  It s by far
the most confusing thing I ever heard  )
 
(

   Yes  I think you d better leave off   said the Gryphon  )
 
( and
Alice was only too glad to do so )
 
(

   Shall we try another figure of the Lobster Quadrille  the
Gryphon went on )
 
(   Or would you like the Mock Turtle to sing you
a song)
 
(

   Oh  a song  please  if the Mock Turtle would be so kind  
Alice replied  )
 
(so eagerly that the Gryphon said  in a rather
offended tone   Hm )
 
( No accounting for tastes   Sing her  Turtle
Soup   will you  old fellow)
 
(

  The Mock Turtle sighed deeply  and began  in a voice sometimes
choked with sobs  to sing this  )
 
(


     Beautiful Soup  so rich and green )
 
(
    Waiting in a hot tureen )
 
(
    Who for such dainties would not stoop)
 
(
    Soup of the evening  beautiful Soup )
 
(
    Soup of the evening  beautiful Soup )
 
(
        Beau ootiful Soo oop )
 
(
        Beau ootiful Soo oop )
 
(
    Soo oop of the e e evening )
 
(
        Beautiful  beautiful Soup )
 
(

     Beautiful Soup   Who cares for fish )
 
(
    Game  or any other dish)
 
(
    Who would not give all else for two p)
 
(
    ennyworth only of beautiful Soup)
 
(
    Pennyworth only of beautiful Soup)
 
(
        Beau ootiful Soo oop )
 
(
        Beau ootiful Soo oop )
 
(
    Soo oop of the e e evening )
 
(
        Beautiful  beauti FUL SOUP  )
 
(

   Chorus again   cried the Gryphon )
 
( and the Mock Turtle had
just begun to repeat it  )
 
(when a cry of  The trial s beginning  
was heard in the distance )
 
(

   Come on   cried the Gryphon  and  taking Alice by the hand 
it hurried off  without waiting for the end of the song )
 
(

   What trial is it  Alice panted as she ran  )
 
(but the Gryphon
only answered  Come on )
 
(  and ran the faster  while more and more
faintly came  carried on the breeze that followed them  the
melancholy words  )
 
(

     Soo oop of the e e evening )
 
(
        Beautiful  beautiful Soup  )
 
(



  The King and Queen of Hearts were seated on their throne when
they arrived  )
 
(with a great crowd assembled about them all sorts
of little birds and beasts )
 
( as well as the whole pack of cards 
the Knave was standing before them  in chains  )
 
(with a soldier on
each side to guard him  and near the King was the White Rabbit )
 
(
with a trumpet in one hand  and a scroll of parchment in the
other   In the very middle of the court was a table  )
 
(with a large
dish of tarts upon it   )
 
(they looked so good  that it made Alice
quite hungry to look at them  )
 
(I wish they d get the trial done  
she thought  )
 
( and hand round the refreshments  )
 
(  But there seemed
to be no chance of this  so she began looking at everything about
her  to pass away the time )
 
(

  Alice had never been in a court of justice before  but she had
read about them in books )
 
( and she was quite pleased to find that
she knew the name of nearly everything there   )
 
(That s the
judge   she said to herself   because of his great wig  )
 
(

  The judge  by the way  was the King  and as he wore his crown
over the wig )
 
(look at the frontispiece if you want to see how he
did it)
 
( he did not look at all comfortable  and it was certainly
not becoming )
 
(

   And that s the jury-box   thought Alice  )
 
( and those twelve
creatures   )
 
(she was obliged to say  creatures   you see  because
some of them were animals  and some were birds)
 
(  I suppose they
are the jurors  )
 
(  She said this last word two or three times over
to herself  being rather proud of it   )
 
(for she thought  and
rightly too  that very few little girls of her age knew the
meaning of it at all )
 
(  However   jury-men  would have done just
as well )
 
(

  The twelve jurors were all writing very busily on slates 
 What are they doing)
 
(   Alice whispered to the Gryphon  )
 
(  They
can t have anything to put down yet  before the trial s begun  )
 
(

   They re putting down their names   the Gryphon whispered in
reply  )
 
( for fear they should forget them before the end of the
trial  )
 
(

   Stupid things   Alice began in a loud  indignant voice  but
she stopped hastily  for the White Rabbit cried out )
 
( Silence in
the court  )
 
( and the King put on his spectacles and looked
anxiously round  to make out who was talking )
 
(

  Alice could see  as well as if she were looking over their
shoulders  that all the jurors were writing down  )
 
(stupid things  
on their slates  and she could even make out that one of them
didn t know how to spell  stupid )
 
(  and that he had to ask his
neighbour to tell him   )
 
( A nice muddle their slates ll be in
before the trial s over   thought Alice )
 
(

  One of the jurors had a pencil that squeaked   )
 
(This of course 
Alice could not stand  and she went round the court and got
behind him  )
 
(and very soon found an opportunity of taking it
away )
 
(  She did it so quickly that the poor little juror it was
Bill  the Lizard could not make out at all what had become of
it  so  after hunting all about for it  )
 
(he was obliged to write
with one finger for the rest of the day  )
 
(and this was of very
little use  as it left no mark on the slate )
 
(

   Herald  read the accusation   said the King )
 
(

  On this the White Rabbit blew three blasts on the trumpet  )
 
(and
then unrolled the parchment scroll  and read as follows  )
 
(

     The Queen of Hearts  she made some tarts )
 
(
          All on a summer day )
 
(
      The Knave of Hearts  he stole those tarts )
 
(
          And took them quite away  )
 
(

   Consider your verdict   the King said to the jury )
 
(

   Not yet  not yet   the Rabbit hastily interrupted    )
 
(There s
a great deal to come before that  )
 
(

   Call the first witness   said the King  )
 
(and the White Rabbit
blew three blasts on the trumpet  and called out )
 
(  First
witness  )
 
(

  The first witness was the Hatter  )
 
( He came in with a teacup in
one hand and a piece of bread-and-butter in the other )
 
(   I beg
pardon  your Majesty   he began)
 
(   for bringing these in   but I
hadn t quite finished my tea when I was sent for  )
 
(

   You ought to have finished   said the King    When did you
begin)
 
(

  The Hatter looked at the March Hare )
 
( who had followed him into
the court  arm-in-arm with the Dormouse   )
 
( Fourteenth of March  I
think it was   he said )
 
(

   Fifteenth   said the March Hare )
 
(

   Sixteenth   added the Dormouse )
 
(

   Write that down   the King said to the jury  and the jury)
 
(
eagerly wrote down all three dates on their slates )
 
( and then
added them up  and reduced the answer to shillings and pence )
 
(

   Take off your hat   the King said to the Hatter )
 
(

   It isn t mine   said the Hatter )
 
(

   Stolen   the King exclaimed  turning to the jury  who
instantly made a memorandum of the fact )
 
(

   I keep them to sell   the Hatter added as an explanation 
 I ve none of my own   I m a hatter  )
 
(

  Here the Queen put on her spectacles  and began staring at the
Hatter  who turned pale and fidgeted )
 
(

   Give your evidence   said the King   and don t be nervous  or
I ll have you executed on the spot  )
 
(

  This did not seem to encourage the witness at all   )
 
(he kept
shifting from one foot to the other  looking uneasily at the
Queen )
 
( and in his confusion he bit a large piece out of his
teacup instead of the bread-and-butter )
 
(

  Just at this moment Alice felt a very curious sensation  )
 
(which
puzzled her a good deal until she made out what it was   )
 
(she was
beginning to grow larger again  )
 
(and she thought at first she
would get up and leave the court  )
 
(but on second thoughts she
decided to remain where she was as long as there was room for
her )
 
(

   I wish you wouldn t squeeze so   said the Dormouse )
 
( who was
sitting next to her    I can hardly breathe  )
 
(

   I can t help it   said Alice very meekly    I m growing  )
 
(

   You ve no right to grow here   said the Dormouse )
 
(

   Don t talk nonsense   said Alice more boldly    you know
you re growing too  )
 
(

   Yes  but I grow at a reasonable pace   said the Dormouse )
 
(
 not in that ridiculous fashion    )
 
(And he got up very sulkily
and crossed over to the other side of the court )
 
(

  All this time the Queen had never left off staring at the
Hatter  )
 
(and  just as the Dormouse crossed the court  she said to
one of the officers of the court  )
 
( Bring me the list of the
singers in the last concert )
 
(  on which the wretched Hatter
trembled so  that he shook both his shoes off )
 
(

   Give your evidence   the King repeated angrily  )
 
( or I ll have
you executed  whether you re nervous or not  )
 
(

   I m a poor man  your Majesty   the Hatter began  in a
trembling voice  )
 
(  and I hadn t begun my tea not above a week
or so and what with the bread-and-butter getting so thin and
the twinkling of the tea  )
 
(

   The twinkling of the what  said the King )
 
(

   It began with the tea   the Hatter replied )
 
(

   Of course twinkling begins with a T   said the King sharply 
 Do you take me for a dunce  Go on  )
 
(

   I m a poor man   the Hatter went on )
 
(  and most things
twinkled after that only the March Hare said  )
 
(

   I didn t   the March Hare interrupted in a great hurry )
 
(

   You did   said the Hatter )
 
(

   I deny it   said the March Hare )
 
(

   He denies it   said the King    leave out that part  )
 
(

   Well  at any rate  the Dormouse said   the Hatter went on 
looking anxiously round to see if he would deny it too  )
 
( but the
Dormouse denied nothing  being fast asleep )
 
(

   After that   continued the Hatter   I cut some more bread-
and-butter  )
 
(

   But what did the Dormouse say  one of the jury asked )
 
(

   That I can t remember   said the Hatter )
 
(

   You MUST remember   remarked the King   or I ll have you
executed  )
 
(

  The miserable Hatter dropped his teacup and bread-and-butter 
and went down on one knee   )
 
( I m a poor man  your Majesty   he
began )
 
(

   You re a very poor speaker   said the King )
 
(

  Here one of the guinea-pigs cheered )
 
( and was immediately
suppressed by the officers of the court   )
 
(As that is rather a
hard word  I will just explain to you how it was done  )
 
( They had
a large canvas bag  which tied up at the mouth with strings 
into this they slipped the guinea-pig  head first  and then sat
upon it )
 
(

   I m glad I ve seen that done   thought Alice   )
 
( I ve so often
read in the newspapers  at the end of trials  )
 
( There was some
attempts at applause  )
 
(which was immediately suppressed by the
officers of the court  )
 
( and I never understood what it meant
till now  )
 
(

   If that s all you know about it  you may stand down  
continued the King )
 
(

   I can t go no lower   said the Hatter   )
 
( I m on the floor  as
it is  )
 
(

   Then you may SIT down   the King replied )
 
(

  Here the other guinea-pig cheered  and was suppressed )
 
(

   Come  that finished the guinea-pigs   thought Alice  )
 
(  Now we
shall get on better  )
 
(

   I d rather finish my tea   said the Hatter  )
 
(with an anxious
look at the Queen  who was reading the list of singers )
 
(

   You may go   said the King  )
 
(and the Hatter hurriedly left the
court  without even waiting to put his shoes on )
 
(

    and just take his head off outside   )
 
(the Queen added to one
of the officers  )
 
( but the Hatter was out of sight before the
officer could get to the door )
 
(

   Call the next witness   said the King )
 
(

  The next witness was the Duchess s cook   )
 
(She carried the
pepper-box in her hand  and Alice guessed )
 
(who it was  even before
she got into the court  by the way the people near the door began
sneezing all at once )
 
(

   Give your evidence   said the King )
 
(

   Shan t   said the cook )
 
(

  The King looked anxiously at the White Rabbit  )
 
(who said in a
low voice   )
 
(Your Majesty must cross-examine THIS witness  )
 
(

   Well  if I must  I must   the King said  with a melancholy)
 
(
air  and  after folding his arms and frowning at the cook till
his eyes were nearly out of sight )
 
( he said in a deep voice   What
are tarts made of)
 
(

   Pepper  mostly   said the cook )
 
(

   Treacle   said a sleepy voice behind her )
 
(

   Collar that Dormouse   the Queen shrieked out   )
 
( Behead that
Dormouse   Turn that Dormouse out of court   )
 
(Suppress him   Pinch
him   Off with his whiskers  )
 
(

  For some minutes the whole court was in confusion  )
 
(getting the
Dormouse turned out  and  by the time they had settled down
again  the cook had disappeared )
 
(

   Never mind   said the King  with an air of great relief 
 Call the next witness  )
 
(  And he added in an undertone to the
Queen   Really  my dear  )
 
(YOU must cross-examine the next witness 
It quite makes my forehead ache  )
 
(

  Alice watched the White Rabbit )
 
(as he fumbled over the list 
feeling very curious to see what the next witness would be like 
  for they haven t got much evidence YET   )
 
(she said to herself 
Imagine her surprise  )
 
(when the White Rabbit read out  at the top
of his shrill little voice  the name  Alice  )
 
(


   Here   cried Alice )
 
( quite forgetting in the flurry of the
moment how large she had grown in the last few minutes )
 
( and she
jumped up in such a hurry that she tipped over the jury-box with
the edge of her skirt  )
 
(upsetting all the jurymen on to the heads
of the crowd below )
 
( and there they lay sprawling about  reminding
her very much of a globe of goldfish she had accidentally upset
the week before )
 
(

   Oh  I BEG your pardon   she exclaimed in a tone of great
dismay  )
 
(and began picking them up again as quickly as she could 
for the accident of the goldfish kept running in her head  )
 
(and
she had a vague sort of idea that they must be collected at once
and put back into the jury-box  or they would die )
 
(

   The trial cannot proceed   said the King in a very grave
voice  )
 
( until all the jurymen are back in their proper places )
 
(
ALL   he repeated with great emphasis  looking hard at Alice as
he said do )
 
(

  Alice looked at the jury-box  )
 
(and saw that  in her haste  she
had put the Lizard in head downwards  )
 
(and the poor little thing
was waving its tail about in a melancholy way )
 
( being quite unable
to move  )
 
( She soon got it out again  and put it right   not that
it signifies much   she said to herself   )
 
(I should think it
would be QUITE as much use in the trial one way up as the other  )
 
(

  As soon as the jury had a little recovered from the shock of
being upset)
 
(  and their slates and pencils had been found and
handed back to them  )
 
(they set to work very diligently to write
out a history of the accident  )
 
(all except the Lizard  who seemed
too much overcome to do anything but sit with its mouth open 
gazing up into the roof of the court )
 
(

   What do you know about this business  the King said to
Alice )
 
(

   Nothing   said Alice )
 
(

   Nothing WHATEVER  persisted the King )
 
(

   Nothing whatever   said Alice )
 
(

   That s very important   the King said  )
 
(turning to the jury )
 
(
They were just beginning to write this down on their slates  when
the White Rabbit interrupted  )
 
(  UNimportant  your Majesty means 
of course   he said in a very respectful tone  but frowning and
making faces at him as he spoke )
 
(

   UNimportant  of course  I meant   )
 
(the King hastily said  and
went on to himself in an undertone   )
 
(important unimportant 
unimportant important   )
 
(as if he were trying which word
sounded best )
 
(

  Some of the jury wrote it down  important  )
 
( and some
 unimportant   )
 
( Alice could see this  as she was near enough to
look over their slates   )
 
(but it doesn t matter a bit   she
thought to herself )
 
(

  At this moment the King  who had been for some time busily
writing in his note-book  cackled out  Silence  )
 
( and read out
from his book  )
 
( Rule Forty-two   ALL PERSONS MORE THAN A MILE
HIGH TO LEAVE THE COURT  )
 
(

  Everybody looked at Alice )
 
(

   I M not a mile high   said Alice )
 
(

   You are   said the King )
 
(

   Nearly two miles high   added the Queen )
 
(

   Well  I shan t go  at any rate   said Alice   )
 
( besides 
that s not a regular rule   you invented it just now  )
 
(

   It s the oldest rule in the book   said the King )
 
(

   Then it ought to be Number One   said Alice )
 
(

  The King turned pale  and shut his note-book hastily )
 
(
 Consider your verdict   he said to the jury  in a low  trembling
voice )
 
(

   There s more evidence to come yet  please your Majesty   said
the White Rabbit)
 
(  jumping up in a great hurry   this paper has
just been picked up  )
 
(

   What s in it  said the Queen )
 
(

   I haven t opened it yet   said the White Rabbit  )
 
( but it seems
to be a letter  written by the prisoner to to somebody  )
 
(

   It must have been that   said the King  )
 
( unless it was
written to nobody  which isn t usual  you know  )
 
(

   Who is it directed to  said one of the jurymen )
 
(

   It isn t directed at all   said the White Rabbit  )
 
( in fact 
there s nothing written on the OUTSIDE   )
 
( He unfolded the paper
as he spoke  and added )
 
( It isn t a letter  after all  )
 
( it s a set
of verses  

   Are they in the prisoner s handwriting  asked another of
they jurymen )
 
(

   No  they re not   said the White Rabbit  )
 
( and that s the
queerest thing about it    )
 
(The jury all looked puzzled )
 
(

   He must have imitated somebody else s hand   said the King )
 
(
The jury all brightened up again )
 
(

   Please your Majesty   said the Knave  )
 
( I didn t write it  and
they can t prove I did  )
 
( there s no name signed at the end  )
 
(

   If you didn t sign it   said the King   that only makes the
matter worse )
 
(  You MUST have meant some mischief  or else you d
have signed your name like an honest man  )
 
(

  There was a general clapping of hands at this )
 
(  it was the
first really clever thing the King had said that day )
 
(

   That PROVES his guilt   said the Queen )
 
(

   It proves nothing of the sort   said Alice  )
 
(  Why  you don t
even know what they re about  )
 
(

   Read them   said the King )
 
(

  The White Rabbit put on his spectacles   )
 
( Where shall I begin 
please your Majesty  he asked )
 
(

   Begin at the beginning   the King said gravely   and go on
till you come to the end   then stop  )
 
(

  These were the verses the White Rabbit read  )
 
(

         They told me you had been to her )
 
(
          And mentioned me to him )
 
(
        She gave me a good character )
 
(
          But said I could not swim )
 
(

        He sent them word I had not gone)
 
(
          We know it to be true)
 
(
        If she should push the matter on )
 
(
          What would become of you)
 
(

        I gave her one  they gave him two )
 
(
          You gave us three or more )
 
(
        They all returned from him to you )
 
(
          Though they were mine before )
 
(

        If I or she should chance to be)
 
(
          Involved in this affair )
 
(
        He trusts to you to set them free )
 
(
          Exactly as we were )
 
(

        My notion was that you had been)
 
(
          Before she had this fit)
 
(
        An obstacle that came between)
 
(
          Him  and ourselves  and it )
 
(

        Don t let him know she liked them best )
 
(
          For this must ever be)
 
(
        A secret  kept from all the rest )
 
(
          Between yourself and me  )
 
(

   That s the most important piece of evidence we ve heard yet  
said the King  rubbing his hands   so now let the jury  )
 
(

   If any one of them can explain it   said Alice  )
 
(she had
grown so large in the last few minutes that she wasn t a bit
afraid of interrupting him)
 
( I ll give him sixpence   _I_ don t
believe there s an atom of meaning in it  )
 
(

  The jury all wrote down on their slates   )
 
(SHE doesn t believe
there s an atom of meaning in it  )
 
( but none of them attempted to
explain the paper )
 
(

   If there s no meaning in it   said the King   )
 
(that saves a
world of trouble  you know  )
 
(as we needn t try to find any   And
yet I don t know  )
 
(he went on  spreading out the verses on his
knee  and looking at them with one eye   )
 
(I seem to see some
meaning in them  after all    )
 
( SAID I COULD NOT SWIM   you
can t swim  can you  he added  turning to the Knave )
 
(

  The Knave shook his head sadly    Do I look like it he said 
)
 
(Which he certainly did NOT  being made entirely of cardboard)
 
(

   All right  so far   said the King)
 
( and he went on muttering
over the verses to himself    )
 
( WE KNOW IT TO BE TRUE   that s
the jury  of course  )
 
( I GAVE HER ONE  THEY GAVE HIM TWO  )
 
( why 
that must be what he did with the tarts  you know  )
 
(

   But  it goes on  THEY ALL RETURNED FROM HIM TO YOU    said
Alice )
 
(

   Why  there they are   said the King triumphantly  pointing to
the tarts on the table   )
 
( Nothing can be clearer than THAT 
Then again  BEFORE SHE HAD THIS FIT )
 
(  you never had fits  my
dear  I think  he said to the Queen )
 
(

   Never   said the Queen furiously  throwing an inkstand at the
Lizard as she spoke  )
 
(The unfortunate little Bill had left off
writing on his slate with one finger  )
 
(as he found it made no
mark  but he now hastily began again  )
 
(using the ink  that was
trickling down his face  as long as it lasted )
 
(

   Then the words don t FIT you   said the King )
 
( looking round
the court with a smile   )
 
(There was a dead silence )
 
(

   It s a pun   the King added in an offended tone  )
 
(and
everybody laughed  )
 
( Let the jury consider their verdict)
 
(   the
King said  for about the twentieth time that day )
 
(

   No  no   said the Queen )
 
(   Sentence first verdict afterwards  )
 
(

   Stuff and nonsense   said Alice loudly  )
 
( The idea of having
the sentence first  )
 
(

   Hold your tongue   said the Queen  turning purple )
 
(

   I won t   said Alice )
 
(

   Off with her head   the Queen shouted at the top of her voice 
Nobody moved )
 
(

   Who cares for you  said Alice  )
 
(she had grown to her full
size by this time)
 
(  You re nothing but a pack of cards  )
 
(

  At this the whole pack rose up into the air )
 
( and came flying
down upon her  )
 
( she gave a little scream )
 
( half of fright and half
of anger  and tried to beat them off  and found herself lying on
the bank)
 
(  with her head in the lap of her sister )
 
( who was gently
brushing away some dead leaves that had fluttered down from the
trees upon her face )
 
(

   Wake up  Alice dear   said her sister )
 
(  Why  what a long
sleep you ve had  )
 
(

   Oh  I ve had such a curious dream   )
 
(said Alice  and she told
her sister  as well as she could remember them )
 
( all these strange
Adventures of hers that you have just been reading about  )
 
(and
when she had finished  her sister kissed her  and said   )
 
(It WAS a
curious dream  dear  certainly  )
 
( but now run in to your tea  it s
getting late    )
 
(So Alice got up and ran off  thinking while she
ran  as well she might  what a wonderful dream it had been )
 
(

  But her sister sat still just as she left her )
 
( leaning her
head on her hand  watching the setting sun )
 
( and thinking of
little Alice and all her wonderful Adventures  )
 
(till she too began
dreaming after a fashion  and this was her dream  )
 
(

  First  she dreamed of little Alice herself  and once again the
tiny hands were clasped upon her knee  )
 
(and the bright eager eyes
were looking up into hers she could hear the very tones of her
voice )
 
( and see that queer little toss of her head to keep back
the wandering hair that WOULD always get into her eyes and
still as she listened  or seemed to listen )
 
( the whole place
around her became alive the strange creatures of her little
sister s dream )
 
(

  The long grass rustled at her feet as the White Rabbit hurried
by the frightened Mouse splashed his way through the
neighbouring pool )
 
(she could hear the rattle of the teacups as
the March Hare and his friends shared their never-ending meal 
and the shrill voice of the Queen ordering off her unfortunate
guests )
 
(to execution once more the pig-baby was sneezing on the
Duchess s knee  )
 
(while plates and dishes crashed around it once
more the shriek of the Gryphon )
 
( the squeaking of the Lizard s
slate-pencil  and the choking of the suppressed guinea-pigs )
 
(
filled the air  mixed up with the distant sobs of the miserable
Mock Turtle )
 
(

  So she sat on  with closed eyes  )
 
(and half believed herself in
Wonderland)
 
(  though she knew she had but to open them again  and
all would change to dull reality the grass would be only
rustling in the wind  )
 
(and the pool rippling to the waving of the
reeds the rattling teacups would change to tinkling sheep-
bells  )
 
(and the Queen s shrill cries to the voice of the shepherd
boy and the sneeze of the baby  )
 
(the shriek of the Gryphon  and
all thy other queer noises  )
 
(would change she knew to the
confused clamour of the busy farm-yard while the lowing of the
cattle in the distance )
 
(would take the place of the Mock Turtle s
heavy sobs )
 
(

  Lastly  she pictured to herself how this same little sister of
hers would  in the after-time  )
 
(be herself a grown woman  and how
she would keep  through all her riper years  )
 
(the simple and
loving heart of her childhood )
 
(  and how she would gather about
her other little children )
 
( and make THEIR eyes bright and eager
with many a strange tale  )
 
(perhaps even with the dream of
Wonderland of long ago  )
 
( and how she would feel with all their
simple sorrows  and find a pleasure in all their simple joys 
remembering her own child-life )
 
( and the happy summer days )))

        


(defvar emerson '((
Delicate omens traced in air)
 
(
To the lone bard true witness bare)
 
(
Birds with auguries on their wings)
 
(
Chanted undeceiving things)

 
(
Well might then the poet scorn)
 
(
To learn of scribe or courier)
 
(
Hints writ in vaster character)
 
(
And on his mind at dawn of day)
 
(
Soft shadows of the evening lay)
 
(
For the prevision is allied)
 
(
Unto the thing so signified)
 
(
Or say the foresight that awaits)
 
(
Is the same Genius that creates)
 
(
It chanced during one winter)
 

 
( that our cities were bent on discussing the theory of the Age)
 
( By an odd
coincidence four or five noted men )
 
(were each reading a discourse to the citizens of Boston or New York)
 
 
( It so happened that the subject had the same prominence in some remarkable pamphlets )
 
 
( To me however the question of the times resolved itself into a practical question of the conduct
of life)
 
( How shall I live)
 
( We are incompetent to solve the times)
 
( Our geometry cannot span the huge orbits of the prevailing
ideas)
 
( behold their return and reconcile their opposition)
 
( We can only obey our own polarity)
 
(Tis fine for us to speculate
and elect our course)
 
( if we must accept an irresistible dictation)
 
(

In our first steps to gain our wishes)
 
( we come upon immovable limitations)
 
(We are fired with the hope to reform men)
 
 
( we find that we must begin earlier at school)
 
( But the boys and girls are not docile)
 
( we can make
nothing of them)
 
( We decide that they are not of good stock)
 
( We must begin our reform earlier still at generation)
 
( that is to
say there is Fate or laws of the world)
 
(

But if there be irresistible dictation)
 
( this dictation understands itself)
 
( If we must accept Fate)
 
( we are not less compelled to
affirm liberty)
 
( the significance of the individual)
 
( the grandeur of duty)
 
( the power of character)
 
( This is true and that other
is true)
 
( But our geometry cannot span these extreme points)
 
( and reconcile them)
 
(What to do)
 
(By obeying each thought
frankly)
 
( by harping or if you will pounding on each string)
 
( we learn at last its power)
 
(By the same obedience to other
thoughts)
 
( we learn theirs and then comes some reasonable hope of harmonizing them)
 
(We are sure that though we know
not how )
 
( necessity does comport with liberty)
 
( the individual with the world)
 
(my polarity with the spirit of the times)
 
( The
riddle of the age has for each a private solution)
 
( If one would study his own time)
 
( it must be by this method of taking up in
turn)
 
( each of the leading topics which belong to our scheme of human life)
 
( by firmly stating all that is agreeable to
experience on one)
 
( and doing the same justice to the opposing facts in the others)
 
( the true limitations will appear)
 
( Any
excess of emphasis)
 
( would be corrected and a just balance would be made)
 
(

But let us honestly state the facts)
 
( Our America has a bad name for superficialness)
 
( Great men great nations have not
been boasters and buffoons)
 
( but perceivers of the terror of life)
 
( and have manned themselves to face it)
 
( The Spartan
embodying his religion in his country)
 
( dies before its majesty without a question)
 
(The Turk who believes his doom is
written on the iron leaf in the moment when he entered the world)
 
( rushes on the enemys sabre with undivided will)
 
 
(

On two days it steads not to run from thy grave)
 
(
The appointed and the unappointed day)
 
(
On the first neither balm nor physician can save)
 
(
Nor thee on the second the Universe slay)
 
(

The Hindoo under the wheel is as firm)
 
 
( had something of the same dignity)
 
( They
felt that the weight of the Universe held them down to their place)
 
( What could they do)
 
( Wise men feel that there is
something)
 
( which cannot be talked or voted away)
 
( a strap or belt which girds the world)
 
(

The Destiny minister general)
 
(
That executeth in the world over all)
 
(
The purveyance which God hath seen beforne)
 
(
So strong it is that tho the world had sworn)
 
(
The contrary of a thing by yea or nay)
 
(
Yet sometime it shall fallen on a day)
 
(
That falleth not oft in a thousand year)
 
(
For certainly our appetites here)
 
(
Be it of war or peace or hate or love)
 
(
All this is ruled by the sight above)
 
(
The Greek Tragedy expressed the same sense)
 
( Whatever is fated that will take place)
 
( The great immense mind of Jove is
not to be transgressed)
 
( Savages cling to a local god of one tribe or town)
 
( The broad ethics of Jesus were quickly narrowed to
village theologies)
 
( which preach an election or favoritism)
 
( And now and then an amiable parson)
 
( like Jung Stilling or
Robert Huntington)
 
( believes in a pistareen-Providence)
 
( which whenever the good man wants a dinner)
 
( makes that
somebody shall knock at his door)
 

 
(But Nature is no sentimentalist)
 
( does not cosset or pamper us)
 
(
We must see that the world is rough and surly)
 
( and will not mind drowning a man or a woman)
 
( but swallows your ship like
a grain of dust)
 
( The cold inconsiderate of persons tingles your blood)
 
( benumbs your feet freezes a man like an apple)
 
(The
diseases the elements fortune gravity lightning respect no persons)
 
( The way of Providence is a little rude)
 
( The habit of
snake and spider)
 
( the snap of the tiger and other leapers and bloody jumpers)
 
(the crackle of the bones of his prey in the coil
of the anaconda)
 
( these are in the system)
 
(and our habits are like theirs)
 
( You have just dined)
 
( and however scrupulously
the slaughter-house is concealed in the graceful distance of miles)
 
( there is complicity expensive races)
 
( race living at
the expense of race)
 
( The planet is liable to shocks from comets)
 
( perturbations from planets)
 
( rendings from earthquake and
volcano)
 
( alterations of climate)
 
( precessions of equinoxes)
 
( Rivers dry up by opening of the forest)
 
( The sea changes its bed)
 
(
Towns and counties fall into it)
 
( At Lisbon an earthquake killed men like fliesç At Naples three years ago ten thousand
persons were crushed in a few minutes)
 
(The scurvy at sea)
 
( the sword of the climate in the west of Africa)
 
(at Cayenne at
Panama at New Orleans cut off men like a massacre)
 
( Our western prairie shakes with fever and ague)
 
 
( have proved as mortal to some tribes as a frost to the crickets )
 
(which having filled the summer with noise are
silenced by a fall of the temperature of one night)
 
( Without uncovering what does not concern us )
 
(or counting how many
species of parasites hang on a bombyx)
 
( or groping after intestinal parasites )
 
(or infusory biters or the obscurities of
alternate generation)
 
(the forms of the shark the labrus the jaw of the sea-wolf )
 
(paved with crushing teeth the weapons)
 
(
of the grampus and other warriors hidden in the sea )
 
( are hints of ferocity in the interiors of nature )
 
(Let us not deny it up
and down )
 
(Providence has a wild rough incalculable road to its end )
 
(and it is of no use to try to whitewash)
 
( its huge mixed
instrumentalities )
 
(or to dress up that terrific benefactor in a clean shirt and white neckcloth of a student in divinity)
 
(

Will you say the disasters which threaten mankind are exceptional )
 
(and one need not lay his account for cataclysms every
day)
 
( Aye but what happens once may happen again )
 
(and so long as these strokes are not to be parried by us they must be
feared)
 
(

But these shocks and ruins are less destructive to us )
 
(than the stealthy power of other laws which act on us daily )
 
(An
expense of ends to means is fateorganization tyrannizing over character )
 
(The menagerie or forms and powers of the
spine is a book of fate)
 
( the bill of the bird the skull of the snake )
 
(determines tyrannically its limits )
 
(So is the scale of races
of temperaments )
 
(so is sex so is climate so is the reaction of talents )
 
(imprisoning the vital power in certain directions)
 
(
Every spirit makes its house but afterwards the house confines the spirit)
 
(

The gross lines are legible to the dull)
 
( the cabman is phrenologist so far)
 
( he looks in your face to see if his shilling is sure)
 
( A
dome of brow denotes one thing )
 
(a pot-belly another a squint a pug-nose mats of hair the pigment of the epidermis
betray character )
 
(People seem sheathed in their tough organization )
 
(Ask Spurzheim ask the doctors )
 
(ask Quetelet if
temperaments decide nothing)
 
( or if there be any-thing they do not decide)
 
( Read the description in medical books of the
four temperaments)
 
( and you will think you are reading your own thoughts )
 
(which you had not yet told )
 
(Find the part which
black eyes and which blue eyes )
 
(play severally in the company )
 
(How shall a man escape from his ancestors )
 
(or draw off
from his veins the black drop which he drew from his fathers or his mothers life)
 
( It often appears in a family as if all the
qualities of the progenitors )
 
(were potted in several jars)
 
(some ruling quality in each son or daughter of the houseand)
 
(
sometimes the unmixed temperament the rank unmitigated elixir)
 
( the family vice is drawn off in a separate individual)
 
(
and the others are proportionally relieved )
 
(We sometimes see a change of expression in our companion )
 
(and say his father
or his mother comes to the windows of his eyes )
 
(and sometimes a remote relative )
 
(In different hours a man represents each
of several of his ancestors )
 
(as if there were seven or eight of us)
 
( rolled up in each mans skin)
 
( seven or eight ancestors at
least)
 
(and they constitute the variety of notes )
 
(for that new piece of music which his life is )
 
(At the corner of the street you
read the possibility of each passenger )
 
(in the facial angle in the complexion in the depth of his eye )
 
(His parentage
determines it )
 
(Men are what their mothers made them )
 
(You may as well ask a loom which weaves huckaback why it does
not make cashmere as expect poetry from this engineer )
 
(or a chemical discovery from that jobber )
 
(Ask the digger in the
ditch to explain Newtons laws)
 
( the fine organs of his brain have been pinched by overwork)
 
( and squalid poverty from father
to son for a hundred years )
 
(When each comes forth from his mothers womb )
 
(the gate of gifts closes behind him )
 
(Let him
value his hands and feet he has but one pair )
 
(So he has but one future and that is already predetermined in his lobes )
 
(and
described in that little fatty face pig-eye and squat form )
 
(All the privilege and all the legislation of the world )
 
(cannot
meddle or help to make a poet or a prince of him)
 
(

Jesus said \When he looketh on her he hath committed adultery)
 
( But he is an adulterer before he has yet looked on the
woman)
 
( by the superfluity of animal and the defect of thought in his constitution)
 
( Who meets him or who meets her )
 
(in
the street sees that they are ripe to be each others victim)
 
(

In certain men digestion and sex )
 
(absorb the vital force and the stronger these are the individual is so much weaker )
 
(The
more of these drones perish the better for the hive )
 
(If later they give birth to some superior individual with force enough)
 
(
to add to this animal a new aim and a complete apparatus)
 
( to work it out all the ancestors are gladly forgotten )
 
(Most men
and most women are merely one couple more )
 
(Now and then one has a new cell or camarilla opened in his brain)
 
(an
architectural a musical or a philological knack )
 
(some stray taste or talent for flowers or chemistry or pigments or
story-telling )
 
(a good hand for drawing a good foot for dancing an athletic frame for wide journeying )
 
(which skill
nowise alters rank in the scale of nature)
 
( but serves to pass the time the life of sensation going on as before )
 
(At last these
hints and tendencies )
 
(are fixed in one or in a succession )
 
(Each absorbs so much food and force as to become itself a new
centre )
 
(The new talent draws off so rapidly the vital force )
 
(that not enough remains for the animal functions hardly
enough )
 
(for health so that in the second generation )
 
(if the like genius appear the health is visibly deteriorated and the
generative force impaired)
 
(

People are born with the moral or with the material biasuterine brothers with this diverging destination)
 
( and I
suppose with high magnifiers Mr Frauenhofer )
 
(or Dr Carpenter might come to distinguish in the embryo at the fourth
day )
 
(this is a Whig and that a Free-soiler)
 
(

It was a poetic attempt to lift this mountain of Fate)
 
( to reconcile this despotism of race with liberty which led the Hindoos)
 
(
to say Fate is nothing but the deeds committed in a prior state of existence)
 
( I find the coincidence of the extremes of
eastern and western speculation )
 
(in the daring statement of Schelling there is in every man a certain feeling )
 
(that he has
been what he is from all eternity and by no means became such in time)
 
( To say it less sublimelyin the history of the
individual )
 
(is always an account of his condition and he knows himself )
 
(to be a party to his present estate)
 
(

A good deal of our politics is physiological )
 
(Now and then a man of wealth in the heyday of youth )
 
(adopts the tenet of
broadest freedom )
 
(In England there is always some man of wealth and large connection)
 
(planting himself during all his
years of health )
 
(on the side of progress who as soon )
 
(as he begins to die checks his forward play calls in his troops and
becomes conservative )
 
(All conservatives are such from personal defects )
 
(They have been effeminated by position or nature)
 
(
born halt and blind through luxury of their parents )
 
(and can only like invalids act on the defensive )
 
(But strong natures
backwoodsmen New Hampshire giants Napoleons Burkes Broughams )
 
(Websters Kossuths are inevitable patriots until
their life ebbs )
 
(and their defects and gout palsy and money warp them)
 
(

The strongest idea incarnates itself in majorities and nations in the healthiest and strongest )
 
(Probably the election goes
by avoirdupois weight )
 
(and if you could weigh bodily the tonnage of any hundred of the Whig and the Democratic party )
 
(in
a town on the Dearborn balance )
 
(as they passed the hayscales you could predict with certainty which party would carry it)
 
(
On the whole it would be rather the speediest way of deciding )
 
(the vote to put the selectmen or the mayor and aldermen at
the hayscales)
 
(

In science we have to consider two things power and circumstance )
 
(All we know of the egg from each successive discovery)
 
(
is another vesicle and if after five hundred years )
 
(you get a better observer or a better glass he finds within the last
observed another )
 
(In vegetable and animal tissue it is just alike )
 
(and all that the primary power or spasm operates is
still vesicles vesicles )
 
(Yes but the tyrannical Circumstance )
 
(A vesicle in new circumstances a vesicle lodged in
darkness )
 
(Oken thought became animal in light a plant )
 
(Lodged in the parent animal it suffers changes )
 
(which end in
unsheathing miraculous capability in the unaltered vesicle )
 
(and it unlocks itself to fish bird or quadruped head and
foot eye and claw )
 
(The Circumstance is Nature )
 
(Nature is what you may do There is much you may not )
 
(We have two
things the circumstance )
 
(and the life Once we thought positive power )
 
(was all Now we learn that negative power or
circumstance is half )
 
(Nature is the tyrannous circumstance the thick skull )
 
(the sheathed snake the ponderous rock-like
jaw )
 
(necessitated activity violent direction)
 
( the conditions of a tool like the locomotive )
 
(strong enough on its track but
which can do nothing but mischief off of it )
 
(or skates which are wings on the ice but fetters on the ground)
 
(

The book of Nature is the book of Fate )
 
(She turns the gigantic pagesleaf after leafnever returning one )
 
(One leaf she
lays down a floor of granite then a thousand ages )
 
(and a bed of slate a thousand ages and a measure of coal a thousand
ages)
 
( and a layer of marl and mud)
 
( vegetable forms appear her first misshapen animals )
 
(zoophyte trilobium fish then
sauriansrude forms )
 
(in which she has only blocked her future statue )
 
(concealing under these unwieldly monsters )
 
(the
fine type of her coming king )
 
(The face of the planet cools and dries )
 
(the races meliorate and man is born )
 
(But when a race
has lived its term)
 
( it comes no more again)
 


 
(not the best but the best that could live now)
 
 
(with which victory adheres to one tribe )
 
(and defeat to another is as uniform as the superposition of
strata )
 
(We know in history what weight belongs to race )
 
(We see the English French and Germans planting themselves on
every shore)
 
( and market of America and Australia )
 
(and monopolizing the commerce of these countries )
 
(We like the nervous
and victorious habit of our own branch of the family )
 
(We see how
much will has been expended to extinguish the Jew in vain )
 
(Look at the unpalatable conclusions of Knox in his Fragment
of Races )
 
(a rash and unsatisfactory writer but charged with pungent and unforgetable truths Nature respects race)
 
(Detach a colony from the race and it deteriorates to the crab )
 
(See the
shades of the picture )
 
(They are
ferried over the Atlantic)
 
( and carted over America to ditch and to drudge )
 
(to make corn cheap and then to lie down
prematurely )
 
(to make a spot of green grass on the prairie)
 
(

One more fagot of these adamantine bandages)
 
( is the new science of Statistics It is a rule that the most casual )
 
(and
extraordinary eventsif the basis of population )
 
(is broad enoughbecome matter of fixed calculation )
 
(It would not be safe
to say when a captain like Bonaparte )
 
(a singer like Jenny Lind or a navigator like Bowditch )
 
(would be born in Boston)
 
( but
on a population of twenty or two hundred millions )
 
(something like accuracy may be had)
 
(

                   Everything which pertains to the human species considered as a whole )
 
(belongs to
                    the order of physical facts )
 
(The greater the number of individuals the more does the
                    influence of the individual )
 
(will disappear leaving predominance to a series of general
                    facts )
 
(dependent on causes by which society exists and is preserved Quetelet)
 
(

Tis frivolous to fix pedantically the date of particular inventions )
 
(They have all been invented over and over fifty times)
 
(
Man is the arch machine of which all these shifts )
 
(drawn from himself are toy models )
 
(He helps himself on each
emergency by copying or duplicating his own structure )
 
(just so far as the need is Tis hard to find the right)
 
(the
indisputable inventor )
 
(There are scores and centuries of them )
 
(The air is full of men )
 
(This kind of talent so abounds this
constructive tool-making efficiency)
 
( as if it adhered to the chemic atoms as if the air he breathes )
 
(were made of
Vaucansons Franklins and Watts)
 
(

Doubtless in every million )
 
(there will be an astronomer a mathematician a comic poet a mystic)
 
( No one can read the
history of astronomy )
 
(without perceiving that Copernicus Newton Laplace )
 
(are not new men or a new kind of men but
that Thales Anaximenes Hipparchus Empedocles Aristarchus Pythagoras oEnopides)
 
( had anticipated them each had
the same tense geometrical brain )
 
(apt for the same vigorous computation and logic a mind parallel to the movement of the
world)
 
( The Roman mile probably rested on a measure of a degree of the meridian Mahometan and Chinese )
 
(know what we
know of leap-year of the Gregorian calendar )
 
(and of the precession of the equinoxes )
 
(As in every barrel of cowries brought
to New Bedford there shall be one orangia so there will in a dozen millions of Malays and Mahometans be one or two)
 
(
astronomical skulls )
 
(In a large city the most casual things and things )
 
(whose beauty lies in their casualty are produced)
 
(
as punctually and to order as the bakers muffin)
 
( for breakfast Punch makes exactly one capital joke a week )
 
(and the
journals contrive to furnish one good piece of news every day)
 
(

And not less work the laws of repression )
 
(the penalties of violated functions Famine typhus frost war suicide and effete
races)
 
( must be reckoned calculable parts of the system of the world)
 
(

These are pebbles from the mountain hints of the terms )
 
(by which our life is walled up and which show a kind of
mechanical exactness )
 
(as of a loom or mill in what we call casual or fortuitous events)
 
(

The force with which we resist these torrents of tendency looks so ridiculously inadequate )
 
(that it amounts to little more
than a criticism or a protest made by a minority of one under compulsion of millions)
 
( I seemed in the height of a tempest
to see men overboard struggling in the waves and driven about here)
 
(They glanced intelligently at each other)
 
(
but twas little they could do for one another)
 
( twas much if each could keep afloat alone )
 
(Well they had a right to their
eye-beams and all the rest was Fate)
 
(

We cannot trifle with this reality this cropping-out in our planted gardens of the core of the world)
 
( No picture of life can
have any veracity that does not admit the odious facts )
 
(A mans power is hooped in by a necessity which by many
experiments )
 
(he touches on every side until he learns its arc)
 
(

The element running through entire nature )
 
(which we popularly call Fate is known to us as limitation )
 
(Whatever limits
us we call Fate )
 
(If we are brute and barbarous the fate takes a brute and dreadful shape )
 
(As we refine our checks become
finer)
 
( If we rise to spiritual culture the antagonism takes a spiritual form In the Hindoo fables Vishnu )
 
(follows Maya
through all her ascending changes )
 
(from insect and crawfish up to elephant whatever form)
 
( she took he took the male
form of that kind until she became at last woman and goddess )
 
(and he a man and a god)
 
(The limitations refine as the soul
purifies but the ring of necessity )
 
(is always perched at the top)
 
(

When the gods in the Norse heaven )
 
(were unable to bind the Fenris Wolf with steel )
 
(or with weight of mountainsthe one
he snapped )
 
(and the other he spurned with his heel)
 
(they put round his foot a limp band softer than silk or cobweb and
this held him)
 
(the more he spurned it the stiffer it drew )
 
(So soft and so stanch is the ring of Fate Neither brandy )
 
(nor
nectar nor sulphuric ether nor hell-fire nor ichor )
 
(nor poetry nor genius can get rid of this limp band )
 
(For if we give it
the high sense in which the poets use it )
 
(even thought itself is not above Fate)
 
( that too must act according to eternal laws
and all )
 
(that is wilful and fantastic in it is in opposition to its fundamental essence)
 
(

And last of all high over thought in the world of morals Fate)
 
( appears as vindicator levelling the high lifting the low)
 
(
requiring justice in man and always striking soon or late )
 
(when justice is not done What is useful will last what is
hurtful will sink )
 
(The doer must suffer said the Greeks)
 
( you would soothe a Deity not to be soothed God himself cannot
procure good for the wicked said the Welsh triad )
 
(God may consent but only for a time said the bard of Spain)
 
(The
limitation is impassable by any insight of man)
 
( In its last and loftiest ascensions insight itself and the freedom of the
will is one of its obedient members )
 
(But we must not run into generalizations too large but show the natural bounds)
 
( or
essential distinctions and seek to do justice to the other elements as well)
 
(

Thus we trace Fate in matter mind and moralsin race in retardations of strata and in thought and character as well)
 
(
It is everywhere bound or limitation )
 
(But Fate has its lord limitation its limits is different seen from above and from)
 
(
below from within and from without )
 
(For though Fate is immense so is power which is the other fact in the dual world)
 
(
immense If Fate follows and limits power power attends and antagonizes Fate )
 
(We must respect Fate as natural history
but there is more than natural history )
 
(For who and what is this criticism that pries into the matter)
 
( Man is not order of
nature sack and sack belly and members link )
 
(in a chain nor any ignominious baggage but a stupendous antagonism a
dragging together of the poles of the Universe )
 
(He betrays his relation to what is below himthick-skulled
small-brained fishy quadrumanousquadruped ill-disguised hardly escaped into biped )
 
(and has paid for the new
powers by loss of some of the old ones )
 
(But the lightning which explodes and fashions planets maker of planets and suns is
in him )
 
(On one side elemental order sandstone and granite rock-ledges peat-bog forest sea and shore and on the other
part thought the spirit )
 
(which composes and decomposes naturehere they are side by side god and devil mind and
matter )
 
(king and conspirator belt and spasm riding peacefully together in the eye and brain of every man)
 
(

Nor can he blink the freewill )
 
(To hazard the contradictionfreedom is necessary If you please to plant yourself on the
side of )
 
(Fate and say Fate is all then we say a part of Fate is the freedom of man Forever wells up the impulse of choosing)
 
(
and acting in the soul Intellect annuls Fate )
 
(So far as a man thinks he is free And though nothing is more disgusting
than the crowing about liberty by slaves as most men are and the flippant mistaking for freedom of some paper preamble
like a Declaration of Independence )
 
(or the statute right to vote by those who have never dared to think or to act yet it is
wholesome to man to look not at Fate but the other way)
 
(the practical view is the other His sound relation to these facts is to
use and command not to cringe to them )
 
(Look not on nature for her name is fatal said the oracle )
 
(The too much
contemplation of these limits induces meanness )
 
(They who talk much of destiny their birth-star &c are in a lower
dangerous plane and invite the evils they fear)
 
(

I cited the instinctive and heroic races as proud believers in Destiny )
 
(They conspire with it a loving resignation is with the
event )
 
(But the dogma makes a different impression when it is held by the weak and lazy )
 
(Tis weak and vicious people who
cast the blame on Fate )
 
(The right use of Fate is to bring up our conduct to the loftiness of nature Rude)
 
( and invincible except
by themselves are the elements)
 
( So let man be Let him empty his breast of his windy conceits and show his lordship by
manners and deeds on the scale of nature )
 
(Let him hold his purpose as with the tug of gravitation )
 
(No power no persuasion
no bribe shall make him give up his point )
 
(A man ought to compare advantageously with a river an oak or a mountain )
 
(He
shall have not less the flow the expansion and the resistance of these)
 
(

Tis the best use of Fate to teach a fatal courage)
 
( Go face the fire at sea or the cholera in your friends house or the burglar
in your own or what danger lies in the way of duty knowing you are guarded by the cherubim of Destiny )
 
(If you believe in
Fate to your harm believe it at least for your good)
 
(

For if Fate is so prevailing man also is part of it and can confront fate with fate )
 
(If the Universe have these savage
accidents our atoms are as savage in resistance )
 
(We should be crushed by the atmosphere but for the reaction of the air
within the body )
 
(A tube made of a film of glass can resist the shock of the ocean if filled with the same water )
 
(If there be
omnipotence in the stroke there is omnipotence of recoil)
 
(

1 But Fate against Fate is only parrying and defence)
 
(there are also the noble creative forces )
 
(The revelation of Thought
takes man out of servitude into freedom )
 
(We rightly say of ourselves we were born and afterward we were born again and
many times )
 
(We have successive experiences so important that the new forgets the old and hence the mythology of the
seven or the nine heavens )
 
(The day of days the great day of the feast of life is that in which the inward eye opens to the
Unity in things )
 
(to the omnipresence of lawsees that what is must be and ought to be or is the best )
 
(This beatitude dips
from on high down on us and we see )
 
(It is not in us so much as we are in it If the air come to our lungs we breathe and live
if not we die )
 
(If the light come to our eyes we see else not And if truth come to our mind we suddenly expand to its
dimensions as if we grew to worlds )
 
(We are as lawgivers we speak for Nature we prophesy and divine)
 
(

This insight throws us on the party and interest of the Universe against all and sundry against ourselves as much as
others )
 
(A man speaking from insight affirms of himself what is true of the mind)
 
( seeing its immortality he says I am
immortal seeing its invincibility he says I am strong )
 
(It is not in us but we are in it It is of the maker not of what is
made )
 
(All things are touched and changed by it )
 
(This uses and is not used It distances those who share it from those who
share it not )
 
(Those who share it not are flocks and herds It dates from itselfnot from former men or better mengospel
or constitution or college or custom )
 
(Where it shines Nature is no longer intrusive but all things make a musical or
pictorial impression )
 
(The world of men show like a comedy without laughterpopulations interests government
historytis all toy figures in a toy house It does not overvalue particular truths )
 
(We hear eagerly every thought and
word quoted from an intellectual man )
 
(But in his presence our own mind is roused to activity and we forget very fast
what he says much more interested in the new play of our own thought than in any thought of his )
 
(Tis the majesty into
which we have suddenly mounted the impersonality the scorn of egotisms the sphere of laws that engage us )
 
(Once we
were stepping a little this way and a little that way now we are as men in a balloon and do not think )
 
(so much of the point
we have left or the point we would make as of the liberty and glory of the way)
 
(

Just as much intellect as you add so much organic power He who sees through the design presides over it and must will
that which must be )
 
(We sit and rule and though we sleep our dream will come to pass )
 
(Our thought though it were only
an hour old affirms an oldest necessity not to be separated from thought and not to be separated from will )
 
(They must
always have coexisted )
 
(It apprises us of its sovereignty and godhead which refuse to be severed from it )
 
(It is not mine or
thine but the will of all mind )
 
(It is poured into the souls of all men as the soul itself which constitutes them men I know
not whether there be as is alleged in the upper region of our atmosphere a permanent westerly current )
 
(which carries
with it all atoms which rise to that height but I see that when souls reach a certain clearness of perception they accept a
knowledge and motive above selfishness )
 
(A breath of will blows eternally through the universe of souls in the direction of
the Right and Necessary )
 
(It is the air which all intellects inhale and exhale and it is the wind which blows the worlds into
order and orbit)
 
(

Thought dissolves the material universe by carrying the mind up into a sphere where all is plastic )
 
(Of two men each
obeying his own thought he whose thought is deepest will be the strongest character )
 
(Always one man more than another
represents the will of Divine Providence to the period)
 
(

2 If thought makes free so does the moral sentiment )
 
(The mixtures of spiritual chemistry refuse to be analyzed )
 
(Yet we
can see that with the perception of truth is joined the desire that it shall prevail )
 
(That affection is essential to will
)
 
(Moreover when a strong will appears it usually results from a certain unity of organization as if the whole energy of body
and mind flowed in one direction )
 
(All great force is real and elemental There is no manufacturing a strong will There
must be a pound to balance a pound )
 
(Where power is shown in will it must rest on the universal force Alaric and
Bonaparte must believe they rest on a truth or their will can be bought or bent )
 
(There is a bribe possible for any finite will
)
 
(But the pure sympathy with universal ends is an infinite force and cannot be bribed or bent )
 
(Whoever has had experience
of the moral sentiment cannot choose but believe in unlimited power )
 
(Each pulse from that heart is an oath from the Most
High )
 
(I know not what the word sublime means if it be not the intimations in this infant of a terrific force )
 
(A text of
heroism a name and anecdote of courage are not arguments but sallies of freedom )
 
(One of these is the verse of the Persian
Hafiz Tis written on the gate of Heaven )
 
(Wo unto him who suffers himself to be betrayed by Fate )
 
(Does the reading of
history make us fatalists)
 
(What courage does not the opposite opinion show A little whim of will to be free gallantly
contending against the universe of chemistry)
 
(

But insight is not will nor is affection will )
 
(Perception is cold and goodness dies in wishes as Voltaire said tis the
misfortune of worthy people that they are cowards un des plus grands malheurs des honnetes gens cest quils sont des
lafaches )
 
(There must be a fusion of these two to generate the energy of will )
 
(There can be no driving force except through
the conversion of the man into his will making him the will and the will him )
 
(And one may say boldly that no man has a
right perception of any truth who has not been reacted on by it so as to be ready to be its martyr)
 
(

The one serious and formidable thing in nature is a will )
 
(Society is servile from want of will and therefore the world wants
saviours and religions )
 
(One way is right to go the hero sees it and moves on that aim and has the world under him for root
and support He is to others as the world )
 
(His approbation is honor his dissent infamy )
 
(The glance of his eye has the force
of sunbeams )
 
(A personal influence towers up in memory only worthy and we gladly forget numbers money climate
gravitation and the rest of Fate)
 
(

We can afford to allow the limitation if we know it is the meter of the growing man )
 
(We stand against Fate as children
stand up against the wall in their fathers house and notch their height from year to year )
 
(But when the boy grows to man
and is master of the house he pulls down that wall and builds a new and bigger Tis only a question of time )
 
(Every brave
youth is in training to ride and rule this dragon )
 
(His science is to make weapons and wings of these passions and retarding
forces )
 
(Now whether seeing these two things fate and power we are permitted to believe in unity)
 
( The bulk of mankind
believe in two gods )
 
(They are under one dominion here in the house as friend and parent in social circles in letters in art
in love in religion)
 
( but in mechanics in dealing with steam and climate in trade in politics they think they come under
another and that it would be a practical blunder to transfer the method and way of working of one sphere into the other)
 
(
What good honest generous men at home will be wolves and foxes on change )
 
(What pious men in the parlor will vote for
what reprobates at the polls )
 
(To a certain point they believe themselves the care of a Providence )
 
(But in a steamboat in an
epidemic in war they believe a malignant energy rules)
 
(

But relation and connection are not somewhere and sometimes but everywhere and always )
 
(The divine order does not stop
where their sight stops )
 
(The friendly power works on the same rules in the next farm and the next planet )
 
(But where they
have not experience they run against it and hurt themselves )
 
(Fate then is a name for facts not yet passed under the fire
of thoughtfor causes which are unpenetrated)
 
(

But every jet of chaos which threatens to exterminate us is convertible by intellect into wholesome force )
 
(Fate is
unpenetrated causes )
 
(The water drowns ship and sailor like a grain of dust But learn to swim trim your bark and the
wave which drowned it will be cloven by it and carry it like its own foam a plume and a power )
 
(The cold is inconsiderate
of persons tingles your blood freezes a man like a dew-drop But learn to skate and the ice will give you a graceful sweet
and poetic motion )
 
(The cold will brace your limbs and brain to genius and make you foremost men of time )
 
(Cold and sea
will train an imperial Saxon race which nature cannot bear to lose and after cooping it up for a thousand years in yonder
)
 
(England gives a hundred Englands a hundred Mexicos )
 
(All the bloods it shall absorb and domineer)
 
( and more than
Mexicosthe secrets of water and steam the spasms of electricity the ductility of metals the chariot of the air the
ruddered balloon are awaiting you )
 
(

The annual slaughter from typhus far exceeds that of war but right drainage destroys typhus )
 
(The plague in the
sea-service from scurvy is healed by lemon juice and other diets portable or procurable the depopulation by cholera and
small-pox is ended by drainage and vaccination )
 
(and every other pest is not less in the chain of cause and effect and may
be fought off )
 
(And whilst art draws out the venom it commonly extorts some benefit from the vanquished enemy )
 
(The
mischievous torrent is taught to drudge for man)
 
(the wild beasts he makes useful for food or dress or labor the chemic
explosions are controlled like his watch )
 
(These are now the steeds on which he rides Man moves in all modes by legs of
horses by wings of wind by steam by gas of balloon )
 
(by electricity and stands on tiptoe threatening to hunt the eagle in
his own element )
 
(Theres nothing he will not make his carrier)
 
(

Steam was till the other day the devil which we dreaded )
 
(Every pot made by any human potter or brazier had a hole in its
cover to let off the enemy lest he should lift pot and roof and carry the house away )
 
(But the Marquis of Worcester Watt
and Fulton bethought themselves that where was power was not devil but was God that it must be availed of and not by
any means let off and wasted )
 
(Could he lift pots and roofs and houses so handily he was the workman they were in search
of )
 
(He could be used to lift away chain and compel other devils far more reluctant and dangerous namely cubic miles of
earth mountains weight or resistance of water machinery )
 
(and the labors of all men in the world and time he shall
lengthen and shorten space)
 
(

It has not fared much otherwise with higher kinds of steam )
 
(The opinion of the million was the terror of the world and it
was attempted either to dissipate )
 
(it by amusing nations or to pile it over with strata of societya layer of soldiers )
 
(over
that a layer of lords and a king on the top with clamps and hoops of castles garrisons and police )
 
(But sometimes the
religious principle would get in and burst the hoops and rive every mountain laid on top of it )
 
(The Fultons and Watts of
politics believing in unity saw that it was a power and by satisfying it )
 
(as justice satisfies everybodyvthrough a
different disposition of societygrouping it )
 
(on a level instead of piling it into a mountainthey have contrived to make
of his terror the most harmless and energetic form of a State)
 
(

Very odious I confess are the lessons of Fate Who likes to have a dapper phrenologist pronouncing on his fortunes)
 
( Who
likes to believe that he has hidden in his skull spine and pelvis all the vices of a Saxon or Celtic race )
 
(which will be sure to
pull him downwith what grandeur of hope and resolve )
 
(he is firedinto a selfish huckstering servile dodging
animal)
 
( A learned physician tells us the fact is invariable with the Neapolitan)
 
(that when mature he assumes the forms
of the unmistakable scoundrel)
 
( That is a little overstatedbut may pass)
 
(

But these are magazines and arsenals )
 
(A man must thank his defects and stand in some terror of his talents )
 
(A
transcendent talent draws so largely on his forces as to lame him a defect pays him revenues on the other side )
 
(The
sufferance which is the badge of the Jew has made him in these days the ruler of the rulers of the earth )
 
(If Fate is ore and
quarry if evil is good in the making if limitation is power)
 
( that shall be if calamities oppositions and weights are wings
and meanswe are reconciled)
 
(

Fate involves the melioration )
 
(No statement of the Universe can have any soundness which does not admit its ascending
effort)
 
(The direction of the whole and of the parts is toward benefit and in proportion to the health )
 
(Behind every
individual closes organization)
 
( before him opens libertythe Better the Best )
 
(The first and worst races are dead)
 
( The
second and imperfect races are dying out or remain for the maturing of higher In the latest race in man )
 
(every
generosity every new perception the love and praise he extorts from his fellows)
 
( are certificates of advance out of fate into
freedom )
 
(Liberation of the will from the sheaths and clogs of organization which he has outgrown is the end and aim of
this world )
 
(Every calamity is a spur and valuable hint and where his endeavors do not yet fully avail they tell as
tendency )
 
(The whole circle of animal lifetooth against toothdevouring war war for food a yelp of pain and a grunt of
triumph )
 
(until at last the whole menagerie the whole chemical mass is mellowed and refined for higher use pleases at
a sufficient perspective)
 
(

But to see how fate slides into freedom and freedom into fate observe how far the roots of every creature run or find )
 
(if you
can a point where there is no thread of connection )
 
(Our life is consentaneous and far-related)
 
( This knot of nature is so well
tied that nobody was ever cunning enough to find the two ends )
 
(Nature is intricate overlapped interweaved and endless)
 
(
Christopher Wren said of the beautiful Kings College chapel that if anybody would tell him )
 
(where to lay the first stone
he would build such another )
 
(But where shall we find the first atom in this house of man which is all consent
inosculation and balance of parts)
 
(

The web of relation is shown in habitat shown in hybernation )
 
(When hybernation was observed it was found that )
 
(whilst
some animals became torpid in winter others were torpid in summer hybernation then was a false name )
 
(The long sleep
is not an effect of cold but is regulated by the supply of food proper to the animal )
 
(It becomes torpid when the fruit or prey it
lives on is not in season and regains its activity when its food is ready)
 
(

Eyes are found in light ears in auricular air feet on land fins in water wings in air )
 
(and each creature where it was
meant to be with a mutual fitness )
 
(Every zone has its own Fauna )
 
(There is adjustment between the animal and its food its
parasite its enemy )
 
(Balances are kept It is not allowed to diminish in numbers nor to exceed )
 
(The like adjustments exist
for man )
 
(His food is cooked when he arrives his coal in the pit the house ventilated the mud of the deluge dried his
companions arrived at the same hour and awaiting him with love concert laughter and tears )
 
(These are coarse
adjustments but the invisible are not less )
 
(There are more belongings to every creature than his air and his food )
 
(His
instincts must be met and he has predisposing power)
 
(that bends and fits what is near him to his use )
 
(He is not possible
until the invisible things are right for him as well as the visible )
 
(Of what changes then in sky and earth )
 
(and in finer
skies and earths does the appearance of some Dante or Columbus apprise us)
 
(

How is this effected)
 
( Nature is no spendthrift but takes the shortest way to her ends As the general says to his soldiers)
 
( if
you want a fort build a fort so nature makes every creature do its own work )
 
(and get its livingis it planet animal or
tree )
 
(The planet makes itself The animal cell makes itselfthen what it wants )
 
(Every creaturewren or dragon —
shall make its own lair )
 
(As soon as there is life there is self-direction and absorbing and using of material )
 
(Life is
freedomlife in the direct ratio of its amount )
 
(You may be sure the new-born man is not inert)
 
( Life works both
voluntarily and supernaturally in its neighborhood )
 
(Do you suppose he can be estimated by his weight in pounds or that
he is contained in his skinthis reaching radiating jaculating fellow)
 
( The smallest candle fills a mile with its rays)
 
(
and the papillae of a man run out to every star)
 
(

When there is something to be done the world knows how to get it done )
 
(The vegetable eye makes leaf pericarp root bark
or thorn)
 
( as the need is the first cell converts itself into stomach mouth nose or nail according to the want)
 
( the world
throws its life into a hero or a shepherd and puts him )
 
(where he is wanted Dante and Columbus were Italians in their
timev they would be Russians or Americans to-day )
 
(Things ripen new men come The adaptation is not capricious )
 
(The
ulterior aim the purpose beyond itself the correlation by which planets subside )
 
(and crystallize then animate beasts and
men will not stop but will work into finer particulars and from finer to finest)
 
(

The secret of the world is the tie between person and event Person makes event and event person)
 
( The times the age
what is that but a few profound persons and a few active persons who epitomize the times)
 
(Goethe Hegel Metternich
Adams Calhoun Guizot Peel Cobden Kossuth Rothschild Astor Brunel and the rest)
 
( The same fitness must be
presumed between a man and the time and event as between the sexes or between a race of animals and the food it eats or
the inferior races it uses )
 
(He thinks his fate alien because the copula is hidden )
 
(But the soul contains the event that shall
befall it for the event is only the actualization of its thoughts and what we pray to ourselves for is always granted The
event is the print of your form It fits you like your skin What each does is proper to him Events are the children of his
body and mind We learn that the soul of Fate is the soul of us as Hafiz sings

Alas till now I had not known
My guide and fortunes guide are one

All the toys that infatuate men and which they play forhouses land money luxury power fame are the selfsame
thing with a new gauze or two of illusion overlaid And of all the drums and rattles by which men are made willing to
have their heads broke and are led out solemnly every morning to paradethe most admirable is this by which we are
brought to believe that events are arbitrary and independent of actions At the conjurors we detect the hair by which he
moves his puppet but we have not eyes sharp enough to descry the thread that ties cause and effect)
 
(

Nature magically suits the man to his fortunes by making these the fruit of his character )
 
(Ducks take to the water eagles
to the sky waders to the sea margin hunters to the forest clerks to counting-rooms soldiers to the frontier )
 
(Thus events
grow on the same stem with persons are sub-persons )
 
(The pleasure of life is according to the man that lives it and not
according to the work or the place Life is an ecstasy )
 
(We know what madness belongs to lovewhat power to paint a vile
object in hues of heaven )
 
(As insane persons are indifferent to their dress diet and other accommodations and as we do in
dreams )
 
(with equanimity the most absurd acts so a drop more of wine in our cup of life will reconcile us to strange
company and work )
 
(Each creature puts forth from itself its own condition and sphere as the slug sweats out its slimy
house on the pear-leaf )
 
(and the woolly aphides on the apple perspire their own bed and the fish its shell )
 
(In youth we
clothe ourselves with rainbows )
 
(and go as brave as the zodiac In age we put out another sort of perspirationgout fever
rheumatism caprice doubt fretting and avarice)
 
(

A mans fortunes are the fruit of his character )
 
(A mans friends are his magnetisms )
 
(We go to Herodotus and Plutarch for
examples of Fate but we are examples Quisque suos patimur manes)
 
( The tendency of every man to enact all that is in his
constitution is expressed)
 
( in the old belief that the efforts which we make to escape from our destiny only serve to lead us
into it)
 
(and I have noticed a man likes better to be complimented on his position as the proof of the last or total excellence
than on his merits)
 
(

A man will see his character emitted in the events that seem to meet but which exude from and accompany him )
 
(Events
expand with the character )
 
(As once he found himself among toys so now he plays a part in colossal systems and his
growth is declared in his ambition his companions and his performance )
 
(He looks like a piece of luck but is a piece of
causationthe mosaic angulated and ground to fit into the gap )
 
(he fills Hence in each town there is some man who is in
his brain )
 
(and performance an explanation of the tillage production factories banks churches ways of living )
 
(and
society of that town )
 
(If you do not chance to meet him all that you see will leave you a little puzzled)
 
(if you see him it will
become plain )
 
(We know in Massachusetts who built New Bedford who built Lynn Lowell Lawrence Clinton Fitchburg
Holyoke Portland and many another noisy mart )
 
(Each of these men if they were transparent would seem to you not so
much men as walking cities )
 
(and wherever you put them they would build one)
 
(

History is the action and reaction of these two )
 
(Nature and Thoughttwo boys pushing each other on the curb-stone of
the pavement )
 
(Everything is pusher or pushed and matter and mind are in perpetual tilt and balance so )
 
(Whilst the man
is weak the earth takes up him )
 
(He plants his brain and affections By and by he will take up the earth )
 
(and have his
gardens and vineyards in the beautiful order and productiveness of his thought )
 
(Every solid in the universe is ready to
become fluid on the approach of the mind and the power to flux it is the measure of the mind)
 
(If the wall remain adamant
it accuses the want of thought )
 
(To a subtler force it will stream into new forms expressive of the character of the mind)
 
(
What is the city in which we sit here but an aggregate of incongruous materials which have obeyed the will of some man)
 
(
The granite was reluctant but his hands were stronger )
 
(and it came Iron was deep in the ground and well combined with
stone but could not hide from his fires )
 
(Wood lime stuffs fruits gums were dispersed over the earth and sea in vain)
 
(
Here they are within reach of every mans day-laborwhat he wants of them )
 
(The whole world is the flux of matter over
the wires of thought to the poles or points where it would build )
 
(The races of men rise out of the ground preoccupied with a
thought which rules them and divided into parties ready armed )
 
(and angry to fight for this metaphysical abstraction )
 
(The
quality of the thought differences the Egyptian and the Roman the Austrian and the American )
 
(The men who come on the
stage at one period are all found to be related to each other )
 
(Certain ideas are in the air )
 
(We are all impressionable for we
are made of them all impressionable but some more than others and these first express them )
 
(This explains the curious
contemporaneousness of inventions and discoveries )
 
(The truth is in the air and the most impressionable brain will
announce it first but all will announce it a few minutes later )
 
(So women as most susceptible are the best index of the
coming hour )
 
(So the great man that is the man most imbued with the spirit of the time is the impressionable manof a
fibre irritable and delicate like iodine to light )
 
(He feels the infinitesimal attractions )
 
(His mind is righter than others
because he yields to a current so feeble as can be felt only by a needle delicately poised)
 
(

The correlation is shown in defects Moller in his Essay on Architecture )
 
(taught that the building which was fitted
accurately to answer its end would turn out to be beautiful though beauty had not been intended )
 
(I find the like unity in
human structures rather virulent and pervasive that a crudity in the blood)
 
( will appear in the argument a hump in the
shoulder will appear in the speech and handiwork)
 
(If his mind could be seen the hump would be seen If a man has a
seesaw in his voice it will run into his sentences into his poem into the structure of his fable into his speculation into
his charity And as every man is hunted )
 
(by his own daemon vexed by his own disease this checks all his activity)
 
(

So each man like each plant has his parasites )
 
(A strong astringent bilious nature has more truculent enemies )
 
(than the
slugs and moths that fret my leaves )
 
(Such an one has curculios borers knife-worms a swindler ate him first then a
client then a quack)
 
( then smooth plausible gentlemen bitter and selfish as Moloch )
 
(

This correlation really existing can be divined)
 
( If the threads are there thought can follow and show them Especially
when a soul is quick and docile as Chaucer sings)
 
(

Or if the soul of proper kind)
 
(
Be so perfect as men find)
 
(
That it wot what is to come)
 
(
And that he warneth all and some)
 
(
Of every of their aventures)
 
(
By previsions or figures)
 
(
But that our flesh hath not might)
 
(
It to understand aright)
 
(
For it is warned too darkly —)
 
(

Some people are made up of rhyme coincidence omen periodicity and presage)
 
( they meet the person they seek what their
companion prepares to say to them)
 
( they first say to him and a hundred signs apprise them of what is about to befall)
 
(

Wonderful intricacy in the web wonderful constancy in the design this vagabond life admits )
 
(We wonder how the fly finds
its mate )
 
(and yet year after year we find two men two women without legal or carnal tie )
 
(spend a great part of their best
time within a few feet of each other )
 
(And the moral is that what we seek )
 
(we shall find what we flee from flees from us as)
 
(
Goethe said what we wish for in youth comes in heaps on us in old age )
 
(too often cursed with the granting of our prayer)
 
(
and hence the high caution that since we are sure of having what we wish we beware to ask only for high things)
 
(

One key one solution to the mysteries of human condition )
 
(one solution to the old knots of fate freedom and foreknowledge)
 
(
exists the propounding namely of the double consciousness )
 
(A man must ride alternately on the horses of his private and
his public nature as the equestrians in the circus throw themselves nimbly from horse to horse or plant one foot on the
back of one and the other foot on the back of the other )
 
(So when a man is the victim of his fate has sciatica in his loins and
cramp in his mind a club-foot and a club in his wit a sour face and a selfish temper a strut in his gait and a conceit in
his affection or is ground to powder by the vice of his race he is to rally on his relation to the Universe )
 
(which his ruin
benefits )
 
(Leaving the daemon who suffers he is to take sides with the Deity who secures universal benefit by his pain)
 
(

To offset the drag of temperament and race which pulls down learn this lesson )
 
(namely that by the cunning copresence of
two elements which is throughout nature )
 
(whatever lames or paralyzes you draws in with it the divinity in some form to
repay )
 
(A good intention clothes itself with sudden power)
 
( When a god wishes to ride any chip or pebble will bud and shoot
out winged feet and serve him for a horse)
 
(

Let us build altars to the Blessed Unity )
 
(which holds nature and souls in perfect solution)
 
( and compels every atom to serve
an universal end)
 
(I do not wonder at a snow-flake a shell a summer landscape or the glory of the stars)
 
( but at the
necessity of beauty under which the universe lies that all is and must be pictorial that the rainbow and the curve of the
horizon and the arch of the blue vault are only results from the organism of the eye)
 
( There is no need for foolish amateurs
to fetch me to admire a garden of flowers or a sun-gilt cloud or a waterfall when)
 
( I cannot look without seeing splendor and
grace )
 
(How idle to choose a random sparkle here or there when the indwelling necessity plants the rose of beauty on the
brow of chaos and discloses the central intention of Nature to be harmony and joy)
 
(

Let us build altars to the Beautiful Necessity)
 
( If we thought men were free in the sense that in a single exception one
fantastical will could prevail over the law of things it were all one as if a childs hand could pull down the sun If in the
least particular one could derange the order of nature who would accept the gift of life)
 
(

Let us build altars to the Beautiful Necessity)
 
( which secures that all is made of one piece that plaintiff and defendant
friend and enemy animal and planet food and eater are of one kind In astronomy is vast space but no foreign system in
geology vast time but the same laws as to-day )
 
(Why should we be afraid of Nature)
 
( which is no other than philosophy and
theology embodied)
 
( Why should we fear to be crushed by savage elements we who are made up of the same elements Let
us build to the Beautiful Necessity which makes man brave in believing that he cannot shun a danger that is appointed
nor incur one that is not to the Necessity )
 
(which rudely or softly educates him to the perception that there are no
contingencies that Law rules throughout existence a Law)
 
( which is not intelligent but intelligencenot personal nor
impersonalit disdains words)
 
( and passes understanding it dissolves persons it vivifies nature yet solicits the pure in
heart to draw on all its omnipotence)))



(defvar hamlet-5 '((

Is she to be buried in Christian burial )
 
(when she wilfully seeks her 
own salvation)
 
(
I tell thee she is therefore make her grave straight)
 
(
The crowner hath sate on her and finds it Christian burial)
 
(
How can that be unless she drownd herself in her own defence)
 
(
Why tis found so
It must be se offendendo)
 
( it cannot be else For here lies the point if I 
drown myself wittingly )
 
(it argues an act and an act hath three branches-it is to
act to do and to perform)
 
( argal she drownd herself wittingly
Nay but hear you Goodman Delver)
 
(
Give me leave Here lies the water)
 
( good Here stands the man)
 
( 
If the man go to this water and drown himself it is will he nill he )
 
(he goes-
mark you that But if the water come to him and drown him )
 
(he drowns not
himself Argal he that is not guilty of his own death )
 
(shortens not his own life
But is this law)
 
(
Ay marry ist- crowners quest law)
 
(
Will you ha the truth ant)
 
( If this had not been a gentlewoman she 
should have been buried out o Christian burial)
 
(
Why there thou sayst)
 
( And the more pity that great folk should have
countnance in this world to drown)
 
( or hang themselves more than their 
even Christian Come my spade)
 
( There is no ancient gentlemen but gardners 
ditchers and grave-makers )
 
(They hold up Adams profession
Was he a gentleman)
 
(
A was the first that ever bore arms)
 
(
Why he had none)
 
(
What art a heathen How dost thou understand the Scripture)
 
(
The Scripture says Adam diggd Could he dig without arms)
 
( Ill put another
question to thee)
 
( If thou answerest me not to the purpose confess thyself- 
Go to)
 
(
What is he that builds stronger than either the mason the 
shipwright or the carpenter)
 
(
The gallows-maker for that frame outlives a thousand tenants)
 
(
I like thy wit well in good faith )
 
(The gallows does well But how does
it well It does well to those that do ill )
 
(Now thou dost ill to say the gallows
is built stronger than the church Argal the gallows may do well to thee Tot 
again come)
 
(
Who builds stronger than a mason a shipwright or a carpenter)
 
(
Ay tell me that and unyoke )
 
(
Marry now I can tell)
 
(
Tot
Mass I cannot tell )
 
(
Cudgel thy brains no more about it for your dull ass )
 
(will not mend his 
pace with beating and when you are askd this question )
 
(next say a grave-maker 
The houses he makes lasts till doomsday )
 
(Go get thee to Yaughan fetch
me a stoup of liquor)
 
(

In youth when I did love did love )
 
(
Methought it was very sweet)
 
(
To contract- O- the time for- a- my behove)
 
(
O methought there- a- was nothing- a- meet)
 
(
Has this fellow no feeling of his business that he sings at grave-making)
 
(
Custom hath made it in him a Property of easiness)
 
(
Tis een so The hand of little employment hath the daintier sense)
 
(
But age with his stealing steps)
 
(
Hath clawed me in his clutch)
 
(
And hath shipped me intil the land)
 
(
As if I had never been such)
 
(

That skull had a tongue in it and could sing once How the knave)
 
(
jowls it to the groundas if twere Cains jawbone that did the first murther)
 
(
This might be the pate of a Politician which this ass now oerreaches )
 
(one
that would circumvent God might it not)
 
(
It might my lord )
 
(
Or of a courtier which could say Good morrow sweet lord)
 
(How dost
thou good lord )
 
(This might be my Lord Such-a-one that praisd my Lord)
 
(
Such-a-ones horse when he meant to beg it- might it not)
 
(
Ay my lord)
 
(
Why een so and now my Lady Worms chapless and knockd )
 
(
about the mazzard with a sextons spade Heres fine revolution and we had)
 
(
the trick to seet Did these bones cost no more the breeding but to play at )
 
(loggets with em Mine ache to think ont)
 
(
A pickaxe and a spade a spade)
 
(
For and a shrouding sheet)
 
(
O a Pit of clay for to be made)
 
(
For such a guest is meet)
 
(
Throws up another skull)
 
(
Theres anWhy may not that be the skull of a lawyer )
 
(Where
be his quiddits now his quillets his cases his tenures and his tricks )
 
(Why 
does he suffer this rude knave now to knock him about the sconce )
 
(with a
dirty shovel and will not tell him of his action of battery Hum)
 
( This fellow 
might be ins time a great buyer of land with his statutes his recognizances
his fines his double vouchers his recoveries )
 
(Is this the fine of his fines and
the recovery of his recoveries to have his fine pate full of fine dirt )
 
(Will his 
vouchers vouch him no more of his purchases and double ones too than 
the length and breadth of a pair of indentures )
 
(The very conveyances of his 
lands will scarcely lie in this box and must th inheritor himself have no more ha)
 
(
Not a jot more my lord)
 
(
Is not parchment made of sheepskins)
 
(
Ay my lord And of calveskins too)
 
(
They are sheep and calves which seek out assurance in that I will )
 
(
speak to this fellow Whose graves this sirrah)
 
(

O a pit of clay for to be made)
 
(
For such a guest is meet)
 
(
I think it be thine indeed for thou liest int)
 
(
You lie out ont sir and therefore tis not yours )
 
(For my part I do not 
lie int yet it is mine )
 
(
Thou dost lie int to be int and say it is thine )
 
(Tis for the dead not 
for the quick therefore thou liest)
 
(
Tis a quick lie sir twill away again from me to you)
 
(
What man dost thou dig it for)
 
(
For no man sir)
 
(
What woman then)
 
(
For none neither)
 
(
Who is to be buried int)
 
(
One that was a woman sir but rest her soul shes dead)
 
(
How absolute the knave is  We must speak by the card or )
 
(
equivocation will undo us )
 
(By the Lord Horatio this three years I have 
taken note of it the age is grown so picked that the toe of the peasant )
 
(
comes so near the heel of the courtier he galls his kibe- )
 
(How long hast thou
been a grave-maker)
 
(
Of all the days i th year I came tot that day that our last king )
 
(
Hamlet overcame Fortinbras How long is that since)
 
(
Cannot you tell that Every fool can tell that It was the very day )
 
(
that young Hamlet was born- he that is mad and sent into England)
 
(
Ay marry why was be sent into England)
 
(
Why because a was mad A shall recover his wits there or if a do )
 
(
not tis no great matter there )
 
(Why
Twill not he seen in him there )
 
(There the men are as mad as he
How came he mad)
 
(
Very strangely they say)
 
(
How strangely 
Faith een with losing his wits)
 
(
Upon what ground
Why here in Denmark I have been sexton here man and boy )
 
(
thirty years
How long will a man lie i th earth ere he rot)
 
(
Faith if a be not rotten before a die as we have many pocky corses )
 
(
now-a-days that will scarce hold the laying in I will last you some eight year )
 
(
or nine year A tanner will last you nine year)
 
(
Why he more than another)
 
(
Why sir his hide is so tannd with his trade that a will keep out )
 
(
water a great while and your water is a sore decayer of your whoreson )
 
(
dead body Heres a skull now This skull hath lien you i th earth )
 
(
three-and-twenty years)
 
(
Whose was it
A whoreson mad fellows it was )
 
(Whose do you think it was)
 
(
Nay I know not)
 
(
A pestilence on him for a mad rogue )
 
(A pourd a flagon of Rhenish 
on my head once )
 
(This same skull sir was Yoricks skull the Kings jester )
 
(
This 
Een that
Let me see Takes the skull )
 
(Alas poor Yorick I knew him )
 
(
A fellow of infinite jest of most excellent fancy )
 
(He hath borne me 
on his back a thousand tunes )
 
(And now how abhorred in my imagination 
it is)
 
( My gorge rises at it )
 
(Here hung those lips that I have kissd I know not
how oft )
 
(Where be your gibes now your gambols your songs )
 
(your 
flashes of merriment that were wont to set the table on a roar )
 
(Not one 
now to mock your own grinning)
 
( Quite chap- falln Now get you to my 
ladys chamber )
 
(and tell her let her paint an inch thick to this favour she 
must come Make her laugh at that Prithee)
 
( Horatio tell me one thing )
 
(
Whats that my lord)
 
(
Dost thou think Alexander lookd o this fashion i th earth)
 
(
Een so
And smelt so Pah)
 
(
Puts down the skull)
 
(
Een so my lord)
 
(
To what base uses we may return Horatio)
 
( Why may not imagination
trace the noble dust of Alexander)
 
( till he find it stopping a bunghole )
 
(
Twere to consider too curiously to consider so )
 
(
No faith not a jot but to follow him thither)
 
( with modesty enough 
and likelihood to lead it as thus Alexander died )
 
(Alexander was buried )
 
(
Alexander returneth into dust the dust is earth of earth we make loam 
and why of that loam)
 
( whereto he was converted might they not stop a 
beer barrel)
 
(

                    Imperious Caesar dead and turnd to clay )
 
(
                    Might stop a hole to keep the wind away )
 
(
                    O that that earth which kept the world in awe )
 
(
                    Should patch a wall t expel the winters flaw)
 
(

But soft  but soft  aside )
 
( Here comes the King- 
And with such maimed rites )
 
(This doth betoken)
 
(
The corse they follow did with desprate hand)
 
(
Fordo it own life Twas of some estate)
 
(
Couch we awhile and mark)
 
(
Retires with Horatio)
 
(
What ceremony else)
 
(
That is Laertes)
 
(
A very noble youth Mark )
 
(
What ceremony else)
 
(
Priest Her obsequies have been as far enlargd)
 
(
As we have warranty Her death was doubtful)
 
(
And but that great command oersways the order)
 
(
She should in ground unsanctified have lodgd)
 
(
Till the last trumpet For charitable prayers)
 
(
Shards flints and pebbles should be thrown on her )
 
(
Yet here she is allowd her virgin crants)
 
(
Her maiden strewments and the bringing home)
 
(
Of bell and burial
Must there no more be done )
 
(
Priest No more be done)
 
(
We should profane the service of the dead)
 
(
To sing a requiem and such rest to her )
 
(
As to peace-parted souls )
 
(
Lay her i th earth)
 
(
And from her fair and unpolluted flesh )
 
(
May violets spring  I tell thee churlish priest)
 
(
A ministring angel shall my sister be )
 
(
When thou liest howling)
 
(
What the fair Ophelia )
 
(
Queen Sweets to the sweet  Farewell)
 
(
Scatters flowers)
 
(
I hopd thou shouldst have been my Hamlets wife)
 
(
I thought thy bride-bed to have deckd sweet maid)
 
(
And not have strewd thy grave)
 
(
O treble woe 
Fall ten times treble on that cursed head)
 
(
Whose wicked deed thy most ingenious sense)
 
(
Deprivd thee of  Hold off the earth awhile)
 
(
Till I have caught her once more in mine arms)
 
(
Leaps in the grave)
 
(
Now pile your dust upon the quick and dead)
 
(
Till of this flat a mountain you have made)
 
(
T oertop old Pelion or the skyish head)
 
(
Of blue Olympus )
 
(
Comes forward What is he whose grief)
 
(
Bears such an emphasis whose phrase of sorrow)
 
(
Conjures the wandring stars and makes them stand)
 
(
Like wonder-wounded hearers This is I)
 
(
Hamlet the Dane Leaps in after Laertes)
 
(
The devil take thy soul )
 
(
Grapples with him)
 
(
Thou prayst not well)
 
(
I prithee take thy fingers from my throat)
 
(
For though I am not splenitive and rash)
 
(
Yet have I in me something dangerous)
 
(
Which let thy wisdom fear Hold off thy hand )
 
(
King Pluck thein asunder)
 
(
Good my lord be quiet)
 
(
The Attendants part them and they come out of the grave)
 
(
Why I will fight with him upon this theme)
 
(
Until my eyelids will no longer wag)
 
(
Queen O my son what theme)
 
(
I lovd Ophelia Forty thousand brothers)
 
(
Could not with all their quantity of love)
 
(
Make up my sum What wilt thou do for her)
 
(
King O he is mad Laertes)
 
(
Queen For love of God forbear him  )
 
(
Swounds show me what thout do)
 
(
Woot weep woot fight woot fast woot tear thyself )
 
(
Woot drink up esill eat a crocodile )
 
(
Ill dot Dost thou come here to whine)
 
(
To outface me with leaping in her grave)
 
(
Be buried quick with her and so will I)
 
(
And if thou prate of mountains let them throw)
 
(
Millions of acres on us till our ground)
 
(
Singeing his pate against the burning zone )
 
(
Make Ossa like a wart  Nay an thoult mouth)
 
(
Ill rant as well as thou)
 
(
This is mere madness)
 
(
And thus a while the fit will work on him)
 
(
Anon as patient as the female dove)
 
(
When that her golden couplets are disclosd)
 
(
His silence will sit drooping)
 
(
Hear you sir  
What is the reason that you use me thus)
 
(
I lovd you ever But it is no matter )
 
(
Let Hercules himself do what he may)
 
(
The cat will mew and dog will have his day)
 
(I pray thee good Horatio wait upon him)
 
(
To Laertes Strengthen your patience in our last nights speech)
 
(
Well put the matter to the present push-)
 
(
Good Gertrude set some watch over your son-)
 
(
This grave shall have a living monument)
 
(
An hour of quiet shortly shall we see )
 
(
Till then in patience our proceeding be)
 
(

So much for this sir now shall you see the other)
 
(
You do remember all the circumstance)
 
(
Remember it my lord )
 
(
Sir in my heart there was a kind of fighting)
 
(
That would not let me sleep Methought I lay)
 
(
Worse than the mutinies in the bilboes Rashly- )
 
(
And praisd be rashness for it let us know)
 
(
Our indiscretion sometime serves us well)
 
(
When our deep plots do pall and that should learn us)
 
(
Theres a divinity that shapes our ends)
 
(
Rough-hew them how we will- )
 
(
That is most certain)
 
(
Up from my cabin 
My sea-gown scarfd about me in the dark )
 
(
Gropd I to find out them had my desire )
 
(
Fingerd their packet and in fine withdrew )
 
(
To mine own room again making so bold )
 
(
My fears forgetting manners to unseal)
 
(
Their grand commission where I found Horatio)
 
(
an exact command
Larded with many several sorts of reasons)
 
(
Importing Denmarks health and Englands too)
 
(
With hoo  such bugs and goblins in my life-)
 
(
That on the supervise no leisure bated )
 
(
No not to stay the finding of the axe)
 
(
My head should be struck off )
 
(
Ist possible
Heres the commission read it at more leisure)
 
(
But wilt thou bear me how I did proceed)
 
(
I beseech you
Being thus benetted round with villanies )
 
(
Or I could make a prologue to my brains)
 
(
They had begun the play I sat me down)
 
(
Devisd a new commission wrote it fair)
 
(
I once did hold it as our statists do)
 
(
A baseness to write fair and labourd much )
 
(
How to forget that learning but sir now)
 
(
It did me yeomans service Wilt thou know)
 
(
Th effect of what I wrote )
 
(
Ay good my lord
An earnest conjuration from the King )
 
(
As England was his faithful tributary )
 
(
As love between them like the palm might flourish )
 
(
As peace should still her wheaten garland wear)
 
(
And stand a comma tween their amities)
 
(
And many such-like ass of great charge)
 
(
That on the view and knowing of these contents)
 
(
Without debatement further more or less )
 
(
He should the bearers put to sudden death)
 
(
Not shriving time allowd)
 
(
How was this seald )
 
(
Why even in that was heaven ordinant)
 
(
I had my fathers signet in my purse)
 
(
which was the model of that Danish seal)
 
(
Folded the writ up in the form of th other)
 
(
Subscribd it gavet th impression placd it safely)
 
(
The changeling never known Now the next day )
 
(
Was our sea-fight and what to this was sequent )
 
(
Thou knowst already)
 
(
So Guildenstern and Rosencrantz go tot)
 
(
Why man they did make love to this employment )
 
(
They are not near my conscience their defeat )
 
(
Does by their own insinuation grow)
 
(
Tis dangerous when the baser nature comes)
 
(
Between the pass and fell incensed points )
 
(
Of mighty opposites 
Why what a king is this )
 
(
Does it not thinkst thee stand me now upon-)
 
(
He that hath killd my king and whord my mother )
 
(
Poppd in between th election and my hopes)
 
(
Thrown out his angle for my Proper life)
 
(
And with such coznage- ist not perfect conscience)
 
(
To quit him with this arm And ist not to be damnd )
 
(
To let this canker of our nature come)
 
(
In further evil 
It must be shortly known to him from England)
 
(
What is the issue of the business there)
 
(
It will be short the interim is mine)
 
(
And a mans life is no more than to say one)
 
(
But I am very sorry good Horatio)
 
(
That to I forgot myself
For by the image of my cause I see)
 
(
The portraiture of his Ill court his favours)
 
(
But sure the bravery of his grief did put me)
 
(
Into a towring passion
Peace  Who comes here)
 
(
Enter young Osric a courtier)
 
(
Osric Your lordship is right welcome back to Denmark)
 
(
I humbly thank you sir Aside to Horatio Dost know this waterfly)
 
(
Aside to Hamlet No my good lord)
 
(
Aside to Horatio Thy state is the more gracious for tis a )
 
(
vice to know him He hath much land and fertile Let a beast be lord of )
 
(
beasts and his crib shall stand at the kings mess Tis a chough but )
 
(
as I say spacious in the possession of dirt)
 
(
Osric Sweet lord if your lordship were at leisure I should impart a thing to )
 
(
you from his Majesty
I will receive it sir with all diligence of spirit )
 
(
Put your bonnet to his right use Tis for the head)
 
(
Osric I thank your lordship it is very hot)
 
(
No believe me tis very cold the wind is northerly)
 
(
Osric It is indifferent cold my lord indeed)
 
(
But yet methinks it is very sultry and hot for my complexion )
 
(
Osric Exceedingly my lord it is very sultry as twere- I cannot tell how )
 
(
But my lord his Majesty bade me signify to you that he has laid a great )
 
(
wager on your head Sir this is the matter-)
 
(
I beseech you remember)
 
(
Hamlet moves him to put on his hat)
 
(
Osric Nay good my lord for mine ease in good faith )
 
(Sir here is newly 
come to court Laertes believe me an absolute gentleman full of most 
excellent differences of very soft society )
 
(and great showing Indeed 
to speak feelingly of him )
 
(he is the card or calendar of gentry for you 
shall find in him the continent of what part a gentleman )
 
(would see
Sir his definement suffers no perdition in you )
 
(though I know to 
divide him inventorially would dozy th arithmetic of memory and yet but
yaw neither in respect of his quick sail )
 
(But in the verity of extolment I
take him to be a soul of great article )
 
(and his infusion of such dearth 
and rareness as to make true diction of him )
 
(his semblable is his mirror
and who else would trace him his umbrage nothing more
Osric )
 
(Your lordship speaks most infallibly of him)
 
(
The concernancy sir Why do we wrap the gentleman in our 
more rawer breath)
 
( Osric Sir 
Aside to Hamlet Ist not possible to understand in another)
 
(
tongue)
 
( You will dot sir really )
 
(What imports the nomination of this gentleman)
 
(
Osric Of Laertes 
Aside His purse is empty already )
 
(Alls golden words are spent 
Of him sir
Osric I know you are not ignorant- )
 
(
I would you did sir yet in faith if you did it would not much 
approve me )
 
(Well sir 
Osric You are not ignorant of what excellence is- )
 
(
I dare not confess that lest I should compare with him in )
 
(
excellence but to know a man well were to know himself)
 
(
Osric I mean sir for his weapon but in the imputation laid on him by them 
in his meed hes unfellowed )
 
(
Whats his weapon 
Osric Rapier and dagger)
 
(
Thats two of his weapons- but well )
 
(
Osric The King sir hath wagerd with him six Barbary horses )
 
(against the
which he has impond as I take it six French rapiers and poniards )
 
(with 
their assigns as girdle hangers and so )
 
(Three of the carriages in faith are 
very dear to fancy very responsive to the hilts most delicate carriages 
and of very liberal conceit)
 
(
What call you the carriages)
 
(
I knew you must be edified by the )
 
(
margent ere you had done
The carriages sir are the hangers)
 
(
The phrase would be more germane to the matter if we could )
 
(
carry cannon by our sides I would it might be hangers till then )
 
(But on  
Six Barbary horses against six French swords their assigns )
 
(and three 
liberal-conceited carriages thats the French bet against the Danish )
 
(Why is this all impond as you call it
The King sir hath laid that in a dozen passes between yourself)
 
(
and him he shall not exceed you three hits he hath laid on twelve )
 
(for 
nine and it would come to immediate trial if your lordship would vouchsafe 
the answer)
 
(
How if I answer no)
 
(
I mean my lord the opposition of your person in trial)
 
(
Sir I will walk here in the hall If it please his Majesty it is the
breathing time of day with me )
 
(Let the foils be brought the gentleman 
willing and the King hold his purpose I will win for him if I can if not I 
will gain nothing but my shame and the odd hits)
 
(
Shall I redeliver you een so)
 
(
To this effect sir after what flourish your nature will)
 
(
I commend my duty to your lordship)
 
(
Yours yours He does well to commend it himself )
 
(
there are no tongues else fors turn)
 
( This lapwing runs away with the shell on his head)
 
(
He did comply with his dug before he suckd it )
 
(Thus has he 
and many more of the same bevy that I know the drossy age dotes on 
only got the tune of the time and outward habit of encounter- )
 
(a kind of yesty collection which carries them through and
through the 
most fannd and winnowed opinions )
 
(and do but blow them to their 
trial-the bubbles are out )
 
(
Lord My lord his Majesty commended him to you by young Osric )
 
(who
brings back to him that you attend him in the hall )
 
(He sends to know
if your pleasure hold to play with Laertes or that you will take longer time)
 
(
I am constant to my purposes they follow the Kings pleasure )
 
(If 
his fitness speaks mine is ready now or whensoever provided I be so 
able as now)
 
(
Lord The King and Queen and all are coming down
In happy time)
 
(
Lord The Queen desires you to use some gentle entertainment to)
 
(
Laertes before you fall to play )
 
(
She well instructs me )
 
(
You will lose this wager my lord)
 
(
I do not think so Since he went into France)
 
( I have been in 
continual practice I shall win at the odds )
 
(But thou wouldst not think 
how ill alls here about my heart )
 
(But it is no matter)
 
(
Nay good my lord - )
 
(
It is but foolery but it is such a kind of gaingiving as would )
 
(
perhaps trouble a woman )
 
(
If your mind dislike anything obey it I will forestall their repair )
 
(
hither and say you are not fit)
 
(
Not a whit we defy augury theres a special providence in the
fall of a sparrow )
 
(If it be now tis not to come )
 
(if it be not to come it 
will be now)
 
( if it be not now yet it will come the readiness is all)
 
(Since 
no man knows aught of what he leaves what ist to leave betimes )
 
(Let be
Come Hamlet come and take this hand from me)
 
(

Give me your pardon sir I have done you wrong)
 
(
But pardont as you are a gentleman )
 
(
This presence knows
And you must needs have heard how I am punishd)
 
(
With sore distraction What I have done)
 
(
That might your nature honour and exception)
 
(
Roughly awake I here proclaim was madness)
 
(
Wast Hamlet wrongd Laertes Never Hamlet )
 
(
If Hamlet from himself be taken away )
 
(
And when hes not himself does wrong Laertes )
 
(
Then Hamlet does it not Hamlet denies it )
 
(
Who does it then His madness Ift be so)
 
(
Hamlet is of the faction that is wrongd)
 
(
His madness is poor Hamlets enemy )
 
(
Sir in this audience 
Let my disclaiming from a purposd evil)
 
(
Free me so far in your most generous thoughts)
 
(
That I have shot my arrow oer the house )
 
(
And hurt my brother
I am satisfied in nature)
 
(
Whose motive in this case should stir me most)
 
(
To my revenge But in my terms of honour )
 
(
I stand aloof and will no reconcilement )
 
(
Till by some elder masters of known honour )
 
(
I have a voice and precedent of peace )
 
(
To keep my name ungord But till that time)
 
(
I do receive your offerd love like love)
 
(
And will not wrong it )
 
(
I embrace it freely 
And will this brothers wager frankly play)
 
(
Give us the foils Come on)
 
(
Come one for me 
Ill be your foil Laertes In mine ignorance)
 
(
Your skill shall like a star i th darkest night )
 
(
Stick fiery off indeed
You mock me sir )
 
(
No by this bad 
King Give them the foils young Osric Cousin Hamlet)
 
(
You know the wager 
Very well my lord)
 
(
Your Grace has laid the odds o th weaker side)
 
(
King I do not fear it I have seen you both)
 
(
But since he is betterd we have therefore odds )
 
(
This is too heavy let me see another)
 
(
This likes me well These foils have all a length)
 
(
They prepare to play)
 
(
Ay my good lord
Set me the stoups of wine upon that table )
 
(
If Hamlet give the first or second hit)
 
(
Or quit in answer of the third exchange )
 
(
Let all the battlements their ordnance fire )
 
(
The King shall drink to Hamlets better breath)
 
(
And in the cup an union shall he throw)
 
(
Richer than that which four successive kings )
 
(
In Denmarks crown have worn Give me the cups)
 
(
And let the kettle to the trumpet speak )
 
(
The trumpet to the cannoneer without )
 
(
The cannons to the heavens the heaven to earth)
 
(
Now the King drinks to Hamlet Come begin)
 
(
And you the judges bear a wary eye)
 
(
King Stay give me drink Hamlet this pearl is thine )
 
(
Heres to thy health
Give him the cup )
 
(
Ill play this bout first set it by awhile)
 
(
Come They play Another hit What say you)
 
(
A touch a touch I do confesst )
 
(
Our son shall win )
 
(
Hes fat and scant of breath )
 
(
Here Hamlet take my napkin rub thy brows )
 
(
I will my lord I pray you pardon me Drinks )
 
(
Aside It is the poisond cup it is too late )
 
(
I dare not drink yet madam by-and-by)
 
(
Come let me wipe thy face)
 
(
My lord Ill hit him now )
 
(
I do not thinkt)
 
(
Aside And yet it is almost against my conscience )
 
(
Come for the third Laertes  You but dally)
 
(
I pray You Pass with your best violence)
 
(
I am afeard You make a wanton of me)
 
(
Say you so Come on )
 
(
They play 
Nothing neither way)
 
(
Have at you now )
 
(
Laertes wounds Hamlet then in scuffling they change rapiers and Hamlet )
 
(

Nay come  again The Queen falls )
 
(
Look to the Queen there ho  )
 
(
They bleed on both sides How is it my lord)
 
(
How ist Laertes )
 
(
Why as a woodcock to mine own springe Osric )
 
(
I am justly killd with mine own treachery)
 
(
How does the Queen
King She sounds to see them bleed )
 
(
Queen No no  the drink the drink  O my dear Hamlet )
 
(
The drink the drink  I am poisond Dies)
 
(
O villany  Ho  let the door be lockd )
 
(
Treachery  Seek it out)
 
(
It is here Hamlet Hamlet thou art slain)
 
(
No medicine in the world can do thee good)
 
(
In thee there is not half an hour of life)
 
(
The treacherous instrument is in thy hand)
 
(
Unbated and envenomd The foul practice)
 
(
Hath turnd itself on me Lo here I lie)
 
(
Never to rise again Thy mothers poisond)
 
(
I can no more The King the Kings to blame)
 
(
The point envenomd too )
 
(
Then venom to thy workHurts the King)
 
(
All Treason  treason 
King O yet defend me friends  I am but hurt)
 
(
Here thou incestuous murdrous damned Dane)
 
(
Drink off this potion  Is thy union here)
 
(
Follow my mother King dies)
 
(
He is justly servd
It is a poison temperd by himself)
 
(
Exchange forgiveness with me noble Hamlet)
 
(
Mine and my fathers death come not upon thee)
 
(
Nor thine on me  )
 
(
Heaven make thee free of it  I follow thee)
 
(
I am dead Horatio Wretched queen adieu )
 
(
You that look pale and tremble at this chance)
 
(
That are but mutes or audience to this act)
 
(
Had I but time as this fell sergeant Death)
 
(
Is strict in his arrest O  I could tell you-)
 
(
But let it be Horatio I am dead)
 
(
Thou livst report me and my cause aright)
 
(
To the unsatisfied
Never believe it)
 
(
I am more an antique Roman than a Dane)
 
(
Heres yet some liquor left)
 
(
As thart a man 
Give me the cup Let go  By heaven Ill hat)
 
(
O good Horatio what a wounded name)
 
(
Things standing thus unknown shall live behind me )
 
(
If thou didst ever hold me in thy heart)
 
(
Absent thee from felicity awhile)
 
(
And in this harsh world draw thy breath in pain)
 
(
To tell my story March afar off and shot within)
 
(
What warlike noise is this)
 
(
Osric Young Fortinbras with conquest come from Poland)
 
(
To the ambassadors of England gives)
 
(
This warlike volley
O I die Horatio )
 
(
The potent poison quite oercrows my spirit)
 
(
I cannot live to hear the news from England)
 
(
But I do prophesy th election lights)
 
(
On Fortinbras He has my dying voice)
 
(
So tell him with th occurrents more and less)
 
(
Which have solicited- the rest is silence Dies)
 
(
Now cracks a noble heart Good night sweet prince)
 
(
And flights of angels sing thee to thy rest )
 
(
March within
Why does the drum come hither)
 
(
Enter Fortinbras and English Ambassadors with Drum Colours and )
 
(
Fortinbras Where is this sight)
 
(
What is it you will see)
 
(
If aught of woe or wonder cease your search)
 
(
Fortinbras This quarry cries on havoc O proud Death)
 
(
What feast is toward in thine eternal cell)
 
(
That thou so many princes at a shot)
 
(
So bloodily hast struck)
 
(
The sight is dismal)
 
(
And our affairs from England come too late)
 
(
The ears are senseless that should give us bearing)
 
(
To tell him his commandment is fulfilld)
 
(
That Rosencrantz and Guildenstern are dead)
 
(
Where should We have our thanks)
 
(
Not from his mouth)
 
(
Had it th ability of life to thank you)
 
(
He never gave commandment for their death)
 
(
But since so jump upon this bloody question)
 
(
You from the Polack wars and you from England)
 
(
Are here arrivd give order that these bodies)
 
(
High on a stage be placed to the view)
 
(
And let me speak to the yet unknowing world)
 
(
How these things came about So shall You hear)
 
(
Of carnal bloody and unnatural acts)
 
(
Of accidental judgments casual slaughters)
 
(
Of deaths put on by cunning and forcd cause)
 
(
And in this upshot purposes mistook)
 
(
Falln on th inventors heads All this can I)
 
(
Truly deliver
Fortinbras Let us haste to hear it)
 
(
And call the noblest to the audience)
 
(
For me with sorrow I embrace my fortune)
 
(
I have some rights of memory in this kingdom)
 
(
Which now to claim my vantage doth invite me)
 
(
Of that I shall have also cause to speak)
 
(
And from his mouth whose voice will draw on more)
 
(
But let this same be presently performd)
 
(
Even while mens minds are wild lest more mischance)
 
(
 
On plots and errors happen)
 
(
Fortinbras Let four captains)
 
(
Bear Hamlet like a soldier to the stage)
 
(
For he was likely had he been put on)
 
(
To have provd most royally and for his passage)
 
(
The soldiers music and the rites of war)
 
(
Speak loudly for him)
 
(
Take up the bodies Such a sight as this)
 
(
Becomes the field but here shows much amiss)
 
(
Go bid the soldiers shoot)
 
()
 
(
A dead march Exeunt bearing off the bodies after which a peal of ordinance is shot off)))




(defvar kepler '((because the causes of harmonic proportions)
 (might be discovered by us from the divisions of a circle into the equal
  aliquot parts)
 (which are constructed geometrically and scientifically)
 (from the provable regular plane figures)
 (i begin with what must be made known)
 (i have brought forward at this time the conceptual differentiations of
  geometrical matters that)
 (in as far as may be clear from what has been published)
 (have not been known in the case of the solids)
 (all the more so in that besides euclid and his commentator proclus)
 (no one appeared among the ancients)
 (who indicated that he himself had knowledge of these specific
  differentiations of geometrical matters)
 (precisely the distribution of problems into planes)
 (solids and lines of pappus of alexandria)
 (and of the ancients who he followed)
 (was close enough to the quality of conception of one part of the geometrical
  subject arising to be explained)
 (however that treatment is both brief) (and applied to practical questions)
 (pappus makes no mention of theory)
 (but if we do not occupy our whole mind with the theory of this question)
 (we will never be able to understand harmonic ratios)
 (when proclus diadochus had published four books on euclid s)
 (first he
        wanted
        theoretical
        philosophy
        to
        be
        incorporated
        in
        the
        subject
        of
        mathematics
        by
        public
        profession)
 (if he were to have left us his commentaries on the tenth book of euclid)
 (he would both have freed our geometers from stupid ignorance)
 (and would
      have
      reduced
      my
      work
      in
      developing
      the
      differentiations
      of
      geometrical
      things
      in
      solid)
 (for those distinctions of conceptual existences were known well enough by
  him)
 (as is easily seen in the preface to his book)
 (because he established that the principles of the whole essence of
  mathematics)
 (are the same as that principle which also pervades all existence)
 (and that
      everything
      is
      caused
      by
      that
      the
      finite
      and
      the
      infinite
      the
      limited
      and
      the
      unlimited
      the
      limit
      or
      circumscription
      in
      relation
      to
      a
      form)
 (and the
      knowledge
      of
      the
      unlimited
      in
      relation
      to
      the
      matter
      of
      geometrical
      things)
 (form and proportion are the characteristics of quantities)
 (form of the particular proportion of the combined form is completed by limits
  a straight line by points)
 (a plane surface by lines)
 (a body is limited circumscribed and formed by surfaces)
 (therefore what has been made finite circumscribed and formed can also be
  comprehended by the mind)
 (the infinite
      and
      the
      indeterminate
      cannot
      be
      constrained
      by
      any
      part
      of
      the
      knowledge
      which
      is
      given
      by
      definitions)
 (and by no restraint of proofs)
 (but the figures have prior existence in the archetype then in the work)
 (first they exist in the divine mind)
 (then in created things in the different modes of the subject but nonetheless
  with the same form of its essence)
 (therefore the formation with quantities a certain mental or intellectual
  essence)
 (creates the differentiations of their essence)
 (that is much clearer when derived from proportions)
 (when a form is completed by many limits)
 (it is effected in such a way that the form would make use of proportions
  because of this plurality)
 (but it cannot be possible to know what proportion would be at all without the
  action of the mind)
 (and for
      that
      reason
      the
      person
      who
      gives
      limits
      to
      quantities
      in
      relation
      to
      the
      principle
      of
      essence
      that
      person)
 (asserts that formed quantities have an intellectual essence)
 (but there is no need for argument)
 (proclus whole book should be read it will be evident enough)
 (that he knew the intellectual differentiation of geometrical things in a
  provable way)
 (and yet when this had been confirmed)
 (he did not go off on his own and assert it in isolation)
 (but cried aloud so that the assertion)
 (could not be ignored and so that he might even wake up sleepy minds)
 (his language flows like a flooding river layered thoughout)
 (with the most abundant and abstruse propositions of platonic philosophy)
 (which is this the argument of his whole book)
 (truth has not freed our century from penetrating)
 (to such hidden matters proclus book has been read by pierre ramee)
 (but in what concerns the heart of philosophy)
 (it has been scorned and thrown aside along with euclid s tenth book)
 (and anyone who has written a commentary on euclid)
 (if such were to have been written in his defense)
 (has been ridiculed and ordered to remain silent)
 (the aroused
      wrath
      of
      the
      embittered
      censor
      has
      been
      turned
      on
      euclid
      as
      on
      a
      criminal)
 (the tenth book of euclid) (which when read and understood)
 (may be able to unfold the mysteries of philosophy)
 (has been doomed by savage sentence to not be read)
 (nothing more shameful was ever written by ramee)
 (i ask you to read his words from) (the study of mathematics)
 (stuff he says has been handed down in that tenth book)
 (in such a way that i would never have found the same obscurity in human
  letters)
 (and arts i say obscurity not to be understood) (euclid anticipates that)
 (that could be clear to the illiterate and uneducated who only look at what is
  right in front of their eyes)
 (but in order to investigate and search out what the purpose)
 (and proposed use of the work might be what the classes)
 (types and differences of the subjects might be)
 (i have never read anything more confused and involuted)
 (might not the pythagorean superstition seem to have been drawn into this book
  as if into a pit)
 (by god ramee) (if you would not have believed that this book)
 (may be read with too much ease)
 (you would never have slandered so much obscurity) (you need more work)
 (you need quiet) (you need forethought)
 (and above all you need attentiveness of mind)
 (then you may understand the intent of the writer)
 (with that the good sort of mind will be lifted up to the point)
 (where resolving to live at last in the light of truth)
 (inspired exulting with incredible pleasure it perceives the whole world)
 (and all its different parts most exactly as if from a very high place)
 (but to you you who act in this place as the advocate of ignorance)
 (and of
      the
      common
      man
      seeking
      advantage
      from
      everything
      whether
      divine
      or
      human)
 (i say to you that these matters may be unnatural sophistries to you)
 (euclid will have been quickly and immoderately taken advantage of to you)
 (this subtlety has no place in geometry)
 (let it be your lot to slander what you do not understand)
 (for me who hunts for the causes of things no other path will lead to them
  apart)
 (from that which is in the tenth book of euclid)
 (lazarus schoener followed ramee in his geometry)
 (he confessed that he was not able to see any use in the world f)
 (or the five regular solids) (then he would have read the book i wrote)
 (the secret of the universe)
 (in which i prove that the numbers and distances of the planets)
 (hae been chosen from the five regular solids)
 (now look what injuries professor ramee inflicted on his student schoener)
 (first once ramee had read aristotle)
 (who refuted pythagorean philosophy on the properties of the elements derived
  from the five solids)
 (he at once conceived in his soul)
 (contempt for the whole of the pythagorean philosophy)
 (then when he knows that proclus was part of the pythagorean sect)
 (he used to affirm to his student that he did not believe)
 (what was most true that the ultimate purpose of euclid s book)
 (towards which all the propositions of all the books together are brought
  back)
 (is the five regular solids)
 (this is the origin of ramee s most confident conviction that the five solids
  ought to be removed)
 (from the end of the books of euclid s elements)
 (after the end of the book has been chopped off)
 (like the shell of a levelled building euclid was left)
 (a formless heap of propositions against which)
 (as if against some ghost ramee inveighs in all the 28 books of his study of
  mathematics)
 (speaking with a great harshness and a great rashness)
 (most unbecoming to such a great man schoener followed this conviction of
  ramee)
 (and he himself believed that there is no use for the regular solids at all)
 (but not completely he neglected or refused to follow ramee s judgment on
  proclus)
 (he was able to learn the use of the five solids both in euclid s elements)
 (and in the making of the world from proclus)
 (and the student was much happier than)
 (the professor because he accepted the use of the solids opened)
 (up by me in the making of the world which ramee refused to impress upon him
  from proclus)
 (for what does it matter if the pythagoreans did attribute these figures)
 (to the elements but not to the spheres of the world as i do)
 (ramee would not have offered up one tyrannical word against this whole
  philosophy)
 (he would have exerted himself to have removed this error of theirs in regard
  to the real subject of the figures)
 (as i did) (what if the pythagoreans did teach the same thing that i do)
 (weaving their meaning into a cover of words)
 (may not the copernican form of the world have existed in aristotle)
 (may it not have been incorrectly refuted by him in other words when he would
  call the sun)
 (fire and the moon antichthone)
 (for if the same ordering of the orbits which was known to copernicus)
 (was known to the pythagoreans)
 (if the five solids and the necessity for their five-fold number was known)
 (if they
     continuously
     taught
     that
     the
     five
     solids
     are
     the
     archetypes
     of
     the
     parts
     of
     the
     world
     how
     little
     more
     would
     it
     be
     for
     us
     to
     believe)
 (that the thinking of the pythagoreans has been collected)
 (together secretly by aristotle)
 (but had alredy been refuted by the meaning of the words)
 (when aristotle reads earth)
 (they were giving him cube because they may perhaps have understood saturn)
 (whose orbit is distanced from jupiter by an interposed cube)
 (and the common sort attribute rest to earth)
 (but saturn moving the slowest of all has been marked out as the closest to
  rest for which reason the planet was given the name rest by the hebrews)
 (in the same way aristotle reads octahedron as given to air)
 (when they
   may
   perhaps
   understand
   mercury
   whose
   orbit
   is
   contained
   by
   an
   octahedron)
 (and mercury is no less fast) (certainly the fastest of all the planets)
 (than the air is mobile) (mars was perhaps worked in with the word fire)
 (the name
      for
      this
      planet
      is
      pyrois
      which
      is
      derived
      from
      fire
      and
      a
      tetrahedron
      was
      given
      to
      it
      perhaps
      because
      the
      orbit
      of
      this
      planet
      has
      been
      enclosed
      by
      this
      figure)
 (and water
      could
      have
      concealed
      the
      star
      of
      venus
      to
      which
      the
      icosahedron
      was
      allotted)
 (because the orbit is contained by an icosahedron)
 (because fluids are subject to venus)
 (and she herself was said to have been born from the ocean spray)
 (whence the name aphrodite)
 (and lastly the sound of the word world could mean)
 (earth and the dodecahedron be allotted to the world because its orbit is
  contained by this figure)
 (separated into twelve longitudinal parts)
 (so that this figure is contained by twelve planes for the whole orbit)
 (agreement has therefore been reached that in the mysteries of the
  pythagoreans)
 (the five
      figures
      have
      not
      been
      distributed
      among
      the
      elements
      as
      aristotle
      believed)
 (but rther among the planets)
 (this is a great confirmation of what proclus handed down)
 (among other things as th purpose of geometry) (and what he would tech)
 (namely how heaven would have accepted harmonious forms for its particular
  parts)
 (although not this purpose) (but injury is what ramee inflicts on us so snell)
 (the most skillful of today s geometers giving open support to ramus)
 (says first that the very division of the unnameables into thirteen different
  types is of no profitable use)
 (in the preface to the problems of ludolph of coellen)
 (i concede that if he should know no use if not in every day life)
 (and if there would be no use for life in the investigations of physics)
 (but why does he not follow proclus)
 (who said he did not know any greater good of geometry than the arts which are
  necessary to life)
 (but then the use of the tenth book)
 (would be clear from the evaluated types of the figures)
 (snell says that all those authors of geometrical works)
 (who do not use the tenth book of euclid)
 (deal with either the problems of lines or solids)
 (and with
      figures
      or
      such
      quantities
      which
      do
      not
      have
      their
      purpose
      within
      themselves
      but
      tend
      toward
      other
      uses)
 (and that they may not be investigated separately from those other purposes)
 (but the regular figures are investigated for intrinsic reasons)
 (they have their own perfection in themselves)
 (and are included among the problems of planes)
 (not withstanding the fact that a solid is enclosed by plane surfaces)
 (and that the most important subject matter of the tenth book concerns planes)
 (why would anything different be mentioned or why are the goods)
 (which codrus does not buy to stuff his belly but cleopatra does to decorate
  her ears)
 (thought so worthless in value has so much torment been fashioned by minds)
 (the unnameables
      are
      offensive
      to
      those
      for
      whom
      this
      question
      must
      be
      defined
      by
      numbers)
 (but i deal with these types by the reasoning of the mind)
 (not by numbers and not by algebra)
 (because there is no work for me in reckoning up the balance sheets of
  merchants)
 (but there is in developing the causes of things it is a common opinion that
  these subtleties must be separated out from the elements of geometry)
 (and must
      be
      stuffed
      away
      in
      the
      archives
      ramee
      s
      altogether
      faithful
      student
      discusses
      that)
 (and he
      does
      not
      perform
      an
      academic
      undertaking
      ramee
      took
      away
      the
      form
      from
      euclid
      s
      construction)
 (and overthrew the crown of the work the five solids)
 (the whole structure wa destroyed after these had been removed)
 (the cracked walls remain standing)
 (the jutting arches left in ruins then snell took away the cement as well)
 (so there has been no use for the solidity of euclid s house cemented
  together)
 (under the five figures) (what a happy comprehension by the student)
 (how correctly he affirmed that he understood euclid by reading ramus)
 (so they think of what was called the elements)
 (because an abundant variety of propositions)
 (problems and theorems is discovered in euclid for all the different kinds and
  quantities of the arts bound up with them)
 (although the book the elements might have been named from its form because a
  subsequent proposition)
 (is always supported by a preceding one right through to the last proposition
  of the last book)
 (what lack could there be of anything prior)
 (they make a forest-ranger or timber merchant out of the architect by thinking
  that euclid)
 (obviously wrote his book)
 (so that it might supply all others only he would have no dwelling of his own
  this is more than enough of these matters for this point)
 (now we must return to the main line of discussion)
 (because i would understand the true and real differentiations of the
  geometrical matters)
 (by which the causes of harmonic proportions must be derived by me)
 (i declare the following to be widely unknown that euclid)
 (who handed on these proportions carefully has been driven away)
 (overwhelmed by the mockery of ramus)
 (and confused by the babbling of the lewd is heard by nobody)
 (or else tells the mysteries of philosophy to the deaf)
 (that proclus who lays bare the mind of euclid digs up the hidden things)
 (and may have been able to make easy again what is difficult to comprehend)
 (was made an object of derision and did not continue his commentary up to the
  tenth book)
 (i saw that all this was to be done to me by me)
 (as i begin i would transcribe those things from euclid s tenth book)
 (which may contribute in an especial way to my present undertaking)
 (i would bring into the light the series of matters in that book separated by
  certain definite divisions)
 (i would show the reasons why some members of the divisions were omitted by
  euclid)
 (then lastly the figures themselves must be discussed)
 (i have been content to simply reference the propositions in those cases which
  were proven clearly by euclid)
 (there are many questions which have been proven by euclid in a different way
  now these must be reworked)
 (or were separated joined together again)
 (or the
     order
     must
     be
     changed
     on
     account
     of
     the
     purpose
     that
     has
     been
     given
     me)
 (namely the comparison of the figures which can be known and those which
  cannot)
 (i have combined the series of definitions)
 (propositions and theorems in numerical order as i did in the dioptics)
 (because of the ease of reference)
 (i have not been accurate in regard to the lemmasd)
 (nor over anxious in regard to names)
 (being more concerned with the constructions themselves)
 (certainly this is not yet geometry in philosophical terms but in this part i
  do discuss the philosophy of geometry)
 (would that i were able to treat still more popularly of geometrical questions
  provided the treatment were clearer and more palpable)
 (but i hope that readers equal to both will think about my work for the good
  both)
 (because i teach geometry popularly)
 (and because
      i
      was
      not
      able
      to
      overcome
      by
      my
      effort
      the
      obscurity
      of
      the
      subject
      matter)
 (finally i give this advice to any people who might be completely unfamiliar
  wityh mathematical questions)
 (carried along by my exposition they should red only the propositions from
  number 30 to the end)
 (and faithfully
      employing
      those
      propositions
      without
      proof
      they
      should
      proceed
      to
      the
      other
      books)
 (especially to the last if such readers were to be terrified by the difficulty
  of the geometrical argument)
 (they might deprive themselves of the most joyful fruit of contemplating the
  harmonies)
 ))


(defvar newton '(
                                                                                       ( The quantity of matter )
 
(is the measure of the same)
 
(arising from its density and bulk conjunctly)
 
(

                    THUS air of double density)
 
(in a double space)
 
(is quadruple in quantity)
 
(in a triple space sextuple in quantity)
 
(The same thing is to be understood of snow)
 
(
                    and fine dust or powders)
 
(that are condensed by compression or liquefaction)
 
(and of all bodies that are by any caused whatever differently condensed)
 
(
                    have no regard in this place to a medium)
 
(if any such there is)
 
(that freely pervades the interstices between the parts of bodies)
 
(t is this quantity that I
                    mean hereafter )
 
(everywhere under the name of body or mass)
 
(And the same is known by the weight of each body)
 
(for it is proportional to the weight)
 
(as I
                    have found by experiments on pendulums)
 
(very accurately made)
 
(which shall be shewn hereafter)
 
(
The quantity of motion is the measure of the same)
 
( arising from the velocity and quantity of matter conjunctly)
 
(

                    The motion of the whole is the sum of the motions of all the parts)
 
(and therefore in a body double in quantity)
 
( with equal velocity)
 
( the motion is double)
 
(
                    with twice the velocity)
 
(it is quadruple)
 
(
                         The vis insita)
 
(or innate force of matter)
 
(is a power of resisting)
 
( by which every body)
 
( as much as in it lies)
 
( endeavours to persevere in its present state)
 
(
                                                                                                                      whether it be of rest)
 
( or of moving uniformly forward in a right line)
 
(

                    This force is ever proportional to the body whose force it is)
 
( and differs nothing from the inactivity of the mass)
 
( but in our manner of conceiving it)
 
( A body
                    from the inactivity of matter)
 
(is not without difficulty put out of its state of rest or motion)
 
( Upon which account this vis insita)
 
( may by a most significant
                    name)
 
( be called vis inertiæ)
 
( or force of inactivity)
 
( But a body exerts this force only)
 
( when another force)
 
( impressed upon it)
 
( endeavours to change its
                    condition)
 
( and the exercise of this force)
 
( may be considered both as resistance and impulse)
 
( it is resistance in so far as the body)
 
( for maintaining its present
                    state)
 
( withstands the force impressed)
 
( it is impulse in so far as the body)
 
( by not easily giving way to the impressed force of another)
 
( endeavours to change
                    the state of that other)
 
( Resistance is usually ascribed to bodies at rest)
 
( and impulse to those in motion)
 
( but motion and rest as commonly conceived)
 
( are only
                    relatively distinguished)
 
( nor are those bodies always truly at rest)
 
(which commonly are taken to be so)
 
(

                                     An impressed force is an action exerted upon a body)
 
( in order to change its state)
 
( either of rest or of moving uniformly forward in a right line)
 
(

                    This force consists in the action only)
 
(and remains no longer in the body when the action is over)
 
( For a body maintains every new state it acquires)
 
( by its vis
                    inertiæ only)
 
( Impressed forces are of different origins as from percussion)
 
( from pressure from centripetal force)
 
(

                                                                     A centripetal force is that by which bodies are drawn or impelled)
 
( or any way tend towards a point as a centre)
 
(

                    Of this sort is gravity)
 
( by which bodies tend to the centre of the earth  magnetism)
 
( by which iron tends to the load-stone)
 
( and that force whatever it is)
 
( by
                    which the planets are perpetually drawn aside)
 
( from the rectilinear motions)
 
( which otherwise they would pursue)
 
( and made to revolve in curvilinear
                    orbits)
 
( A stone whirled about in a sling)
 
( endeavours to recede from the hand that turns it)
 
( and by that endeavour distends the sling and that with so much
                    the greater force)
 
( as it is revolved with the greater velocity)
 
( and as soon as ever it is let go flies away)
 
( That force which opposes itself to this endeavour)
 
(
                    and by which the sling perpetually )
 
(draws back the stone towards the hand)
 
(and retains it in its orbit)
 
( because it is directed to the hand as the centre of the
                    orbit)
 
( I call the centripetal force)
 
( And the thing is to be understood of all bodies)
 
( revolved in any orbits)
 
(They all endeavour to recede from the centres of
                    their orbits)
 
( and were it not for the opposition of a contrary force )
 
(which restrains them to)
 
( and detains them in their orbits)
 
( which I therefore call
                    centripetal)
 
( would fly off in right lines)
 
( with a uniform motion)
 
(A projectile if it was not for the force of gravity)
 
( would not deviate towards the earth)
 
( but
                    would go off from it in a right line)
 
( and that with an uniform motion)
 
( if the resistance of the air was taken away)
 
( It is by its gravity that it is drawn aside)
 
(
                    perpetually from its rectilinear course)
 
( and made to deviate towards the earth more or less)
 
( according to the force of its gravity)
 
( and the velocity of its
                    motion)
 
( The less its gravity is)
 
( for the quantity of its matter)
 
( or the greater the velocity with which it is projected)
 
( the less will it deviate from a
                    rectilinear course)
 
( and the farther it will go)
 
( If a leaden ball)
 
(projected from the top of a mountain )
 
(by the force of gunpowder with a given velocity)
 
(and in a
                    direction parallel to the horizon)
 
( is carried in a curve line to the distance of two miles)
 
( before it falls to the ground)
 
( the same if the resistance of the air
                    were taken away)
 
(with a double or decuple velocity)
 
( would fly twice or ten times as far)
 
( And by increasing the velocity)
 
( we may at pleasure increase the
                    distance )
 
(to which it might be projected)
 
( and diminish the curvature of the line)
 
( which it might describe)
 
( till at last it should fall at the distance )
 
(or even might go quite round the whole earth before it falls)
 
(or lastly so that it might never fall to the earth)
 
( but go forward into the
                    celestial spaces)
 
( and proceed in its motion in infinitum)
 
(And after the same manner that a projectile)
 
( by the force of gravity may be made to revolve in an
                    orbit and go round the whole earth)
 
( the moon also either by the force of gravity)
 
( if it is endued with gravity)
 
( or by any other force that impels it towards
                    the earth)
 
( may be perpetually drawn aside towards the earth)
 
( out of the rectilinear way)
 
( which by its innate force it would pursue)
 
( and would be made to
                    revolve in the orbit which it now describes)
 
( nor could the moon without some such force)
 
( be retained in its orbit)
 
( If this force was too small)
 
(it would not
                    sufficiently turn the moon out of a rectilinear course)
 
( if it was too great it would turn it too much)
 
( and draw down the moon from its orbit towards the earth)
 
(
                    It is necessary that the force be of a just quantity)
 
(and it belongs to the mathematicians to find the force)
 
( that may serve exactly to retain a body in a given
                    orbit)
 
( with a given velocity)
 
( and vice versa to determine the curvilinear way)
 
( into which a body projected from a given place)
 
( with a given velocity may
                    be made to deviate )
 
(from its natural rectilinear way)
 
( by means of a given force)
 
(

                    The quantity of any centripetal force may be considered as of three kinds )
 
(absolute accelerative and motive)
 
(
                    The absolute quantity of a centripetal force )
 
(is the measure of the same proportional to the efficacy of the cause)
 
( that propagates it from the centre)
 
( through
                                                                                                                                                                        the spaces round about)
 
(

                    Thus the magnetic force is greater )
 
(in one load-stone and less in another )
 
(according to their sizes and strength of intensity)
 
( 

                                      The accelerative quantity of a centripetal force is the measure of the same)
 
( proportional to the velocity which it generates in a given time)
 
(

                    Thus the force of the same load-stone is greater at a less distance)
 
( and less at a greater  also the force of gravity )
 
(is greater in valleys less on tops of
                    exceeding high mountains)
 
( and yet less as shall hereafter be shown)
 
( at greater distances from the body of the earth)
 
( but at equal distances it is the same
                    everywhere)
 
( because taking away or allowing for the resistance of the air)
 
( it equally accelerates all falling bodies)
 
( whether heavy or light great or
                    small)
 
(
                                             The motive quantity of a centripetal force)
 
( is the measure of the same proportional to the motion which it generates in a given time)
 
(

                    Thus the weight is greater in a greater body)
 
( less in a less body and in the same body)
 
( it is greater near to the earth)
 
( and less at remoter distances)
 
( This
                    sort of quantity is the centripetency)
 
(or propension of the whole body )
 
(towards the centre as I may say its weight)
 
( and it is always known by the
                    quantity of an equal and contrary force)
 
( just sufficient to hinder the descent of the body)
 
(

                    These quantities of forces)
 
(call by the names of motive)
 
( accelerative and absolute forces)
 
( and consider
                    them with respect to the bodies that tend to the centre)
 
(to the places of those bodies)
 
( and to the centre of force towards which they tend)
 
(I
                    refer the motive force to the body )
 
(as an endeavour and propensity of the whole towards a centre)
 
(arising from the propensities of the several parts taken
                    together)
 
( the accelerative force to the place of the body)
 
( as a certain power or energy diffused from the centre)
 
( to all places around to move the bodies that
                    are in them)
 
( and the absolute force to the centre)
 
( as endued with some cause)
 
( without which those motive forces would not be propagated through the
                    spaces round about)
 
( whether that cause be some central body )
 
(such as is the load-stone in the centre of the magnetic force)
 
( or the earth in the centre of the
                    gravitating force)
 
( or anything else that does not yet appear)
 
( For I here design only to give a mathematical notion of those forces)
 
( without considering their
                    physical causes and seats)
 
(

                    Wherefore the accelerative force )
 
(will stand in the same relation to the motive as celerity does to motion)
 
(  For the quantity of motion arises from the
                    celerity drawn into the quantity of matter  )
 
(and the motive force arises from the accelerative force drawn into the same quantity of matter  )
 
(For the sum of
                    the actions of the accelerative force)
 
( upon the several particles of the body  is the motive force of the whole  )
 
(Hence it is  that near the surface of the earth )
 
(
                    where the accelerative gravity  or force productive of gravity )
 
( in all bodies is the same  )
 
(the motive gravity or the weight is as the body  but if we should
                    ascend)
 
( to higher regions  where the accelerative gravity is less  the weight )
 
(would be equally diminished  and would always be as the product of the body )
 
(
                    by the accelerative gravity  )
 
(So in those regions  where the accelerative gravity is diminished into one half  )
 
(the weight of a body two or three times less 
                    will be four or six times less )
 
(

                    I likewise call attractions and impulses  )
 
(in the same sense  accelerative  and motive )
 
( and use the words attraction  impulse or propensity of any sort)
 
(
                    towards a centre  promiscuously  and indifferently )
 
( one for another  considering those forces not physically )
 
( but mathematically  wherefore  the reader is
                    not to imagine )
 
( that by those words  I anywhere take upon me to define the kind  )
 
(or the manner of any action  )
 
(the causes or the physical reason thereof  or
                    that I attribute forces )
 
( in a true and physical sense  to certain centres)
 
(which are only mathematical points)
 
( when at any time I happen to speak of centres)
 
(                    as attracting  or as endued with attractive powers )
 
(

                                                                                                                                                                                 
                    Hitherto I have laid down the definitions of such words as are less known  )
 
(and explained the sense in which I would have them to be understood in the
                    following discourse )
 
( I do not define time  space  place and motion  as being well known to all )
 
( Only I must observe  that the vulgar conceive those quantities)
 
(
                    under no other notions but from the relation they bear to sensible objects  )
 
(And thence arise certain prejudices  for the removing of which  it will be
                    convenient )
 
(to distinguish them into absolute and relative  true and apparent  mathematical and common )
 
(

                   Absolute  true  and mathematical time  of itself  and from its own nature )
 
(flows equably without regard to anything external  and by another name is
                    called duration)
 
(  relative  apparent  and common time  is some sensible and external )
 
(whether accurate or unequable)
 
( measure of duration by the means of
                    motion  )
 
(which is commonly used instead of true time  such as an hour  a day  a month  a year )
 
(

                    Absolute space  in its own nature  without regard to anything external  remains always similar and immovable  )
 
(Relative space is some movable
                    dimension or measure of the absolute spaces )
 
( which our senses determine by its position to bodies )
 
( and which is vulgarly taken for immovable space  such is
                    the dimension of a subterraneaneous  )
 
(an æreal  or celestial space  determined by its position in respect of the earth  )
 
(Absolute and relative space  are the
                    same in figure and magnitude  )

;;;;;;;;
 
(but they do not remain always numerically the same  )
 
(For if the earth  for instance  moves  a space of our air  )
 
(which
                    relatively and in respect of the earth remains always the same  )
 
(will at one time be one part of the absolute space)
 
( into which the air passes  at another
                    time )
 
(it will be another part of the same  and so  absolutely understood  it will be perpetually mutable )
 
(

                   Place is a part of space which a body takes up  )
 
(and is according to the space  either absolute or relative )
 
(I say  a part of space  not the situation nor the
                    external surface of the body  )
 
(For the places of equal solids are always equal  but their superfices  by reason of their dissimilar figures )
 
( are often unequal )
 
(
                    Positions properly have no quantity  nor are they so much )
 
(the places themselves  as the properties of places  )
 
(The motion of the whole is the same thing
                    with the sum of the motions of the parts  )
 
(that is  the translation of the whole  out of its place )
 
( is the same thing with the sum of the translations of the
                    parts out of their places  )
 
(and therefore the place of the whole is the same thing )
 
(with the sum of the places of the parts  and for that reason  it is internal 
                    and in the whole body )
 
(

                   Absolute motion is the translation of a body from one absolute place into another )
 
( and relative motion  the translation from one relative place into
                    another  )
 
(Thus in a ship under sail )
 
( the relative place of a body is that part of the ship which the body possesses )
 
( or that part of its cavity which the body
                    fills )
 
(and which therefore moves together with the ship )
 
( and relative rest is the continuance of the body in the same part of the ship  )
 
(or of its cavity  )
 
(But
                    real  absolute rest  )
 
(is the continuance of the body in the same part of that immovable space )
 
( in which the ship itself  its cavity  )
 
(and all that it contains  is
                    moved  )
 
(Wherefore if the earth is really at rest  )
 
(the body  which relatively rests in the ship  )
 
(will really and absolutely move with the same velocity)
 
(
                    which the ship has on the earth  )
 
(But if the earth also moves  )
 
(the true and absolute motion of the body will arise)
 
(  partly from the true motion of the earth 
                    in immovable space )
 
( partly from the relative motion of the ship on the earth  )
 
(and if the body moves also relatively in the ship )
 
( its true motion will arise 
                    partly from the true motion of the earth  )
 
(in immovable space  and partly from the relative motions )
 
(as well of the ship on the earth  )
 
(as of the body in the
                    ship  )
 
(and from these relative motions will arise the relative motion of the body on the earth )
 
( As if that part of the earth  )
 
(where the ship is  was truly
                    moved toward the east )
 
( with a velocity of 10010 parts  )
 
(while the ship itself )
 
( with fresh gale  and full sails )
 
( is carried towards the west  with a velocity
                    expressed)
 
( by 10 of those parts  but a sailor walks in the ship towards the east  )
 
(with 1 part of the said velocity  )
 
(then the sailor will be moved truly in
                    immovable space towards the east )
 
( with a velocity of 10001 parts  and relatively on the earth towards the west )
 
(with a velocity of 9 of those parts )
 
(

                    Absolute time  in astronomy  )
 
(is distinguished from relative  by the equation or correlation of the vulgar time )
 
( For the natural days are truly unequal 
                    though they are commonly considered as equal )
 
(and used for a measure of time  )
 
(astronomers correct this inequality for their more accurate deducing of the
                    celestial motions )
 
(It may be  that there is no such thing as an equable motion )
 
( whereby time may be accurately measured  )
 
(All motions may be accelerated
                    and retarded  )
 
(but the true  or equable  )
 
(progress of absolute time is liable to no change )
 
( The duration or perseverance of the existence of things remains the
                    same )
 
( whether the motions are swift or slow  or none at all)
 
(  and therefore )
 
( it ought to be distinguished from what are only sensible measures thereof  and)
 
(
                    out of which we collect it  )
 
(by means of the astronomical equation  The necessity of which equation )
 
( for determining the times of a phænomenon  is evinced)
 
(
                    as well from the experiments of the pendulum clock  )
 
(as by eclipses of the satellites of Jupiter )
 
(

                    As the order of the parts of time is immutable  )
 
(so also is the order of the parts of space  )
 
(Suppose those parts to be moved out of their places  )
 
(and they will
                    be moved if the expression may be allowed)
 
( out of themselves  For times and spaces are )
 
(as it were  the places as well of themselves as of all other things )
 
(
                    All things are placed in time as to order of succession  )
 
(and in space as to order of situation  )
 
(It is from their essence or nature that they are places )
 
( and that
                    the primary places of things should be moveable  is absurd  )
 
(These are therefore the absolute places )
 
( and translations out of those places  are the only
                    absolute motions )
 
(

                    But because the parts of space cannot be seen )
 
( or distinguished from one another by our senses)
 
(  therefore in their stead we use sensible measures of them  )
 
(For
                    from the positions and distances of things from any body considered as immovable  )
 
(we define all places )
 
( and then with respect to such places  )
 
(we estimate
                    all motions  considering bodies as transferred )
 
(from some of those places into others  )
 
(And so  instead of absolute places and motions  we use relative ones  )
 
(and
                    that without any inconvenience in common affairs )
 
( but in philosophical disquisitions  )
 
(we ought to abstract from our senses  and consider things themselves )
 
(
                    distinct from what are only sensible measures of them )
 
( For it may be that there is no body really at rest  )
 
(to which the places and motions of others may be
                    referred )
 
(

                    But we my distinguish rest and motion  )
 
(absolute and relative  one from the other by their properties  causes and effects )
 
( It is a property of rest  that bodies)
 
(
                    really at rest do rest in respect to one another  )
 
(And therefore as it is possible )
 
( that in the remote regions of the fixed stars )
 
( or perhaps far beyond them 
                    there may be some body absolutely at rest )
 
( but impossible to know  from the position of bodies )
 
(to one another in our regions whether any of these do keep)
 
(
                    the same position to that remote body )
 
( it follows that absolute rest cannot be determined from the position of bodies in our regions )
 
(

                    It is a property of motion )
 
( that the parts  which retain given positions to their wholes )
 
( do partake of the motions of those wholes  )
 
(For all the parts of
                    revolving bodies endeavour to recede from the axis of motion  )
 
(and the impetus of bodies moving forward  )
 
(arises from the joint impetus of all the parts )
 
(
                    Therefore  if surrounding bodies are moved  )
 
(those that are relatively at rest within them  )
 
(will partake of their motion )
 
( Upon which account  the true and
                    absolute motion of a body cannot be determined by the translation )
 
(of it from those which only seem to rest )
 
(for the external bodies ought not only to appear)
 
(
                    at rest  but to be really at rest  )
 
(For otherwise all included bodies  beside their translation from near the surrounding ones  )
 
(partake likewise of their true
                    motions  )
 
(and though that translation were not made they would not be really at rest )
 
( but only seem to be so )
 
( For the surrounding bodies stand in the like
                    relation to the surrounded )
 
(as the exterior part of a whole does to the interior  )
 
(or as the shell does to the kernel  )
 
(but  if the shell moves  the kernel will also
                    move )
 
( as being part of the whole  )
 
(without any removal from near the shell )
 
(

                    A property  near akin to the preceding  is this )
 
( that if a place is moved )
 
( whatever is placed therein moves along with it )
 
( and therefore a body  which is
                    moved )
 
(from a place in motion )
 
( partakes also of the motion of its place  )

 
(all motions  from places in motion  )
 
(are no other than parts of)
 
(
                    entire and absolute motions )
 
( and every entire motion is composed of the motion of the body out of its first place  )
 
(and the motion of this place out of its place 
                    and so on  )
 
(until we come to some immovable place  )
 
(as in the before-mentioned example of the sailor  )
 
(Wherefore  entire and absolute motions can be no)
 
(
                    otherwise determined than by immovable places )
 
( and for that reason I did before refer those absolute motions to immovable places  )
 
(but relative ones to
                    movable places )
 
( Now no other places are immovable but those that)
 
(  from infinity to infinity  do all retain the same given position to one another  and upon)
 
(
                    this account must ever remain unmoved  )
 
(and do thereby constitute immovable space )
 
(

                    The causes by which true  )
 
(and relative motions are distinguished  one from the other  )
 
(are the forces impressed upon bodies to generate motion )
 
( True motion
                    is neither generated nor altered )
 
( but by some force impressed upon the body moved )
 
( but relative motion may be generated or altered without any force)
 
(
                    impressed upon the body)
 
(  For it is sufficient only to impress some force on other bodies)
 
( with which the former is compared )
 
( that by their giving way  that
                    relation may be changed )
 
( in which the relative rest or motion of this other body did consist )
 
( Again  true motion suffers always some change from any force)
 
(
                    impressed upon  )
 
(the moving body  but relative motion does not necessarily )
 
(undergo any change by such forces )
 
( For if the same forces are likewise impressed)
 
(
                    on those other bodies  )
 
(with which the comparison is made  )
 
(that the relative position may be preserved )
 
( then that condition will be preserved in which)
 
(
                    the relative motion consists  )
 
(And therefore any relative motion may be changed )
 
(when the true motion remains unaltered  )
 
(and the relative may be
                    preserved when the true suffers some change  )
 
(Upon which accounts  )
 
(true motion does by no means consist in such relations )
 
(

                    The effects which distinguish absolute )
 
(from relative motion are  the forces of receding from the axis of circular motion  )
 
(For there are no such forces in a
                    circular motion )
 
(purely relative  )
 
(but in a true and absolute circular motion  )
 
(they are greater or less )
 
( according to the quantity of the motion )
 
( If a vessel  hung
                    by a long cord  )
 
(is so often turned about that the cord is strongly twisted )
 
( then filled with water  )
 
(and held at rest together with the water  after)
 
(  by the
                    sudden action of another force)
 
(  it is whirled about the contrary way  and while the cord is untwisting itself )
 
( the vessel continues  for some time in this
                    motion  )
 
(the surface of the water will at first be plain )
 
( as before the vessel began to move )
 
( but the vessel  by gradually communicating its motion to the
                    water )
 
( will make it begin sensibly to evolve )
 
( and recede by little and little from the middle )
 
( and ascend to the sides of the vessel  forming itself into a
                    concave figure )
 
(as I have experienced)
 
( and the swifter the motion becomes )
 
( the higher will the water rise )
 
( till at last  performing its revolutions in the
                    same times with the vessel )
 
( it becomes relatively at rest in it  )
 
(This ascent of the water shows its endeavour to recede )
 
(from the axis of its motion  )
 
(and the
                    true and absolute circular motion of the water  )
 
(which is here directly contrary to the relative  )
 
(discovers itself  and may be measured by this endeavour  )
 
(At
                    first  when the relative motion of the water in the vessel was greatest )
 
( it produced no endeavour to recede from the axis  )
 
(the water showed no tendency to
                    the circumference  )
 
(nor any ascent towards the sides of the vessel  but remained of a plain surface )
 
( and therefore its true circular motion had not yet begun )
 
(
                    But afterwards  when the relative motion of the water had decreased )
 
( the ascent thereof towards the sides of the vessel)
 
( proved its endeavour to recede
                    from the axis  and this endeavour)
 
( showed the real circular motion of the water perpetually increasing )
 
( till it had acquired its greatest quantity  )
 
(when the
                    water rested relatively in the vessel  )
 
(And therefore this endeavour  )
 
(does not depend upon any translation of the water in respect of the ambient bodies  )
 
(nor
                    can true circular motion be defined by such translation )
 
( There is only one real circular motion of any one revolving body  )
 
(corresponding to only one power of
                    endeavouring to recede from its axis of motion )
 
( as its proper and adequate effect  but relative motions )
 
( in one and the same body  are innumerable  )
 
(according
                    to the various relations it bears to external bodies )
 
( and like other relations  are altogether destitute of any real effect )
 
( any otherwise than they may
                    partake of that one only true motion )
 
( And therefore in their system who suppose that our heavens )
 
( revolving below the sphere of the fixed stars  carry the
                    planets along with them )
 
( the several parts of those heavens and the planets )
 
( which are indeed relatively at rest in their heavens  do yet really move  )
 
(For
                    they change their position one to another )
 
(which never happens to bodies truly at rest)
 
(  and being carried together with their heavens  )
 
(partake of their
                    motions )
 
( and as parts of revolving wholes  endeavour to recede from the axis of their motions )
 
(

                    Wherefore relative quantities are not the quantities themselves  )
 
(whose names they bear )
 
( but those sensible measures of them)
 
(either accurate or
                    inaccurate)
 
( which are commonly used instead of the measured quantities themselves )
 
( And if the meaning of words is to be determined by their use  then by
                    the names time )
 
( space  place and motion )
 
( their measures are properly to be understood  )
 
(and the expression will be unusual  )
 
(and purely mathematical  )
 
(if the
                    measured quantities themselves are meant  )
 
(Upon which account  they do strain the sacred writings )
 
( who there interpret those words for the measured
                    quantities  )
 
(Nor do those less defile the purity of mathematical and philosophical truths  )
 
(who confound real quantities themselves with their relations
                    and vulgar measures )
 
(

                    It is indeed a matter of great difficulty to discover  )
 
(and effectually to distinguish  )
 
(the true motion of particular bodies from the apparent  because the parts)
 
(
                    of that immovable space )
 
( in which those motions are performed  do by no means come under the observation of our senses  )
 
(Yet the thing is not altogether
                    desperate  for we have some arguments to guide us )
 
( partly from the apparent motions  )
 
(which are the differences of the true motions  partly from the forces )
 
(
                    which are the causes and effects of the true motion  )
 
(For instance  if two globes )
 
( kept at a given distance one from the other by means of a cord that connects
                    them)
 
(  were revolved about their common centre of gravity  we might  from the tension of the cord )
 
( discover the endeavour of the globes to recede from the
                    axis of their motion )
 
( and from thence we might compute the quantity of their circular motions)
 
(  And then if any equal forces should be impressed at once )
 
(on
                    the alternate faces of the globes to augment or diminish their circular motions  )
 
(from the increase or decrease of the tension of the cord )
 
( we might infer the
                    increment or decrement of their motions  )
 
(and thence would be found on what faces those forces ought to be impressed )
 
( that the motions of the globes might be
                    most augmented  that is )
 
( we might discover their hindermost faces  or those which  in the circular motion )
 
( do follow )
 
( But the faces which follow being
                    known and consequently the opposite ones that precede  )
 
(we should likewise know the determination of their motions )
 
( And thus we might find both the
                    quantity and the determination of this circular motion )
 
( even in an immense vacuum  )
 
(where there was nothing external or sensible with which the globes
                    could be compared )
 
( But now  if in that space some remote bodies were placed the kept always a given position one to another)
 
(  as the fixed stars do in our
                    regions  )
 
(we could not indeed determine from the relative translation of the globes among those bodies )
 
( whether the motion did belong to the globes or to the
                    bodies )
 
( But if we observed the cord )
 
( and found that its tension was that very tension which the motions f the globes required  )
 
(we might conclude the motion
                    to be in the globes )
 
( and the bodies to be at rest  )
 
(and then  lastly  from the translation of the globes among the bodies  )
 
(we should find the determination of
                    their motions )
 
( But how we are to collect the true motions from their causes  effects )
 
( and apparent differences  and )
 
( vice versa  how from the motions  either
                    true or apparent  )
 
(we may come to the knowledge of their causes and effects )
 
( shall be explained more at large in the following tract )
 
( For to this end it was
                    that I composed it )
 
(

                            Every body perseveres in its state of rest  or of uniform motion in a right line  )
 
(unless it is compelled to change that state by forces impressed thereon )
 
(

                    PROJECTILES persevere in their motions )
 
( so far as they are not retarded by the resistance of the air)
 
(  or impelled downwards by the force of gravity  )
 
(A top 
                    whose parts by their cohesion are perpetually drawn aside )
 
(from rectilinear motions  does not cease its rotation )
 
( otherwise than as it is retarded by the air )
 
(
                    The greater bodies of the planets and comets  )
 
(meeting with less resistance in more free spaces  )
 
(preserve their motions both progressive and circular for a
                    much longer time )
 
(

                     The alteration of motion is ever proportional to the motive force impressed  )
 
(and is made in the direction of the right line in )
 
(which that force is impressed )
 
(

                    If any force generates a motion )
 
( a double force will generate double the motion  )
 
(a triple force triple the motion  )
 
(whether that force be impressed altogether)
 
(
                    and at once  or gradually and successively )
 
( And this motion )
 
(being always directed the same way with the generating force)
 
( if the body moved before )
 
( is
                    added to or subtracted from the former motion  )
 
(according as they directly conspire with or are directly contrary to each other  )
 
(or obliquely joined  when
                    they are oblique )
 
( so as to produce a new motion compounded from the determination of both )
 
(

                      To every action there is always opposed an equal reaction  )
 
(or the mutual actions of two bodies upon each other are always equal )
 
( and directed to contrary
                                                                                                                                                                                            parts )
 
(

                    Whatever draws or presses another is as much drawn)
 
( or pressed by that other  )
 
(If you press a stone with your finger )
 
( the finger is also pressed by the stone 
                    If a horse draws a stone tied to a rope  )
 
(the horse will be equally drawn back towards the stone  )
 
(for the distended rope  by the same
                    endeavour to relax or unbend itself )
 
( will draw the horse as much towards the stone as it does the stone towards the horse  )
 
(and will obstruct the progress of
                    the one as much as it advances that of the other )
 
(

                    If a body impinges upon another  )
 
(and by its force change the motion of the other  )
 
(that body also became of the quality of  the mutual pressure will
                    undergo an equal change )
 
( in its own motion  towards the contrary part )
 
( The changes made by these actions are equal )
 
( not in the velocities but in the motions
                    of bodies  )
 
(that is to say  if the bodies are not hindered by any other impediments )
 
( For  because the motions are equally changed  the changes of the
                    velocities made towards contrary parts are reciprocally proportional to the bodies  )
 
(This law takes place also in attractions  as will be proved in the next
                    scholium )
 
(

                          A body by two forces conjoined will describe the diagonal of a parallelogram )
 
( in the same time that it would describe the sides  by those forces apart )
 
(

                    If a body in a given time  by the force M impressed apart in the place A )
 
( should with an uniform motion be
                    carried from A to B )
 
( and by the force N impressed apart in the same place  should be carried from A to C )
 
(
                    complete the parallelogram ABCD )
 
( and  by both forces acting together  it will in the same time be carried in
                    the diagonal from A to D  )
 
(For since the force N acts in the direction of the line AC  parallel to BD  this force
                   will not at all alter the velocity generated by the other force M  by which the body is
                    carried towards the line BD  )
 
(The body therefore will arrive at the line BD in the same time )
 
( whether the
                    force N be impressed or not )
 
(and therefore at the end of that time it will be found somewhere in the line BD )
 
(
                    By the same argument  at the end of the same time it will be found somewhere in the line CD  )
 
(Therefore it
                    will be found in the point D  )
 
(where both lines meet  But it will move in a right line from A to D  by Law I )
 
(

                      And hence is explained the composition of any one direct force AD  out of any two oblique forces AC and CD  and  on the contrary  )
 
(the resolution of any one
                                                   direct force AD into two oblique forces AC and CD  which composition and resolution are abundantly confirmed from mechanics )
 
(

                    As if the unequal radii OM and ON drawn from the centre O of any wheel  )
 
(should sustain the weights
                    A and P by the cords MA and NP )
 
( and the forces of those weights to move the wheel were required )
 
(
                    Through the centre O draw the right line KOL )
 
(
                    may find by experiment )
 
(

                    But the weight p pressing upon those two oblique planes )
 
( may be considered as a wedge between the two internal surfaces of a body split by it  and hence
                    the forces of the wedge and the mallet may be determined  )
 
(for because the force with which the weight p presses the plane pQ is to the force with which
                    the same  )
 
(whether by its own gravity  or by the blow of a mallet  is impelled in the direction of the line pH towards both the planes  )
 
(as pN to pH  and to
                    the force with which it presses the other plane pG )
 
( as pN to NH  And thus the force of the screw may be deduced from a like resolution of forces  it being)
 
(
                    no other than a wedge impelled with the force of a lever  Therefore the use of this Corollary spreads far and wide  )
 
(and by that diffusive extent the truth
                    thereof is farther confirmed )
 
( For on what has been said depends the whole doctrine of mechanics variously demonstrated by different authors  )
 
(For from
                    hence are easily deduced the forces of machines )
 
( which are compounded of wheels  pullies  levers  cords  and weights  )
 
(ascending directly or obliquely  and
                    other mechanical powers )
 
( as also the force of the tendons to move the bones of animals )))

(defvar turing '((i propose to consider the question) (can machines think)
 (this should begin with definitions of the meaning of the terms machine and think)
 (the definitions might be framed so as to reflect)
 (so far as possible the normal use of the words) (but this attitude is dangerous)
 (if the meaning of the words machine and think are to be found by examining)
 (how they are commonly used it is difficult to escape the conclusion that the meaning and the
  answer to the question)
 (can machines think) (is to be sought in a statistical survey such as a gallup poll)
 (but this is absurd) (instead of attempting such a definition)
 (i shall replace the question by another) (which is closely related to it)
 (and is expressed in relatively unambiguous words)
 (the new form of the problem can be described in terms of a game)
 (which we call the imitation game) (it is played with three people)
 (the interrogator stays in a room apart from the other two)
 (the object of the game for the interrogator) (is to determine which of the other two)
 (is the man and which is the woman) (he knows them by labels x and y)
 (and at the end of the game he says) (either x is a and y is b or x is b and y is a)
 (the interrogator is allowed to put questions to a and b thus) nil
 (will please tell me the length of his or her hair) (now suppose x is actually a)
 (then a must answer)
 (it is a s object in the game to try and cause c to make the wrong identification)
 (his answer might therefore be)
 (my hair is shingled and the longest strands are about nine inches long)
 (in order that tones of voice may not help the interrogator) (the answers should be written)
 (or better still typewritten)
 (the ideal arrangement is to have a teleprinter communicating between the two rooms)
 (alternatively the question and answers can be repeated by an intermediary)
 (the object of the game for the third player) (is to help the interrogator)
 (the best strategy for her is probably to give truthful answers) (she can add such things as)
 (i am the woman dont listen to him)
 (to her answers but it will avail nothing as the man can make similar remarks)
 (we now ask the question) (what will happen when a machine takes the part of a in this game)
 (will the interrogator decide wrongly as often when the game is played like this)
 (as he does when the game is played between a man and a woman)
 (these questions replace our original) (can machines think)
 (as well as asking what is the answer to this new form of the question)
 (one may ask is this new question a worthy one to investigate)
 (this latter question we investigate without further ado)
 (thereby cutting short an infinite regress) (the new problem has the advantage)
 (of drawing a fairly sharp line between the physical and the intellectual capacities of a man)
 (no engineer or chemist claims to be able to produce a material)
 (which is indistinguishable from the human skin)
 (it is possible that at some time this might be done)
 (but even supposing this invention available we should feel)
 (there was little point in trying to make a thinking machine more human)
 (by dressing it up in such artificial flesh)
 (the form in which we have set the problem reflects this fact in the condition)
 (which prevents the interrogator from seeing or touching the other competitors)
 (or hearing -their voices) (some other advantages of the proposed criterion)
 (may be shown up by specimen questions and answers)
 (please write me a sonnet on the subject of the forth bridge) (count me out on this one)
 (never could write poetry) (add 34957 to 70764) (105621) (do you play  chess)
 (i have k at my k1) (and no other pieces) (you have only k at k6 and r at r1) (it is your move)
 (what do you play) (r-r8 mate) (the question and answer method seems to be suitable f)
 (or introducing almost any one of the fields of human endeavour) (that we wish to include)
 (we do not wish to penalise the machine for its inability to shine in beauty competitions)
 (nor to penalise a man for losing in a race against an aeroplane)
 (the conditions of our game make these disabilities irrelevant) (the witnesses can brag)
 (if they consider it advisable) (as much as they please about their charms)
 (strength or heroism) (but the interrogator cannot demand practical demonstrations)
 (the game may perhaps be criticised)
 (on the ground that the odds are weighted too heavily against the machine)
 (if the man were to try and pretend to be the machine)
 (he would clearly make a very poor showing)
 (he would be given away at once by slowness and inaccuracy in arithmetic)
 (may not machines carry out something which ought to be described)
 (as thinking but which is very different from what a man does)
 (this objection is a very strong one but at least we can say that if nevertheless)
 (a machine can be constructed to play the imitation game satisfactorily)
 (we need not be troubled by this objection)
 (it might be urged that when playing the imitation game) (the best strategy for the machine)
 (may possibly be something other than imitation of the behaviour of a man)
 (this may be but i think it is unlikely that there is any great effect of this kind)
 (in any case there is no intention to investigate here) (the theory of the game)
 (and it will be assumed that the best strategy) (is to try to provide answers that)
 (would naturally be given by a man) (the machines concerned in the game)
 (the question which we put in 1 will not be quite definite)
 (until we have specified what we mean by the word machine)
 (it is natural that we should wish to permit every kind of engineering technique)
 (to be used in our machines) (we also wish to allow the possibility)
 (than an engineer or team of engineers may construct a machine which works)
 (but whose manner of operation cannot be satisfactorily described)
 (by its constructors because they have applied a method which is largely experimental)
 (finally we wish to exclude from the machines) (men born in the usual manner)
 (it is difficult to frame the definitions) (so as to satisfy these three conditions)
 (one might for instance insist that the team of engineers) (should be all of one sex)
 (but this would not really be satisfactory)
 (for it is probably possible to rear a complete individual)
 (from a single cell of the skin of a man) (to do so would be a feat of biological technique)
 (deserving of the very highest praise)
 (but we would not be inclined to regard it as a case of constructing a thinking machine)
 (this prompts us to abandon the requirement that every kind of technique should be permitted)
 (we are the more ready to do so in view of the fact)
 (that the present interest in thinking machines)
 (has been aroused by a particular kind of machine)
 (usually called an electronic computer or digital computer)
 (following this suggestion we only permit digital computers to take part in our game)
 (this restriction appears at first sight to be a very drastic one)
 (i shall attempt to show that it is not so in reality)
 (to do this necessitates a short account of the nature and properties of these computers)
 (it may also be said that this identification of machines with digital computers)
 (like our criterion for thinking) (will only be unsatisfactory if contrary to my belief)
 (it turns out that digital computers are unable to give a good showing in the game)
 (there are already a number of digital computers in working order) (and it may be asked)
 (why not try the experiment straight away)
 (it would be easy to satisfy the conditions of the game)
 (a number of interrogators could be used) (and statistics compiled)
 (to show how often the right identification was given)
 (the short answer is that we are not asking)
 (whether all digital computers would do well in the game)
 (nor whether the computers at present available would do well)
 (but whether there are imaginable computers) (which would do well)
 (but this is only the short answer) (we shall see this question in a different light later)
 (the idea behind digital computers) (may be explained by saying that these machines)
 (are intended to carry out any operations which could be done by a human computer)
 (the human computer) (is supposed to be following fixed rules)
 (he has no authority to deviate from them in any detail)
 (we may suppose that these rules are supplied in a book)
 (which is altered whenever he is put on to a new job)
 (he has also an unlimited supply of paper on which he does his calculations)
 (he may also do his multiplications and additions on a desk machine)
 (but this is not important) (if we use the above explanation as a definition)
 (we shall be in danger of circularity of argument)
 (we avoid this by giving an outline of the means) (by which the desired effect is achieved)
 (a digital computer can usually be regarded) (as consisting of three parts)
 (the store is a store of information) (and corresponds to the human computers paper)
 (whether this is the paper on which he does his calculations)
 (or that on which his book of rules is printed)
 (in so far as the human computer does calculations in his bead)
 (a part of the store will correspond to his memory)
 (the executive
      unit
      is
      the
      part
      which
      carries
      out
      the
      various
      individual
      operations
      involved
      in
      a
      calculation)
 (what these individual operations are will vary from machine to machine)
 (usually fairly lengthy operations can be done such as)
 (in some machines only very simple ones such as) (we have mentioned that the book of rules)
 (supplied to the computer is replaced in the machine by a part of the store)
 (it is then called the table of instructions)
 (it is the duty of the control to see that these instructions are obeyed correctly)
 (and in the right order) (the control is so constructed that this necessarily happens)
 (the information in the store is usually broken up into packets of moderately small size)
 (in one machine for instance a packet might consist of ten decimal digits)
 (numbers are assigned to the parts of the store)
 (in which the various packets of information are stored in some systematic manner)
 (a typical instruction might say) (add the number stored in position 6809 to that in 4302)
 (and put the result back into the latter storage position)
 (needless to say it would not occur in the machine expressed in english)
 (it would more likely be coded in a form such as 6809430217)
 (here 17 says which of various possible operations is to be performed on the two numbers)
 (in this case the operation is that described above viz)
 (it will be noticed that the instruction takes up 10 digits and so forms one packet of
  information very conveniently)
 (the control
      will
      normally
      take
      the
      instructions
      to
      be
      obeyed
      in
      the
      order
      of
      the
      positions
      in
      which)
 (they are stored but occasionally an instruction such as)
 (now obey the instruction stored in position 5606 and continue from there may be encountered)
 (or again
     if
     position
     4505
     contains
     0
     obey
     next
     the
     instruction
     stored
     in
     6707
     otherwise
     continue
     straight
     on)
 (instructions of these latter types are very important because they)
 (make it possible for a sequence of operations to be replaced over and over again until some
  condition is fulfilled)
 (but in doing so to obey not fresh instructions on each repetition but the same ones over and
  over again)
 (to take a domestic analogy)
 (suppose mother wants tommy to call at the cobblers every morning on his way to school)
 (to see if her shoes are done she can ask him afresh every morning)
 (alternatively she can stick up a notice once and for all in the hall which he will see when he
  leaves for school)
 (and which tells him to call for the shoes)
 (and also to destroy the notice when he comes back if he has the shoes with him)
 (the reader must accept it as a fact that digital computers can be constructed)
 (and indeed have been constructed) (according to the principles we have described)
 (and that they can in fact mimic the actions of a human computer very closely)
 (the book of rules which we have described our human computer)
 (as using is of course a convenient fiction)
 (actual human computers really remember what they have got to do)
 (if one wants to make a machine mimic the behaviour of the human computer in some)
 (complex operation one has to ask him how it is done)
 (and then translate the answer into the form of an instruction table)
 (constructing instruction tables is usually described as programming)
 (to programme a machine to carry out the operation)
 (a means to put the appropriate instruction table into the machine so that it will do a)
 (an interesting variant on the idea of a digital computer is a digital computer with a random
  element)
 (these have instructions involving the throwing of a die or some equivalent electronic process
  one such instruction might for instance be)
 (throw the die and put the-resulting number into store 1000)
 (sometimes such a machine is described as having free will)
 (though i would not use this phrase myself)
 (it is not normally possible to determine from observing a machine)
 (whether it has a random element for a similar effect can be produced by such devices as making
  the choices)
 (depend on the digits of the decimal for)
 (most actual digital computers have only a finite store)
 (there is no theoretical difficulty in the idea of a computer with an unlimited store)
 (of course only a finite part can have been used at any one time)
 (likewise only a finite amount can have been constructed)
 (but we can imagine more and more being added as required)
 (such computers have special theoretical interest and will be called infinitive capacity
  computers)
 (the idea of a digital computer is an old one)
 (charles babbage lucasian professor of mathematics at cambridge from 1828 to 1839)
 (planned such a machine) (called the analytical engine but it was never completed)
 (although babbage had all the essential ideas)
 (his machine was not at that time such a very attractive prospect)
 (the speed which would have been available would be definitely faster than a human computer)
 (but something like i 0 times slower than the manchester machine)
 (itself one of the slower of the modern machines) (the storage was to be purely mechanical)
 (using wheels and cards)
 (the fact
      that
      babbages
      analytical
      engine
      was
      to
      be
      entirely
      mechanical
      will
      help
      us
      to
      rid
      ourselves
      of
      a
      superstition)
 (importance is often attached to the fact that modern digital computers are electrical)
 (and that the nervous system also is electrical)
 (since babbages machine was not electrical and since all digital computers are in a sense
  equivalent)
 (we see that this use of electricity cannot be of theoretical importance)
 (of course electricity usually comes in where fast signalling is concerned)
 (so that it is not surprising that we find it in both these connections)
 (in the nervous system chemical phenomena are at least as important as electrical)
 (in certain computers the storage system is mainly acoustic)
 (the feature of using electricity is thus seen to be only a very superficial similarity)
 (if we
     wish
     to
     find
     such
     similarities
     we
     should
     took
     rather
     for
     mathematical
     analogies
     of
     function)
 (the digital computers considered in the last section)
 (may be classified amongst the discrete-state machines)
 (these are the machines which move by sudden jumps or clicks from one quite definite state to
  another)
 (these states are sufficiently different for the possibility of confusion between them to be
  ignored)
 (strictly speaking there are no such machines) (everything really moves continuously)
 (but there are many kinds of machine)
 (which can profitably be thought of as being discrete-state machines)
 (for instance in considering the switches for a lighting system)
 (it is a convenient fiction that each switch) (must be definitely on or definitely off)
 (there must be intermediate positions but for most purposes we can forget about them)
 (as an example of a discrete-state machine we might consider a wheel)
 (which clicks round through 120 once a second)
 (but may be stopped by a ]ever which can be operated from outside)
 (in addition a lamp is to light in one of the positions of the wheel)
 (this machine could be described abstractly as follows) (the internal state of the machine)
 (which is described by the position of the wheel)
 (may be q1 q2 or q3 there is an input signal i0 or i1) (position of ]ever)
 (the internal state at any moment is determined)
 (by the last state and input signal according to the table) (the output signals)
 (the only externally visible indication of the internal state) (are described by the table)
 (this example is typical of discrete-state machines) (they can be described by such tables)
 (provided they have only a finite number of possible states)
 (it will seem that given the initial state of the machine)
 (and the input signals it is always possible to predict all future states)
 (this is reminiscent of laplaces view)
 (that from the complete state of the universe at one moment of time)
 (as described by the positions and velocities of all particles)
 (it should be possible to predict all future states)
 (the prediction which we are considering is however)
 (rather nearer to practicability than that considered by laplace) (the system of the)
 (universe as a whole)
 (is such that quite small errors in the initial conditions can have an overwhelming effect at a
  later time)
 (the displacement of a single electron by a billionth of a centimetre)
 (at one moment might make the difference between a man being killed by an avalanche a year
  later)
 (or escaping)
 (it is an essential property of the mechanical systems which we have called discrete-state
  machines)
 (that this phenomenon does not occur)
 (even when we consider the actual physical machines instead of the idealised machines)
 (reasonably accurate knowledge of the state at one moment yields reasonably accurate knowledge
  any number of steps later)
 (as we have mentioned) (digital computers fall within the class of discrete-state machines)
 (but the number of states of which such a machine is capable is usually enormously large)
 (for instance the number for the machine now working at manchester is about 2 165 0 i e about
  10 50 0)
 (compare this with our example of the clicking wheel described above which had three states)
 (it is not difficult to see why the number of states should be so immense)
 (the computer includes a store corresponding to the paper used by a human computer)
 (it must be possible to write into the store any one of the combinations of symbols)
 (which might have been written on the paper)
 (for simplicity suppose that only digits from 0 to 9 are used as symbols)
 (variations in handwriting are ignored)
 (suppose the computer is allowed 100 sheets of paper each containing 50 lines each with room
  for 30 digits)
 (then the number of states is 10 100x50x30 i e 10 150 0)
 (this is about the number of states of three manchester machines put together)
 (the logarithm
      to
      the
      base
      two
      of
      the
      number
      of
      states
      is
      usually
      called
      the
      storage
      capacity
      of
      the
      machine)
 (thus the manchester machine has a storage capacity of about 165 0 and the wheel machine of our
  example about 1 6)
 (if two
     machines
     are
     put
     together
     their
     capacities
     must
     be
     added
     to
     obtain
     the
     capacity
     of
     the
     resultant
     machine)
 (this leads to the possibility of statements such as)
 (the manchester
      machine
      contains
      64
      magnetic
      tracks
      each
      with
      a
      capacity
      of
      2560
      eight
      electronic
      tubes)
 (with a capacity of 1280)
 (miscellaneous storage amounts to about 300 making a total of 174 380)
 (given the table corresponding to a discrete-state machine it is possible to predict what it
  will do)
 (there is no reason why this calculation should not be carried out by means of a digital
  computer)
 (provided it could be carried out sufficiently quickly the digital computer could mimic the
  behavior of any discrete-state machine)
 (the imitation game could then be played with the machine in question)
 (and the mimicking digital computer) (and the interrogator would be unable to distinguish them)
 (of course the digital computer must have an adequate storage capacity as well as working
  sufficiently fast)
 (moreover it must be programmed afresh for each new machine which it is desired to mimic)
 (this special property of digital computers) (that they can mimic any discrete-state machine)
 (is described by saying that they are universal machines)
 (the existence
      of
      machines
      with
      this
      property
      has
      the
      important
      consequence
      that
      considerations
      of
      speed
      apart)
 (it is unnecessary to design various new machines to do various computing processes)
 (they can all be done with one digital computer) (suitably programmed for each case)
 (it will be seen that as a consequence of this all digital computers are in a sense equivalent)
 (we may now consider again the point raised at the end of \§3)
 (it was suggested tentatively that the question) (can machines think) (should be replaced by)
 (are there imaginable digital computers which would do well in the imitation game)
 (if we wish we can make this superficially more general and ask)
 (are there discrete-state machines which would do wel)
 (but in view of the universality property we see that either of these questions is equivalent
  to this)
 (let us fix our attention on one particular digital computer c)
 (is it true that by modifying this computer to have an adequate storage)
 (suitably increasing its speed of action and providing it with an appropriate programme)
 (c can be made to play satisfactorily the part of a in the imitation game)
 (the part of b being taken by a man)
 (we may now consider the ground to have been cleared and we are ready to proceed to the debate
  on our question)
 (can machines think) (and the variant of it quoted at the end of the last section)
 (we cannot altogether abandon the original form of the problem)
 (for opinions will differ as to the appropriateness of the substitution)
 (and we must at least listen to what has to be said in this connexion)
 (it will simplify matters for the reader if i explain first my own beliefs in the matter)
 (consider first the more accurate form of the question)
 (i believe that in about fifty years time it will be possible to programme computers)
 (with a storage capacity of about 109)
 (to make them play the imitation game so well that an average interrogator will not have more
  than 70 per cent chance of making the right identification after five minutes of questioning)
 (the original question can machines think)
 (i believe to be too meaningless to deserve discussion)
 (nevertheless i believe that at the end of the century the use of words and general educated
  opinion)
 (will have altered so much that one will be able to speak of machines thinking without
  expecting to be contradicted)
 (i believe further that no useful purpose is served by concealing these beliefs)
 (the popular
      view
      that
      scientists
      proceed
      inexorably
      from
      well-established
      fact
      to
      well-established
      fact)
 (never being influenced by any improved conjecture is quite mistaken)
 (provided it is made clear which are proved facts and which are conjectures no harm can result)
 (conjectures are of great importance since they suggest useful lines of research)
 (i now proceed to consider opinions opposed to my own)
 (thinking is a function of mans immortal soul)
 (god has given an immortal soul to every man and woman)
 (but not to any other animal or to machines) (hence no animal or machine can think)
 (i am unable to accept any part of this) (but will attempt to reply in theological terms)
 (i should find the argument more convincing if animals were classed with men)
 (for there is a greater difference to my mind between the typical animate and the inanimate
  than there is between man and the other animals)
 (the arbitrary character of the orthodox view becomes clearer if we consider)
 (how it might appear to a member of some other religious community)
 (how do christians regard the moslem view that women have no souls)
 (but let us leave this point aside and return to the main argument)
 (it appears to me that the argument quoted above implies a serious restriction of the
  omnipotence of the almighty)
 (it is admitted that there are certain things that he cannot do such as making one equal to
  two)
 (but should we not believe that he has freedom to confer a soul on an elephant if he sees fit)
 (we might expect that he would only exercise this power in conjunction with a mutation which
  provided the elephant with an)
 (appropriately improved brain to minister to the needs of this sort)
 (an argument of exactly similar form may be made for the case of machines)
 (it may seem different because it is more difficult to swallow)
 (but this really only means that we think it would be less likely that he would consider the
  circumstances suitable for conferring a soul)
 (the circumstances in question are discussed in the rest of this paper)
 (in attempting to construct such machines we should not be irreverently usurping his power of
  creating souls)
 (any more than we are in the procreation of children rather we are in either case instruments
  of his will providing mansions for the souls that he creates)
 (however this is mere speculation)
 (i am not very impressed with theological arguments whatever they may be used to support)
 (such arguments have often been found unsatisfactory in the past in the time of galileo it was
  argued that the texts)
 (and the sun stood still) (and hasted not to go down about a whole day)
 (and he laid the foundations of the earth) (that it should not move at any time)
 (were an adequate refutation of the copernican theory)
 (with our present knowledge such an argument appears futile)
 (when that
   knowledge
   was
   not
   available
   it
   made
   a
   quite
   different
   impression
   the
   consequences
   of
   machines
   thinking)
 (would be too dreadful let us hope and believe that they cannot do so)
 (this argument is seldom expressed quite so openly as in the form above)
 (but it affects most of us who think about it at all)
 (we like to believe that man is in some subtle way superior to the rest of creation)
 (it is best if he can be shown to be necessarily superior)
 (for then there is no danger of him losing his commanding position)
 (the popularity of the theological argument is clearly connected with this feeling)
 (it is likely to be quite strong in intellectual people since they value the power of thinking
  more highly than others)
 (and are more inclined to base their belief in the superiority of man on this power)
 (i do not think that this argument is sufficiently substantial to require refutation)
 (consolation would be more appropriate perhaps this should be sought in the transmigration of
  souls)
 (there are a number of results of mathematical logic)
 (which can be used to show that there are limitations to the powers of discrete-state machines)
 (the best known of these results is known as godels theorem)
 (and shows that in any sufficiently powerful logical system statements)
 (can be formulated which can neither be proved nor disproved within the system)
 (unless possibly the system itself is inconsistent)
 (there are other in some respects similar results due to church kleene rosser and turing)
 (the latter result is the most convenient to consider since it refers directly to machines)
 (whereas the others can only be used in a comparatively indirect argument)
 (for instance if godels theorem is to be used we need in addition to have some means of
  describing logical systems in terms of machines and machines in terms of logical systems)
 (the result
      in
      question
      refers
      to
      a
      type
      of
      machine
      which
      is
      essentially
      a
      digital
      computer
      with
      an
      infinite
      capacity)
 (it states that there are certain things that such a machine cannot do)
 (if it is rigged up to give answers to questions as in the imitation game)
 (there will be some questions to which it will either give a wrong answer)
 (or fail to give an answer at all however much time is allowed for a reply)
 (there may of course be many such questions and questions which cannot be answered by one
  machine may be satisfactorily answered by another)
 (we are of course supposing for the present that the questions are of the kind to which an
  answer yes or no is appropriate)
 (rather than questions such as) (what do you think of picasso)
 (the questions that we know the machines must fail on are of this type)
 (consider the machine specified as follows) (will this machine ever answer yes to any question)
 (the dots are to be replaced by a description of some machine in a standard form)
 (which could be something like that used in)
 (when the
   machine
   described
   bears
   a
   certain
   comparatively
   simple
   relation
   to
   the
   machine
   which
   is
   under
   interrogation)
 (it can be shown that the answer is either wrong or not forthcoming)
 (this is the mathematical result it is argued that it proves a disability of machines to which
  the human intellect is not subject)
 (the short
      answer
      to
      this
      argument
      is
      that
      although
      it
      is
      established
      that
      there
      are
      limitations
      to
      the
      powers)
 (if any particular machine it has only been stated)
 (without any sort of proof that no such limitations apply to the human intellect)
 (but i do not think this view can be dismissed quite so lightly)
 (whenever one of these machines is asked the appropriate critical question and gives a definite
  answer)
 (we know that this answer must be wrong and this gives us a certain feeling of superiority is
  this feeling illusory)
 (it is no doubt quite genuine but i do not think too much importance should be attached to it)
 (we too often give wrong answers to questions ourselves to be justified in being very pleased
  at such evidence of fallibility on the part of the machines)
 (further our superiority can only be felt on such an occasion in relation to the one machine
  over which we have scored our petty triumph)
 (there would be no question of triumphing simultaneously over all machines)
 (in short then there might be men cleverer than any given machine but then again there might be
  other machines cleverer again and so on)
 (those who hold to the mathematical argument would i think mostly he willing to accept the
  imitation game as a basis for discussion)
 (those who believe in the two previous objections would probably not be interested in any
  criteria)
 (this argument is very well expressed in professor jeffersons lister oration for 1949 from
  which i quote)
 (not until
      a
      machine
      can
      write
      a
      sonnet
      or
      compose
      a
      concerto
      because
      of
      thoughts
      and
      emotions
      felt)
 (and not by the chance fall of symbols) (could we agree that machine equals brain-that is)
 (not only write it but know that it had written it no mechanism could feel)
 (and not merely artificially signal an easy contrivance)
 (pleasure at its successes grief when its valves fuse be warmed by flattery be made miserable
  by its mistakes)
 (be charmed by sex be angry or depressed when it cannot get what it wants)
 (this argument appears to be a denial of the validity of our test)
 (according to the most extreme form of this view the only way by which one could be sure that
  machine thinks)
 (is to be the machine and to feel oneself thinking)
 (one could then describe these feelings to the world but of course no one would be justified in
  taking any notice)
 (likewise according to this view the only way to know that a man thinks is to be that
  particular man)
 (it is in fact the solipsist point of view it may be the most logical view to hold but it makes
  communication of ideas difficult)
 (a is liable to believe a thinks but b does not whilst b believes b thinks but a does not)
 (instead of arguing continually over this point it is usual to have the polite convention that
  everyone thinks)
 (i am sure that professor jefferson does not wish to adopt the extreme and solipsist point of
  view)
 (probably he would be quite willing to accept the imitation game as a test)
 (the game with the player b omitted is frequently used in practice)
 (under the name of viva voce to discover whether some one really understands something or has
  learnt it parrot fashion)
 (let us listen in to a part of such a viva voce)
 (interrogator in the first line of your sonnet which reads shall i compare thee to a summers
  day would not a spring day do as well or better)
 (witness it wouldnt scan) (interrogator how about a winters day) (that would scan all right)
 (witness yes but nobody wants to be compared to a winters day)
 (interrogator would you say mr pickwick reminded you of christmas) (witness in a way)
 (interrogator yet christmas is a winters day and i do not think mr pickwick would mind the
  comparison)
 (witness i dont think youre serious by a winters day one means a typical winters day rather
  than a special one like christmas)
 (and so
      on
      what
      would
      professor
      jefferson
      say
      if
      the
      sonnet-writing
      machine
      was
      able
      to
      answer
      like
      this
      in
      the
      viva
      voce)
 (i do not know whether he would regard the machine as merely artificially signalling these
  answers)
 (but if the answers were as satisfactory and sustained as in the above passage i do not think
  he would describe it as an easy contrivance)
 (this phrase is i think intended to cover such devices as the inclusion in the machine of a
  record of someone reading a sonnet)
 (with appropriate switching to turn it on from time to time)
 (in short then i think that most of those who support the argument from consciousness could be
  persuaded to abandon it rather than be forced into the solipsist position)
 (they will then probably be willing to accept our test)
 (i do not wish to give the impression that i think there is no mystery about consciousness)
 (there is for instance something of a paradox connected with any attempt to localise it but i
  do not think these mysteries necessarily need to be solved before we can answer the question
  with which we are concerned in this paper)
 (these arguments take the form)
 (i grant you that you can make machines do all the things you have mentioned but you will never
  be able to make one to do x)
 (numerous features x are suggested in this connexion i offer a selection)
 (be kind resourceful beautiful friendly have initiative have a sense of humour tell right from
  wrong)
 (make mistakes fall in love enjoy strawberries and cream)
 (make some one fall in love with it learn from experience)
 (use words properly be the subject of its own thought have as much diversity of behaviour as a
  man do something really new)
 (no support is usually offered for these statements)
 (i believe they are mostly founded on the principle of scientific induction)
 (a man has seen thousands of machines in his lifetime)
 (from what he sees of them he draws a number of general conclusions)
 (they are ugly each is designed for a very limited purpose)
 (when required
   for
   a
   minutely
   different
   purpose
   they
   are
   useless
   the
   variety
   of
   behaviour
   of
   any
   one
   of
   them
   is
   very
   small
   etc
   etc)
 (naturally he concludes that these are necessary properties of machines in general)
 (many of these limitations are associated with the very small storage capacity of most
  machines)
 (i am assuming that the idea of storage capacity is extended in some way to cover machines
  other than discrete-state machines)
 (the exact
      definition
      does
      not
      matter
      as
      no
      mathematical
      accuracy
      is
      claimed
      in
      the
      present
      discussion)
 (a few years ago when very little had been heard of digital computers)
 (it was possible to elicit much incredulity concerning them if one mentioned their properties
  without describing their construction)
 (that was presumably due to a similar application of the principle of scientific induction)
 (these applications of the principle are of course largely unconscious)
 (when a burnt child fears the fire and shows that he fears it by avoiding it)
 (f should say that he was applying scientific induction)
 (i could of course also describe his behaviour in many other ways)
 (the works
      and
      customs
      of
      mankind
      do
      not
      seem
      to
      be
      very
      suitable
      material
      to
      which
      to
      apply
      scientific
      induction)
 (a very large part of space-time must be investigated)
 (if reliable results are to be obtained otherwise we may)
 (as most english children do decide that everybody speaks english)
 (and that it is silly to learn french)
 (there are however special remarks to be made about many of the disabilities that have been
  mentioned)
 (the inability
      to
      enjoy
      strawberries
      and
      cream
      may
      have
      struck
      the
      reader
      as
      frivolous
      possibly
      a
      machine
      might
      be
      made
      to
      enjoy
      this
      delicious
      dish
      but
      any
      attempt
      to
      make
      one
      do
      so
      would
      be
      idiotic)
 (what is important about this disability is that it contributes to some of the other
  disabilities)
 (e g to the difficulty of the same kind of friendliness occurring between man and machine as
  between white man and white man or between black man and black man)
 (the claim that machines cannot make mistakes seems a curious one one is tempted to retort)
 (are they any the worse for that)
 (but let us adopt a more sympathetic attitude and try to see what is really meant)
 (i think this criticism can be explained in terms of the imitation game)
 (it is claimed that the interrogator could distinguish the machine from the man simply by
  setting them a number of problems in arithmetic)
 (the machine would be unmasked because of its deadly accuracy) (the reply to this is simple)
 (the machine
      programmed
      for
      playing
      the
      game
      would
      not
      attempt
      to
      give
      the
      right
      answers
      to
      the
      arithmetic
      problems)
 (it would deliberately introduce mistakes in a manner calculated to confuse the interrogator)
 (a mechanical fault would probably show itself through an unsuitable decision as to what sort
  of a mistake to make in the arithmetic)
 (even this interpretation of the criticism is not sufficiently sympathetic but we cannot afford
  the space to go into it much further)
 (it seems to me that this criticism depends on a confusion between two kinds of mistake)
 (we may call them errors of functioning and errors of conclusion)
 (errors of functioning are due to some mechanical or electrical fault which causes the machine
  to behave otherwise than it was designed to do)
 (in philosophical discussions one likes to ignore the possibility of such errors one is
  therefore discussing abstract machines)
 (these abstract machines are mathematical fictions rather than physical objects)
 (by definition they are incapable of errors of functioning)
 (in this sense we can truly say that machines can never make mistakes)
 (errors of conclusion can only arise when some meaning is attached to the output signals from
  the machine)
 (the machine might for instance type out mathematical equations or sentences in english)
 (when a
   false
   proposition
   is
   typed
   we
   say
   that
   the
   machine
   has
   committed
   an
   error
   of
   conclusion)
 (there is clearly no reason at all for saying that a machine cannot make this kind of mistake
  it might do nothing but type out repeatedly o = i)
 (to take a less perverse example it might have some method for drawing conclusions by
  scientific induction)
 (we must expect such a method to lead occasionally to erroneous results)
 (the claim
      that
      a
      machine
      cannot
      be
      the
      subject
      of
      its
      own
      thought
      can
      of
      course
      only
      be
      answered)
 (if it can be shown that the machine has some thought with some subject matter)
 (nevertheless the subject matter of a machines operations)
 (does seem to mean something at least to the people who deal with it)
 (if for
     instance
     the
     machine
     was
     trying
     to
     find
     a
     solution
     of
     the
     equation
     x2
     -
     40x
     -
     11
     =
     0
     one
     would
     be
     tempted
     to
     describe
     this
     equation
     as
     part
     of
     the
     machines
     subject
     matter
     at
     that
     moment)
 (in this sort of sense a machine undoubtedly can be its own subject matter)
 (it may be used to help in making up its own programmes or to predict the effect of alterations
  in its own structure)
 (by observing the results of its own behaviour it can modify its own programmes so as to
  achieve some purpose more effectively)
 (these are possibilities of the near future rather than utopian dreams)
 (the criticism
      that
      a
      machine
      cannot
      have
      much
      diversity
      of
      behaviour
      is
      just
      a
      way
      of
      saying
      that
      it
      cannot
      have
      much
      storage
      capacity)
 (until fairly recently a storage capacity of even a thousand digits was very rare)
 (the criticisms
      that
      we
      are
      considering
      here
      are
      often
      disguised
      forms
      of
      the
      argument
      from
      consciousness)
 (usually if one maintains that a machine can do one of these things)
 (and describes
      the
      kind
      of
      method
      that
      the
      machine
      could
      use
      one
      will
      not
      make
      much
      of
      an
      impression)
 (it is thought that tile method whatever it may be for it must be mechanical is really rather
  base)
 (compare the parentheses in jeffersons statement quoted on page 22)
 (our most detailed information of babbages analytical engine comes from a memoir by lady
  lovelace)
 (in it she states the analytical engine has no pretensions to originate anything)
 (it can do whatever we know how to order it to perform) (this statement is quoted by hartree)
 (who adds this does not imply that it may not be possible to construct electronic equipment
  which will think for itself)
 (or in which in biological terms one could set up a conditioned reflex)
 (which would serve as a basis for learning)
 (whether this is possible in principle or not is a stimulating and exciting question)
 (suggested by some of these recent developments)
 (but it did not seem that the machines constructed or projected at the time had this property)
 (i am in thorough agreement with hartree over this)
 (it will be noticed that he does not assert that the machines in question had not got the
  property)
 (but rather that the evidence available to lady lovelace did not encourage her to believe that
  they had it)
 (it is quite possible that the machines in question had in a sense got this property)
 (for suppose that some discrete-state machine has the property)
 (the analytical
      engine
      was
      a
      universal
      digital
      computer
      so
      that
      if
      its
      storage
      capacity
      and
      speed
      were
      adequate)
 (it could by suitable programming be made to mimic the machine in question)
 (probably this argument did not occur to the countess or to babbage)
 (in any case there was no obligation on them to claim all that could be claimed)
 (this whole question will be considered again under the heading of learning machines)
 (a variant of lady lovelaces objection states that a machine can never do anything really new)
 (this may be parried for a moment with the saw) (there is nothing new under the sun)
 (who can be certain that original work)
 (that he has done was not simply the growth of the seed planted in him by teaching)
 (or the effect of following well-known general principles) (a better)
 (variant of the objection says that a machine can never take us by surprise)
 (this statement is a more direct challenge and can be met directly)
 (machines take me by surprise with great frequency)
 (this is largely because i do not do sufficient calculation to decide what to expect them to
  do)
 (or rather because although i do a calculation i do it in a hurried slipshod fashion)
 (taking risks perhaps i say to myself)
 (i suppose the voltage here ought to he the same as there anyway lets assume it is)
 (naturally i am often wrong and the result is a surprise for me for by the time the experiment
  is done these assumptions have been forgotten)
 (these admissions lay me open to lectures on the subject of my vicious ways)
 (but do not throw any doubt on my credibility when i testify to the surprises i experience)
 (i do not expect this reply to silence my critic)
 (he will probably say that h surprises are due to some creative mental act on my part and
  reflect no credit on the machine)
 (this leads us back to the argument from consciousness and far from the idea of surprise)
 (it is a line of argument we must consider closed)
 (but it is perhaps worth remarking that the appreciation of something as surprising requires as
  much of a creative mental act)
 (whether the surprising event originates from a man a book a machine or anything else)
 (the view that machines cannot give rise to surprises is due)
 (i believe to a fallacy to which philosophers and mathematicians are particularly subject)
 (this is the assumption that as soon as a fact is presented to a mind all consequences of that
  fact spring)
 (into the mind simultaneously with it)
 (it is a very useful assumption under many circumstances)
 (but one too easily forgets that it is false)
 (a natural consequence of doing so is that one then assumes that there is no virtue in the mere
  working out of consequences from data and general principles)
 (the nervous system is certainly not a discrete-state machine)
 (a small error in the information about the size of a nervous impulse impinging on a neuron)
 (may make a large difference to the size of the outgoing impulse)
 (it may be argued that this being so one cannot expect to be able to mimic the behaviour of the
  nervous system with a discrete-state system)
 (it is true that a discrete-state machine must be different from a continuous machine)
 (but if we adhere to the conditions of the imitation game the interrogator will not be able to
  take any advantage of this difference)
 (the situation can be made clearer if we consider sonic other simpler continuous machine)
 (a differential analyser will do very well)
 (a differential analyser is a certain kind of machine not of the discrete-state type used for
  some kinds of calculation)
 (some of these provide their answers in a typed form)
 (and so are suitable for taking part in the game)
 (it would not be possible for a digital computer to predict exactly what answers the
  differential analyser would give to a problem)
 (but it would be quite capable of giving the right sort of answer)
 (for instance if asked to give the value of it would be reasonable to choose at random between
  the values 3 12 3 13 3 14 3 15 3 16)
 (with the probabilities of 0 5 0 15 0 55 0 19 0 6)
 (under these circumstances it would be very difficult for the interrogator to distinguish the
  differential analyser from the digital computer)
 (it is not possible to produce a set of rules purporting to describe what a man should do in
  every conceivable set of circumstances)
 (one might for instance have a rule that one is to stop when one sees a red traffic light)
 (and to go if one sees a green one but what if by some fault both appear together)
 (one may perhaps decide that it is safest to stop but some further difficulty may well arise
  from this decision later)
 (to attempt to provide rules of conduct to cover every eventuality even those arising from
  traffic lights appears to be impossible)
 (with all this i agree) nil
 (from this it is argued that we cannot be machines i shall try to reproduce the argument but i
  fear i shall hardly do it justice)
 (it seems to run something like this)
 (if each
     man
     had
     a
     definite
     set
     of
     rules
     of
     conduct
     by
     which
     he
     regulated
     his
     life
     he
     would
     be
     no
     better
     than
     a
     machine)
 (but there are no such rules so men cannot be machines)
 (the undistributed middle is glaring i do not think the argument is ever put quite like this)
 (but i believe this is the argument used nevertheless)
 (there may however be a certain confusion between)
 (rules of conduct and laws of behaviour to cloud the issue by rules of conduct i mean precepts
  such as)
 (stop if you see red lights)
 (on which one can act and of which one can be conscious by laws of behaviour)
 (i mean laws of nature as applied to a mans body such as if you pinch him he will squeak)
 (if we
     substitute
     laws
     of
     behaviour
     which
     regulate
     his
     life
     for
     laws
     of
     conduct
     by
     which
     he
     regulates
     his
     life)
 (in the argument quoted the undistributed middle is no longer insuperable)
 (for we believe that it is not only true that being regulated by laws of behaviour implies
  being some sort of machine)
 (though not necessarily a discrete-state machine)
 (but that conversely being such a machine implies being regulated by such laws)
 (however we cannot so easily convince ourselves of the absence of complete laws of behaviour as
  of complete rules of conduct)
 (the only
      way
      we
      know
      of
      for
      finding
      such
      laws
      is
      scientific
      observation
      and
      we
      certainly
      know
      of
      no
      circumstances
      under
      which
      we
      could
      say)
 (we have searched enough) (there are no such laws)
 (we can demonstrate more forcibly that any such statement would be unjustified)
 (for suppose we could be sure of finding such laws if they existed)
 (then given a discrete-state machine it should certainly be possible to discover by observation
  sufficient about it to predict its future behaviour)
 (and this within a reasonable time say a thousand years)
 (but this does not seem to be the case i have set up on the manchester computer a small
  programme using only 1 0 units of storage)
 (whereby the machine supplied with one sixteen-figure number replies with another within two
  seconds)
 (i would defy anyone to learn from these replies sufficient about the programme to be able to
  predict any replies to untried values)
 (i assume that the reader is familiar with the idea of extrasensory perception and the meaning
  of the four items of it)
 (telepathy clairvoyance precognition and psychokinesis)
 (these disturbing phenomena seem to deny all our usual scientific ideas)
 (how we should like to discredit them)
 (unfortunately the statistical evidence at least for telepathy is overwhelming)
 (it is very difficult to rearrange ones ideas so as to fit these new facts in)
 (once one has accepted them it does not seem a very big step to believe in ghosts and bogies)
 (the idea that our bodies move simply according to the known laws of physics)
 (together with some others not yet discovered but somewhat similar)
 (would be one of the first to go) (this argument is to my mind quite a strong one)
 (one can say in reply that many scientific theories seem to remain workable in practice in
  spite of clashing with esp)
 (that in fact one can get along very nicely if one forgets about it)
 (this is rather cold comfort and one fears that thinking is just the kind of phenomenon where
  esp may be especially relevant)
 (a more specific argument based on esp might run as follows) (let us play the imitation game)
 (using as witnesses a man who is good as a telepathic receiver and a digital computer)
 (the interrogator can ask such questions as)
 (what suit does the card in my right hand belong to)
 (the man by telepathy or clairvoyance gives the right answer 130 times out of 400 cards)
 (the machine can only guess at random)
 (and perhaps gets 104 right so the interrogator makes the right identification)
 (there is an interesting possibility which opens here suppose the digital computer contains a
  random number generator)
 (then it will be natural to use this to decide what answer to give)
 (but then the random number generator will be subject to the psychokinetic powers of the
  interrogator)
 (perhaps this psychokinesis might cause the machine to guess right more often than would be
  expected on a probability calculation)
 (so that the interrogator might still be unable to make the right identification)
 (on the other hand he might be able to guess right without any questioning by clairvoyance with
  esp anything may happen)
 (if telepathy is admitted it will be necessary to tighten our test up)
 (the situation
      could
      be
      regarded
      as
      analogous
      to
      that
      which
      would
      occur
      if
      the
      interrogator
      were
      talking
      to
      himself
      and
      one
      of
      the
      competitors
      was
      listening
      with
      his
      ear
      to
      the
      wall)
 (to put the competitors into a telepathy-proof room) (would satisfy all requirements)
 (the reader
      will
      have
      anticipated
      that
      i
      have
      no
      very
      convincing
      arguments
      of
      a
      positive
      nature
      to
      support
      my
      views)
 (if i had i should not have taken such pains to point out the fallacies in contrary views)
 (such evidence as i have i shall now give)
 (let us return for a moment to lady lovelaces objection)
 (which stated that the machine can only do what we tell it to do)
 (one could say that a man can inject an idea into the machine and that it will respond to a
  certain extent)
 (and then drop into quiescence like a piano string struck by a hammer)
 (another simile would be an atomic pile of less than critical size)
 (an injected idea is to correspond to a neutron)
 (entering the pile from without each such neutron will cause a certain disturbance which
  eventually dies away)
 (if however the size of the pile is sufficiently increased)
 (tire disturbance caused by such an incoming neutron will very likely go on and on increasing
  until the whole pile is destroyed)
 (is there a corresponding phenomenon for minds and is there one for machines)
 (there does seem to be one for the human mind)
 (the majority
      of
      them
      seem
      to
      be
      subcritical
      i
      e
      to
      correspond
      in
      this
      analogy
      to
      piles
      of
      subcritical
      size)
 (an idea presented to such a mind will on average give rise to less than one idea in reply)
 (a smallish proportion are supercritical)
 (an idea presented to such a mind that may give rise to a whole theory consisting of secondary)
 (tertiary and more remote ideas) (animals minds seem to be very definitely subcritical)
 (adhering to this analogy we ask) (can a machine be made to be supercritical)
 (the skin-of-an-onion analogy is also helpful)
 (in considering the functions of the mind or the brain we find certain operations which we can
  explain in purely mechanical terms)
 (this we say does not correspond to the real mind)
 (it is a sort of skin which we must strip off if we are to find the real mind)
 (but then in what remains we find a further skin to be stripped off and so on)
 (proceeding in this way do we ever come to the real mind or do we eventually come to the skin
  which has nothing in it)
 (in the latter case the whole mind is mechanical)
 (it would not be a discrete-state machine however we have discussed this)
 (these last two paragraphs do not claim to be convincing arguments)
 (they should rather be described as recitations tending to produce belief)
 (the only
      really
      satisfactory
      support
      that
      can
      be
      given
      for
      the
      view
      expressed
      at
      the
      beginning
      of)
 (will be that provided by waiting for the end of the century and then doing the experiment
  described but what can we say in the meantime)
 (what steps should be taken now if the experiment is to be successful)
 (as i have explained the problem is mainly one of programming)
 (advances in engineering will have to be made too)
 (but it seems unlikely that these will not be adequate for the requirements)
 (estimates of the storage capacity of the brain vary from 1010 to 1015 binary digits)
 (i incline to the lower values and believe that only a very small fraction is used for the
  higher types of thinking)
 (most of it is probably used for the retention of visual impressions i should be surprised if
  more than 109 was required for satisfactory playing of the imitation game)
 (at any rate against a blind man)
 (the capacity of the encyclopaedia britannica 11th edition is 2 x 109)
 (a storage capacity of 107 would be a very practicable possibility even by present techniques)
 (it is probably not necessary to increase the speed of operations of the machines at all)
 (parts of modern machines which can be regarded as analogs of nerve cells work about a thousand
  times faster than the latter)
 (this should provide a margin of safety which could cover losses of speed arising in many ways)
 (our problem then is to find out how to programme these machines to play the game)
 (at my present rate of working i produce about a thousand digits of progratiirne a day so that
  about sixty workers working steadily through the fifty years might accomplish the job)
 (if nothing went into the wastepaper basket) (some more expeditious method seems desirable)
 (in the process of trying to imitate an adult human mind)
 (we are bound to think a good deal about the process which has brought it to the state that it
  is in)
 (we may notice three components) (the initial state of the mind say at birth)
 (the education to which it has been subjected)
 (other experience not to be described as education to which it has been subjected)
 (instead of trying to produce a programme to simulate the adult mind)
 (why not rather try to produce one which simulates the childs)
 (if this
     were
     then
     subjected
     to
     an
     appropriate
     course
     of
     education
     one
     would
     obtain
     the
     adult
     brain)
 (presumably the child brain is something like a notebook as one buys it from the stationers
  rather little mechanism and lots of blank sheets)
 (mechanism and writing are from our point of view almost synonymous)
 (our hope is that there is so little mechanism in the child brain that something like it can be
  easily programmed)
 (the amount of work in the education we can assume)
 (as a first approximation to be much the same as for the human child)
 (we have thus divided our problem into two parts)
 (the child programme and the education process) (these two remain very closely connected)
 (we cannot expect to find a good child machine at the first attempt)
 (one must experiment with teaching one such machine and see how well it learns)
 (one can then try another and see if it is better or worse)
 (there is an obvious connection between this process and evolution by the identifications)
 (structure of the child machine) (changes of the child machine)
 (natural selection judgment of the experimenter)
 (one may hope however that this process will be more expeditious than evolution)
 (the survival of the fittest is a slow method for measuring advantages)
 (the experimenter by the exercise of intelligence should he able to speed it up)
 (equally important is the fact that he is not restricted to random mutations)
 (if he
     can
     trace
     a
     cause
     for
     some
     weakness
     he
     can
     probably
     think
     of
     the
     kind
     of
     mutation
     which
     will
     improve
     it)
 (it will not be possible to apply exactly the same teaching process to the machine as to a
  normal child)
 (it will not for instance be provided with legs)
 (so that it could not be asked to go out and fill the coal scuttle possibly it might not have
  eyes)
 (but however well these deficiencies might be overcome by clever engineering one could not send
  the creature to school without the other children making excessive fun of it)
 (it must be given some tuition we need not be too concerned about the legs eyes etc)
 (the example
      of
      miss
      helen
      keller
      shows
      that
      education
      can
      take
      place
      provided
      that
      communication
      in
      both
      directions
      between
      teacher
      and
      pupil
      can
      take
      place
      by
      some
      means
      or
      other)
 (we normally associate punishments and rewards with the teaching process some simple child
  machines can be constructed)
 (or programmed on this sort of principle)
 (the machine
      has
      to
      be
      so
      constructed
      that
      events
      which
      shortly
      preceded
      the
      occurrence
      of
      a
      punishment
      signal
      are
      unlikely
      to
      be
      repeated)
 (whereas a reward signal increased the probability of repetition of the events which led up to
  it)
 (these definitions do not presuppose any feelings on the part of the machine)
 (i have done some experiments with one such child machine)
 (and succeeded
      in
      teaching
      it
      a
      few
      things
      but
      the
      teaching
      method
      was
      too
      unorthodox
      for
      the
      experiment
      to
      be
      considered
      really
      successful)
 (the use of punishments and rewards can at best be a part of the teaching process)
 (roughly speaking if the teacher has no other means of communicating to the pupil)
 (the amount
      of
      information
      which
      can
      reach
      him
      does
      not
      exceed
      the
      total
      number
      of
      rewards
      and
      punishments
      applied)
 (by the time a child has learnt to repeat casabianca)
 (twenty questions technique every no taking the form of a blow)
 (it is necessary therefore to have some other)
 (unemotional channels of communication if these are available it is possible to teach a machine
  by punishments and rewards to obey orders given in some language e g a symbolic language)
 (these orders are to be transmitted through the unemotional channels)
 (the use of this language will diminish greatly the number of punishments and rewards required)
 (opinions may vary as to the complexity which is suitable in the child machine)
 (one might try to make it as simple as possible consistently with the general principles)
 (alternatively one might have a complete system of logical inference built in)
 (in the latter case the store would be largely occupied with definitions and propositions)
 (the propositions would have various kinds of status)
 (e g well-established facts conjectures mathematically proved theorems statements given by an
  authority expressions having the logical form of proposition but not belief-value certain
  propositions may be described as imperatives)
 (the machine
      should
      be
      so
      constructed
      that
      as
      soon
      as
      an
      imperative
      is
      classed
      as
      well
      established)
 (the appropriate action automatically takes place)
 (to illustrate this suppose the teacher says to the machine do your homework now)
 (this may cause teacher says do your homework now)
 (to be included amongst the well-established facts)
 (another such fact might be everything that teacher says is true)
 (combining these may eventually lead to the imperative)
 (do your homework  now being included amongst the well-established facts)
 (and this by the construction of the machine will mean that the homework actually gets started)
 (but the effect is very satisfactory)
 (the processes
      of
      inference
      used
      by
      the
      machine
      need
      not
      be
      such
      as
      would
      satisfy
      the
      most
      exacting
      logicians)
 (there might for instance be no hierarchy of types)
 (but this need not mean that type fallacies will occur)
 (any more than we are bound to fall over unfenced cliffs suitable imperatives)
 (expressed within the systems not forming part of the rules of the system)
 (such as do not use a class unless it is a subclass of one which has been mentioned by teacher
  can have a similar effect to)
 (do not go  too near the edge)
 (the imperatives
      that
      can
      be
      obeyed
      by
      a
      machine
      that
      has
      no
      limbs
      are
      bound
      to
      be
      of
      a
      rather
      intellectual
      character)
 (as in the example doing homework)
 (given above important amongst such imperatives will be ones which regulate the order in)
 (which the rules of the logical system concerned are to be applied)
 (for at each stage when one is using a logical system)
 (there is a very large number of alternative steps)
 (any of which one is permitted to apply so far as obedience to the rules of the logical system
  is concerned)
 (these choices make the difference between a brilliant and a footling reasoner not the
  difference between a sound and a fallacious one)
 (propositions leading to imperatives of this kind might be)
 (when socrates is mentioned use the syllogism in barbara)
 (or if one method has been proved to be quicker than another do not use the slower method)
 (some of these may be given by authority)
 (but others may be produced by the machine itself e g by scientific induction)
 (the idea of a learning machine may appear paradoxical to some readers)
 (how can the rules of operation of the machine change)
 (they should describe completely how the machine will react whatever its history might be)
 (whatever changes it might undergo the rules are thus quite time-invariant)
 (this is quite true the explanation of the paradox is that the rules which get changed in the
  learning process are of a rather less pretentious kind claiming only an ephemeral validity)
 (the reader may draw a parallel with the constitution of the united states)
 (an important feature of a learning machine is that its teacher will often be very largely
  ignorant of quite what is going on inside although he may still be able to some extent to
  predict his pupils behavior)
 (this should apply most strongly to the later education of a machine arising from a child
  machine of well-tried design)
 (this is in clear contrast with normal procedure when using a machine to do computations ones
  object is then to have a clear mental picture of the state of the machine at each moment in
  the computation)
 (this object can only be achieved with a struggle)
 (the view that the machine can only do what we know how to order it to do)
 (appears strange in face of this most of the programmes which we can put into the machine will
  result in its doing something that we cannot make sense)
 (or which we regard as completely random behaviour)
 (intelligent behaviour presumably consists in a departure from the completely disciplined
  behaviour involved in computation)
 (but a rather slight one which does not give rise to random behaviour or to pointless
  repetitive loops)
 (another important result of preparing our machine for its part in the imitation game by a
  process of teaching and learning is that human fallibility)
 (is likely to be omitted in a rather natural way i e without special coaching)
 (processes that are learnt do not produce a hundred per cent certainty of result if they did
  they could not be unlearnt)
 (it is probably wise to include a random element in a learning machine a random element is
  rather useful)
 (when we are searching for a solution of some problem)
 (suppose for instance we wanted to find a number between 50 and 200 which was equal to the
  square of the sum of its digits we might start at 51 then try 52 and go on until we got a
  number that worked)
 (alternatively we might choose numbers at random until we got a good one)
 (this method has the advantage that it is unnecessary to keep track of the values that have
  been tried but the disadvantage that one may try the same one twice)
 (but this is not very important if there are several solutions)
 (the systematic
      method
      has
      the
      disadvantage
      that
      there
      may
      be
      an
      enormous
      block
      without
      any
      solutions
      in
      the
      region
      which
      has
      to
      be
      investigated
      first)
 (now the learning process may be regarded as a search for a form of behaviour which will
  satisfy the teacher)
 (since there is probably a very large number of satisfactory solutions the random method seems
  to be better than the systematic)
 (it should be noticed that it is used in the analogous process of evolution)
 (but there the systematic method is not possible)
 (how could one keep track of the different genetical combinations that had been tried so as to
  avoid trying them again)
 (we may hope that machines will eventually compete with men in all purely intellectual fields)
 (but which are the best ones to start with)
 (even this is a difficult decision many people think that a very abstract activity like the
  playing of chess)
 (would be best it can also be maintained that it is best to provide the machine with the best
  sense organs that money can buy)
 (and then teach it to understand and speak english)
 
 
 (we can only see a short distance ahead) (but we can see plenty there that needs to be done)))

(defvar war-1 '(

(No one would have believed  )
 
( in the last years of the nineteenth century  )
 
( that
this world was being watched keenly and closely  )
 
( by intelligences greater
than mans  )
 
( and yet as mortal as his own )
 
(  that as men busied themselves about
their various concerns )
 
(  they were scrutinised and studied )
 
(  perhaps almost as
narrowly as a man with a microscope  )
 
( might scrutinise the transient creatures
that swarm and multiply in a drop of water )
 
( With infinite complacency men
went to and fro over this globe )
 
(  about their little affairs )
 
( serene in their
assurance of their empire over matter )
 
(  It is possible that the infusoria
under the microscope do the same )
 
(  No one gave a thought to the older worlds
of space  )
 
( as sources of human danger )
 
(  or thought of them only to dismiss  )
 
( the
idea of life upon them as impossible or improbable )
 
( It is curious to recall
some of the mental habits of those departed days )
 
(  At most terrestrial men
fancied there might be other men upon Mars )
 
(  perhaps inferior to themselves )
 
( 
and ready to welcome a missionary enterprise )
 
( Yet across the gulf of space )
 
( 
minds that are to our minds as ours are to those of the beasts that perish )
 
( 
intellects vast and cool and unsympathetic )
 
(  regarded this earth with envious
eyes )
 
(  and slowly and surely drew their plans against us )
 
(  And early in the
twentieth century came the great disillusionment )
 
( 

I scarcely need remind the reader )
 
(  revolves about the sun
 )
 
( and the light and heat it receives
from the sun  )
 
( is barely half of that received by this world )
 
(  It must be )
 
( if
the nebular hypothesis has any truth )
 
(  older than our world )
 
(  and long before
this earth ceased to be molten )
 
(  life upon its surface must have begun its
course )
 
(  The fact that it is scarcely one seventh of the volume of the earth )
 
( 
must have accelerated its cooling to the temperature at which life could
begin )
 
( It has air and water and all that is necessary for the support of
animated existence )
 
( 

Yet so vain is man)
 
( and so blinded by his vanity)
 
( that no writer)
 
(  up to the
very end of the nineteenth century)
 
( expressed any idea that intelligent life
might have developed there far)
 
(  or indeed at all)
 
(  beyond its earthly level)
 
( 
Nor was it generally understood )
 
( that since Mars is older than our earth)
 
( 
with scarcely a quarter of the superficial area)
 
(  and remoter from the sun)
 
(  it
necessarily follows that it is not only more distant from times beginning
but nearer its end)
 
( 

The secular cooling that must someday overtake our planet)
 
(  has already gone
far indeed with our neighbour)
 
( Its physical condition is still largely a
mystery)
 
(  but we know now )
 
( that even in its equatorial region )
 
( the midday
temperature barely approaches that of our coldest winter)
 
(  Its air is much
more attenuated than ours)
 
(  its oceans have shrunk until they cover but a
third of its surface)
 
(  and as its slow seasons change huge snowcaps gather)
 
( 
and melt about either pole)
 
(  and periodically inundate its temperate zones)
 
( 
That last stage of exhaustion)
 
(  which to us is still incredibly remote)
 
(  has
become a present-day problem for the inhabitants of Mars)
 
( The immediate
pressure of necessity has brightened their intellects)
 
(  enlarged their
powers)
 
(  and hardened their hearts)
 
( And looking across space with
instruments)
 
(  and intelligences such as we have scarcely dreamed of)
 
(  they
see )
 
( sunward of them a
morning star of hope)
 
(  our own warmer planet)
 
( green with vegetation and grey
with water)
 
(  with a cloudy atmosphere eloquent of fertility)
 
( with glimpses
through its drifting cloud wisps )
 
( of broad stretches of populous country and
narrow navy-crowded seas)

;;;;;;
 
( 

And we men the creatures who inhabit this earth)
 
(  must be to them at least
as alien)
 
(  and lowly as are the monkeys and lemurs to us)
 
( The intellectual
side of man already admits that life is an incessant struggle for existence)
 
( 
and it would seem that this too is the belief of the minds upon Mars)
 
( Their
world is far gone in its cooling )
 
( and this world is still crowded with life)
 
( 
but crowded only with what they regard as inferior animals)
 
(  To carry warfare
sunward is their only escape from the destruction that)
 
(  generation
after generation creeps upon them)
 
( 

And before we judge of them too harshly )
 
( we must remember what ruthless )
 
( and
utter destruction our own species has wrought)
 
( not only upon animals)
 
(  such
as the vanished bison and the dodo)
 
(  but upon its inferior races)
 
(  The
Tasmanians in spite of their human likeness)
 
(  were entirely swept out of
existence in a war of extermination)
 
(  waged by European immigrants in the
space of fifty years)
 
(  Are we such apostles of mercy as to complain )
 
( if the
Martians warred in the same spirit)
 
( 

The Martians seem to have calculated their descent )
 
( with amazing
subtlety)
 
( their mathematical learning is evidently far in excess of
ours)
 
( and to have carried out their preparations )
 
( with a well-nigh perfect
unanimity)
 
(  Had our instruments permitted it)
 
(  we might have seen the
gathering trouble far back in the nineteenth century)
 
(  Men like Schiaparelli
watched the red planet)
 
( it is odd by-the-bye)
 
(  that for countless centuries
Mars has been the star of war)
 
( but failed to interpret the fluctuating
appearances of the markings they mapped so well)
 
(  All that time the Martians
must have been getting ready)
 
( 

During the opposition of 1894)
 
(  a great light was seen on the illuminated part
of the disk)
 
(  first at the Lick Observatory)
 
(  then by Perrotin of Nice)
 
(  and
then by other observers)
 
(  English readers heard of it first)
 
( I am inclined to think that this blaze may have been)
 
( 
the casting of the huge gun)
 
(  in the vast pit sunk into their planet)
 
( from
which their shots were fired at us)
 
( Peculiar markings as yet unexplained)
 
( 
were seen near the site of that outbreak )
 
( during the next two oppositions)
 
( 

The storm burst upon us six years ago now)
 
(  As Mars approached opposition
Lavelle of Java set the wires of the astronomical exchange )
 
( palpitating with
the amazing intelligence of a huge outbreak of incandescent gas upon the
planet)
 
(  It had occurred towards midnight of the twelfth)
 
(  and the
spectroscope to which he had at once resorted)
 
(  indicated a mass of flaming
gas chiefly hydrogen)
 
( moving with an enormous velocity towards this earth)
 
( 
This jet of fire had become invisible about a quarter past twelve)
 
(  He
compared it to a colossal puff of flame suddenly)
 
(  and violently squirted out
of the planet)
 
(  as flaming gases rushed out of a gun)

;;;;
 
( 

A singularly appropriate phrase it proved)
 
( Yet the next day there was
nothing of this in the papers )
 
( except a little note in the Daily Telegraph)
 
( 
and the world went in ignorance)
 
(  of one of the gravest dangers that ever
threatened the human race)
 
(  I might not have heard of the eruption at all had
I not met Ogilvy)
 
( the well-known astronomer)
 
( He was immensely
excited at the news)
 
(  and in the excess of his feelings invited me up to take
a turn with him that night )
 
( in a scrutiny of the red planet)
 
( 

In spite of all that has happened since)
 
(  I still remember that vigil very
distinctly)
 
(  the black and silent observatory)
 
(  the shadowed lantern throwing
a feeble glow upon the floor in the corner)
 
(  the steady ticking of the
clockwork of the telescope)
 
( the little slit in the roof )
 
( an oblong
profundity with the stardust streaked across it)
 
(  Ogilvy moved about
invisible but audible)
 
(  Looking through the telescope)
 
(  one saw a circle of
deep blue )
 
( and the little round planet swimming in the field)
 
(  It seemed such
a little thing)
 
(  so bright and small and still)
 
(  faintly marked with
transverse stripes)
 
(  and slightly flattened from the perfect round)
 
(  But so
little it was)
 
(  so silvery warm)
 
( a pins-head of light)
 
( It was as if it
quivered)
 
(  but really this was the telescope vibrating )
 
( with the activity of
the clockwork that kept the planet in view)
 
( 

As I watched the planet seemed to grow larger and smaller )
 
( and to advance
and recede)
 
(  but that was simply that my eye was tired)
 
( Forty millions of
miles it was from us)
 
( more than forty millions of miles of void)
 
( Few people
realise the immensity of vacancy in which the dust of the material universe
swims)
 
( Near it in the field)
 
(  were three faint points of light)
 
( 
three telescopic stars infinitely remote)
 
(  and all around it was the
unfathomable darkness of empty space)
 
( You know how that blackness looks on a
frosty starlight night)
 
(  In a telescope it seems far profounder)
 
( And
invisible to me )

;;;;;;new
 
( because it was so remote and small)
 
( flying swiftly and
steadily towards me )
 
( across that incredible distance)
 
(  drawing nearer every
minute )
 
( by so many thousands of miles)
 
( came the Thing they were sending us)
 
( 
the Thing that was to bring so much struggle and calamity and death)
 
( I never dreamed of it then as I watched)
 
( no one on earth dreamed of
that unerring missile)
 
( 

That nightthere was another jetting out of gas from the distant
planet)
 
( A reddish flash at the edge)
 
( the slightest projection of
the outline just as the chronometer struck midnight)
 
( and at that I told
Ogilvy and he took my place)
 
(  The night was warm and I was thirsty)
 
( and I
went stretching my legs clumsily)
 
(  and feeling my way in the darkness)
 
( to the
little table where the siphon stood)
 
( while Ogilvy exclaimed at the streamer
of gas that came out towards us)
 
( 

That night another invisible missile started on its way to the earth from
Mars)
 
( just a second or so under twenty-four hours after the first one)
 
(  I
remember how I sat on the table there in the blackness)
 
(  with patches of
green and crimson swimming before my eyes)
 
(  I wished I had a light to smoke
by)
 
( little suspecting the meaning of the minute gleam I had seen)
 
(  and all
that it would presently bring me)
 
(  Ogilvy watched till one)
 
(  and then gave it
up)
 
( and we lit the lantern and walked over to his house)
 
(  Down below in the
darkness)
 
(  were Ottershaw and Chertsey and all their hundreds of people)
 
( 
sleeping in peace)
 
( 

He was full of speculation that night )
 
( about the condition of Mars)
 
(  and
scoffed at the vulgar idea)
 
(  of its having inhabitants who were signalling us)
 
( 
His idea was that meteorites might be falling )
 
( in a heavy shower upon the
planet)
 
(  or that a huge volcanic explosion was in progress)
 
(  He pointed out to
me how unlikely it was )
 
( that organic evolution had taken the same direction
in the two adjacent planets)
 
( 

The chances against anything manlike on Mars)
 
(  are a million to one)
 
( 
Hundreds of observers saw the flame that night)
 
(  and the night after about
midnight)
 
(  and again the night after)
 
( and so for ten nights)
 
(  a flame each
night)
 
( Why the shots ceased after the tenth)
 
(  no one on earth has attempted to
explain)
 
(  It may be the gases of the firing caused the Martians
inconvenience)
 
(  Dense clouds of smoke or dust)
 
(  visible through a powerful
telescope on earth as little grey fluctuating patches)
 
(  spread through the
clearness of the planets atmosphere and obscured its more familiar
features)
 
( 

Even the daily papers woke up to the disturbances at last)
 
(  and popular notes
appeared here there and everywhere )
 
( concerning the volcanoes upon Mars)
 
( The
seriocomic periodical Punch)
 
( made a happy use of it in the
political cartoon)
 
(  all unsuspected those missiles the Martians had
fired at us drew earthward)
 
(  rushing now at a pace of many miles a second)
 
( 
through the empty gulf of space)
 
(  hour by hour and day by day)
 
(  nearer and
nearer)
 
( It seems to me now almost incredibly wonderful)
 
( with that swift
fate hanging over us)
 
(  men could go about their petty concerns as they did)
 
(  I
remember how jubilant Markham was )
 
( at securing a new photograph of the planet)
 
( 
for the illustrated paper he edited in those days)
 
(  People in these latter
times scarcely realise the abundance and enterprise of our
nineteenth-century papers)
 
(  For my own part I was much occupied in learning
to ride the bicycle)
 
( and busy upon a series of papers )
 
( discussing the
probable developments of moral ideas as civilisation progressed)
 
( 

One night I went for a walk with my wife)
 
( It was starlight and I explained the
Signs of the Zodiac to her)
 
(  and pointed out Mars)
 
(  a bright dot of light
creeping zenithward)
 
(  towards which so many telescopes were pointed)
 
(  It was a
warm night)
 
(  Coming home a party of excursionists from Chertsey or Isleworth)
 
( 
passed us singing and playing music)
 
(  There were lights in the upper windows
of the houses)
 
(  as the people went to bed)
 
(  From the railway station in the
distance)
 
(  came the sound of shunting trains)
 
(  ringing and rumbling)
 
( softened
almost into melody by the distance)
 
(  My wife pointed out to me the brightness)
 
( 
of the signal lights hanging in a framework against
the sky.)
 
(  It seemed so safe and tranquil)
 
( 

  Then came the night of the first falling star)
 
(  It was seen early in the
morning)
 
(  rushing over Winchester eastward)
 
( a line of flame high in the
atmosphere)
 
( Hundreds must have seen it)
 
(  and taken it for an ordinary falling
star)
 
( Albin described it as leaving a greenish streak behind it)
 
(  that glowed
for some seconds)
 
(  Denning our greatest authority on meteorites)
 
(  stated that
the height of its first appearance )
 
( was about ninety or one hundred miles)
 
(  It
seemed to him )
 
( that it fell to earth about one hundred miles east of him)
 

;;;new

( 

I was at home at that hour )
 
( and writing in my study)
 
(  and although my French
windows )
 
( face towards Ottershaw )
 
( and the blind was up)
 
( for I loved in those
days to look up at the night sky)
 
( I saw nothing of it)
 
( Yet this strangest
of all things that ever came to earth from outer space )
 
( must have fallen
while I was sitting there)
 
(  visible to me had I only looked up as it passed)
 
( 
Some of those who saw its flight )
 
( say it travelled with a hissing sound)
 
(  I
myself heard nothing of that)
 
(  Many people in Berkshire Surrey and
Middlesex )
 
( must have seen the fall of it)
 
(  and at most have thought that
another meteorite had descended)
 
(  No one seems to have troubled to look)
 
(  for
the fallen mass that night)
 
( 

But very early in the morning poor Ogilvy)
 
( who had seen the shooting star)
 
( 
and who was persuaded )
 
( that a meteorite lay somewhere on the common )
 
( rose early with the idea of finding it)
 
(  Find
it he did)
 
(  soon after dawn= and not far from the sand pits)
 
(  An enormous hole
had been made )
 
( by the impact of the projectile)
 
(  and the sand and gravel had
been flung violently )
 
( in every direction over the heath)
 
(  forming heaps
visible a mile and a half away)
 
(  The heather was on fire eastward)
 
(  and a thin
blue smoke rose against the dawn)
 
( 

The Thing itself lay almost entirely buried in sand)
 
(  amidst the scattered
splinters of a fir tree)
 
(  it had shivered to fragments in its descent)
 
(  The
uncovered part )
 
( had the appearance of a huge cylinder)
 
(  caked over and its
outline softened )
 
( by a thick scaly dun-coloured incrustation)
 
(  It had a
diameter of about thirty yards)
 
( He approached the mass)
 
(  surprised at the
size and more so at the shape)
 
(  since most meteorites are rounded more or
less completely)
 
(  It was still so hot from its flight through the
air )
 
( as to forbid his near approach)
 
(  A stirring noise within its cylinder )
 
( he
ascribed to the unequal cooling of its surface)
 
(  for at that time it had not
occurred to him that it might be hollow)
 
( 

He remained standing at the edge of the pit )
 
( that the Thing had made for
itself)
 
(  staring at its strange appearance)
 
(  astonished chiefly at its unusual
shape and colour)
 
(  and dimly perceiving even then )
 
( some evidence of design in
its arrival)
 
(  The early morning was wonderfully still)
 
(  and the sun just
clearing the pine trees towards Weybridge)
 
(  was already warm)
 
(  He did not
remember hearing any birds that morning)
 
( there was certainly no breeze
stirring)
 
(  and the only sounds )
 
( were the faint movements from within the
cindery cylinder)
 
(  He was all alone on the common)
 
( 

Then suddenly he noticed )
 
( with a start that some of the grey clinker)
 
(  the
ashy incrustation that covered the meteorite)
 
(  was falling off the circular
edge of the end)
 
( It was dropping off in flakes )
 
( and raining down upon the
sand)
 
( A large piece suddenly came off )
 
( nd fell with a sharp noise )
 
( that
brought his heart into his mouth)
 
( 

For a minute he scarcely realised what this meant)
 
( although the heat
was excessive)
 
(  he clambered down into the pit )
 
( close to the bulk to see the
Thing more clearly)
 
(  He fancied even then that the cooling of the body )
 
( might
account for this)
 
( but what disturbed that idea )
 
( was the fact that the ash was
falling only from the end of the cylinder)
 
( 

And then he perceived that)
 
( very slowly the circular top of the cylinder
was rotating on its body)
 
( It was such a gradual movement )
 
( that he discovered
it only through noticing that a black mark that had been near him five
minutes ago )
 
( was now at the other side of the circumference)
 
( Even then he
scarcely understood what this indicated)
 
( until he heard a muffled grating
sound )
 
( and saw the black mark jerk forward an inch or so)
 
( Then the thing came
upon him in a flash)
 
( The cylinder was artificial)
 
( with an end that
screwed out)
 
(  Something within the cylinder was unscrewing the top)
 
( 

There is a man in it)
 
( men in it)
 
( Half roasted
to death)
 
( Trying to escape)
 
( 

with a quick mental leap)
 
( he linked the Thing with the flash upon
Mars)
 
( 

The thought of the confined creature )
 
( was so dreadful to him that he forgot
the heat )
 
( and went forward to the cylinder to help turn)
 
( But luckily the dull
radiation arrested him )
 
( before he could burn his hands on the still-glowing
metal)
 
( At that he stood irresolute for a moment)
 
( then turned and scrambled out
of the pit)
 
( and set off running wildly into Woking)
 
( The time then must have
been somewhere about six oclock)
 
( He met a waggoner )
 
( and tried to make him
understand)
 
( but the tale he told )
 
( and his appearance were so wild)
 
( his hat
had fallen off in the pit)
 
( that the man simply drove on)
 
( He was equally
unsuccessful with the potman )
 
( who was just unlocking the doors of the
public-house)
 
(  The fellow thought he was a lunatic at large)
 
( 
and made an unsuccessful attempt)
 
(  to shut him into the taproom)
 
(  That sobered
him a little)
 
(  and when he saw Henderson)
 
(  the London journalist in his
garden)
 
(  he called over the palings )
 
( and made himself understood

you saw that shooting star last night)
 
( 

But it is something more than a meteorite)
 
( It is a cylinder)
 
( an artificial
cylinder)
 
( And there's something inside)
 
( 

Henderson was a minute or so taking it
in)
 
( Then he dropped his spade)
 
( snatched up his jacket)
 
( and came out into the
road)
 
( The two men hurried back )
 
( and found the cylinder)
 
( 
still lying in the same position)
 
( But now the sounds inside )
 
( had ceased and
a thin circle of bright metal )
 
( showed between the top and the body of the
cylinder)
 
( Air was either entering or escaping at the rim)
 
(  with a thin
sizzling sound)
 
( 

They listened rapped on the scaly burnt metal with a stick)
 
( meeting
with no response)
 
(  they both concluded the man or men inside must be
insensible or dead)
 
( 

Of course the two were quite unable to do anything)
 
( They shouted consolation
and promises)
 
( and went off back to the town again to get help)

;;;;new
 
( One can
imagine them)
 
( covered with sand)
 
( excited and disordered)
 
( running up the
little street in the bright sunlight )
 
( just as the shop folks were taking down
their shutters )
 
( and people were opening their bedroom windows)
 
( Henderson went
into the railway station at once)
 
( in order to telegraph the news to London)
 
( 
The newspaper articles had prepared mens minds )
 
( for the reception of the
idea)
 
( 

By eight oclock)
 
(  a number of boys and unemployed men)
 
(  had already started for
the common )
 
( to see the dead men from Mars)
 
(  That was the form the story
took)
 
( I heard of it first )
 
( from my newspaper boy about a quarter to nine )
 
( when
I went out to get my Daily Chronicle)
 
(  I was naturally startled)
 
( and lost no
time in going out )
 
( and across the Ottershaw bridge to the sand pits)
 
( 

I found a little crowd of perhaps twenty people )
 
(surrounding the huge hole in
which the cylinder lay)
 
( I have already described the appearance of that
colossal bulk  )
 
(embedded in the ground  )
 
(The turf and gravel about it seemed
charred as if by a sudden explosion )
 
( No doubt its impact had caused a flash
of fire  )
 
(Henderson and Ogilvy were not there )
 
( I think they perceived that
nothing was to be done for the present  )
 
(and had gone away to breakfast at
Henderson s house )
 
(

There were four or five boys sitting on the edge of the Pit  )
 
(with their feet
dangling )
 
( and amusing themselves until I stopped them by throwing stones
at the giant mass  )
 
(After I had spoken to them about it )
 
(they began playing
at  touch  )
 
(in and out of the group of bystanders )
 
(

Among these were a couple of cyclists )
 
( a jobbing gardener I employed
sometimes )
 
( a girl carrying a baby  )
 
(Gregg the butcher and his little boy )
 
( and
two or three loafers and golf caddies)
 
( who were accustomed to hang about the
railway station  )
 
(There was very little talking  )
 
(Few of the common people in
England had anything )
 
(but the vaguest astronomical ideas in those days )
 
( Most
of them were staring quietly at the big tablelike end of the cylinder  )
 
(which
was still as Ogilvy and Henderson had left it  )
 
(I fancy the popular
expectation of a heap of charred corpses )
 
(was disappointed at this inanimate
bulk  )
 
(Some went away while I was there  )
 
(and other people came  I clambered)
 
(
into the pit and fancied I heard a faint movement under my feet  )
 
(The top had
certainly ceased to rotate )
 
(

It was only when I got thus close to it )
 
(that the strangeness of this object
was at all evident to me  )
 
(At the first glance it was really no more exciting)
 
(
than an overturned carriage or a tree blown across the road )
 
( Not so much so 
indeed  )
 
(It looked like a rusty gas float  )
 
(It required a certain amount of
scientific education)
 
( to perceive that the grey scale of the Thing )
 
(was no
common oxide  )
 
(that the yellowish-white metal that gleamed )
 
(in the crack
between the lid and the cylinder had an unfamiliar hue  )
 
( Extra-terrestrial 
had no meaning for most of the onlookers )
 
(

At that time it was quite clear in my own mind that the Thing )
 
(had come from
the planet Mars )
 
( but I judged it improbable that it contained any living
creature  )
 
(I thought the unscrewing might be automatic  )
 
(In spite of Ogilvy  I
still believed that there were men in Mars )
 
( My mind ran fancifully on the
possibilities of its containing manuscript )
 
( on the difficulties in
translation that might arise  )
 
(whether we should find coins and models in it 
and so forth )
 
( Yet it was a little too large for assurance on this idea  )
 
(I
felt an impatience to see it opened )
 
(About eleven  as nothing seemed
happening )
 
( I walked back )
 
( full of such thought  to my home in Maybury  But I
found it difficult to get to work upon my abstract investigations )
 
(

In the afternoon the appearance of the common had altered very much)
 
(  The
early editions of the evening papers had startled London with enormous
headlines )
 
(Ogilvy s wire to the Astronomical Exchange )
 
(had
roused every observatory in the three kingdoms )
 
(

There were half a dozen flies or more from the Woking station)
 
( standing in
the road by the sand pits)
 
(  a basket-chaise from Chobham )
 
( and a rather lordly
carriage )
 
( Besides that  there was quite a heap of bicycles  )
 
(In addition  a
large number of people must have walked  )
 
(in spite of the heat of the day 
from Woking and Chertsey  )
 
(so that there was altogether quite a considerable
crowd )
 
(one or two gaily dressed ladies among the others )
 
(

It was glaringly hot )
 
( not a cloud in the sky nor a breath of wind )
 
(and the
only shadow was that of the few scattered pine trees )
 
( The burning heather
had been extinguished )
 
( but the level ground towards Ottershaw was blackened)
 
(
as far as one could see  )
 
(and still giving off vertical streamers of smoke )
 
(
An enterprising sweet-stuff dealer in the Chobham Road )
 
(had sent up his son
with a barrow-load of green apples and ginger beer )
 
(

Going to the edge of the pit )
 
( I found it occupied by a group of about half a
dozen men Henderson )
 
( Ogilvy  and a tall  fair-haired man that I afterwards
learned )
 
(was Stent  the Astronomer Royal  )
 
(with several workmen wielding
spades and pickaxes  )
 
(Stent was giving directions in a clear  high-pitched
voice  )
 
(He was standing on the cylinder )
 
( which was now evidently much cooler 
his face was crimson )
 
(and streaming with perspiration  and something seemed
to have irritated him )
 
(

A large portion of the cylinder had been uncovered )
 
( though its lower end was
still embedded  )
 
(As soon as Ogilvy saw me among the staring crowd on the edge)
 
(
of the pit he called to me to come down  )
 
(and asked me if I would mind going
over to see Lord Hilton  the lord of the manor )
 
(

The growing crowd  he said  )
 
(was becoming a serious impediment to their
excavations  especially the boys  )
 
(They wanted a light railing put up  and
help to keep the people back  )
 
(He told me that a faint stirring was)
 
(
occasionally still audible within the case )
 
( but that the workmen had failed
to unscrew the top)
 
(  as it afforded no grip to them  )
 
(The case appeared to be
enormously thick)
 
(  and it was possible that the faint sounds we heard
represented a noisy tumult in the interior )
 
(

I was very glad to do as he asked  )
 
(and so become one of the privileged
spectators within the contemplated enclosure )
 
( I failed to find Lord Hilton
at his house )
 
( but I was told he was expected from London by the six o clock
train from Waterloo)
 
(  and as it was then about a quarter past five )
 
( I went
home  had some tea )
 
( and walked up to the station to waylay him )
 
(
When I returned to the common the sun was setting  )
 
(Scattered groups were
hurrying from the direction of Woking  )
 
(and one or two persons were
returning  )
 
(The crowd about the pit had increased )
 
( and stood out black
against the lemon yellow of the sky a couple of hundred people  )
 
(perhaps 
There were raised voices  )
 
(and some sort of struggle appeared to be going on
about the pit )
 
( Strange imaginings passed through my mind  )
 
(As I drew nearer I
heard Stent s voice )
 
(

 Keep back  Keep back  )
 
(

A boy came running towards me )
 
(

 It s a-movin)
 
(    he said to me as he passed  )
 
( a-screwin  and a-screwin  out )
 
(
I don t like it )
 
( I m a-goin   ome  I am  )
 
(

I went on to the crowd  )
 
(There were really  I should think  )
 
(two or three
hundred people elbowing and jostling one another  )
 
(the one or two ladies
there being by no means the least active )
 
(

 He s fallen in the pit )
 
(  cried some one )
 
(

 Keep back   said several )
 
(

The crowd swayed a little  )
 
(and I elbowed my way through  )
 
(Every one seemed
greatly excited )
 
( I heard a peculiar humming sound from the pit )
 
(

 I say   said Ogilvy  )
 
( help keep these idiots back)
 
(  We don t know what s in
the confounded thing  you know  )
 
(

I saw a young man  )
 
(a shop assistant in Woking I believe )
 
(he was  standing on
the cylinder )
 
(and trying to scramble out of the hole again )
 
( The crowd had
pushed him in )
 
(

The end of the cylinder was being screwed out from within  )
 
(Nearly two feet
of shining screw projected )
 
( Somebody blundered against me )
 
( and I narrowly
missed being pitched onto the top of the screw )
 
( I turned  and as I did so
the screw must have come out )
 
( for the lid of the cylinder fell upon the
gravel with a ringing concussion )
 
( I stuck my elbow into the person behind
me  and turned my head towards the )
 
(Thing again  )
 
(For a moment that circular
cavity seemed perfectly black)
 
(  I had the sunset in my eyes )
 
(

I think everyone expected to see a man emerge )
 
(possibly something a little
unlike us terrestrial men )
 
( but in all essentials a man)
 
(  I know I did  But 
looking  )
 
(I presently saw something stirring within the shadow  )
 
(greyish
billowy movements  one above another  and then two luminous disks like
eyes  )
 
(Then something resembling a little grey snake )
 
( about the thickness of
a walking stick  )
 
(coiled up out of the writhing middle  )
 
(and wriggled in the
air towards me and then another )
 
(

A sudden chill came over me  )
 
(There was a loud shriek from a woman behind  I
half turned )
 
( keeping my eyes fixed upon the cylinder still  )
 
(from which other
tentacles were now projecting)
 
( and began pushing my way back from the edge
of the pit  )
 
(I saw astonishment giving place to horror on the faces of the
people about me )
 
( I heard inarticulate exclamations on all sides )
 
( There was a
general movement backwards  )
 
(I saw the shopman struggling still on the edge
of the pit  )
 
(I found myself alone )
 
( and saw the people on the other side of
the pit running off )
 
( Stent among them )
 
( I looked again at the cylinder )
 
( and
ungovernable terror gripped me  )
 
(I stood petrified and staring )
 
(

A big greyish rounded bulk  the size  )
 
(perhaps  of a bear )
 
( was rising slowly
and painfully out of the cylinder  )
 
(As it bulged up and caught the light  it
glistened like wet leather )
 
(

Two large dark-coloured eyes were regarding me steadfastly )
 
( The mass that
framed them)
 
(  the head of the thing  )
 
(was rounded  and had )
 
( one might say  a
face  There was a mouth under the eyes  )
 
(the lipless brim of which quivered
and panted  )
 
(and dropped saliva )
 
( The whole creature heaved and pulsated
convulsively )
 
( A lank tentacular appendage gripped the edge of the cylinder 
another swayed in the air )
 
(

Those who have never seen a living Martian)
 
( can scarcely imagine the strange
horror of its appearance )
 
( The peculiar V-shaped mouth with its pointed upper
lip )
 
( the absence of brow ridges )
 
( the absence of a chin beneath the wedgelike
lower lip )
 
( the incessant quivering of this mouth )
 
( the Gorgon groups of
tentacles  )
 
(the tumultuous breathing of the lungs in a strange atmosphere )
 
(
the evident heaviness and painfulness of movement )
 
(due to the greater
gravitational energy of the earth above all  )
 
(the extraordinary intensity of
the immense eyes )
 
(were at once vital  intense  inhuman  crippled and
monstrous  )
 
(There was something fungoid in the oily brown skin  )
 
(something in
the clumsy deliberation of the tedious movements )
 
(unspeakably nasty  )
 
(Even at
this first encounter )
 
( this first glimpse  )
 
(I was overcome with disgust and
dread )
 
(

Suddenly the monster vanished  )
 
(It had toppled over the brim of the cylinder
and fallen into the pit  )
 
(with a thud like the fall of a great mass of
leather  )
 
(I heard it give a peculiar thick cry )
 
( and forthwith another of
these creatures appeared darkly )
 
(in the deep shadow of the aperture )
 
(

I turned and  running madly  )
 
(made for the first group of trees  )
 
(perhaps a
hundred yards away  )
 
(but I ran slantingly and stumbling  )
 
(for I could not
avert my face from these things )
 
(

There  among some young pine trees and furze bushes  )
 
(I stopped  panting  and
waited further developments )
 
( The common round the sand pits was dotted with
people )
 
( standing like myself in a half-fascinated terror )
 
( staring at these
creatures )
 
( or rather at the heaped gravel at the edge of the pit)
 
( in which
they lay )
 
( And then  with a renewed horror  )
 
(I saw a round  black object
bobbing up and down on the edge of the pit  )
 
(It was the head of the shopman
who had fallen in )
 
( but showing as a little black object against the hot
western sun  )
 
(Now he got his shoulder and knee up)
 
(  and again he seemed to
slip back until only his head was visible )
 
( Suddenly he vanished )
 
( and I could
have fancied a faint shriek had reached me )
 
( I had a momentary impulse to go
back and help him)
 
( that my fears overruled )
 
(

Everything was then quite invisible)
 
(  hidden by the deep pit and the heap of
sand that the fall of the cylinder had made  )
 
(Anyone coming along the road
from Chobham or Woking)
 
(would have been amazed at the sight a dwindling
multitude of perhaps a hundred people or more standing in a great irregular
circle)
 
(  in ditches  behind bushes )
 
( behind gates and hedges )
 
( saying little to
one another and that in short  )
 
(excited shouts  and staring  )
 
(staring hard at
a few heaps of sand  )
 
(The barrow of ginger beer stood )
 
( a queer derelict 
black against the burning sky )
 
( and in the sand pits was a row of deserted
vehicles with their horses feeding out of nosebags or pawing the ground )
 
(

After the glimpse I had had of the Martians emerging from the cylinder )
 
(in
which they had come to the earth from their planet )
 
( a kind of fascination
paralysed my actions  )
 
(I remained standing knee-deep in the heather )
 
( staring
at the mound that hid them )
 
( I was a battleground of fear and curiosity )
 
(

I did not dare to go back towards the pit  )
 
(but I felt a passionate longing
to peer into it  )
 
(I began walking  therefore )
 
( in a big curve  )
 
(seeking some
point of vantage and continually looking at the sand heaps )
 
(that hid these
new-comers to our earth )
 
( Once a leash of thin black whips  )
 
(like the arms of
an octopus )
 
( flashed across the sunset and was immediately withdrawn  )
 
(and
afterwards a thin rod rose up )
 
( joint by joint  bearing at its apex a
circular disk that spun with a wobbling motion )
 
( What could be going on
there)
 
(

Most of the spectators had gathered in one or two groups one a little crowd
towards Woking  )
 
(the other a knot of people in the direction of Chobham )
 
(
Evidently they shared my mental conflict  )
 
(There were few near me )
 
( One man I
approached he was )
 
( I perceived  a neighbour of mine  )
 
(though I did not know
his name and accosted  )
 
(But it was scarcely a time for articulate
conversation )
 
(

 What ugly brutes   he said  )
 
( Good God  What ugly brutes )
 
(  He repeated this
over and over again )
 
(

 Did you see a man in the pit)
 
(  I said  but he made no answer to that  )
 
(We
became silent  and stood watching for a time side by side  deriving  I
fancy  )
 
(a certain comfort in one another s company  )
 
(Then I shifted my
position to a little knoll that gave me the advantage of a yard)
 
( or more of
elevation and when I looked for him presently he was )
 
(walking towards Woking )
 
(

The sunset faded to twilight before anything further happened  )
 
(The crowd far
away on the left  towards Woking  seemed to grow )
 
( and I heard now a faint
murmur from it  )
 
(The little knot of people towards Chobham dispersed  )
 
(There
was scarcely an intimation of movement from the pit )
 
(

It was this  as much as anything  that gave people courage  )
 
(and I suppose
the new arrivals from Woking )
 
(also helped to restore confidence  )
 
(At any rate 
as the dusk came on a slow  )
 
(intermittent movement upon the sand pits began 
a movement that seemed to gather force )
 
(as the stillness of the evening about)
 
(
the cylinder remained unbroken  Vertical black figures in twos and threes
would advance  stop  watch )
 
( and advance again  spreading out as they did so
in a thin irregular crescent )
 
(that promised to enclose the pit in its
attenuated horns )
 
( I  too  on my side began to move towards the pit )
 
(

Then I saw some cabmen and others had walked boldly into the sand pits  )
 
(and
heard the clatter of hoofs and the gride of wheels )
 
( I saw a lad trundling
off the barrow of apples  )
 
(And then  within thirty yards of the pit 
advancing from the direction of Horsell  )
 
(I noted a little black knot of men )
 
(
the foremost of whom was waving a white flag )
 
(

This was the Deputation  )
 
(There had been a hasty consultation )
 
( and since the
Martians were evidently )
 
( in spite of their repulsive forms  )
 
(intelligent
creatures )
 
( it had been resolved to show them )
 
( by approaching them with
signals  that we too were intelligent )
 
(

Flutter  flutter  went the flag )
 
( first to the right  then to the left )
 
( It
was too far for me to recognise anyone there  )
 
(but afterwards I learned that
Ogilvy  Stent  and Henderson )
 
(were with others in this attempt at
communication )
 
( This little group had in its advance dragged inward )
 
( so to
speak  the circumference of the now)
 
( almost complete circle of people )
 
( and a
number of dim black figures followed it at discreet distances )
 
(

Suddenly there was a flash of light  )
 
(and a quantity of luminous greenish
smoke came out of the pit in three distinct puffs  )
 
(which drove up  one after
the other  straight into the still air )
 
(

This smoke or flame  perhaps  would be the better word for it)
 
(was so
bright that the deep blue sky overhead )
 
(and the hazy stretches of brown
common towards Chertsey )
 
( set with black pine trees  seemed to darken
abruptly as these puffs arose )
 
( and to remain the darker after their
dispersal  )
 
(At the same time a faint hissing sound became audible )
 
(

Beyond the pit stood the little wedge of people with the white flag at its
apex  )
 
(arrested by these phenomena  )
 
(a little knot of small vertical black
shapes upon the black ground  )
 
(As the green smoke arose )
 
( their faces flashed
out pallid green )
 
( and faded again as it vanished )
 
( Then slowly the hissing
passed into a humming )
 
( into a long  loud  droning noise  )
 
(Slowly a humped
shape rose out of the pit)
 
(  and the ghost of a beam of light seemed to
flicker out from it )
 
(

Forthwith flashes of actual flame  )
 
(a bright glare leaping from one to
another )
 
( sprang from the scattered group of men)
 
(  It was as if some invisible
jet impinged upon them and flashed into white flame )
 
( It was as if each man
were suddenly and momentarily turned to fire )
 
(

Then  by the light of their own destruction )
 
( I saw them staggering and
falling )
 
( and their supporters turning to run )
 
(

I stood staring  )
 
(not as yet realising that this was death leaping from man
to man in that little distant crowd )
 
( All I felt was that it was something
very strange  )
 
(An almost noiseless and blinding flash of light )
 
( and a man
fell headlong and lay still  )
 
(and as the unseen shaft of heat passed over
them  pine trees burst into fire  )
 
(and every dry furze bush became with one
dull thud a mass of flames  )
 
(And far away towards Knaphill I saw the flashes
of trees and hedges )
 
(and wooden buildings suddenly set alight )
 
(

It was sweeping round swiftly and steadily  )
 
(this flaming death  t)
 
(his
invisible )
 
( inevitable sword of heat )
 
( I perceived it coming towards me by the
flashing bushes it touched  )
 
(and was too astounded and stupefied to stir )
 
( I
heard the crackle of fire in the sand pits and the sudden squeal of a horse
that was as suddenly stilled )
 
( Then it was as if an invisible yet intensely
heated finger were drawn through the heather)
 
( between me and the Martians 
and all along a curving line beyond the sand pits the dark ground smoked and
crackled )
 
( Something fell with a crash far away to the left where the road
from )
 
(Woking station opens out on the common  )
 
(Forthwith the hissing and
humming ceased  )
 
(and the black  domelike object sank slowly out of sight into
the pit )
 
(

All this had happened with such swiftness that I had stood motionless 
dumbfounded)
 
( and dazzled by the flashes of light )
 
(Had that death swept
through a full circle )
 
( it must inevitably have slain me in my surprise  )
 
(But
it passed and spared me  )
 
(and left the night about me suddenly dark and
unfamiliar )
 
(

The undulating common )
 
(seemed now dark almost to blackness  except where its
roadways lay grey and pale under the deep blue sky of the early night )
 
( It
was dark  and suddenly void of men )
 
( Overhead the stars were mustering  and
in the west the sky was still a pale  bright  almost greenish blue )
 
( The tops
of the pine trees and the roofs of Horsell came out sharp and black against
the western afterglow )
 
( The Martians and their appliances were altogether
invisible )
 
( save for that thin mast upon which their restless mirror )
 
(wobbled 
Patches of bush )
 
(and isolated trees here and there smoked and glowed still 
and the houses towards Woking station )
 
(were sending up spires of flame into
the stillness of the evening air )
 
(

Nothing was changed save for that and a terrible astonishment  )
 
(The little
group of black specks with the flag of white had been swept out of
existence )
 
( and the stillness of the evening  )
 
(so it seemed to me  had
scarcely been broken )
 
(

It came to me that I was upon this dark common )
 
( helpless  unprotected  and
alone )
 
( Suddenly  like a thing falling upon me from without  came fear )
 
(

With an effort I turned and began a stumbling run through the heather )
 
(

The fear I felt was no rational fear )
 
( but a panic terror not only of the
Martians  )
 
(but of the dusk and stillness all about me  )
 
(Such an extraordinary
effect in unmanning me it had )
 
(that I ran weeping silently as a child might
do  Once I had turned  )
 
(I did not dare to look back)
 
(  I remember I felt an
extraordinary persuasion that I was being played with )
 
( that presently  when
I was upon the very verge of safety  )
 
(this mysterious death as swift as the
passage of light would leap after me)
 
( from the pit about the cylinder and
strike me down )
 
(

It is still a matter of wonder )
 
(how the Martians are able to slay men so
swiftly and so silently )
 
( Many think that in some way they are able to
generate an intense heat in a chamber of practically absolute
non-conductivity )
 
( This intense heat they project in a parallel beam against
any object they choose )
 
( by means of a polished parabolic mirror of unknown
composition)
 
(  much as the parabolic mirror of a lighthouse projects a beam of
light )
 
( But no one has absolutely proved these details )
 
(However it is done 
it is certain that a beam of heat is the essence of the matter  )
 
(Heat  and
invisible  instead of visible  light  )
 
(Whatever is combustible flashes into
flame at its touch )
 
( lead runs like water )
 
( it softens iron  cracks and melts
glass  and when it falls upon water  )
 
(incontinently that explodes into steam )
 
(

That night nearly forty people lay under the starlight about the pit )
 
(
charred and distorted beyond recognition )
 
( and all night long the common from
Horsell to Maybury )
 
(was deserted and brightly ablaze )
 
(

The news of the massacre probably reached Chobham  )
 
(Woking  and Ottershaw
about the same time  )
 
(In Woking the shops had closed when the tragedy
happened  )
 
(and a number of people  shop people and so forth )
 
( attracted by the
stories they had heard  )
 
(were walking over the Horsell Bridge and along the
road between the hedges that runs out at last upon the common )
 
( You may
imagine the young people brushed up )
 
(after the labours of the day  and making
this novelty )
 
( as they would make any novelty )
 
( the excuse for walking
together and enjoying a trivial flirtation )
 
( You may figure to yourself the
hum of voices along the road in the gloaming   )
 
( 

As yet  of course  few people in Woking )
 
(even knew that the cylinder had
opened  though poor Henderson )
 
(had sent a messenger on a bicycle to the post
office with a special wire to an evening paper )
 
(

As these folks came out by twos and threes upon the open )
 
( they found little
knots of people talking excitedly and peering at the spinning mirror over
the sand pits)
 
(  and the new-comers were )
 
( no doubt  soon infected by the
excitement of the occasion )
 
(

By half past eight  when the Deputation was destroyed  )
 
(there may have been a
crowd of three hundred people or more at this place  besides those who had
left the road to approach the Martians nearer )
 
( There were three policemen
too  one of whom was mounted  doing their best  )
 
(under instructions from
Stent )
 
( to keep the people back and deter them from approaching the cylinder )
 
(
There was some booing from those more thoughtless and excitable souls to
whom a crowd is always an occasion for noise and horse-play )
 
(

Stent and Ogilvy  anticipating some possibilities of a collision)
 
(  had
telegraphed from Horsell to the barracks as soon as the Martians emerged 
for the help )
 
(of a company of soldiers to protect these strange creatures
from violence )
 
( After that they returned to lead that ill-fated advance  The
description of their death )
 
( as it was seen by the crowd )
 
( tallies very
closely with my own impressions )
 
( the three puffs of green smoke  )
 
(the deep
humming note  and the flashes of flame )
 
(

But that crowd of people had a far narrower escape than mine  )
 
(Only the fact
that a hummock of heathery sand intercepted the lower part of the Heat-Ray
saved them )
 
( Had the elevation of the parabolic mirror been a few yards
higher )
 
( none could have lived to tell the tale )
 
( They saw the flashes and the
men falling and an invisible hand )
 
( as it were  lit the bushes as it hurried
towards them through the twilight  )
 
(Then  with a whistling note that rose
above the droning of the pit )
 
( the beam swung close over their heads 
lighting the tops of the beech trees that line the road )
 
( and splitting the
bricks  smashing the windows  )
 
(firing the window frames  and bringing down in
crumbling ruin a portion of the gable of the house nearest the corner )
 
(

In the sudden thud  hiss  and glare of the igniting trees )
 
( the
panic-stricken crowd seems to have swayed hesitatingly for some moments )
 
(
Sparks and burning twigs began to fall into the road  )
 
(and single leaves like
puffs of flame  )
 
(Hats and dresses caught fire  )
 
(Then came a crying from the
common )
 
( There were shrieks and shouts )
 
( and suddenly a mounted policeman came)
 
(
galloping through the confusion with his hands clasped over his head 
screaming )
 
(

 They re coming   a woman shrieked )
 
( and incontinently everyone was turning)
 
(
and pushing at those behind )
 
( in order to clear their way to Woking again )
 
(
They must have bolted as blindly as a flock of sheep)
 
(  Where the road grows
narrow and black between the high banks the crowd jammed )
 
( and a desperate
struggle occurred )
 
( All that crowd did not escape  three persons at least 
two women and a little boy  )
 
(were crushed and trampled there  )
 
(and left to die
amid the terror and the darkness )
 
(


For my own part  I remember nothing of my flight )
 
(except the stress of
blundering against trees and stumbling through the heather )
 
( All about me
gathered the invisible terrors of the Martians  )
 
(that pitiless sword of heat
seemed whirling to and fro )
 
( flourishing overhead before it descended and
smote me out of life)
 
(  I came into the road between the crossroads and
Horsell )
 
( and ran along this to the crossroads )
 
(

At last I could go no further )
 
( I was exhausted with the violence of my
emotion and of my flight )
 
( and I staggered and fell by the wayside )
 
( That was
near the bridge that crosses the canal by the gasworks  )
 
(I fell and lay
still )
 
(

I must have remained there some time )
 
(

I sat up  strangely perplexed  )
 
(For a moment  perhaps  I could not clearly
understand )
 
(how I came there  )
 
(My terror had fallen from me like a garment  )
 
(My
hat had gone  and my collar had burst away from its fastener )
 
( A few minutes
before  )
 
(there had only been three real things before me the immensity of
the night and space and nature )
 
( my own feebleness and anguish )
 
( and the near
approach of death )
 
( Now it was as if something turned over  )
 
(and the point of
view altered abruptly )
 
( There was no sensible transition from one state of
mind to the other )
 
( I was immediately the self of every day again a decent 
ordinary citizen )
 
( The silent common  )
 
(the impulse of my flight  )
 
(the starting
flames )
 
( were as if they had been in a dream )
 
( I asked myself had these latter
things indeed happened)
 
( I could not credit it )
 
(



;;;;;;


I rose and walked unsteadily up the steep incline of the bridge  )
 
(My mind was
blank wonder)
 
(  My muscles and nerves seemed drained of their strength )
 
( I dare
say I staggered drunkenly  )
 
(A head rose over the arch  )
 
(and the figure of a
workman carrying a basket appeared )
 
( Beside him ran a little boy  )
 
(He passed
me  wishing me good night )
 
( I was minded to speak to him  )
 
(but did not  I
answered his greeting with a meaningless mumble and went on over the bridge )
 
(

Over the Maybury arch a train )
 
( a billowing tumult of white  firelit smoke 
and a long caterpillar of lighted windows  )
 
(went flying south clatter 
clatter  clap  rap  and it had gone)
 
( A dim group of people talked in the
gate of one of the houses in the pretty little row of gables that was called
Oriental Terrace )
 
( It was all so real and so familiar  )
 
(And that behind me  It
was frantic  fantastic )
 
( Such things  I told myself  )
 
(could not be )


;;;;new
 
(

Perhaps I am a man of exceptional moods )
 
( I do not know how far my experience
is common)
 
(  At times I suffer from the strangest sense of detachment from
myself and the world about me  )
 
(I seem to watch it all from the outside  )
 
(from
somewhere inconceivably remote)
 
(  out of time  out of space )
 
( out of the stress
and tragedy of it all)
 
(  This feeling was very strong upon me that night  )
 
(Here
was another side to my dream )
 
(

But the trouble was the blank incongruity of this serenity and the swift
death flying yonder)
 
(  not two miles away  )
 
(There was a noise of business from
the gasworks)
 
(  and the electric lamps were all alight  )
 
(I stopped at the group
of people )
 
(

 What news from the common  said I )
 
(

There were two men and a woman at the gate )
 
(

 Eh  said one of the men  turning )
 
(

 What news from the common)
 
(Ain t yer just been there  asked the
men )
 
(

 People seem fair silly about the common  )
 
( said the woman over the gate )
 
(
 What s it all abart)
 
(

 Haven t you heard of the men from Mars  said I  )
 
( the creatures from Mars)
 
(

 Quite enough   said the woman over the gate  )
 
( Thenks   and all three of
them laughed )
 
(

I felt foolish and angry  )
 
(I tried and found I could not tell them )
 
(what I had
seen)
 
(  They laughed again at my broken sentences )
 
(

 You ll hear more yet   I said )
 
(and went on to my home )
 
(

I startled my wife at the doorway  so haggard was I )
 
( I went into the dining
room  sat down  )
 
(drank some wine  and so soon as I could collect myself)
 
(
sufficiently I told her the things I had seen  )
 
(The dinner  which was a cold
one )
 
( had already been served  )
 
(and remained neglected on the table while I
told my story )
 
(

 There is one thing  )
 
( I said  to allay the fears I had aroused  )
 
( they are
the most sluggish things I ever saw crawl  )
 
(They may keep the pit and kill
people who come near them  but they cannot get out of it  )
 
(   But the horror
of them  )
 
(

 Don t  dear   said my wife )
 
( knitting her brows and putting her hand on
mine )
 
(

 Poor Ogilvy   I said   )
 
(To think he may be lying dead there  )
 
(

My wife at least did not find my experience incredible  )
 
(When I saw how
deadly white her face was  I ceased abruptly )
 
(

 They may come here   )
 
(she said again and again )
 
(

I pressed her to take wine  and tried to reassure her )
 
(

 They can scarcely move   I said )
 
(

I began to comfort her and myself )
 
(by repeating all that Ogilvy had told me
of the impossibility of the Martians )
 
(establishing themselves on the earth )
 
(
In particular I laid stress on the gravitational difficulty )
 
( On the surface
of the earth the force of gravity is three times what it is on the surface
of Mars )
 
( A Martian  therefore  would weigh three times more than on Mars )
 
(
albeit his muscular strength would be the same )
 
( His own body would be a cope
of lead to him)
 
(  That  indeed  was the general opinion  )
 
(Both The Times and
the Daily Telegraph  for instance  )
 
(insisted on it the next morning  )
 
(and both
overlooked )
 
( just as I did  two obvious modifying influences )
 
(

The atmosphere of the earth  )
 
(we now know  contains far more oxygen or far
less argon )
 
(whichever way one likes to put it)
 
( than does Mars  )
 
(The
invigorating influences of this excess of oxygen )
 
(upon the Martians
indisputably did much to counterbalance the increased weight of their
bodies )
 
( And  in the second place )
 
( we all overlooked the fact that such
mechanical intelligence as the Martian possessed )
 
(was quite able to dispense
with muscular exertion at a pinch )
 
(

But I did not consider these points at the time  )
 
(and so my reasoning was
dead against the chances of the invaders )
 
( With wine and food  the confidence
of my own table )
 
( and the necessity of reassuring my wife )
 
( I grew by
insensible degrees courageous and secure )
 
(

 They have done a foolish thing   )
 
(said I  fingering my wineglass)
 
(   They are
dangerous because  )
 
(no doubt  they are mad with terror  )
 
(Perhaps they expected
to find no living things certainly no intelligent living things )
 
(

 A shell in the pit  said I  )
 
( if the worst comes to the worst will kill them
all  )
 
(

The intense excitement of the events had no doubt left my perceptive powers
in a state of erethism )
 
( I remember that dinner table with extraordinary
vividness even now  )
 
(My dear wife s sweet anxious face peering at me from
under the pink lamp shade )
 
( the white cloth with its silver and glass table
furniture for in those days even philosophical writers)
 
( had many little
luxuries the crimson-purple wine in my glass )
 
( are photographically
distinct  )
 
(At the end of it I sat  tempering nuts with a cigarette )
 
(
regretting Ogilvy s rashness )
 
( and denouncing the shortsighted timidity of
the Martians )
 
(

So some respectable dodo in the Mauritius )
 
(might have lorded it in his nest 
and discussed the arrival of that shipful of pitiless sailors in want of
animal food )
 
(  We will peck them to death tomorrow  my dear  )
 
(

I did not know it  but that was the last civilised dinner I was to eat for
very many strange and terrible days )
 
(

The most extraordinary thing to my mind  )
 
(of all the strange and wonderful
things that happened upon that Friday  )
 
(was the dovetailing of the
commonplace habits of our social order with the first beginnings of the
series of events that was to topple that social order headlong  )
 
(If on Friday
night you had taken )
 
(a pair of compasses and drawn a circle with a radius of
five miles round the Woking sand pits  )
 
(I doubt if you would have had one
human being outside it  )
 
(unless it were some relation of Stent or of the
three or four cyclists or London people lying dead on the common )
 
( whose
emotions or habits were at all affected by the new-comers  )
 
(Many people had
heard of the cylinder  of course )
 
( and talked about it in their leisure  but
it certainly did not make the sensation )
 
(that an ultimatum to Germany would
have done )
 
(

In London that night poor Henderson s telegram )
 
(describing the gradual
unscrewing of the shot was judged to be a canard )
 
( and his evening paper 
after wiring for authentication from him)
 
( and receiving no reply the man was
killed decided not to print a special edition )
 
(

Even within the five-mile circle)
 
( the great majority of people were inert )
 
( I
have already described the behaviour of the men and women to whom I spoke )
 
(
All over the district people were dining and supping  )
 
(working men were
gardening after the labours of the day  children )
 
(were being put to bed 
young people were wandering through )
 
(the lanes love-making  students sat over
their books )
 
(

Maybe there was a murmur in the village streets)
 
(  a novel and dominant topic
in the public-houses  )
 
(and here and there a messenger  )
 
(or even an eye-witness
of the later occurrences  caused a whirl of excitement  )
 
(a shouting  and a
running to and fro )
 
( but for the most part the daily routine of working 
eating  drinking  sleeping  )
 
(went on as it had done for countless years as
though no planet Mars existed in the sky  )
 
(Even at Woking station and Horsell
and Chobham that was the case )
 
(

In Woking junction  until a late hour  trains were stopping and going on )
 
(
others were shunting on the sidings  passengers were alighting and waiting )
 
(
and everything was proceeding in the most ordinary way )
 
( A boy from the town 
trenching on Smith s monopoly  )
 
(was selling papers with the afternoon s news )
 
(
The ringing impact of trucks  the sharp whistle of the engines from the
junction  )
 
(mingled with their shouts of  Men from Mars  )
 
( Excited men came
into the station about nine o clock with incredible tidings  )
 
(and caused no
more disturbance than drunkards might have done )
 
( People rattling Londonwards
peered into the darkness outside the carriage windows )
 
( and saw only a rare 
flickering  vanishing spark dance up from the direction of Horsell)
 
(  a red
glow and a thin veil of smoke driving across the stars )
 
( and thought that
nothing more serious than a heath fire was happening )
 
( It was only round the
edge of the common that any disturbance was perceptible  )
 
(There were half a
dozen villas burning on the Woking border  )
 
(There were lights in all the
houses on the common side of the three villages)
 
(  and the people there kept
awake till dawn )
 
(

A curious crowd lingered restlessly  )
 
(people coming and going but the crowd
remaining  both on the Chobham and Horsell bridges )
 
( One or two adventurous
souls)
 
(  it was afterwards found  )
 
(went into the darkness and crawled quite
near the Martians )
 
( but they never returned  )
 
(for now and again a light-ray 
like the beam of a warship s searchlight swept the common )
 
( and the Heat-Ray
was ready to follow  )
 
(Save for such  that big area of common was silent and
desolate  and the charred bodies lay about on it all night )
 
(under the stars 
and all the next day  )
 
(A noise of hammering from the pit )
 
(was heard by many
people )
 
(

So you have the state of things on Friday night  )
 
(In the centre  sticking
into the skin of our old planet Earth)
 
( like a poisoned dart  )
 
(was this
cylinder )
 
( But the poison was scarcely working yet )
 
( Around it was a patch of
silent common  )
 
(smouldering in places  )
 
(and with a few dark  )
 
(dimly seen
objects lying in contorted attitudes here and there )
 
( Here and there was a
burning bush or tree  )
 
(Beyond was a fringe of excitement  and farther than
that fringe the inflammation had not crept as yet  )
 
(In the rest of the world
the stream of life still flowed as it had flowed for immemorial years )
 
( The
fever of war that would presently clog vein and artery )
 
( deaden nerve and
destroy brain )
 
( had still to develop 

All night long the Martians were hammering and stirring )
 
( sleepless 
indefatigable  at work upon the machines they were making ready )
 
( and ever
and again a puff of greenish-white smoke whirled up to the starlit sky )
 
(

About eleven a company of soldiers came through Horsell )
 
( and deployed along
the edge of the common to form a cordon )
 
( Later a second company marched
through Chobham to deploy on the north side of the common)
 
(  Several officers
from the Inkerman barracks)
 
( had been on the common earlier in the day  and
one  Major Eden )
 
( was reported to be missing )
 
( The colonel of the regiment
came to the Chobham bridge)
 
( and was busy questioning the crowd at midnight 
The military authorities were certainly alive to the seriousness of the
business )
 
( About eleven  the next morning s papers )
 
(were able to say  a
squadron of hussars  two Maxims  )
 
(and about four hundred men of the Cardigan
regiment started from Aldershot )
 
(

A few seconds after midnight the crowd in the Chertsey road  )
 
(Woking  saw a
star fall from heaven into the pine woods to the northwest )
 
( It had a
greenish colour  and caused a silent brightness like summer lightning )
 
( This
was the second cylinder )
 
(

Saturday lives in my memory as a day of suspense  )
 
(It was a day of lassitude
too  hot and close  with  I am told)
 
(  a rapidly fluctuating barometer  )
 
(I had
slept but little  though my wife had succeeded in sleeping  )
 
(and I rose
early  I went into my garden before breakfast )
 
(and stood listening  but
towards the common there was nothing stirring but a lark )
 
(

The milkman came as usual )
 
( I heard the rattle of his chariot and I went
round to the side gate to ask the latest news  )
 
(He told me that during the
night the Martians had been surrounded by troops )
 
( and that guns were
expected )
 
( Then a familiar  reassuring note)
 
( I heard a train running towards
Woking )
 
(

 They aren t to be killed   said the milkman  )
 
( if that can possibly be
avoided  )
 
(

I saw my neighbour gardening )
 
( chatted with him for a time  and then strolled
in to breakfast )
 
( It was a most unexceptional morning )
 
( My neighbour was of
opinion that the troops would be able to capture )
 
(or to destroy the Martians
during the day )
 
(

 It s a pity they make themselves so unapproachable   he said   )
 
(It would be
curious to know how they live on another planet )
 
( we might learn a thing or
two  )
 
(

He came up to the fence and extended a handful of strawberries)
 
(  for his
gardening was as generous as it was enthusiastic  )
 
(At the same time he told
me of the burning of the pine woods about the Byfleet Golf Links )
 
(

 They say   said he )
 
(  that there s another of those blessed things fallen
there number two )
 
( But one s enough  surely  )
 
(This lot ll cost the insurance
people a pretty penny before everything s settled   )
 
(He laughed with an air
of the greatest good humour as he said this )
 
( The woods  he said  )
 
(were still
burning )
 
( and pointed out a haze of smoke to me   )
 
(They will be hot under foot
for days  )
 
(on account of the thick soil of pine needles and turf  )
 
( he said 
and then grew serious over  poor Ogilvy  )
 
(

After breakfast  instead of working )
 
( I decided to walk down towards the
common )
 
( Under the railway bridge I found a group of soldiers sappers)
 
(  I
think  men in small round caps  )
 
(dirty red jackets unbuttoned  and showing
their blue shirts  dark trousers  )
 
(and boots coming to the calf)
 
(  They told me
no one was allowed over the canal  )
 
(and  looking along the road towards the
bridge )
 
( I saw one of the Cardigan men standing sentinel there  )
 
(I talked with
these soldiers for a time)
 
(  I told them of my sight of the Martians on the
previous evening )
 
( None of them had seen the Martians  )
 
(and they had but the
vaguest ideas of them )
 
( so that they plied me with questions  )
 
(They said that
they did not know who had authorised the movements of the troops )
 
( their idea
was that a dispute had arisen at the Horse Guards )
 
( The ordinary sapper is a
great deal better educated than the common soldier  )
 
(and they discussed the
peculiar conditions of the possible fight with some acuteness  )
 
(I described
the Heat-Ray to them  )
 
(and they began to argue among themselves )
 
(

 Crawl up under cover and rush  em  say I   said one )
 
(

 Get aht    said another )
 
(  What s cover against this  ere  eat)
 
( Sticks to
cook yer  )
 
(What we got to do is to go as near as the ground ll let us )
 
( and
then drive a trench  )
 
(

 Blow yer trenches)
 
(  You always want trenches  you ought to ha  been born a
rabbit Snippy  )
 
(

  Ain t they got any necks  then said a third )
 
( abruptly a little 
contemplative  dark man  smoking a pipe )
 
(

I repeated my description )
 
(

 Octopuses   said he )
 
(  that s what I calls  em  )
 
(Talk about fishers of
men fighters of fish it is this time  )
 
(

 It ain t no murder killing beasts like that   )
 
(said the first speaker )
 
(

 Why not shell the darned things strite off and finish  em)
 
(  said the little
dark man   )
 
(You carn tell what they might do  )
 
(

 Where s your shells)
 
(  said the first speaker   )
 
(There ain t no time )
 
( Do it
in a rush  that s my tip  and do it at once  )
 
(

So they discussed it  )
 
(After a while I left them )
 
( and went on to the railway
station)
 
( to get as many morning papers as I could 

But I will not weary the reader)
 
( with a description of that long morning and
of the longer afternoon )
 
( I did not succeed in getting a glimpse of the
common )
 
( for even Horsell and Chobham church towers were in the hands of the
military authorities)
 
(  The soldiers I addressed didn t know anything )
 
( the
officers were mysterious as well as busy )
 
( I found people in the town quite
secure again in the presence of the military  )
 
(and I heard for the first time
from Marshall )
 
( the tobacconist  that his son was among the dead on the
common )
 
( The soldiers had made the people on the outskirts of Horsell lock up)
 
(
and leave their houses )
 
(

I got back to lunch about two )
 
( very tired for  as I have said )
 
( the day was
extremely hot and dull )
 
( and in order to refresh myself I took a cold bath in
the afternoon )
 
( About half past four I went up to the railway station )
 
(to get
an evening paper )
 
( for the morning papers had contained only a very
inaccurate description of the killing of Stent  )
 
(Henderson  Ogilvy  and the
others )
 
( But there was little I didn t know  )
 
(The Martians did not show an
inch of themselves )
 
( They seemed busy in their pit  )
 
(and there was a sound of
hammering )
 
(and an almost continuous streamer of smoke )
 
( Apparently they were
busy getting ready for a struggle   )
 
(Fresh attempts have been made to signal 
but without success )
 
(  was the stereotyped formula of the papers  )
 
(A sapper
told me it was done by a man in a ditch with a flag on a long pole  )
 
(The
Martians took as much notice of such advances as we should of the lowing of
a cow )
 
(

I must confess the sight of all this armament  all this preparation  )
 
(greatly
excited me)
 
(  My imagination became belligerent )
 
( and defeated the invaders in
a dozen striking ways  something of my schoolboy dreams of battle and
heroism came back)
 
(  It hardly seemed a fair fight to me at that time  )
 
(They
seemed very helpless in that pit of theirs )
 
(

About three o clock there began the thud of a gun at measured intervals from
Chertsey or Addlestone )
 
( I learned that the smouldering pine wood into which
the second cylinder had fallen was being shelled )
 
( in the hope of destroying
that object before it opened )
 
( It was only about five )
 
( however  that a field
gun reached Chobham )
 
(for use against the first body of Martians )
 
(

About six in the evening )
 
( as I sat at tea with my wife in the summerhouse
talking vigorously about the battle that was lowering upon us )
 
( I heard a
muffled detonation from the common )
 
( and immediately after a gust of firing 
Close on the heels of that came a violent rattling crash  )
 
(quite close to us 
that shook the ground  and  starting out upon the lawn  )
 
(I saw the tops of
the trees about the Oriental College burst into smoky red flame  )
 
(and the
tower of the little church beside it slide down into ruin )
 
( The pinnacle of
the mosque had vanished )
 
( and the roof line of the college itself looked as
if a hundred-ton gun had been at work upon it  )
 
(One of our chimneys cracked
as if a shot had hit it  flew )
 
( and a piece of it came clattering down the
tiles and made a heap of broken red fragments )
 
(upon the flower bed by my
study window )
 
(

I and my wife stood amazed )
 
( Then I realised that the crest of Maybury Hill
must be within range of the Martians  Heat-Ray )
 
(now that the college was
cleared out of the way )
 
(

At that I gripped my wife s arm )
 
( and without ceremony ran her out into the
road )
 
( Then I fetched out the servant )
 
( telling her I would go upstairs myself
for the box she was clamouring for )
 
(

 We can t possibly stay here  )
 
( I said  and as I spoke the firing reopened
for a moment upon the common )
 
(

 But where are we to go  said my wife in terror )
 
(

I thought perplexed  )
 
(Then I remembered her cousins at Leatherhead )
 
(

 Leatherhead   I shouted above the sudden noise )
 
(

She looked away from me downhill )
 
( The people were coming out of their
houses  astonished )
 
(

 How are we to get to Leatherhead  she said )
 
(

Down the hill I saw a bevy of hussars ride under the railway bridge  )
 
(three
galloped through the open gates of the Oriental College )
 
( two others
dismounted )
 
( and began running from house to house )
 
( The sun  shining through
the smoke that drove up from the tops of the trees  )
 
(seemed blood red  and
threw an unfamiliar lurid light upon everything )
 
(

 Stop here   said I   you are safe here  )
 
( and I started off at once for the
Spotted Dog  )
 
(for I knew the landlord had a horse and dog cart )
 
( I ran  for I
perceived that in a moment everyone upon this side of the hill would be
moving  )
 
(I found him in his bar  )
 
(quite unaware of what was going on behind
his house )
 
( A man stood with his back to me  talking to him )
 
(

 I must have a pound   said the landlord )
 
(  and I ve no one to drive it  )
 
(

 I ll give you two   said I  )
 
(over the stranger s shoulder )
 
(

 What for)
 
(

 And I ll bring it back by midnight   I said )
 
(

 Lord   said the landlord  )
 
( what s the hurry)
 
(I m selling my bit of a pig 
Two pounds  and you bring it back)
 
(What s going on now)
 
(

I explained hastily that I had to leave my home )
 
( and so secured the dog
cart  )
 
(At the time it did not seem to me nearly so urgent)
 
( that the landlord
should leave his )
 
( I took care to have the cart there and then )
 
( drove it off
down the road )
 
( and  leaving it in charge of my wife and servant )
 
( rushed into
my house and packed a few valuables  )
 
(such plate as we had  and so forth  )
 
(The
beech trees below the house were burning)
 
( while I did this  and the palings
up the road glowed red  )
 
(While I was occupied in this way  one of the
dismounted hussars came running up )
 
( He was going from house to house 
warning people to leave)
 
(  He was going on as I came out of my front door 
lugging my treasures )
 
( done up in a tablecloth  I shouted after him )
 
(

He turned  stared  bawled something about  )
 
(crawling out in a thing like a
dish cover  )
 
( and ran on to the gate of the house at the crest )
 
( A sudden
whirl of black smoke driving across the road hid him for a moment )
 
( I ran to
my neighbour s door and rapped to satisfy myself of what I already knew 
that his wife )
 
(had gone to London with him and had locked up their house )
 
( I
went in again  according to my promise )
 
( to get my servant s box  lugged it
out )
 
( clapped it beside her on the tail of the dog cart  )
 
(and then caught the
reins and jumped up into the driver s seat beside my wife )
 
( In another moment
we were clear of the smoke and noise)
 
(  and spanking down the opposite slope
of Maybury Hill towards Old Woking )
 
(

In front was a quiet sunny landscape )
 
( a wheat field ahead on either side of
the road  and the Maybury Inn with its swinging sign  )
 
(I saw the doctor s
cart ahead of me )
 
( At the bottom of the hill I turned my head to look at the
hillside I was leaving  )
 
(Thick streamers o)
 
(f black smoke shot with threads of
red fire were driving up into the still air  and throwing dark shadows upon
the green treetops eastward  )
 
(The smoke already extended far away to the east
and west to the fleet pine woods eastward )
 
( and to Woking on the west )
 
( The
road was dotted with people running towards us  )
 
(And very faint now  but very
distinct through the hot  quiet air )
 
( one heard the whirr of a machine-gun
that was presently stilled  )
 
(and an intermittent cracking of rifles )
 
(
Apparently the Martians were setting fire to everything within range of
their Heat-Ray )
 
(

I am not an expert driver  )
 
(and I had immediately to turn my attention to the
horse  )
 
(When I looked back again the second hill had hidden the black smoke 
I slashed the horse with the whip  )
 
(and gave him a loose rein until Woking
and Send lay between us and that quivering tumult  )
 
(I overtook and passed the
doctor between Woking and Send )
 
(

Leatherhead is about twelve miles from Maybury Hill  )
 
(The scent of hay was in
the air through the lush meadows beyond Pyrford )
 
( and the hedges on either
side were sweet and gay with multitudes of dog-roses )
 
( The heavy firing that
had broken out while we were driving down Maybury Hill ceased as abruptly as
it began )
 
( leaving the evening very peaceful and still )
 
( We got to Leatherhead
without misadventure about nine o clock)
 
( and the horse had an hour s rest
while I took supper with my cousins and commended my wife to their care )
 
(

My wife was curiously silent throughout the drive  )
 
(and seemed oppressed with
forebodings of evil  )
 
(I talked to her reassuringly  pointing out that the
Martians were tied to the Pit by sheer heaviness  )
 
(and at the utmost could
but crawl a little out of it  )
 
(but she answered only in monosyllables  )
 
(Had it
not been for my promise to the innkeeper  )
 
(she would  I think  have urged me
to stay in Leatherhead that night  )
 
(Would that I had  )
 
(Her face  I remember 
was very white as we parted )
 
(

For my own part  I had been feverishly excited all day  )
 
(Something very like
the war fever that occasionally runs through a civilised community had got
into my blood )
 
( and in my heart I was not so very sorry that I had to return
to Maybury that night )
 
( I was even afraid that that last fusillade I had
heard might mean the extermination of our invaders from Mars  )
 
(I can best
express my state of mind by saying that I wanted to be in at the death )
 
(

It was nearly eleven when I started to return )
 
( The night was unexpectedly
dark  to me)
 
(  walking out of the lighted passage of my cousins  )
 
(house  it
seemed indeed black  )
 
(and it was as hot and close as the day  )
 
(Overhead the
clouds were driving fast )
 
( albeit not a breath stirred the shrubs about us 
My cousins  man lit both lamps  )
 
(Happily  I knew the road intimately  My wife
stood in the light of the doorway  )
 
(and watched me until I jumped up into the
dog cart )
 
(Then abruptly she turned and went in )
 
( leaving my cousins side by
side wishing me good hap )
 
(

I was a little depressed at first with the contagion of my wife s fears )
 
( but
very soon my thoughts reverted to the Martians  )
 
(At that time I was
absolutely in the dark as to the course of the evening s fighting  )
 
(I did not
know even the circumstances that had precipitated the conflict )
 
( As I came
through Ockham for that was the way I returned  )
 
(and not through Send and
Old Woking)
 
(I saw along the western horizon a blood-red glow  )
 
(which as I
drew nearer )
 
( crept slowly up the sky)
 
(  The driving clouds of the gathering
thunderstorm mingled there with masses of black and red smoke )
 
(

Ripley Street was deserted  )
 
(and except for a lighted window or so the
village showed not a sign of life )
 
( but I narrowly escaped an accident at the
corner of the road to Pyrford  )
 
(where a knot of people stood with their backs
to me  )
 
(They said nothing to me as I passed )
 
( I do not know what they knew of
the things happening beyond the hill  nor do I know if the silent houses I
passed on my way were sleeping securely  or deserted and empty  )
 
(or harassed
and watching against the terror of the night )
 
(

From Ripley until I came through Pyrford I was in the valley of the Wey  )
 
(and
the red glare was hidden from me  )
 
(As I ascended the little hill beyond
Pyrford Church the glare came into view again )
 
( and the trees about me
shivered with the first intimation of the storm that was upon me  )
 
(Then I
heard midnight pealing out from Pyrford Church behind me  )
 
(and then came the
silhouette of Maybury Hill  )
 
(with its treetops and roofs black and sharp
against the red )
 
(

Even as I beheld this a lurid green glare lit the road about me and showed
the distant woods towards Addlestone )
 
( I felt a tug at the reins )
 
( I saw that
the driving clouds had been pierced as it were by a thread of green fire )
 
(
suddenly lighting their confusion and falling into the field to my left  )
 
(It
was the third falling star )
 
(

Close on its apparition  and blindingly violet by contrast )
 
( danced out the
first lightning of the gathering storm)
 
( and the thunder burst like a rocket
overhead)
 
(  The horse took the bit between his teeth and bolted )
 
(

A moderate incline runs towards the foot of Maybury Hill)
 
(  and down this we
clattered  Once the lightning had begun )
 
( it went on in as rapid a succession
of flashes as I have ever seen  )
 
(The thunderclaps  treading one on the heels
of another and with a strange crackling accompaniment )
 
( sounded more like the
working of a gigantic electric machine than the usual detonating
reverberations )
 
( The flickering light was blinding and confusing  )
 
(and a thin
hail smote gustily at my face as I drove down the slope )
 
(

At first I regarded little but the road before me )
 
( and then abruptly my
attention was arrested by something that was moving rapidly down the
opposite slope of Maybury Hill  )
 
(At first I took it for the wet roof of a
house  but one flash following another showed it to be in swift rolling
movement )
 
( It was an elusive vision a moment of bewildering darkness  and
then  in a flash like daylight  )
 
(the red masses of the Orphanage near the
crest of the hill  the green tops of the pine trees  )
 
(and this problematical
object came out clear and sharp and bright )
 
(

And this Thing I saw  )
 
(How can I describe it)
 
( A monstrous tripod  higher than
many houses )
 
( striding over the young pine trees  )
 
(and smashing them aside in
its career  a walking engine of glittering metal  )
 
(striding now across the
heather  )
 
(articulate ropes of steel dangling from it  )
 
(and the clattering
tumult of its passage mingling with the riot of the thunder )
 
( A flash  and it
came out vividly  )
 
(heeling over one way with two feet in the air  to vanish
and reappear almost instantly as it seemed )
 
( with the next flash  a hundred
yards nearer  )
 
(Can you imagine a milking stool tilted and bowled violently
along the ground)
 
(That was the impression those instant flashes gave  )
 
(But
instead of a milking stool imagine it a great body of machinery on a tripod
stand )
 
(

Then suddenly the trees in the pine wood ahead of me were parted  )
 
(as brittle
reeds are parted by a man thrusting through them)
 
(  they were snapped off and
driven headlong  and a second huge tripod appeared rushing )
 
( as it seemed 
headlong towards me  )
 
( And I was galloping hard to meet it  )
 
( At the sight of
the second monster my nerve went altogether  )
 
( Not stopping to look again  I
wrenched the horse s head hard round to the right and in another moment the
dog cart had heeled over upon the horse   )
 
(the shafts smashed noisily   )
 
(and I
was flung sideways and fell heavily into a shallow pool of water  )
 
(

I crawled out almost immediately  and crouched  )
 
(my feet still in the water 
under a clump of furze  )
 
(The horse lay motionless his neck was broken  poor
brute and by the lightning flashes I saw the black bulk of the overturned
dog cart and the silhouette of the wheel still spinning slowly  )
 
(In another
moment the colossal mechanism went striding by me  and passed uphill towards
Pyrford )
 
(

Seen nearer  the Thing was incredibly strange  )
 
(for it was no mere insensate
machine driving on its way  )
 
(Machine it was  with a ringing metallic pace 
and long  flexible  glittering tentacles )
 
(one of which gripped a young pine
tree)
 
( swinging and rattling about its strange body  )
 
(It picked its road as it
went striding along  )
 
(and the brazen hood that surmounted it moved to and fro)
 
(
with the inevitable suggestion of a head looking about )
 
( Behind the main body
was a huge mass of white metal like a gigantic fisherman s basket  )
 
(and puffs
of green smoke squirted out from the joints of the limbs as the monster
swept by me )
 
( And in an instant it was gone )
 
(

So much I saw then  )
 
(all vaguely for the flickering of the lightning  in
blinding highlights and dense black shadows )
 
(

As it passed it set up an exultant deafening howl that drowned the
thunder )
 
(and in another minute it was with its companion 
half a mile away  )
 
(stooping over something in the field  )
 
(I have no doubt this
Thing in the field was the third of the ten cylinders they had fired at us
from Mars )
 
(

For some minutes I lay there in the rain and darkness )
 
(watching  by the
intermittent light  these monstrous beings of metal moving about in the
distance over the hedge tops )
 
(A thin hail was now beginning  and as it came
and went their figures grew misty and then flashed into clearness again  )
 
(Now
and then came a gap in the lightning  and the night swallowed them up )
 
(

I was soaked with hail above and puddle water below )
 
( It was some time before
my blank astonishment would let me struggle up the bank to a drier position 
or think at all of my imminent peril )
 
(

Not far from me was a little one-roomed squatter s hut of wood  surrounded
by a patch of potato garden )
 
( I struggled to my feet at last  and  crouching
and making use of every chance of cover  )
 
(I made a run for this  I hammered
at the door )
 
( but I could not make the people hear if there were any people
inside and after a time )
 
(I desisted  and  availing myself of a ditch for
the greater part of the way  )
 
(succeeded in crawling  unobserved by these
monstrous machines  into the pine woods towards Maybury )
 
(

Under cover of this I pushed on  wet and shivering now )
 
( towards my own
house  I walked among the trees trying to find the footpath )
 
( It was very
dark indeed in the wood  for the lightning was now becoming infrequent  )
 
(and
the hail  which was pouring down in a torrent )
 
( fell in columns through the
gaps in the heavy foliage )
 
(

If I had fully realised the meaning of all the things I had seen)
 
( I should
have immediately worked my way round through Byfleet to Street Cobham  and
so gone back to rejoin my wife at Leatherhead  )
 
(But that night the
strangeness of things about me  and my physical wretchedness  prevented me 
for I was bruised  )
 
(weary  wet to the skin  deafened and blinded by the
storm )
 
(

I had a vague idea of going on to my own house  )
 
(and that was as much motive
as I had )
 
( I staggered through the trees  fell into a ditch and bruised my
knees against a plank )
 
( and finally splashed out into the lane that ran down
from the College Arms )
 
( I say splashed  for the storm water was sweeping the
sand down the hill in a muddy torrent  )
 
(There in the darkness a man blundered
into me and sent me reeling back )
 
(

He gave a cry of terror  sprang sideways  )
 
(and rushed on before I could
gather my wits sufficiently to speak to him  )
 
(So heavy was the stress of the
storm just at this place that I had the hardest task to win my way up the
hill)
 
(  I went close up to the fence on the left and worked my way along its
palings )
 
(

Near the top I stumbled upon something soft  )
 
(and  by a flash of lightning 
saw between my feet a heap of black broadcloth and a pair of boots )
 
( Before I
could distinguish clearly how the man lay  )
 
(the flicker of light had passed 
I stood over him waiting for the next flash  )
 
(When it came  I saw that he was
a sturdy man  cheaply but not shabbily dressed )
 
( his head was bent under his
body  and he lay crumpled up close to the fence)
 
(  as though he had been flung
violently against it )
 
(

Overcoming the repugnance natural to one who had never before touched a dead
body  )
 
(I stooped and turned him over to feel for his heart  )
 
(He was quite
dead  Apparently his neck had been broken )
 
( The lightning flashed for a third
time  and his face leaped upon me )
 
( I sprang to my feet  It was the landlord
of the Spotted Dog  whose conveyance I had taken )
 
(

I stepped over him gingerly and pushed on up the hill  )
 
(I made my way by the
police station and the College Arms towards my own house )
 
(Nothing was
burning on the hillside  though from the common there still came a red glare)
 
(and a rolling tumult of ruddy smoke beating up against the drenching hail 
So far as I could see by the flashes  the houses about me were mostly
uninjured )
 
( By the College Arms a dark heap lay in the road )
 
(

Down the road towards Maybury Bridge there were voices and the sound of
feet )
 
( but I had not the courage to shout or to go to them  )
 
(I let myself in
with my latchkey  closed )
 
( locked and bolted the door  staggered to the foot
of the staircase  and sat down  )
 
(My imagination was full of those striding
metallic monsters  and of the dead body smashed against the fence )
 
(

I crouched at the foot of the staircase with my back to the wall  shivering
violently )
 
(


I have already said that my storms of emotion have a trick of exhausting
themselves  )
 
(After a time I discovered that I was cold and wet )
 
( and with
little pools of water about me on the stair carpet  )
 
(I got up almost
mechanically  went into the dining room and drank some whiskey  )
 
(and then I
was moved to change my clothes )
 
(

After I had done that I went upstairs to my study  but why I did so I do not
know  )
 
(The window of my study looks over the trees and the railway towards
Horsell Common )
 
( In the hurry of our departure this window had been left
open )
 
( The passage was dark )
 
( and  by contrast with the picture the window
frame enclosed  the side of the room seemed impenetrably dark  )
 
(I stopped
short in the doorway )
 
(

The thunderstorm had passed  )
 
(The towers of the Oriental College and the pine
trees about it had gone )
 
( and very far away  lit by a vivid red glare  the
common about the sand pits was visible )
 
( Across the light huge black shapes 
grotesque and strange  moved busily to and fro )
 
(

It seemed indeed as if the whole country in that direction was on fire a
broad hillside set with minute tongues of flame )
 
( swaying and writhing with
the gusts of the dying storm )
 
( and throwing a red reflection upon the cloud
scud above )
 
( Every now and then a haze of smoke from some nearer
conflagration drove across the window and hid the Martian shapes )
 
( I could
not see what they were doing)
 
(  nor the clear form of them  nor recognise the
black objects they were busied upon  )
 
(Neither could I see the nearer fire 
though the reflections of it danced on the wall and ceiling of the study  )
 
(A
sharp  resinous tang of burning was in the air )
 
(

I closed the door noiselessly and crept towards the window)
 
(  As I did so  the
view opened out until  )
 
(on the one hand  it reached to the houses about
Woking station)
 
(  and on the other to the charred and blackened pine woods of
Byfleet  There was a light down below the hill )
 
( on the railway  near the
arch  and several of the houses along the Maybury road and the streets near
the station were glowing ruins )
 
( The light upon the railway puzzled me at
first  there were a black heap and a vivid glare )
 
( and to the right of that a
row of yellow oblongs  )
 
(Then I perceived this was a wrecked train  the fore
part smashed and on fire  the hinder carriages still upon the rails )
 
(

Between these three main centres of light the houses  the train  )
 
(and the
burning county towards Chobham stretched irregular patches of dark country 
broken here and there by intervals of dimly glowing and smoking ground  )
 
(It
was the strangest spectacle  that black expanse set with fire )
 
( It reminded
me  more than anything else  of the Potteries at night)
 
(  At first I could
distinguish no people at all  though I peered intently for them  )
 
(Later I saw
against the light of Woking station a number of black figures hurrying one
after the other across the line )
 
(

And this was the little world in which I had been living securely for years 
this fiery chaos )
 
( What had happened in the last seven hours I still did not
know  nor did I know )
 
( though I was beginning to guess  the relation between
these mechanical colossi and the sluggish lumps I had seen disgorged from
the cylinder )
 
( With a queer feeling of impersonal interest I turned my desk
chair to the window )
 
( sat down  and stared at the blackened country  )
 
(and
particularly at the three gigantic black things that were going to and fro
in the glare about the sand pits )
 
(

They seemed amazingly busy )
 
( I began to ask myself what they could be )
 
( Were
they intelligent mechanisms)
 
( Such a thing I felt was impossible  )
 
(Or did a
Martian sit within each  ruling )
 
( directing  using  much as a man s brain
sits and rules in his body)
 
( I began to compare the things to human machines 
to ask myself for the first time in my life )
 
(how an ironclad or a steam
engine would seem to an intelligent lower animal )
 
(

The storm had left the sky clear  and over the smoke of the burning land the
little fading pinpoint of Mars)
 
( was dropping into the west )
 
( when a soldier
came into my garden )
 
( I heard a slight scraping at the fence  )
 
(and rousing
myself from the lethargy that had fallen upon me )
 
( I looked down and saw him
dimly  clambering over the palings )
 
( At the sight of another human being my
torpor passed  )
 
(and I leaned out of the window eagerly )
 
(

 Hist   said I  in a whisper )
 
(

He stopped astride of the fence in doubt )
 
( Then he came over and across the
lawn to the corner of the house  )
 
(He bent down and stepped softly )
 
(

 Who s there)
 
(  he said  also whispering  standing under the window and
peering up )
 
(

 Where are you going  I asked )
 
(

 God knows  )
 
(

 Are you trying to hide)
 
(

 That s it  )
 
(

 Come into the house   I said )
 
(

I went down  unfastened the door  and let him in  )
 
(and locked the door again 
I could not see his face )
 
( He was hatless  and his coat was unbuttoned )
 
(

 My God   he said  as I drew him in )
 
(

 What has happened  I asked )
 
(

 What hasn t  In the obscurity I could see he made a gesture of despair 
 They wiped us out simply wiped us out  )
 
( he repeated again and again )
 
(

He followed me  almost mechanically  into the dining room )
 
(

 Take some whiskey   I said  pouring out a stiff dose )
 
(

He drank it  )
 
(Then abruptly he sat down before the table )
 
( put his head on his
arms  and began to sob and weep like a little boy  in a perfect passion of
emotion  while I  )
 
(with a curious forgetfulness of my own recent despair 
stood beside him  wondering )
 
(

It was a long time before he could steady his nerves to answer my questions )
 
(
and then he answered perplexingly and brokenly  )
 
(He was a driver in the
artillery  and had only come into action about seven  )
 
(At that time firing
was going on across the common  )
 
(and it was said the first party of Martians
were crawling slowly towards their second cylinder under cover of a metal
shield )
 
(

Later this shield staggered up on tripod legs and became the first of the
fighting-machines I had seen )
 
( The gun he drove had been unlimbered near
Horsell  in order to command the sand pits  )
 
(and its arrival it was that had
precipitated the action)
 
( As the limber gunners went to the rear  his horse
trod in a rabbit hole and came down )
 
( throwing him into a depression of the
ground )
 
( At the same moment the gun exploded behind him  the ammunition blew
up  there was fire all about him )
 
( and he found himself lying under a heap of
charred dead men and dead horses )
 
(

 I lay still   he said   scared out of my wits  )
 
(with the fore quarter of a
horse atop of me  )
 
(We d been wiped out  )
 
(And the smell good God  Like burnt
meat  I was hurt across the back by the fall of the horse )
 
( and there I had
to lie until I felt better )
 
( Just like parade it had been a minute
before then stumble  bang  swish  )
 
(

 Wiped out   he said )
 
(

He had hid under the dead horse for a long time  )
 
(peeping out furtively
across the common  )
 
(The Cardigan men had tried a rush  in skirmishing order 
at the pit  simply to be swept out of existence )
 
( Then the monster had risen
to its feet and had begun to walk leisurely to and fro across the common
among the few fugitives  )
 
(with its headlike hood turning about exactly like
the head of a cowled human being  )
 
(A kind of arm carried a complicated
metallic case  )
 
(about which green flashes scintillated  and out of the funnel
of this there smoked the Heat-Ray )
 
(

In a few minutes there was  so far as the soldier could see  )
 
(not a living
thing left upon the common  )
 
(and every bush and tree upon it that was not
already a blackened skeleton was burning  )
 
(The hussars had been on the road
beyond the curvature of the ground  and he saw nothing of them  )
 
(He heard the
Martians rattle for a time and then become still )
 
( The giant saved Woking
station and its cluster of houses until the last  )
 
(then in a moment the
Heat-Ray was brought to bear )
 
(and the town became a heap of fiery ruins 
Then the Thing shut off the Heat-Ray  )
 
(and turning its back upon the
artilleryman  )
 
(began to waddle away towards the smouldering pine woods that
sheltered the second cylinder )
 
( As it did so a second glittering Titan built
itself up out of the pit )
 
(

The second monster followed the first  )
 
(and at that the artilleryman began to
crawl very cautiously across the hot heather ash towards Horsell  )
 
(He managed
to get alive into the ditch by the side of the road )
 
( and so escaped to
Woking  )
 
(There his story became ejaculatory  )
 
(The place was impassable  It
seems there were a few people alive there )
 
( frantic for the most part and
many burned and scalded )
 
( He was turned aside by the fire  and hid among some
almost scorching heaps of broken wall as one of the Martian giants returned )
 
(
He saw this one pursue a man)
 
(  catch him up in one of its steely tentacles 
and knock his head against the trunk of a pine tree )
 
( At last  after
nightfall  the artilleryman made a rush for it and got over the railway
embankment )
 
(

Since then he had been skulking along towards Maybury  )
 
(in the hope of
getting out of danger Londonward  )
 
(People were hiding in trenches and
cellars  and many of the survivors had made off towards Woking village and
Send )
 
( He had been consumed with thirst until he found one of the water mains
near the railway arch smashed  )
 
(and the water bubbling out like a spring upon
the road )
 
(

That was the story I got from him  bit by bit  )
 
(He grew calmer telling me and
trying to make me see the things he had seen  )
 
(He had eaten no food since
midday  he told me early in his narrative  )
 
(and I found some mutton and bread
in the pantry and brought it into the room )
 
( We lit no lamp for fear of
attracting the Martians )
 
( and ever and again our hands would touch upon bread
or meat  )
 
(As he talked  things about us came darkly out of the darkness  )
 
(and
the trampled bushes and broken rose trees outside the window grew distinct )
 
(
It would seem that a number of men or animals had rushed across the lawn  )
 
(I
began to see his face  blackened and haggard  as no doubt mine was also )
 
(

When we had finished eating we went softly upstairs to my study  )
 
(and I
looked again out of the open window  )
 
(In one night the valley had become a
valley of ashes  )
 
(The fires had dwindled now )
 
(Where flames had been there
were now streamers of smoke )
 
( but the countless ruins of shattered and gutted
houses and blasted and blackened trees that the night had hidden stood out
now gaunt and terrible in the pitiless light of dawn  )
 
(Yet here and there
some object had had the luck to escape a white railway signal here )
 
( the end
of a greenhouse there  white and fresh amid the wreckage  )
 
(Never before in
the history of warfare had destruction been so indiscriminate and so
universal  )
 
(And shining with the growing light of the east )
 
( three of the
metallic giants stood about the pit  their cowls rotating as though they
were surveying the desolation they had made )
 
(



;;;;


It seemed to me that the pit had been enlarged  )
 
(and ever and again puffs of
vivid green vapour )
 
(streamed up and out of it towards the brightening
dawn streamed up)
 
(  whirled  broke  and vanished 

Beyond were the pillars of fire about Chobham )
 
( They became pillars of
bloodshot smoke at the first touch of day )
 
(
As the dawn grew brighter we withdrew from the window from which we had
watched the Martians  )
 
(and went very quietly downstairs )
 
(

The artilleryman agreed with me that the house was no place to stay in )
 
( He
proposed  he said  to make his way Londonward )
 
( and thence rejoin his
battery No  12  of the Horse Artillery )
 
( My plan was to return at once to
Leatherhead  and so greatly had the strength of the Martians impressed me)
 
(
that I had determined to take my wife to Newhaven  )
 
(and go with her out of
the country forthwith  )
 
(For I already perceived clearly that the country
about London must inevitably be the scene of a disastrous struggle before
such creatures as these could be destroyed )
 
(

Between us and Leatherhead  however  lay the third cylinder  )
 
(with its
guarding giants)
 
(  Had I been alone  )
 
(I think I should have taken my chance and
struck across country  But the artilleryman dissuaded me  )
 
( It s no kindness
to the right sort of wife   he said   to make her a widow   )
 
(and in the end I
agreed to go with him )
 
( under cover of the woods  )
 
(northward as far as Street
Cobham before I parted with him )
 
( Thence I would make a big detour by Epsom
to reach Leatherhead )
 
(

I should have started at once )
 
( but my companion had been in active service
and he knew better than that  )
 
(He made me ransack the house for a flask 
which he filled with whiskey  )
 
(and we lined every available pocket with
packets of biscuits and slices of meat  )
 
(Then we crept out of the house  and
ran as quickly as we could down the ill-made road by which I had come
overnight )
 
( The houses seemed deserted  )
 
(In the road lay a group of three
charred bodies close together  struck dead by the Heat-Ray )
 
( and here and
there were things that people had dropped a clock )
 
( a slipper  a silver
spoon  and the like poor valuables  )
 
(At the corner turning up towards the
post office a little cart  filled with boxes and furniture  )
 
(and horseless 
heeled over on a broken wheel  )
 
(A cash box had been hastily smashed open and
thrown under the debris )
 
(

Except the lodge at the Orphanage )
 
( which was still on fire  none of the
houses had suffered very greatly here  )
 
(The Heat-Ray had shaved the chimney
tops and passed  Yet  save ourselves )
 
( there did not seem to be a living soul
on Maybury Hill)
 
(  The majority of the inhabitants had escaped  )
 
(I suppose  by
way of the Old Woking road the road I had taken )
 
(when I drove to
Leatherhead or they had hidden )
 
(

We went down the lane  by the body of the man in black )
 
( sodden now from the
overnight hail  )
 
(and broke into the woods at the foot of the hill  )
 
(We pushed
through these towards the railway without meeting a soul  )
 
(The woods across
the line were but the scarred and blackened ruins of woods  )
 
(for the most
part the trees had fallen  but a certain proportion still stood  )
 
(dismal grey
stems  with dark brown foliage instead of green )
 
(

On our side the fire had done no more than scorch the nearer trees  )
 
(it had
failed to secure its footing )
 
( In one place the woodmen had been at work on
Saturday  trees  felled and freshly trimmed  )
 
(lay in a clearing  with heaps
of sawdust by the sawing-machine and its engine  )
 
(Hard by was a temporary
hut  deserted)
 
(  There was not a breath of wind this morning )
 
( and everything
was strangely still  Even the birds were hushed  )
 
(and as we hurried along I
and the artilleryman talked in whispers and looked now and again over our
shoulders )
 
( Once or twice we stopped to listen )
 
(

After a time we drew near the road  )
 
(and as we did so we heard the clatter of
hoofs and saw through the tree stems three cavalry soldiers riding slowly
towards Woking )
 
( We hailed them  and they halted while we hurried towards
them  It was a lieutenant and a couple of privates of the 8th Hussars  )
 
(with
a stand like a theodolite  which the artilleryman told me was a heliograph )
 
(

 You are the first men I ve seen coming this way this morning  )
 
( said the
lieutenant   What s brewing)
 
(

His voice and face were eager  )
 
(The men behind him stared curiously  The
artilleryman jumped down the bank into the road and saluted   )
 
(Gun destroyed
last night  sir  Have been hiding  )
 
(Trying to rejoin battery  sir  )
 
(You ll
come in sight of the Martians  )
 
(I expect  about half a mile along this road  )
 
(

 What the dickens are they like  asked the lieutenant )
 
(

 Giants in armour  sir  Hundred feet high  )
 
(Three legs and a body like
 luminium  with a mighty great head in a hood  sir  )
 
(

 Get out   said the lieutenant  )
 
( What confounded nonsense  )
 
(

They carry a kind of box )
 
(  that shoots fire and
strikes you dead  )
 
(

 What d ye mean a gun)
 
(

 No  sir   and the artilleryman began a vivid account of the Heat-Ray )
 
(
Halfway through  the lieutenant interrupted him )
 
(and looked up at me  I was
still standing on the bank by the side of the road )
 
(

 It s perfectly true   I said )
 
(

 Well   said the lieutenant   )
 
(I suppose it s my business to see it too  Look
here  to the artilleryman )
 
( we re detailed here clearing people out of
their houses  )
 
(You d better go along and report yourself to Brigadier-General
Marvin  and tell him all you know  )
 
(He s at Weybridge  Know the way)
 
(

 I do   I said  and he turned his horse southward again )
 
(

 Half a mile  you say said he )
 
(

 At most   I answered  and pointed over the treetops southward  )
 
(He thanked
me and rode on  and we saw them no more )
 
(

Farther along we came upon a group of three women and two children in the
road  )
 
(busy clearing out a labourer s cottage  )
 
(They had got hold of a little
hand truck )
 
( and were piling it up with unclean-looking bundles and shabby
furniture  )
 
(They were all too assiduously engaged to talk to us as we passed )
 
(

By Byfleet station we emerged from the pine trees  )
 
(and found the country
calm and peaceful under the morning sunlight )
 
(We were far beyond the range
of the Heat-Ray there  and had it not been for the silent desertion of some
of the houses  )
 
(the stirring movement of packing in others  )
 
(and the knot of
soldiers standing on the bridge over the railway and staring down the line
towards )
 
(Woking  the day would have seemed very like any other Sunday )
 
(

Several farm waggons and carts were moving creakily along the road to
Addlestone )
 
( and suddenly through the gate of a field we saw )
 
( across a
stretch of flat meadow )
 
( six twelve-pounders standing neatly at equal
distances pointing towards Woking  )
 
(The gunners stood by the guns waiting 
and the ammunition waggons were at a business-like distance  )
 
(The men stood
almost as if under inspection )
 
(

 That s good   said I   They will get one fair shot  at any rate  )
 
(

The artilleryman hesitated at the gate )
 
(

 I shall go on   he said )
 
(

Farther on towards Weybridge  )
 
(just over the bridge  there were a number of
men in white fatigue jackets throwing up a long rampart)
 
(  and more guns
behind )
 
(

 It s bows and arrows against the lightning  anyhow   )
 
(said the artilleryman 
 They  aven t seen that fire-beam yet  )
 
(

The officers who were not actively engaged stood and stared over the
treetops southwestward  )
 
(and the men digging would stop every now and again
to stare in the same direction )
 
(

Byfleet was in a tumult  people packing )
 
(and a score of hussars  some of
them dismounted  some on horseback  )
 
(were hunting them about  Three or four
black government waggons  )
 
(with crosses in white circles  and an old omnibus 
among other vehicles )
 
( were being loaded in the village street  There were
scores of people )
 
( most of them sufficiently sabbatical to have assumed their
best clothes  )
 
(The soldiers were having the greatest difficulty in making
them realise the gravity of their position  )
 
(We saw one shrivelled old fellow
with a huge box and a score or more of flower pots containing orchids 
angrily expostulating )
 
(with the corporal who would leave them behind  I
stopped and gripped his arm )
 
(

 Do you know what s over there  I said  )
 
(pointing at the pine tops that hid
the Martians )
 
(

I was explainin  these is vallyble  )
 
(

 Death   I shouted   Death is coming  Death   )
 
(and leaving him to digest that
if he could  I hurried on after the artilleryman  )
 
(At the corner I looked
back  )
 
(The soldier had left him )
 
( and he was still standing by his box  with
the pots of orchids on the lid of it )
 
( and staring vaguely over the trees )
 
(

No one in Weybridge could tell us where the headquarters were established 
the whole place was in such confusion )
 
(as I had never seen in any town
before )
 
( Carts  carriages everywhere )
 
( the most astonishing miscellany of
conveyances and horseflesh  )
 
(The respectable inhabitants of the place  men in
golf and boating costumes  wives prettily dressed  )
 
(were packing  river-side
loafers energetically helping  children excited )
 
( and  for the most part 
highly delighted at this astonishing variation of their Sunday experiences 
In the midst of it all the worthy vicar was very pluckily holding an early
celebration )
 
( and his bell was jangling out above the excitement )
 
(

I and the artilleryman  seated on the step of the drinking fountain )
 
( made a
very passable meal upon what we had brought with us )
 
( Patrols of
soldiers here no longer hussars )
 
( but grenadiers in white were warning
people to move now or to take refuge in their cellars as soon as the firing
began )
 
( We saw as we crossed the railway bridge that a growing crowd of
people had assembled in and about the railway station )
 
( and the swarming
platform was piled with boxes and packages  )
 
(The ordinary traffic had been
stopped  I believe  )
 
(in order to allow of the passage of troops and guns to
Chertsey  )
 
(and I have heard since that a savage struggle occurred for places
in the special trains that were put on at a later hour )
 
(

We remained at Weybridge until midday  )
 
(and at that hour we found ourselves
at the place near Shepperton Lock )
 
(where the Wey and Thames join  Part of the
time we spent helping two old women to pack a little cart  )
 
(The Wey has a
treble mouth )
 
( and at this point boats are to be hired  )
 
(and there was a ferry
across the river  )
 
(On the Shepperton side was an inn with a lawn  )
 
(and beyond
that the tower of Shepperton Church it has been replaced by a spire rose
above the trees )
 
(

Here we found an excited and noisy crowd of fugitives  )
 
(As yet the flight had
not grown to a panic  but there were already far more people than all the
boats going to and fro could enable to cross  )
 
(People came panting along
under heavy burdens)
 
(  one husband and wife were even carrying a small
outhouse door between them  )
 
(with some of their household goods piled
thereon )
 
( One man told us he meant to try to get away from Shepperton
station )
 
(

There was a lot of shouting  )
 
(and one man was even jesting  )
 
(The idea people
seemed to have here was that the Martians were simply formidable human
beings  who might attack and sack the town )
 
( to be certainly destroyed in the
end )
 
( Every now and then people would glance nervously across the Wey  at the
meadows towards Chertsey  )
 
(but everything over there was still )
 
(

Across the Thames  except just where the boats landed  everything was quiet 
in vivid contrast with the Surrey side  )
 
(The people who landed there from the
boats went tramping off down the lane )
 
( The big ferryboat had just made a
journey )
 
( Three or four soldiers stood on the lawn of the inn  staring and
jesting at the fugitives  )
 
(without offering to help  )
 
(The inn was closed  as
it was now within prohibited hours )
 
(

 What s that  cried a boatman  and  Shut up  you fool  )
 
( said a man near me
to a yelping dog )
 
( Then the sound came again  this time from the direction of
Chertsey  a muffled thud the sound of a gun )
 
(

The fighting was beginning )
 
( Almost immediately unseen batteries across the
river to our right )
 
( unseen because of the trees  took up the chorus )
 
( firing
heavily one after the other )
 
( A woman screamed  )
 
(Everyone stood arrested by
the sudden stir of battle )
 
( near us and yet invisible to us )
 
( Nothing was to
be seen save flat meadows  cows feeding unconcernedly for the most part )
 
( and
silvery pollard willows motionless in the warm sunlight )
 
(

 The sojers ll stop  em   said a woman beside me  doubtfully )
 
( A haziness
rose over the treetops )
 
(

Then suddenly we saw a rush of smoke far away up the river )
 
( a puff of smoke
that jerked up into the air and hung )
 
( and forthwith the ground heaved under
foot and a heavy explosion shook the air  )
 
(smashing two or three windows in
the houses near  and leaving us astonished )
 
(

 Here they are   shouted a man in a blue jersey   Yonder  D yer see them
Yonder  

Quickly  one after the other  one  two  three )
 
( four of the armoured Martians
appeared  far away over the little trees  )
 
(across the flat meadows that
stretched towards Chertsey )
 
( and striding hurriedly towards the river  Little
cowled figures they seemed at first  )
 
(going with a rolling motion and as fast
as flying birds )
 
(

Then  advancing obliquely towards us  came a fifth  )
 
(Their armoured bodies
glittered in the sun as they swept swiftly forward upon the guns )
 
( growing
rapidly larger as they drew nearer )
 
( One on the extreme left  the remotest
that is  flourished a huge case high in the air )
 
( and the ghostly  terrible
Heat-Ray I had already seen on Friday night smote towards Chertsey  and
struck the town )
 
(

At sight of these strange )
 
( swift  and terrible creatures the crowd near the
water s edge seemed to me to be for a moment horror-struck  )
 
(There was no
screaming or shouting  but a silence  )
 
(Then a hoarse murmur and a movement of
feet a splashing from the water  )
 
(A man  too frightened to drop the
portmanteau he carried on his shoulder )
 
( swung round and sent me staggering
with a blow from the corner of his burden  )
 
(A woman thrust at me with her
hand and rushed past me  )
 
(I turned with the rush of the people  but I was not
too terrified for thought  )
 
(The terrible Heat-Ray was in my mind  To get
under water  That was it )
 
(

 Get under water   I shouted  unheeded )
 
(

I faced about again  and rushed towards the approaching Martian  )
 
(rushed
right down the gravelly beach and headlong into the water  )
 
(Others did the
same )
 
( A boatload of people putting back came leaping out as I rushed past 
The stones under my feet were muddy and slippery )
 
( and the river was so low
that I ran perhaps twenty feet scarcely waist-deep )
 
( Then  as the Martian
towered overhead scarcely a couple of hundred yards away  I flung myself
forward under the surface )
 
( The splashes of the people in the boats leaping
into the river sounded like thunderclaps in my ears )
 
( People were landing
hastily on both sides of the river )
 
(

But the Martian machine took no more notice for the moment of the people
running this way and that )
 
(than a man would of the confusion of ants in a
nest against which his foot has kicked  )
 
(When  half suffocated  I raised my
head above water  )
 
(the Martian s hood pointed at the batteries that were
still firing across the river  )
 
(and as it advanced it swung loose what must
have been the generator of the Heat-Ray )
 
(

In another moment it was on the bank  and in a stride wading halfway across )
 
(
The knees of its foremost legs bent at the farther bank )
 
( and in another
moment it had raised itself to its full height again  )
 
(close to the village
of Shepperton )
 
( Forthwith the six guns which  unknown to anyone on the right
bank  had been)
 
( hidden behind the outskirts of that village  fired
simultaneously  )
 
(The sudden near concussion  the last close upon the first 
made my heart jump )
 
( The monster was already raising the case generating the
Heat-Ray as the first shell burst six yards above the hood )
 
(

I gave a cry of astonishment  )
 
(I saw and thought nothing of the other four
Martian monsters  my attention was riveted upon the nearer incident )
 
(
Simultaneously two other shells burst in the air near the body as the hood
twisted round in time to receive)
 
( but not in time to dodge  the fourth
shell )
 
(

The shell burst clean in the face of the Thing  )
 
(The hood bulged  flashed 
was whirled off in a dozen )
 
(tattered fragments of red flesh and glittering
metal )
 
(

 Hit   shouted I  with something between a scream and a cheer )
 
(

I heard answering shouts from the people in the water about me  )
 
(I could have
leaped out of the water with that momentary exultation )
 
(

The decapitated colossus reeled like a drunken giant )
 
( but it did not fall
over  It recovered its balance by a miracle )
 
( and  no longer heeding its
steps and with the camera that fired the Heat-Ray now rigidly upheld )
 
( it
reeled swiftly upon Shepperton  )
 
(The living intelligence  the Martian within
the hood  was slain and splashed to the four winds of heaven  )
 
(and the Thing
was now but a mere intricate device of metal whirling to destruction  )
 
(It
drove along in a straight line  incapable of guidance )
 
( It struck the tower
of Shepperton Church  smashing it down as the impact of a battering ram
might have done  )
 
(swerved aside  blundered on and collapsed with tremendous
force into the river out of my sight )
 
(

A violent explosion shook the air  )
 
(and a spout of water  steam  mud  and
shattered metal shot far up into the sky  )
 
(As the camera of the Heat-Ray hit
the water  the latter had immediately flashed into steam  )
 
(In another moment
a huge wave )
 
( like a muddy tidal bore but almost scaldingly hot  came
sweeping round the bend upstream )
 
( I saw people struggling shorewards  and
heard their screaming and shouting faintly above the seething and roar of
the Martian s collapse )
 
(

For a moment I heeded nothing of the heat  )
 
(forgot the patent need of
self-preservation )
 
( I splashed through the tumultuous water  pushing aside a
man in black to do so )
 
( until I could see round the bend  )
 
(Half a dozen
deserted boats pitched aimlessly upon the confusion of the waves  )
 
(The fallen
Martian came into sight downstream  )
 
(lying across the river  and for the most
part submerged )
 
(

Thick clouds of steam were pouring off the wreckage  )
 
(and through the
tumultuously whirling wisps I could see  intermittently and vaguely  )
 
(the
gigantic limbs churning the water and flinging a splash and spray of mud and
froth into the air )
 
( The tentacles swayed and struck like living arms  and 
save for the helpless purposelessness of these movements  )
 
(it was as if some
wounded thing were struggling for its life amid the waves )
 
( Enormous
quantities of a ruddy-brown fluid were spurting up in noisy jets out of the
machine )
 
(

My attention was diverted from this death flurry by a furious yelling  )
 
(like
that of the thing called a siren in our manufacturing towns)
 
(  A man 
knee-deep near the towing path )
 
( shouted inaudibly to me and pointed  Looking
back  I saw the other Martians advancing with gigantic strides down the
riverbank from the direction of Chertsey  )
 
(The Shepperton guns spoke this
time unavailingly )
 
(

At that I ducked at once under water  )
 
(and  holding my breath until movement
was an agony  blundered painfully ahead under the surface as long as I
could  )
 
(The water was in a tumult about me  and rapidly growing hotter )
 
(

When for a moment I raised my head to take breath and throw the hair and
water from my eyes  )
 
(the steam was rising in a whirling white fog that at
first hid the Martians altogether )
 
( The noise was deafening  Then I saw them
dimly  colossal figures of grey )
 
( magnified by the mist  They had passed by
me  and two were stooping over the frothing )
 
(tumultuous ruins of their
comrade )
 
(

The third and fourth stood beside him in the water )
 
( one perhaps two hundred
yards from me  )
 
(the other towards Lale- ham )
 
(The generators of the Heat-Rays
waved high )
 
(and the hissing beams smote down this way and that )
 
(

The air was full of sound )
 
( a deafening and confusing conflict of noises the
clangorous din of the Martians  the crash of falling houses )
 
( the thud of
trees )
 
( fences  sheds flashing into flame  and the crackling and roaring of
fire)
 
(  Dense black smoke was leaping up to mingle with the steam from the
river  )
 
(and as the Heat-Ray went to and fro over Weybridge its impact was
marked by flashes of incandescent white  )
 
(that gave place at once to a smoky
dance of lurid flames )
 
( The nearer houses still stood intact  )
 
(awaiting their
fate  shadowy  faint and pallid in the steam )
 
( with the fire behind them
going to and fro )
 
(

For a moment perhaps I stood there  breast-high in the almost boiling water 
dumbfounded at my position  hopeless of escape  )
 
(Through the reek I could see
the people who had been with me in the river scrambling out of the water
through the reeds )
 
( like little frogs hurrying through grass from the advance
of a man)
 
(  or running to and fro in utter dismay on the towing path )
 
(

Then suddenly the white flashes of the Heat-Ray came leaping towards me  )
 
(The
houses caved in as they dissolved at its touch )
 
( and darted out flames  the
trees changed to fire with a roar  )
 
(The Ray flickered up and down the towing
path  licking off the people who ran this way and that )
 
( and came down to the
water s edge not fifty yards from where I stood )
 
( It swept across the river
to Shepperton  )
 
(and the water in its track rose in a boiling weal crested
with steam  I turned shoreward )
 
(

In another moment the huge wave )
 
( well-nigh at the boiling-point had rushed
upon me  I screamed aloud  and scalded  half blinded  )
 
(agonised  I staggered
through the leaping  hissing water towards the shore )
 
( Had my foot stumbled 
it would have been the end )
 
( I fell helplessly  in full sight of the
Martians )
 
( upon the broad  bare gravelly spit that runs down to mark the
angle of the Wey and Thames )
 
( I expected nothing but death )
 
(

I have a dim memory of the foot of a Martian coming down within a score of
yards of my head )
 
( driving straight into the loose gravel )
 
( whirling it this
way and that and lifting again  of a long suspense  )
 
(and then of the four
carrying the debris of their comrade between them  )
 
(now clear and then
presently faint through a veil of smoke  )
 
(receding interminably  )
 
(as it seemed
to me  across a vast space of river and meadow  )
 
(And then  very slowly  I
realised that by a miracle I had escaped )
 
(

After getting this sudden lesson in the power of terrestrial weapons  )
 
(the
Martians retreated to their original position upon Horsell Common  )
 
(and in
their haste  and encumbered with the débris of their smashed companion  they
no doubt overlooked many such a stray and negligible victim as myself  )
 
(Had
they left their comrade and pushed on forthwith  there was nothing at that
time between them and London but batteries of twelve-pounder guns  and they
would certainly have reached the capital in advance of the tidings of their
approach  as sudden  )
 
(dreadful  and destructive their advent would have been
as the earthquake that destroyed Lisbon a century ago )
 
(

But they were in no hurry  Cylinder followed cylinder )
 
(on its interplanetary
flight  every twenty-four hours brought them reinforcement  )
 
(And meanwhile
the military and naval authorities  )
 
(now fully alive to the tremendous power
of their antagonists  worked with furious energy )
 
( Every minute a fresh gun
came into position until  before twilight )
 
( every copse  every row of
suburban villas on the hilly slopes about Kingston and Richmond  )
 
(masked an
expectant black muzzle )
 
( And through the charred and desolated area perhaps
twenty square miles altogether that encircled the Martian encampment on
Horsell Common )
 
( through charred and ruined villages among the green trees 
through the blackened and smoking arcades that had been but a day ago pine
spinneys  )
 
(crawled the devoted scouts with the heliographs that were
presently to warn the gunners of the Martian approach  )
 
(But the Martians now
understood our command of artillery )
 
(and the danger of human proximity  and
not a man ventured )
 
(within a mile of either cylinder  save at the price of
his life )
 
(

It would seem that these giants spent the earlier part of the afternoon in
going to and fro  )
 
(transferring everything from the second and third
cylinders the second in Addlestone Golf Links and the third at Pyrford to
their original pit on Horsell Common )
 
( Over that  above the blackened heather
and ruined buildings that stretched far and wide )
 
( stood one as sentinel 
while the rest abandoned their vast fighting-machines and descended into the
pit )
 
( They were hard at work there far into the night  )
 
(and the towering
pillar of dense green smoke that rose therefrom could be seen from the hills
about Merrow )
 
( and even  it is said  from Banstead and Epsom Downs )
 
(

And while the Martians behind me were thus preparing for their next sally 
and in front of me Humanity gathered for the battle )
 
( I made my way with
infinite pains and labour from the fire and smoke of burning Weybridge
towards London )
 
(

I saw an abandoned boat  very small and remote  )
 
(drifting down-stream  and
throwing off the most of my sodden clothes )
 
( I went after it  gained it  and
so escaped out of that destruction )
 
( There were no oars in the boat  but I
contrived to paddle )
 
( as well as my parboiled hands would allow  down the
river towards Halliford and Walton  going very tediously and continually
looking behind me  as you may well understand  )
 
(I followed the river  because
I considered that the water gave me my best chance of escape should these
giants return )
 
(

The hot water from the Martian s overthrow drifted downstream with me  so
that for the best part of a mile I could see little of either bank  )
 
(Once 
however  I made out a string of black figures hurrying across the meadows
from the direction of Weybridge  Halliford )
 
( it seemed  was deserted  and
several of the houses facing the river were on fire )
 
( It was strange to see
the place quite tranquil  quite desolate under the hot blue sky  )
 
(with the
smoke and little threads of flame going straight up into the heat of the
afternoon )
 
( Never before had I seen houses burning without the accompaniment
of an obstructive crowd  )
 
(A little farther on the dry reeds up the bank were
smoking and glowing )
 
( and a line of fire inland was marching steadily across
a late field of hay )
 
(

For a long time I drifted  so painful and weary was I after the violence I
had been through )
 
( and so intense the heat upon the water  Then my fears got
the better of me again  and I resumed my paddling  )
 
(The sun scorched my bare
back  At last  as the bridge at Walton was coming into sight round the bend 
my fever and faintness overcame my fears  )
 
(and I landed on the Middlesex bank
and lay down  deadly sick  amid the long grass )
 
( I suppose the time was then
about four or five o clock  )
 
(I got up presently  walked perhaps half a mile
without meeting a soul )
 
( and then lay down again in the shadow of a hedge  I
seem to remember talking  )
 
(wanderingly  to myself during that last spurt  I
was also very thirsty )
 
( and bitterly regretful I had drunk no more water  It
is a curious thing that I felt angry with my wife  )
 
(I cannot account for it 
but my impotent desire to reach Leatherhead worried me excessively )
 
(

I do not clearly remember the arrival of the curate  )
 
(so that probably I
dozed  I became aware of him as a seated figure in soot-smudged shirt
sleeves  and with his upturned  )
 
(clean-shaven face staring at a faint
flickering that danced over the sky  )
 
(The sky was what is called a mackerel
sky rows and rows of faint down-plumes of cloud  )
 
(just tinted with the
midsummer sunset )
 
(

I sat up  and at the rustle of my motion he looked at me quickly )
 
(

 Have you any water  I asked abruptly )
 
(

He shook his head )
 
(

 You have been asking for water for the last hour   he said )
 
(

For a moment we were silent  taking stock of each other  )
 
(I dare say he found
me a strange enough figure )
 
( naked  save for my water-soaked trousers and
socks  scalded )
 
( and my face and shoulders blackened by the smoke  His face
was a fair weakness )
 
( his chin retreated  and his hair lay in crisp  almost
flaxen curls on his low forehead )
 
( his eyes were rather large  )
 
(pale blue  and
blankly staring  He spoke abruptly )
 
( looking vacantly away from me )
 
(

 What does it mean  he said   What do these things mean)
 
( 

I stared at him and made no answer )
 
(

;;;;;;;

He extended a thin white hand and spoke in almost a complaining tone )
 
(

 Why are these things permitted)
 
( What sins have we done )
 
(The morning service
was over  I was walking through the roads to clear my brain for the
afternoon)
 
( and then fire  earthquake  death )
 
( As if it were Sodom and
Gomorrah )
 
( All our work undone  all the work  )
 
( What are these Martians)
 
(

 What are we  I answered  clearing my throat )
 
(

He gripped his knees and turned to look at me again )
 
( For half a minute 
perhaps  he stared silently )
 
(

 I was walking through the roads to clear my brain   he said   )
 
(And
suddenly fire  earthquake  death  )
 
(

He relapsed into silence  with his chin now sunken almost to his knees )
 
(

Presently he began waving his hand )
 
(

 All the work all the Sunday schools   )
 
(What have we done what has
Weybridge done)
 
( Everything gone everything destroyed  )
 
(The church  We
rebuilt it only three years ago )
 
( Gone  Swept out of existence  Why)
 
(

Another pause  and he broke out again like one demented )
 
(

 The smoke of her burning goeth up for ever and ever   he shouted )
 
(

His eyes flamed  and he pointed a lean finger in the direction of Weybridge )
 
(

By this time I was beginning to take his measure )
 
( The tremendous tragedy in
which he had been involved it was evident he was a fugitive from
Weybridge had driven him to the very verge of his reason )
 
(

 Are we far from Sunbury  I said  in a matter-of-fact tone )
 
(

 What are we to do  he asked )
 
(  Are these creatures everywhere)
 
( Has the
earth been given over to them)
 
(

 Are we far from Sunbury)
 
(

 Only this morning I officiated at early celebration   )
 
(

 Things have changed   I said  quietly   You must keep your head  There is
still hope  )
 
(

 Yes  Plentiful hope for all this destruction  )
 
(

I began to explain my view of our position  )
 
(He listened at first  but as I
went on the interest dawning in his eyes gave place to their former stare 
and his regard wandered from me )
 
(

 This must be the beginning of the end   )
 
(he said  interrupting me   )
 
(The end 
The great and terrible day of the Lord  )
 
(When men shall call upon the
mountains and the rocks to fall upon them and hide them hide them from the
face of Him that sitteth upon the throne  )
 
(

I began to understand the position  )
 
(I ceased my laboured reasoning 
struggled to my feet  and  standing over him  )
 
(laid my hand on his shoulder )
 
(

 Be a man   said I   You are scared out of your wits  )
 
(What good is religion
if it collapses under calamity)
 
( Think of what earthquakes and floods  wars
and volcanoes  have done before to men  )
 
(Did you think God had exempted
Weybridge)
 
( He is not an insurance agent  )
 
(

For a time he sat in blank silence )
 
(

 But how can we escape  he asked  suddenly )
 
(  They are invulnerable  they
are pitiless  )
 
(

 Neither the one nor  perhaps  the other   I answered   )
 
(And the mightier
they are the more sane and wary should we be  )
 
(One of them was killed yonder
not three hours ago  )
 
(

 Killed   he said  staring about him )
 
(  How can God s ministers be killed)
 
(

 I saw it happen   I proceeded to tell him )
 
(  We have chanced to come in for
the thick of it   said I   and that is all  )
 
(

 What is that flicker in the sky he asked abruptly )
 
(

I told him it was the heliograph signalling that it was the sign of human
help and effort in the sky )
 
(

 We are in the midst of it   I said  )
 
( quiet as it is  That flicker in the
sky tells of the gathering storm )
 
( Yonder  I take it are the Martians  and
Londonward  where those hills rise about Richmond and Kingston and the trees
give cover )
 
( earthworks are being thrown up and guns are being placed 
Presently the Martians will be coming this way again  )
 
(

And even as I spoke he sprang to his feet and stopped me by a gesture )
 
(

 Listen   he said )
 
(

From beyond the low hills across the water came the dull resonance of
distant guns and a remote weird crying  )
 
(Then everything was still  )
 
(A
cockchafer came droning over the hedge and past us)
 
(  High in the west the
crescent moon hung faint and pale above the smoke of Weybridge and
Shepperton and the hot  still splendour of the sunset )
 
(

 We had better follow this path   I said   northward  )
 
(


My younger brother was in London )
 
(when the Martians fell at Woking  )
 
(He was a
medical student working for an imminent examination )
 
( and he heard nothing of
the arrival until Saturday morning )
 
( The morning papers on Saturday
contained  in addition to lengthy special articles on the planet Mars  )
 
(on
life in the planets  and so forth )
 
( a brief and vaguely worded telegram  all
the more striking for its brevity )
 
(

The Martians  alarmed by the approach of a crowd  )
 
(had killed a number of
people with a quick-firing gun  so the story ran )
 
( The telegram concluded
with the words   )
 
(Formidable as they seem to be  the Martians have not moved
from the pit into which they have fallen )
 
( and  indeed  seem incapable of
doing so  )
 
(Probably this is due to the relative strength of the earth s
gravitational energy   )
 
(On that last text their leader-writer expanded very
comfortingly )
 
(

Of course all the students in the crammer s biology class )
 
( to which my
brother went that day  were intensely interested )
 
( but there were no signs of
any unusual excitement in the streets )
 
( The afternoon papers puffed scraps of
news under big headlines )
 
( They had nothing to tell beyond the movements of
troops about the common  )
 
(and the burning of the pine woods between Woking
and Weybridge  until eight  )
 
(Then the St  James s Gazette  in an
extra-special edition )
 
( announced the bare fact of the interruption of
telegraphic communication )
 
( This was thought to be due to the falling of
burning pine trees across the line  )
 
(Nothing more of the fighting was known
that night  the night of my drive to Leatherhead and back )
 
(

My brother felt no anxiety about us  as he knew from the description in the
papers that the cylinder was a good two miles from my house  )
 
(He made up his
mind to run down that night to me )
 
( in order  as he says  to see the Things
before they were killed  )
 
(He despatched a telegram  which never reached me 
about four o clock  and spent the evening at a music hall )
 
(

In London  also  on Saturday night there was a thunderstorm )
 
( and my brother
reached Waterloo in a cab )
 
( On the platform from which the midnight train
usually starts he learned )
 
( after some waiting  that an accident prevented
trains from reaching Woking that night  )
 
(The nature of the accident he could
not ascertain  )
 
(indeed  the railway authorities did not clearly know at that
time )
 
( There was very little excitement in the station  as the officials 
failing to realise that anything further than a breakdown between )
 
(Byfleet
and Woking junction had occurred  )
 
(were running the theatre trains w)
 
(hich
usually passed through Woking round by Virginia Water or Guildford  They
were busy making the necessary arrangements to alter the route of the
Southampton and Portsmouth Sunday League excursions )
 
( A nocturnal newspaper
reporter  mistaking my brother for the traffic manager )
 
( to whom he bears a
slight resemblance  waylaid and tried to interview him  )
 
(Few people 
excepting the railway officials  connected the breakdown with the Martians )
 
(

I have read  in another account of these events  that on Sunday morning  all
London was electrified by the news from Woking   )
 
(As a matter of fact  there
was nothing to justify that very extravagant phrase  )
 
(Plenty of Londoners did
not hear of the Martians until the panic of Monday morning  )
 
(Those who did
took some time to realise all that the hastily worded telegrams in the
Sunday papers conveyed  )
 
(The majority of people in London do not read Sunday
papers 

The habit of personal security  moreover)
 
( is so deeply fixed in the
Londoner s mind  and startling intelligence so much a matter of course in
the papers )
 
( that they could read without any personal tremors   About seven
o clock last night the Martians came out of the cylinder  )
 
(and  moving about
under an armour of metallic shields  have completely wrecked Woking station
with the adjacent houses  )
 
(and massacred an entire battalion of the Cardigan
Regiment )
 
( No details are known  Maxims have been absolutely useless against
their armour  )
 
(the field guns have been disabled by them  Flying hussars have
been galloping into Chertsey  )
 
(The Martians appear to be moving slowly
towards Chertsey or Windsor  )
 
(Great anxiety prevails in West Surrey  and
earthworks are being thrown up to check the advance Londonward )
 
(  That was
how the Sunday Sun put it  )
 
(and a clever and remarkably prompt  handbook 
article in the Referee compared the affair to a menagerie suddenly let loose
in a village )
 
(

No one in London knew positively of the nature of the armoured Martians  and
there was still a fixed idea that these monsters must be sluggish 
 crawling   )
 
( creeping painfully  such expressions occurred in almost all
the earlier reports  )
 
(None of the telegrams could have been written by an
eyewitness of their advance  )
 
(The Sunday papers printed separate editions as
further news came to hand )
 
( some even in default of it  )
 
(But there was
practically nothing more to tell people until late in the afternoon )
 
(when
the authorities gave the press agencies the news in their possession )
 
( It was
stated that the people of Walton and Weybridge )
 
( and all the district were
pouring along the roads Londonward  and that was all )
 
(

My brother went to church at the Foundling Hospital in the morning  )
 
(still in
ignorance of what had happened on the previous night  )
 
(There he heard
allusions made to the invasion  and a special prayer for peace )
 
( Coming out 
he bought a Referee  He became alarmed at the news in this  )
 
(and went again
to Waterloo station to find out if communication were restored )
 
( The
omnibuses  carriages  cyclists  )
 
(and innumerable people walking in their best
clothes seemed scarcely affected by the strange intelligence that the news
venders were disseminating  )
 
(People were interested  or  if alarmed  alarmed
only on account of the local residents  )
 
(At the station he heard for the
first time that the Windsor and Chertsey lines were now interrupted  )
 
(The
porters told him that several remarkable telegrams had been received in the
morning from Byfleet and Chertsey stations )
 
(but that these had abruptly
ceased )
 
( My brother could get very little precise detail out of them )
 
(

 There s fighting going on about Weybridge  was the extent of their
information )
 
(

The train service was now very much disorganised  )
 
(Quite a number of people
who had been expecting friends from places on the South-Western network were
standing about the station )
 
( One grey-headed old gentleman came and abused
the South-Western Company bitterly to my brother   )
 
(It wants showing up   he
said )
 
(

One or two trains came in from Richmond  Putney  and Kingston  )
 
(containing
people who had gone out for a day s boating and found the locks closed and a
feeling of panic in the air )
 
( A man in a blue and white blazer addressed my
brother  full of strange tidings )
 
(

 There s hosts of people driving into Kingston in traps and carts and
things  with boxes of valuables and all that   he said   )
 
(They come from
Molesey and Weybridge and Walton  )
 
(and they say there s been guns heard at
Chertsey  heavy firing  and that mounted soldiers have told them to get off
at once because the Martians are coming  )
 
(We heard guns firing at Hampton
Court station  but we thought it was thunder  )
 
(What the dickens does it all
mean)
 
( The Martians can t get out of their pit  can they)
 
(

My brother could not tell him )
 
(

Afterwards he found that the vague feeling of alarm had spread to the
clients of the underground railway  )
 
(and that the Sunday excursionists began
to return from all over the South-Western  lung  Barnes  )
 
(Wimbledon 
Richmond Park  Kew  )
 
(and so forth at unnaturally early hours  but not a soul
had anything more than vague hearsay to tell of  )
 
(Everyone connected with the
terminus seemed ill-tempered )
 
(

About five o clock the gathering crowd in the station was immensely excited
by the opening of the line of communication  )
 
(which is almost invariably
closed  between the South-Eastern and the South-Western stations  )
 
(and the
passage of carriage trucks bearing huge guns and carriages crammed with
soldiers  )
 
(These were the guns that were brought up from Woolwich and Chatham
to cover Kingston  )
 
(There was an exchange of pleasantries  )
 
( You ll get
eaten    )
 
(We re the beast-tamers   and so forth  )
 
(A little while after that a
squad of police came into the station and began to clear the public off the
platforms )
 
( and my brother went out into the street again )
 
(

The church bells were ringing for evensong  and a squad of Salvation Army
lassies came singing down Waterloo Road )
 
( On the bridge a number of loafers
were watching a curious brown scum that came drifting down the stream in
patches  )
 
(The sun was just setting  and the Clock Tower and the Houses of
Parliament rose against one of the most peaceful skies it is possible to
imagine )
 
( a sky of gold  barred with long transverse stripes of
reddish-purple cloud )
 
( There was talk of a floating body  )
 
(One of the men
there )
 
( a reservist he said he was  told my brother he had seen the
heliograph flickering in the west )
 
(

In Wellington Street my brother met a couple of sturdy roughs who had just
been rushed out of Fleet Street with still-wet newspapers and staring
placards )
 
(  Dreadful catastrophe   they bawled one to the other down
Wellington Street  )
 
( Fight ing at Weybridge  Full description  )
 
(Repulse of the
Martians  London in Danger  )
 
( He had to give threepence for a copy of that
paper )
 
(

Then it was  and then only  that he realised something of the full power and
terror of these monsters  )
 
(He learned that they were not merely a handful of
small sluggish creatures)
 
(  but that they were minds swaying vast mechanical
bodies  )
 
(and that they could move swiftly and smite with such power that even
the mightiest guns could not stand against them )
 
(

They were described as  vast spiderlike machines  )
 
(nearly a hundred feet
high  capable of the speed of an express train )
 
( and able to shoot out a beam
of intense heat   )
 
(Masked batteries  chiefly of field guns  had been planted
in the country about Horsell Common )
 
( and especially between the Woking
district and London )
 
( Five of the machines had been seen moving towards the
Thames  and one  by a happy chance  had been destroyed  In the other cases
the shells had missed  )
 
(and the batteries had been at once annihilated by the
Heat-Rays  )
 
(Heavy losses of soldiers were mentioned  but the tone of the
despatch was optimistic )
 
(

The Martians had been repulsed  they were not invulnerable  )
 
(They had
retreated to their triangle of cylinders again  )
 
(in the circle about Woking 
Signallers with heliographs were pushing forward upon them from all sides 
Guns were in rapid transit from Windsor  Portsmouth  )
 
(Aldershot 
Woolwich even from the north )
 
( among others  long wire-guns of ninety-five
tons from Woolwich )
 
( Altogether one hundred and sixteen were in position or
being hastily placed  chiefly covering London  )
 
(Never before in England had
there been such a vast or rapid concentration of military material )
 
(

Any further cylinders that fell  )
 
(it was hoped  could be destroyed at once by
high explosives )
 
(which were being rapidly manufactured and distributed  No
doubt  ran the report )
 
( the situation was of the strangest and gravest
description  but the public was exhorted to avoid and discourage panic  )
 
(No
doubt the Martians were strange and terrible in the extreme  )
 
(but at the
outside there could not be more than twenty of them against our millions )
 
(

The authorities had reason to suppose  )
 
(from the size of the cylinders  that
at the outside there could not be more than five in each cylinder fifteen
altogether  )
 
(And one at least was disposed of perhaps more )
 
( The public would
be fairly warned of the approach of danger  and elaborate measures were
being taken for the protection of the people in the threatened southwestern
suburbs  )
 
(And so  with reiterated assurances of the safety of London and the
ability of the authorities to cope )
 
(with the difficulty  this
quasi-proclamation closed )
 
(

This was printed in enormous type on paper so fresh that it was still wet )
 
(
and there had been no time to add a word of comment  )
 
(It was curious  my
brother said  to see how ruthlessly the usual contents of the paper had been
hacked and taken out to give this place )
 
(

All down Wellington Street people could be seen fluttering out the pink
sheets and reading  )
 
(and the Strand was suddenly noisy with the voices of an
army of hawkers following these pioneers )
 
( Men came scrambling off buses to
secure copies )
 
( Certainly this news excited people intensely  whatever their
previous apathy  )
 
(The shutters of a map shop in the Strand were being taken
down  my brother said  and a man in his Sunday raiment )
 
(lemon-yellow gloves
even  was visible inside the window hastily fastening maps of Surrey to the
glass )
 
(

Going on along the Strand to Trafalgar Square  the paper in his hand  my
brother saw some of the fugitives from West Surrey  )
 
(There was a man with his
wife and two boys and some articles of furniture in a cart such as
greengrocers use )
 
( He was driving from the direction of Westminster Bridge 
and close behind him came a hay waggon)
 
( with five or six respectable-looking
people in it  and some boxes and bundles  )
 
(The faces of these people were
haggard  and their entire appearance contrasted conspicuously with the
Sabbath-best appearance of the people on the omnibuses  )
 
(People in
fashionable clothing peeped at them out of cabs )
 
( They stopped at the Square
as if undecided which way to take )
 
( and finally turned eastward along the
Strand  Some way behind these came a man in workday clothes  )
 
(riding one of
those old-fashioned tricycles with a small front wheel )
 
( He was dirty and
white in the face )
 
(

My brother turned down towards Victoria )
 
( and met a number of such people  He
had a vague idea that he might see something of me )
 
( He noticed an unusual
number of police regulating the traffic  )
 
(Some of the refugees were
exchanging news with the people on the omnibuses  )
 
(One was professing to have
seen the Martians  )
 
( Boilers on stilts  I tell you  striding along like men  
Most of them were excited and animated by their strange experience )
 
(

Beyond Victoria the public-houses were doing a lively trade with these
arrivals  )
 
(At all the street corners groups of people were reading papers 
talking excitedly  or staring at these unusual Sunday visitors )
 
( They seemed
to increase as night drew on )
 
( until at last the roads  my brother said  were
like Epsom High Street on a Derby Day  )
 
(My brother addressed several of these
fugitives and got unsatisfactory answers from most )
 
(

None of them could tell him any news of Woking except one man  )
 
(who assured
him that Woking had been entirely destroyed on the previous night )
 
(

 I come from Byfleet   he said   man on a bicycle came through the place in
the early morning)
 
(  and ran from door to door warning us to come away  Then
came soldiers  )
 
(We went out to look  and there were clouds of smoke to the
south nothing but smoke  and not a soul coming that way  )
 
(Then we heard the
guns at Chertsey  and folks coming from Weybridge )
 
( So I ve locked up my
house and come on  )
 
(

At the time there was a strong feeling in the streets)
 
(that the authorities
were to blame for their incapacity to dispose of the invaders without all
this inconvenience )
 
(

About eight o clock a noise of heavy firing was distinctly audible all over
the south of London  )
 
(My brother could not hear it for the traffic in the
main thoroughfares )
 
( but by striking through the quiet back streets to the
river he was able to distinguish it quite plainly )
 
(

He walked from Westminster to his apartments near Regent s Park  )
 
(about two 
He was now very anxious on my account )
 
( and disturbed at the evident
magnitude of the trouble  )
 
(His mind was inclined to run  even as mine had run
on Saturday  on military details)
 
(  He thought of all those silent  expectant
guns  of the suddenly nomadic countryside  he tried to imagine )
 
( boilers on
stilts  a hundred feet high )
 
(

There were one or two cartloads of refugees passing along Oxford Street )
 
( and
several in the Marylebone Road )
 
( but so slowly was the news spreading that
Regent Street and Portland Place )
 
(were full of their usual Sunday-night
promenaders  )
 
(albeit they talked in groups  and along the edge of Regent s
Park there were as many silent couples  )
 
(walking out  together under the
scattered gas lamps as ever there had been  )
 
(The night was warm and still 
and a little oppressive  )
 
(the sound of guns continued intermittently  and
after midnight there seemed to be sheet lightning in the south )
 
(

He read and re-read the paper  fearing the worst had happened to me )
 
( He was
restless  and after supper prowled out again aimlessly  )
 
(He returned and
tried in vain to divert his attention to his examination notes )
 
( He went to
bed a little after midnight  )
 
(and was awakened from lurid dreams in the small
hours of Monday by the sound of door knockers  )
 
(feet running in the street 
distant drumming  and a clamour of bells  )
 
(Red reflections danced on the
ceiling  For a moment he lay astonished  )
 
(wondering whether day had come or
the world gone mad )
 
( Then he jumped out of bed and ran to the window )
 
(

His room was an attic and as he thrust his head out  )
 
(up and down the street
there were a dozen echoes to the noise of his window sash  )
 
(and heads in
every kind of night disarray appeared  )
 
(Enquiries were being shouted  )
 
(They
are coming   bawled a policeman  hammering at the door  )
 
( the Martians are
coming )
 
(  and hurried to the next door )
 
(

The sound of drumming and trumpeting came from the Albany Street Barracks 
and every church within earshot was hard at work )
 
(killing sleep with a
vehement disorderly tocsin  )
 
(There was a noise of doors opening  and window
after window in the houses opposite flashed from darkness into yellow
illumination )
 
(

Up the street came galloping a closed carriage  bursting abruptly into noise
at the corner )
 
( rising to a clattering climax under the window  and dying
away slowly in the distance  )
 
(Close on the rear of this came a couple of
cabs  the forerunners of a long procession of flying vehicles )
 
( going for the
most part to Chalk Farm station )
 
( where the North-Western special trains were
loading up  instead of coming down the gradient into Euston )
 
(

For a long time my brother stared out of the window in blank astonishment 
watching the policemen hammering at door after door  )
 
(and delivering their
incomprehensible message  )
 
(Then the door behind him opened  )
 
(and the man who
lodged across the landing came in  )
 
(dressed only in shirt  trousers  and
slippers  his braces loose about his waist  )
 
(his hair disordered from his
pillow 

 What the devil is it  he asked   A fire)
 
( What a devil of a row  )
 
(

They both craned their heads out of the window  )
 
(straining to hear what the
policemen were shouting  )
 
(People were coming out of the side streets  and
standing in groups at the corners talking )
 
(

 What the devil is it all about)
 
( said my brother s fellow lodger )
 
(

My brother answered him vaguely and began to dress  )
 
(running with each
garment to the window in order to miss nothing of the growing excitement )
 
(
And presently men selling unnaturally early newspapers came bawling into the
street )
 
(

 London in danger of suffocation  )
 
(The Kingston and Richmond defences forced 
Fearful massacres in the Thames Valley  )
 
(

And all about him in the rooms below )
 
( in the houses on each side and across
the road  and behind in the Park Terraces and in the hundred other streets
of that part of Marylebone  and the Westbourne Park district and St 
Pancras )
 
( and westward and northward in Kilburn and St  John s Wood and
Hampstead  )
 
(and eastward in Shoreditch and Highbury and Haggerston and
Hoxton  )
 
(and  indeed  through all the vastness of London from Ealing to East
Ham people were rubbing their eyes  )
 
(and opening windows to stare out and
ask aimless questions  )
 
(dressing hastily as the first breath of the coming
storm of Fear blew through the streets )
 
( It was the dawn of the great panic 
London  which had gone to bed on Sunday night oblivious and inert )
 
( was
awakened  in the small hours of Monday morning  )
 
(to a vivid sense of danger )
 
(

Unable from his window to learn what was happening )
 
( my brother went down and
out into the street)
 
(  just as the sky between the parapets of the houses grew
pink with the early dawn  )
 
(The flying people on foot and in vehicles grew
more numerous every moment   Black Smoke  )
 
( he heard people crying  and again
 Black Smoke  )
 
( The contagion of such a unanimous fear was inevitable  As my
brother hesitated on the door-step )
 
( he saw another news vender approaching 
and got a paper forthwith )
 
( The man was running away with the rest  and
selling his papers for a shilling each as he ran a grotesque mingling of
profit and panic )
 
(

And from this paper my brother read that catastrophic despatch of the
Commander-in-Chief )
 
(

 The Martians are able to discharge enormous clouds of a black and poisonous
vapour by means of rockets  )
 
(They have smothered our batteries  destroyed
Richmond  Kingston  and Wimbledon  )
 
(and are advancing slowly towards London 
destroying everything on the way )
 
( It is impossible to stop them  )
 
(There is no
safety from the Black Smoke but in instant flight  )
 
(

That was all  but it was enough )
 
( The whole population of the great
six-million city was stirring  )
 
(slipping  running  presently it would be
pouring en masse northward )
 
(

 Black Smoke   the voices cried   Fire  )
 
(

The bells of the neighbouring church made a jangling tumult  a cart
carelessly driven smashed )
 
( amid shrieks and curses )
 
( against the water trough
up the street  )
 
(Sickly yellow lights went to and fro in the houses  )
 
(and some
of the passing cabs flaunted unextinguished lamps  )
 
(And overhead the dawn was
growing brighter  clear and steady and calm )
 
(

He heard footsteps running to and fro in the rooms  )
 
(and up and down stairs
behind him  His landlady came to the door )
 
( loosely wrapped in dressing gown
and shawl  her husband followed ejaculating )
 
(

As my brother began to realise the import of all these things  )
 
(he turned
hastily to his own room )
 
( put all his available money some ten pounds
altogether into his pockets  and went out again into the streets )
 
(

It was while the curate had sat and talked so wildly to me under the hedge
in the flat meadows near Halliford )
 
( and while my brother was watching the
fugitives stream over Westminster Bridge  )
 
(that the Martians had resumed the
offensive )
 
( So far as one can ascertain from the conflicting accounts that
have been put forth  the majority of them remained busied )
 
(with preparations
in the Horsell pit until nine that night  )
 
(hurrying on some operation that
disengaged huge volumes of green smoke )
 
(

But three certainly came out about eight o clock and  )
 
(advancing slowly and
cautiously  )
 
(made their way through Byfleet and Pyrford towards Ripley and
Weybridge )
 
( and so came in sight of the expectant batteries against the
setting sun  )
 
(These Martians did not advance in a body  but in a line  each
perhaps a mile and a half from his nearest fellow  )
 
(They communicated with
one another by means of sirenlike howls )
 
( running up and down the scale from
one note to another )
 
(

It was this howling and firing of the guns at Ripley and St  George s Hill
that we had heard at Upper Halliford )
 
( The Ripley gunners  unseasoned
artillery volunteers who ought never to have been placed in such a position 
fired one wild  premature )
 
( ineffectual volley  and bolted on horse and foot
through the deserted village  )
 
(while the Martian  without using his Heat-Ray 
walked serenely over their guns  stepped gingerly among them  )
 
(passed in
front of them)
 
(  and so came unexpectedly upon the guns in Painshill Park 
which he destroyed )
 
(

The St  George s Hill men  however  )
 
(were better led or of a better mettle 
Hidden by a pine wood as they were )
 
( they seem to have been quite unsuspected
by the Martian nearest to them  )
 
(They laid their guns as deliberately as if
they had been on parade  and fired at about a thousand yards  range )
 
(

The shells flashed all round him  and he was seen to advance a few paces 
stagger  and go down  )
 
(Everybody yelled together  and the guns were reloaded
in frantic haste)
 
(  The overthrown Martian set up a prolonged ululation  and
immediately a second glittering giant  )
 
(answering him  appeared over the
trees to the south )
 
( It would seem that a leg of the tripod had been smashed
by one of the shells  )
 
(The whole of the second volley flew wide of the
Martian on the ground  and  simultaneously  both his companions brought
their Heat-Rays to bear on the battery  )
 
(The ammunition blew up  the pine
trees all about the guns flashed into fire  )
 
(and only one or two of the men
who were already running over the crest of the hill escaped )
 
(

;;;;;


After this it would seem that the three took counsel together and halted 
and the scouts who were watching them )
 
(report that they remained absolutely
stationary for the next half hour  )
 
(The Martian who had been overthrown
crawled tediously out of his hood )
 
( a small brown figure  oddly suggestive
from that distance of a speck of blight  )
 
(and apparently engaged in the
repair of his support  )
 
(About nine he had finished  for his cowl was then
seen above the trees again )
 
(

It was a few minutes past nine that night when these three sentinels were
joined by four other Martians  each carrying a thick black tube )
 
( A similar
tube was handed to each of the three )
 
( and the seven proceeded to distribute
themselves at equal distances along a curved line between St  George s Hill 
Weybridge  and the village of Send  southwest of Ripley )
 
(

A dozen rockets sprang out of the hills before them so soon as they began to
move  )
 
(and warned the waiting batteries about Ditton and Esher  )
 
(At the same
time four of their fighting machines  )
 
(similarly armed with tubes  crossed
the river )
 
( and two of them  black against the western sky )
 
( came into sight
of myself and the curate as we hurried wearily and painfully along the road
that runs northward out of Halliford  )
 
(They moved  as it seemed to us  upon a
cloud  for a milky mist covered the fields and rose to a third of their
height )
 
(

At this sight the curate cried faintly in his throat )
 
( and began running  but
I knew it was no good running from a Martian )
 
( and I turned aside and crawled
through dewy nettles and brambles into the broad ditch by the side of the
road  )
 
(He looked back  saw what I was doing  and turned to join me )
 
(

The two halted  the nearer to us standing and facing Sunbury )
 
( the remoter
being a grey indistinctness towards the evening star  )
 
(

The occasional howling of the Martians had ceased )
 
( they took up their
positions in the huge crescent about their cylinders in absolute silence )
 
( It
was a crescent with twelve miles between its horns  )
 
(Never since the devising
of gunpowder was the beginning of a battle so still )
 
( To us and to an
observer about Ripley it would have had precisely the same effect the
Martians seemed in solitary possession of the darkling night )
 
( lit only as it
was by the slender moon )
 
( the stars  the afterglow of the daylight)
 
( and the
ruddy glare from St  George s Hill and the woods of Painshill )
 
(

But facing that crescent everywhere at Staines  Hounslow  Ditton  Esher 
Ockham )
 
( behind hills and woods south of the river  and across the flat grass
meadows to the north of it )
 
( wherever a cluster of trees or village houses
gave sufficient cover the guns were waiting  )
 
(The signal rockets burst and
rained their sparks through the night and vanished )
 
( and the spirit of all
those watching batteries rose to a tense expectation )
 
( The Martians had but
to advance into the line of fire  )
 
(and instantly those motionless black forms
of men )
 
( those guns glittering so darkly in the early night  would explode
into a thunderous fury of battle )
 
(

No doubt the thought that was uppermost in a thousand of those vigilant
minds  )
 
(even as it was uppermost in mine )
 
( was the riddle how much they
understood of us  )
 
(Did they grasp that we in our millions were organized 
disciplined  working together)
 
( Or did they interpret our spurts of fire )
 
( the
sudden stinging of our shells)
 
(  our steady investment of their encampment  as
we should the furious unanimity of onslaught in a disturbed hive of bees)
 
(
Did they dream they might exterminate us )
 
(At that time no one knew what
food they needed)
 
(A hundred such questions struggled together in my mind as
I watched that vast sentinel shape )
 
( And in the back of my mind was the sense
of all the huge unknown and hidden forces Londonward  )
 
(Had they prepared
pitfalls )
 
(Were the powder mills at Hounslow ready as a snare )
 
(Would the
Londoners have the heart and courage to make a greater Moscow of their
mighty province of houses)
 
(

Then  after an interminable time  as it seemed to us  )
 
(crouching and peering
through the hedge )
 
( came a sound like the distant concussion of a gun )
 
(
Another nearer  and then another  )
 
(And then the Martian beside us raised his
tube on high and discharged it  gunwise )
 
( with a heavy report that made the
ground heave )
 
( The one towards Staines answered him  )
 
(There was no flash  no
smoke  simply that loaded detonation )
 
(

I was so excited by these heavy minute-guns following one another that I so
far forgot my personal safety and my scalded hands as to clamber up into the
hedge and stare towards Sunbury  )
 
(As I did so a second report followed  and a
big projectile hurtled overhead towards Hounslow  )
 
(I expected at least to see
smoke or fire  or some such evidence of its work )
 
( But all I saw was the deep
blue sky above  with one solitary star )
 
( and the white mist spreading wide
and low beneath )
 
( And there had been no crash  no answering explosion  The
silence was restored  the minute lengthened to three )
 
(

 What has happened  said the curate  standing up beside me )
 
(

 Heaven knows   said I )
 
(

A bat flickered by and vanished  )
 
(A distant tumult of shouting began and
ceased  I looked again at the Martian  )
 
(and saw he was now moving eastward
along the riverbank )
 
( with a swift  rolling motion )
 
(

Every moment I expected the fire of some hidden battery to spring upon him 
but the evening calm was unbroken  )
 
(The figure of the Martian grew smaller as
he receded)
 
( and presently the mist and the gathering night had swallowed him
up  By a common impulse we clambered higher )
 
( Towards Sunbury was a dark
appearance  as though a conical hill had suddenly come into being there 
hiding our view of the farther country )
 
( and then  remoter across the river 
over Walton )
 
( we saw another such summit  These hill-like forms grew lower
and broader even as we stared )
 
(

Moved by a sudden thought  I looked northward  )
 
(and there I perceived a third
of these cloudy black kopjes had risen )
 
(

Everything had suddenly become very still  )
 
(Far away to the southeast 
marking the quiet )
 
( we heard the Martians hooting to one another  and then
the air quivered again with the distant thud of their guns)
 
( But the earthly
artillery made no reply )
 
(

Now at the time we could not understand these things  but later I was to
learn the meaning of these ominous kopjes that gathered in the twilight )
 
(
Each of the Martians  standing in the great crescent I have described  had
discharged  by means of the gunlike tube he carried )
 
( a huge canister over
whatever hill )
 
( copse  cluster of houses  or other possible cover for guns 
chanced to be in front of him )
 
( Some fired only one of these  some two as in
the case of the one we had seen )
 
( the one at Ripley is said to have
discharged no fewer than five at that time  )
 
(These canisters smashed on
striking the ground they did not explode and incontinently disengaged an
enormous volume of heavy  inky vapour  coiling and pouring upward in a huge
and ebony cumulus cloud  )
 
(a gaseous hill that sank and spread itself slowly
over the surrounding country  )
 
(And the touch of that vapour  the inhaling of
its pungent wisps  was death to all that breathes )
 
(

It was heavy  this vapour  heavier than the densest smoke )
 
( so that  after
the first tumultuous uprush and outflow of its impact )
 
( it sank down through
the air and poured over the ground in a manner rather liquid than gaseous 
abandoning the hills )
 
( and streaming into the valleys and ditches and
watercourses even as I have heard the carbonic-acid gas that pours from
volcanic clefts is wont to do  )
 
(And where it came upon water some chemical
action occurred  and the surface would be instantly covered with a powdery
scum that sank slowly and made way for more  )
 
(The scum was absolutely
insoluble  and it is a strange thing  seeing the instant effect of the gas 
that one could drink without hurt the water from which it had been strained )
 
(
The vapour did not diffuse as a true gas would do )
 
( It hung together in
banks  flowing sluggishly down the slope of the land and driving reluctantly
before the wind  )
 
(and very slowly it combined with the mist and moisture of
the air  and sank to the earth in the form of dust  )
 
(Save that an unknown
element giving a group of four lines in the blue of the spectrum is
concerned )
 
( we are still entirely ignorant of the nature of this substance )
 
(

Once the tumultuous upheaval of its dispersion was over  the black smoke
clung so closely to the ground)
 
(  even before its precipitation  that fifty
feet up in the air )
 
( on the roofs and upper stories of high houses and on
great trees  there was a chance of escaping its poison altogether)
 
(  as was
proved even that night at Street Cobham and Ditton )
 
(

The man who escaped at the former place tells a wonderful story of the
strangeness of its coiling flow )
 
( and how he looked down from the church
spire and saw the houses of the village rising like ghosts out of its inky
nothingness)
 
(  For a day and a half he remained there  weary  starving and
sun-scorched  the earth under the blue sky and against the prospect of the
distant hills a velvet-black expanse  )
 
(with red roofs  green trees  and 
later  black-veiled shrubs and gates )
 
( barns  outhouses  and walls  rising
here and there into the sunlight )
 
(

But that was at Street Cobham  where the black vapour was allowed to remain
until it sank of its own accord into the ground )
 
( As a rule the Martians 
when it had served its purpose  )
 
(cleared the air of it again by wading into
it and directing a jet of steam upon it )
 
(

This they did with the vapour banks near us  )
 
(as we saw in the starlight from
the window of a deserted house at Upper Halliford )
 
( whither we had returned 
From there we could see the searchlights on Richmond Hill and Kingston Hill
going to and fro  )
 
(and about eleven the windows rattled  )
 
(and we heard the
sound of the huge siege guns that had been put in position there  )
 
(These
continued intermittently for the space of a quarter of an hour  )
 
(sending
chance shots at the invisible Martians at Hampton and Ditton )
 
( and then the
pale beams of the electric light vanished  )
 
(and were replaced by a bright red
glow )
 
(

Then the fourth cylinder fell a brilliant green meteor as I learned
afterwards  in Bushey Park  )
 
(Before the guns on the Richmond and Kingston
line of hills began )
 
( there was a fitful cannonade far away in the southwest 
due  I believe  )
 
(to guns being fired haphazard before the black vapour could
overwhelm the gunners )
 
(

So  setting about it as methodically as men might smoke out a wasps  nest 
the Martians spread this strange stifling vapour over the Londonward
country )
 
( The horns of the crescent slowly moved apart  until at last they
formed a line from Hanwell to Coombe and Malden )
 
( All night through their
destructive tubes advanced )
 
( Never once  after the Martian at St  George s
Hill was brought down )
 
( did they give the artillery the ghost of a chance
against them  )
 
(Wherever there was a possibility of guns being laid for them
unseen  a fresh canister of the black vapour was discharged )
 
( and where the
guns were openly displayed the Heat-Ray was brought to bear )
 
(

By midnight the blazing trees along the slopes of Richmond Park and the
glare of Kingston Hill )
 
(threw their light upon a network of black smoke 
blotting out the whole valley of the Thames )
 
(and extending as far as the eye
could reach )
 
(And through this two Martians slowly waded  and turned their
hissing steam jets this way and that )
 
(

They were sparing of the Heat-Ray that night  )
 
(either because they had but a
limited supply of material for its production or because they did not wish
to destroy the country but only to crush and overawe the opposition they had
aroused)
 
(  In the latter aim they certainly succeeded  )
 
(Sunday night was the
end of the organised opposition to their movements  )
 
(After that no body of
men would stand against them  so hopeless was the enterprise  Even the crews
of the torpedo-boats and destroyers that had brought their quick-firers up
the Thames refused to stop  mutinied )
 
( and went down again )
 
( The only
offensive operation men ventured upon after that night was the preparation
of mines and pitfalls )
 
( and even in that their energies were frantic and
spasmodic )
 
(

One has to imagine  as well as one may  the fate of those batteries towards
Esher )
 
( waiting so tensely in the twilight  )
 
(Survivors there were none  One
may picture the orderly expectation )
 
( the officers alert and watchful  the
gunners ready  the ammunition piled to hand  )
 
(the limber gunners with their
horses and waggons )
 
( the groups of civilian spectators standing as near as
they were permitted )
 
( the evening stillness  the ambulances and hospital
tents with the burned and wounded from Weybridge )
 
( then the dull resonance of
the shots the Martians fired  )
 
(and the clumsy projectile whirling over the
trees and houses and smashing amid the neighbouring fields )
 
(

One may picture  too  the sudden shifting of the attention )
 
( the swiftly
spreading coils and bellyings of that blackness advancing headlong  towering
heavenward  turning the twilight to a palpable darkness )
 
( a strange and
horrible antagonist of vapour striding upon its victims )
 
( men and horses near
it seen dimly  running  shrieking  falling headlong  shouts of dismay  the
guns suddenly abandoned  men choking and writhing on the ground  )
 
(and the
swift broadening-out of the opaque cone of smoke )
 
( And then night and
extinction nothing but a silent mass of impenetrable vapour hiding its
dead )
 
(

Before dawn the black vapour was pouring through the streets of Richmond )
 
(
and the disintegrating organism of government was )
 
( with a last expiring
effort  rousing the population of London to the necessity of flight )
 
(

So you understand the roaring wave of fear that swept through the greatest
city in the world)
 
( just as Monday was dawning the stream of flight rising
swiftly to a torrent  lashing in a foaming tumult round the railway
stations  )
 
(banked up into a horrible struggle about the shipping in the
Thames)
 
(  and hurrying by every available channel northward and eastward  )
 
(By
ten o clock the police organisation )
 
( and by midday even the railway
organisations  )
 
(were losing coherency  losing shape and efficiency 
guttering  softening )
 
( running at last in that swift liquefaction of the
social body )
 
(

All the railway lines north of the Thames and the South-Eastern people at
Cannon Street )
 
(had been warned by midnight on Sunday  and trains were being
filled  )
 
(People were fighting savagely for standing-room in the carriages
even at two o clock  )
 
(By three  people were being trampled and crushed even
in Bishopsgate Street )
 
( a couple of hundred yards or more from Liverpool
Street station  )
 
(revolvers were fired  people stabbed  and the policemen who
had been sent to direct the traffic )
 
( exhausted and infuriated  were breaking
the heads of the people they were called out to protect )
 
(

And as the day advanced and the engine drivers and stokers refused to return
to London )
 
( the pressure of the flight drove the people in an ever-thickening
multitude away from the stations and along the northward-running roads )
 
( By
midday a Martian had been seen at Barnes )
 
( and a cloud of slowly sinking
black vapour drove along the Thames and across the flats of Lambeth)
 
(  cutting
off all escape over the bridges in its sluggish advance  )
 
(Another bank drove
over Ealing  )
 
(and surrounded a little island of survivors on Castle Hill 
alive  but unable to escape )
 
(

;;;;;;;

After a fruitless struggle to get aboard a North-Western train at Chalk)
 
(
Farm the engines of the trains that had loaded in the goods yard there
ploughed through shrieking people )
 
( and a dozen stalwart men fought to keep
the crowd from crushing the driver against his furnace my brother emerged
upon the Chalk Farm road )
 
( dodged across through a hurrying swarm of
vehicles  and had the luck to be foremost in the sack of a cycle shop )
 
( The
front tire of the machine he got was punctured in dragging it through the
window  but he got up and off  )
 
(notwithstanding  with no further injury than
a cut wrist )
 
( The steep foot of Haverstock Hill was impassable owing to
several overturned horses )
 
( and my brother struck into Belsize Road )
 
(

So he got out of the fury of the panic  and  skirting the Edgware Road 
reached Edgware about seven  fasting and wearied )
 
( but well ahead of the
crowd )
 
( Along the road people were standing in the roadway  curious 
wondering  )
 
(He was passed by a number of cyclists  some horsemen  and two
motor cars  )
 
(A mile from Edgware the rim of the wheel broke  and the machine
became unridable  )
 
(He left it by the roadside and trudged through the
village  )
 
(There were shops half opened in the main street of the place  and
people crowded on the pavement and in the doorways and windows )
 
( staring
astonished at this extraordinary procession of fugitives)
 
( that was beginning 
He succeeded in getting some food at an inn )
 
(

For a time he remained in Edgware not knowing what next to do  )
 
(The flying
people increased in number)
 
(  Many of them  like my brother  seemed inclined
to loiter in the place )
 
( There was no fresh news of the invaders from Mars )
 
(

At that time the road was crowded  but as yet far from congested )
 
( Most of
the fugitives at that hour were mounted on cycles )
 
( but there were soon motor
cars  hansom cabs  and carriages hurrying along  )
 
(and the dust hung in heavy
clouds along the road to St  Albans )
 
(

It was perhaps a vague idea of making his way to Chelmsford )
 
( where some
friends of his lived )
 
( that at last induced my brother to strike into a quiet
lane running eastward )
 
( Presently he came upon a stile  and  crossing it 
followed a footpath northeastward)
 
( He passed near several farmhouses and
some little places whose names he did not learn )
 
( He saw few fugitives until 
in a grass lane towards High Barnet )
 
( he happened upon two ladies who became
his fellow travellers  )
 
(He came upon them just in time to save them )
 
(

He heard their screams  and  hurrying round the corner  )
 
( saw a couple of men
struggling to drag them out of the little pony-chaise  )
 
(in which they had been
driving   )
 
(while a third with difficulty held the frightened pony s head   )
 
(One
of the ladies  a short woman dressed in white  )
 
( was simply screaming  the
other  a dark  slender figure   )
 
(slashed at the man who gripped her arm with a
whip she held in her disengaged hand  )
 
(

My brother immediately grasped the situation  shouted   )
 
(and hurried towards
the struggle   )
 
(One of the men desisted and turned towards him  )
 
( and my
brother  realising from his antagonist s face that a fight was unavoidable 
and being an expert boxer  )
 
( went into him forthwith and sent him down against
the wheel of the chaise  )
 
(

It was no time for pugilistic chivalry and my brother laid him quiet with a
kick )
 
(  and gripped the collar of the man who pulled at the slender lady s
arm   )
 
(He heard the clatter of hoofs  )
 
( the whip stung across his face  )
 
( a third
antagonist struck him between the eyes   )
 
(and the man he held wrenched himself
free and made off down the lane in the direction from which he had come  )
 
(

Partly stunned  he found himself facing the man who had held the horse s
head  )
 
( and became aware of the chaise receding from him down the lane 
swaying from side to side )
 
( and with the women in it looking back  )
 
( The man
before him  a burly rough  tried to close  )
 
( and he stopped him with a blow in
the face  )
 
( Then  realising that he was deserted   )
 
(he dodged round and made off
down the lane after the chaise   )
 
(with the sturdy man close behind him  and
the fugitive  who had turned now  following remotely  )
 
(

Suddenly he stumbled and fell  )
 
( his immediate pursuer went headlong  and he
rose to his feet to find himself with a couple of antagonists again  )
 
( He
would have had little chance against them had not the slender lady very
pluckily pulled up and returned to his help   )
 
(It seems she had had a revolver
all this time  but it had been under the seat  )
 
(when she and her companion
were attacked  )
 
( She fired at six yards  distance  narrowly missing my
brother  )
 
( The less courageous of the robbers made off   )
 
(and his companion
followed him  cursing his cowardice   )
 
(They both stopped in sight down the
lane  where the third man lay insensible  )
 
(

 Take this   said the slender lady  and she gave my brother her revolver  )
 
(

 Go back to the chaise   said my brother  )
 
( wiping the blood from his split
lip  )
 
( )
 
(

She turned without a word they were both panting  )
 
(and they went back to
where the lady in white struggled to hold back the frightened pony 

The robbers had evidently had enough of it   )
 
(When my brother looked again
they were retreating  )
 
(

 I ll sit here   said my brother   if I may   and he got upon the empty
front seat  )
 
( The lady looked over her shoulder  )
 
(

 Give me the reins   she said  and laid the whip along the pony s side   )
 
(In
another moment a bend in the road hid the three men from my brother s eyes  )
 
(

So  quite unexpectedly  my brother found himself  panting   )
 
(with a cut mouth 
a bruised jaw  and bloodstained knuckles  )
 
( driving along an unknown lane with
these two women  )
 
(

He learned they were the wife and the younger sister of a surgeon living at
Stanmore )
 
(  who had come in the small hours from a dangerous case at Pinner 
and heard at some railway station on his way of the Martian advance   )
 
(He had
hurried home  roused the women their servant had left them two days
before packed some provisions  )
 
( put his revolver under the seat luckily for
my brother and told them to drive on to Edgware   )
 
(with the idea of getting a
train there  He stopped behind to tell the neighbours   )
 
(He would overtake
them  he said   )
 
(at about half past four in the morning  and now it was nearly
nine and they had seen nothing of him  )
 
( They could not stop in Edgware
because of the growing traffic through the place  )
 
(and so they had come into
this side lane  )
 
(

That was the story they told my brother in fragments when presently they
stopped again  nearer to New Barnet   )
 
(He promised to stay with them  at least
until they could determine what to do  or until the missing man arrived   )
 
(and
professed to be an expert shot with the revolver a weapon strange to
him in order to give them confidence  )
 
(

They made a sort of encampment by the wayside  )
 
( and the pony became happy in
the hedge   )
 
(He told them of his own escape out of London  )
 
( and all that he
knew of these Martians and their ways  )
 
( The sun crept higher in the sky   )
 
(and
after a time their talk died out and gave place to an uneasy state of
anticipation  )
 
( Several wayfarers came along the lane  and of these my brother
gathered such news as he could  )
 
( Every broken answer he had deepened his
impression of the great disaster that had come on humanity  )
 
( deepened his
persuasion of the immediate necessity for prosecuting this flight   )
 
(He urged
the matter upon them  )
 
(

 We have money   said the slender woman  and hesitated  )
 
(

Her eyes met my brother s  and her hesitation ended  )
 
(

 So have I   said my brother  )
 
(

;;;;;


She explained that they had as much as thirty pounds in gold  )
 
( besides a
five-pound note  and suggested that with that they might get upon a train at
St  Albans or New Barnet  )
 
( My brother thought that was hopeless  seeing the
fury of the Londoners to crowd upon the trains  )
 
( and broached his own idea of
striking across Essex towards Harwich and thence escaping from the country
altogether  )
 
(

Mrs  Elphinstone that was the name of the woman in white would listen to
no reasoning  )
 
( and kept calling upon  George  )
 
( but her sister-in-law was
astonishingly quiet and deliberate )
 
(  and at last agreed to my brother s
suggestion  So  designing to cross the Great North Road   )
 
(they went on
towards Barnet  )
 
( my brother leading the pony to save it as much as possible  )
 
(

As the sun crept up the sky the day became excessively hot  )
 
( and under foot a
thick  whitish sand grew burning and blinding  )
 
( so that they travelled only
very slowly )
 
(  The hedges were grey with dust   )
 
(And as they advanced towards
Barnet a tumultuous murmuring grew stronger  )
 
(

They began to meet more people   )
 
(For the most part these were staring before
them  murmuring indistinct questions  jaded  haggard  unclean   )
 
(One man in
evening dress passed them on foot  his eyes on the ground   )
 
(They heard his
voice  and  looking back at him   )
 
(saw one hand clutched in his hair and the
other beating invisible things  )
 
( His paroxysm of rage over  he went on his
way without once looking back  )
 
(

As my brother s party went on towards the crossroads to the south of Barnet
they saw a woman approaching the road across some fields on their left  )
 
(
carrying a child and with two other children  )
 
( and then passed a man in dirty
black  with a thick stick in one hand and a small portmanteau in the other  )
 
(
Then round the corner of the lane  from between the villas that guarded it
at its confluence with the high road  )
 
( came a little cart drawn by a sweating
black pony and driven by a sallow youth in a bowler hat  grey with dust  )
 
(
There were three girls  East End factory girls  )
 
( and a couple of little
children crowded in the cart  )
 
(

 This ll tike us rahnd Edgware  asked the driver  wild-eyed  )
 
( white-faced 
and when my brother told him it would if he turned to the left )
 
(  he whipped
up at once without the formality of thanks  )
 
(

My brother noticed a pale grey smoke or haze rising among the houses in
front of them  )
 
( and veiling the white facade of a terrace beyond the road
that appeared between the backs of the villas   )
 
(Mrs  Elphinstone suddenly
cried out at a number of tongues of smoky red flame leaping up above the
houses in front of them against the hot  blue sky   )
 
(The tumultuous noise
resolved itself now into the disorderly mingling of many voices   )
 
(the gride
of many wheels  the creaking of waggons  and the staccato of hoofs  )
 
( The lane
came round sharply not fifty yards from the crossroads  )
 
(

 Good heavens   cried Mrs  Elphinstone  )
 
(  What is this you are driving us
into  )
 
(

My brother stopped  )
 
(

For the main road was a boiling stream of people  )
 
( a torrent of human beings
rushing northward  )
 
( one pressing on another  )
 
( A great bank of dust  white and
luminous in the blaze of the sun )
 
( made everything within twenty feet of the
ground grey and indistinct and was perpetually renewed by the hurrying feet
of a dense crowd of horses and of men and women on foot   )
 
(and by the wheels
of vehicles of every description  )
 
(

 Way   my brother heard voices crying   Make way   )
 
(

It was like riding into the smoke of a fire to approach the meeting point of
the lane and road   )
 
(the crowd roared like a fire  and the dust was hot and
pungent   )
 
(And  indeed  a little way up the road a villa was burning and
sending rolling masses of black smoke across the road to add to the
confusion  )
 
(

Two men came past them   )
 
(Then a dirty woman  carrying a heavy bundle and
weeping  )
 
( A lost retriever dog  with hanging tongue  circled dubiously round
them  scared and wretched  and fled at my brother s threat  )
 
(

So much as they could see of the road Londonward between the houses to the
right was a tumultuous stream of dirty  hurrying people   )
 
(pent in between the
villas on either side )
 
(  the black heads  the crowded forms  grew into
distinctness as they rushed towards the corner   )
 
(hurried past  and merged
their individuality again in a receding multitude that was swallowed up at
last in a cloud of dust  )
 
(

 Go on  Go on   cried the voices   Way  Way   )
 
(

One man s hands pressed on the back of another   )
 
(My brother stood at the
pony s head  )
 
( Irresistibly attracted  he advanced slowly  pace by pace  down
the lane  )
 
(

Edgware had been a scene of confusion   )
 
(Chalk Farm a riotous tumult  but this
was a whole population in movement   )
 
(It is hard to imagine that host  It had
no character of its own   )
 
(The figures poured out past the corner  and receded
with their backs to the group in the lane   )
 
(Along the margin came those who
were on foot threatened by the wheels   )
 
(stumbling in the ditches  blundering
into one another  )
 
(

The carts and carriages crowded close upon one another   )
 
(making little way
for those swifter and more impatient vehicles that darted forward every now
and then when an opportunity showed itself of doing so  )
 
( sending the people
scattering against the fences and gates of the villas  )
 
(

 Push on   was the cry   Push on  They are coming  )
 
(

In one cart stood a blind man in the uniform of the Salvation Army 
gesticulating with his crooked fingers and bawling   )
 
(Eternity  Eternity  
His voice was hoarse and very loud so that my brother could hear him long
after he was lost to sight in the dust )
 
( Some of the people who crowded in
the carts whipped stupidly at their horses and quarrelled with other
drivers  some sat motionless  )
 
(staring at nothing with miserable eyes  some
gnawed their hands with thirst  )
 
(or lay prostrate in the bottoms of their
conveyances  )
 
(The horses  bits were covered with foam  their eyes bloodshot )
 
(

There were cabs  carriages  shop cars  )
 
(waggons  beyond counting  a mail
cart  a road-cleaner s cart marked )
 
(Vestry of St  Pancras   a huge timber
waggon crowded with roughs )
 
( A brewer s dray rumbled by with its two near
wheels splashed with fresh blood )
 
(

 Clear the way   cried the voices   Clear the way  )
 
(

 Eter-nity  Eter-nity   came echoing down the road )
 
(

There were sad  haggard women tramping by  well dressed  )
 
(with children that
cried and stumbled  their dainty clothes smothered in dust )
 
( their weary
faces smeared with tears  )
 
(With many of these came men  sometimes helpful 
sometimes lowering and savage  )
 
(Fighting side by side with them pushed some
weary street outcast in faded black rags )
 
( wide-eyed  loud-voiced  and
foul-mouthed  )
 
(There were sturdy workmen thrusting their way along  wretched 
unkempt men  clothed like clerks or shopmen  )
 
(struggling spasmodically  a
wounded soldier my brother noticed  )
 
(men dressed in the clothes of railway
porters  one wretched creature in a nightshirt with a coat thrown over it )
 
(

But varied as its composition was  certain things all that host had in
common  )
 
(There were fear and pain on their faces  )
 
(and fear behind them  )
 
(A
tumult up the road  a quarrel for a place in a waggon  )
 
(sent the whole host
of them quickening their pace  )
 
(even a man so scared and broken that his
knees bent under him was galvanised for a moment into renewed activity  )
 
(The
heat and dust had already been at work upon this multitude )
 
( Their skins were
dry  their lips black and cracked  )
 
(They were all thirsty  weary  and
footsore  And amid the various cries one heard disputes  )
 
(reproaches  groans
of weariness and fatigue )
 
( the voices of most of them were hoarse and weak 
Through it all ran a refrain )
 
(

)
 
(The Martians are coming  )
 
(

Few stopped and came aside from that flood  )
 
(The lane opened slantingly into
the main road )
 
(with a narrow opening  and had a delusive appearance of coming
from the direction of London )
 
( Yet a kind of eddy of people drove into its
mouth  )
 
(weaklings elbowed out of the stream )
 
( who for the most part rested but
a moment before plunging into it again )
 
( A little way down the lane  )
 
(with two
friends bending over him  lay a man with a bare leg  )
 
(wrapped about with
bloody rags  )
 
(He was a lucky man to have friends )
 
(

A little old man )
 
( with a grey military moustache and a filthy black frock
coat )
 
( limped out and sat down beside the trap  )
 
(removed his boot his sock
was blood-stained shook out a pebble)
 
(  and hobbled on again )
 
( and then a
little girl of eight or nine  )
 
(all alone  threw herself under the hedge close
by my brother  weeping )
 
(

 I can t go on  I can t go on  )
 
(

My brother woke from his torpor of astonishment )
 
(and lifted her up  speaking
gently to her  )
 
(and carried her to Miss Elphinstone  )
 
(So soon as my brother
touched her she became quite still  )
 
(as if frightened )
 
(

 Ellen   shrieked a woman in the crowd  )
 
(with tears in her voice )
 
( Ellen  
And the child suddenly darted away from my brother  )
 
(crying  Mother  )
 
(

 They are coming )
 
(  said a man on horseback )
 
( riding past along the lane )
 
(

 Out of the way  there  )
 
( bawled a coachman  towering high  )
 
(and my brother
saw a closed carriage turning into the lane )
 
(

The people crushed back on one another to avoid the horse )
 
( My brother pushed
the pony and chaise back into the hedge  )
 
(and the man drove by and stopped at
the turn of the way )
 
( It was a carriage  )
 
(with a pole for a pair of horses 
but only one was in the traces  )
 
(My brother saw dimly through the dust that
two men lifted out something )
 
(on a white stretcher and put it gently on the
grass beneath the privet hedge )
 
(

One of the men came running to my brother )
 
(

 Where is there any water)
 
(He is dying fast )
 
( and very thirsty  It
is Lord Garrick  )
 
(

 Lord Garrick   said my brother   the Chief Justice)
 
(

 The water  he said )
 
(

 There may be a tap   )
 
(said my brother  )
 
( in some of the houses  )
 
(We have no
water  I dare not leave my people  )
 
(

The man pushed against the crowd towards the gate of the corner house )
 
(

 Go on   )
 
(said the people  thrusting at him   )
 
(They are coming  Go on  )
 
(

Then my brother s attention was distracted by a bearded  )
 
(eagle-faced man
lugging a small handbag  )
 
(which split even as my brother s eyes rested on it)
 
(
and disgorged a mass of sovereigns that seemed to break up)
 
( into separate
coins as it struck the ground )
 
(They rolled hither and thither among the
struggling feet of men and horses  )
 
(The man stopped and looked stupidly at
the heap  )
 
(and the shaft of a cab struck his shoulder )
 
(and sent him reeling 
He gave a shriek and dodged back )
 
( and a cartwheel shaved him narrowly )
 
(

 Way   cried the men all about him   )
 
(
So soon as the cab had passed  )
 
(he flung himself  with both hands open  upon
the heap of coins  )
 
(and began thrusting handfuls in his pocket  )
 
(A horse rose
close upon him  )
 
(and in another moment  half rising  )
 
(he had been borne down
under the horse s hoofs )
 
(

 Stop   screamed my brother )
 
( and pushing a woman out of his way  tried to
clutch the bit of the horse )
 
(

Before he could get to it  he heard a scream under the wheels  )
 
(and saw
through the dust )
 
(the rim passing over the poor wretch s back  )
 
(The driver of
the cart slashed his whip at my brother  )
 
(who ran round behind the cart  )
 
(The
multitudinous shouting confused his ears  )
 
(The man was writhing in the dust
among his scattered money  unable to rise  )
 
(for the wheel had broken his
back)
 
(  and his lower limbs lay limp and dead  )
 
(My brother stood up and yelled
at the next driver  )
 
(and a man on a black horse came to his assistance 

 Get him out of the road   )
 
(said he  and  clutching the man s collar with his
free hand  )
 
(my brother lugged him sideways )
 
( But he still clutched after his
money  and regarded )
 
(my brother fiercely  hammering at his arm with a handful
of gold   )
 
(Go on  Go on   shouted angry voices behind )
 
(

 Way  Way  

There was a smash as the pole of a carriage crashed )
 
(into the cart that the
man on horseback stopped  )
 
(My brother looked up  and the man with the gold
twisted his head round and bit the wrist that held his collar )
 
( There was a
concussion )
 
( and the black horse came staggering sideways )
 
( and the carthorse
pushed beside it  )
 
(A hoof missed my brother s foot by a hair s breadth  )
 
(He
released his grip on the fallen man and jumped back  )
 
(He saw anger change to
terror on the face of the poor wretch on the ground  )
 
(and in a moment he was
hidden and my brother )
 
(was borne backward and carried past the entrance of
the lane )
 
( and had to fight hard in the torrent to recover it )
 
(

He saw Miss Elphinstone covering her eyes  and a little child  with all a
child s want of sympathetic imagination  )
 
(staring with dilated eyes at a
dusty something that lay black and still  )
 
(ground and crushed under the
rolling wheels   )
 
(Let us go back  )
 
( he shouted  and began turning the pony
round   )
 
(We cannot cross this hell   )
 
(he said and they went back a hundred
yards the way they had come  until the fighting crowd was hidden )
 
(As they
passed the bend in the lane )
 
(my brother saw the face of the dying man in the
ditch under the privet  )
 
(deadly white and drawn  and shining with
perspiration  )
 
(The two women sat silent  )
 
(crouching in their seat and
shivering )
 
(

Then beyond the bend my brother stopped again  )
 
(Miss Elphinstone was white
and pale  )
 
(and her sister-in-law sat weeping )
 
( too wretched even to call upon
 George  )
 
( My brother was horrified and perplexed )
 
( So soon as they had
retreated he realised how urgent )
 
(and unavoidable it was to attempt this
crossing  )
 
(He turned to Miss Elphinstone  suddenly resolute )
 
(

 We must go that way   )
 
(he said  and led the pony round again )
 
(

For the second time that day this girl proved her quality  )
 
(To force their
way into the torrent of people  )
 
(my brother plunged into the traffic and held
back a cab horse )
 
( while she drove the pony across its head  )
 
(A waggon locked
wheels for a moment )
 
(and ripped a long splinter from the chaise  In another
moment they were caught and swept forward by the stream  )
 
(My brother  with
the cabman s whip marks red across his face and hands  )
 
(scrambled into the
chaise and took the reins from her )
 
(

 Point the revolver at the man behind  )
 
( he said  giving it to her   if he
presses us too hard )
 
( No  point it at his horse  )
 
(

;;;;;

Then he began to look out for a chance of edging to the right across the
road )
 
( But once in the stream he seemed to lose volition  )
 
(to become a part of
that dusty rout  )
 
(They swept through Chipping Barnet with the torrent  )
 
(they
were nearly a mile beyond the centre of the town )
 
(before they had fought
across to the opposite side of the way  )
 
(It was din and confusion
indescribable )
 
( but in and beyond the town the road forks repeatedly  and
this to some extent relieved the stress )
 
(

They struck eastward through Hadley  )
 
(and there on either side of the road 
and at another place )
 
(farther on they came upon a great multitude of people
drinking at the stream)
 
(  some fighting to come at the water  )
 
(And farther on 
from a lull near East Barnet )
 
( they saw two trains running slowly )
 
(one after
the other without signal or order trains swarming with people )
 
( with men
even among the coals behind the engines going northward along the Great
Northern Railway )
 
( My brother supposes they must have filled outside London )
 
(
for at that time the furious terror of the people had rendered the central
termini impossible )
 
(

Near this place they halted for the rest of the afternoon )
 
( for the violence
of the day had already utterly exhausted all three of them  )
 
(They began to
suffer the beginnings of hunger  )
 
(the night was cold  )
 
(and none of them dared
to sleep  )
 
(And in the evening many people came hurrying along the road nearby
their stopping place  )
 
(fleeing from unknown dangers before them  )
 
(and going in
the direction from which my brother had come )
 
(


Had the Martians aimed only at destruction  )
 
(they might on Monday have
annihilated the entire population of London  )
 
(as it spread itself slowly
through the home counties )
 
( Not only along the road through Barnet  but also
through Edgware and Waltham Abbey  )
 
(and along the roads eastward to Southend
and Shoeburyness  )
 
(and south of the Thames to Deal and Broadstairs  poured
the same frantic rout )
 
( If one could have hung that June morning in a balloon
in the blazing blue above London )
 
(every northward and eastward road running
out of the tangled maze of streets would have seemed stippled black with the
streaming fugitives  )
 
(each dot a human agony of terror and physical distress 
)
 
(I have set forth at length in the last chapter my brother s account of the
road through Chipping Barnet )
 
( in order that my readers may realise how that
swarming of black dots appeared to one of those concerned  )
 
(Never before in
the history of the world had such a mass of human beings moved and suffered
together )
 
( The legendary hosts of Goths and Huns  )
 
(the hugest armies Asia has
ever seen )
 
( would have been but a drop in that current  )
 
(And this was no
disciplined march )
 
( it was a stampede a stampede gigantic and
terrible without order and without a goal )
 
( six million people unarmed and
unprovisioned  )
 
(driving headlong  )
 
(It was the beginning of the rout of
civilisation  )
 
(of the massacre of mankind )
 
(

Directly below him the balloonist would have seen the network of streets )
 
(far
and wide  houses  churches  squares  crescents  gardens already
derelict spread out like a huge map  )
 
(and in the southward blotted  Over
Ealing  Richmond  Wimbledon )
 
( it would have seemed as if some monstrous pen
had flung ink upon the chart  )
 
(Steadily  incessantly  each black splash grew
and spread  shooting out ramifications this way and that  )
 
(now banking itself
against rising ground  )
 
(now pouring swiftly over a crest into a new-found
valley  exactly as a gout of ink )
 
(would spread itself upon blotting paper )
 
(

And beyond  over the blue hills that rise southward of the river  )
 
(the
glittering Martians went to and fro  calmly and methodically spreading their
poison cloud )
 
(over this patch of country and then over that )
 
( laying it again
with their steam jets when it had served its purpose  )
 
(and taking possession
of the conquered country  )
 
(They do not seem to have aimed at extermination )
 
(so
much as at complete demoralisation and the destruction of any opposition )
 
(
They exploded any stores of powder they came upon )
 
( cut every telegraph  and
wrecked the railways here and there  )
 
(They were hamstringing mankind  They
seemed in no hurry to extend the field of their operations )
 
( and did not come
beyond the central part of London all that day  )
 
(It is possible that a very
considerable number of people in London stuck to their houses through Monday
morning  )
 
(Certain it is that many died at home suffocated by the Black Smoke )
 
(

Until about midday the Pool of London )
 
(was an astonishing scene  )
 
(Steamboats
and shipping of all sorts lay there  )
 
(tempted by the enormous sums of money
offered by fugitives  )
 
(and it is said that many who swam out to these vessels)
 
(
were thrust off with boathooks and drowned  )
 
(About one o clock in the
afternoon the thinning remnant of a cloud )
 
(of the black vapour appeared
between the arches of Blackfriars Bridge  )
 
(At that the Pool became a scene of
mad confusion  )
 
(fighting  and collision  and for some time a multitude of
boats and barges jammed in the northern arch of the Tower Bridge )
 
( and the
sailors and lightermen had to fight savagely )
 
(against the people who swarmed
upon them from the riverfront )
 
( People were actually clambering down the
piers of the bridge from above )
 
(

When  an hour later )
 
( a Martian appeared beyond the Clock Tower and waded
down the river )
 
( nothing but wreckage floated above Limehouse )
 
(

Of the falling of the fifth cylinder I have presently to tell  )
 
(The sixth
star fell at Wimbledon  )
 
(My brother  keeping watch beside the women in the
chaise in a meadow )
 
( saw the green flash of it far beyond the hills )
 
( On
Tuesday the little party  still set upon getting across the sea )
 
( made its
way through the swarming country towards Colchester  )
 
(The news that the
Martians were now in possession of the whole of London was confirmed  )
 
(They
had been seen at Highgate  )
 
(and even  it was said  at Neasden )
 
( But they did
not come into my brother s view until the morrow )
 
(

That day the scattered multitudes began to realise the urgent need of
provisions )
 
( As they grew hungry the rights of property ceased to be
regarded  )
 
(Farmers were out to defend their cattle-sheds  granaries  and
ripening root crops with arms in their hands )
 
( A number of people now  )
 
(like
my brother  had their faces eastward )
 
( and there were some desperate souls
even going back towards London to get food  )
 
(These were chiefly people from
the northern suburbs  )
 
(whose knowledge of the Black Smoke came by hearsay  He
heard that about half the members of the government had gathered at
Birmingham  )
 
(and that enormous quantities of high explosives )
 
(were being
prepared to be used in automatic mines across the Midland counties )
 
(

He was also told that the Midland Railway Company )
 
(had replaced the
desertions of the first day s panic  )
 
(had resumed traffic  )
 
(and was running
northward trains from St  Albans )
 
(to relieve the congestion of the home
counties )
 
( There was also a placard in Chipping Ongar )
 
(announcing that large
stores of flour)
 
( were available in the northern towns and that within
twenty-four hours bread )
 
(would be distributed among the starving people in
the neighbourhood  )
 
(But this intelligence did not deter him from the plan of
escape he had formed  )
 
(and the three pressed eastward all day  and heard no
more of the bread distribution than this promise  )
 
(Nor  as a matter of fact 
did anyone else hear more of it  )
 
(That night fell the seventh star  falling
upon Primrose Hill  )
 
(It fell while Miss Elphinstone was watching  for she
took that duty alternately with my brother  She saw it )
 
(

On Wednesday the three fugitives they had passed the night in a field of
unripe wheat reached Chelmsford  )
 
(and there a body of the inhabitants 
calling itself the Committee of Public Supply )
 
( seized the pony as
provisions )
 
( and would give nothing in exchange for it but the promise of a
share in it the next day )
 
( Here there were rumours of Martians at Epping  and
news of the destruction of Waltham Abbey)
 
( Powder Mills in a vain attempt to
blow up one of the invaders )
 
(

People were watching for Martians here from the church towers  )
 
(My brother 
very luckily for him as it chanced  preferred )
 
(to push on at once to the
coast rather than wait for food  )
 
(although all three of them were very
hungry  )
 
(By midday they passed through Tillingham  )
 
(which  strangely enough 
seemed to be quit)
 
(e silent and deserted  save for a few furtive plunderers
hunting for food  Near Tillingham )
 
(they suddenly came in sight of the sea 
and the most amazing crowd of shipping of all sorts )
 
(that it is possible to
imagine )
 
(

For after the sailors could no longer come up the Thames  )
 
(they came on to
the Essex coast  )
 
(to Harwich and Walton and Clacton )
 
( and afterwards to
Foulness and Shoebury  to bring off the people )
 
( They lay in a huge
sickle-shaped curve that vanished into mist at last towards the Naze  )
 
(Close
inshore was a multitude of fishing smacks English  )
 
(Scotch  French  Dutch 
and Swedish  )
 
(steam launches from the Thames )
 
( yachts  electric boats  and
beyond were ships of large burden)
 
( a multitude of filthy colliers  trim
merchantmen  cattle ships )
 
( passenger boats  petroleum tanks  )
 
(ocean tramps 
an old white transport even )
 
( neat white and grey liners from Southampton and
Hamburg  )
 
(and along the blue coast across the Blackwater my brother )
 
(could
make out dimly a dense swarm of boats )
 
(chaffering with the people on the
beach  a swarm )
 
(which also extended up the Blackwater almost to Maldon )
 
(

About a couple of miles out lay an ironclad )
 
( very low in the water  almost 
to my brother s perception)
 
(  like a waterlogged ship )
 
( This was the ram
Thunder Child )
 
( It was the only warship in sight )
 
(but far away to the right
)
 
(over the smooth surface of the sea )
 
(for that day there was a dead calm lay
a serpent of black smoke )
 
(to mark the next ironclads of the Channel Fleet )
 
(
which hovered in an extended line  )
 
(steam up and ready for action  across the
Thames estuary )
 
(during the course of the Martian conquest  )
 
(vigilant and yet
powerless to prevent it )
 
(

At the sight of the sea )
 
( Mrs  Elphinstone  in spite of the assurances of her
sister-in-law  gave way to panic )
 
(She had never been out of England before 
she would rather die than trust herself friendless in a foreign country )
 
( She seemed  poor woman )
 
( to imagine that the French and the
Martians might prove very similar  )
 
(She had been growing increasingly
hysterical  fearful  )
 
(and depressed during the two days  journeyings  )
 
(Her
great idea was to return to Stanmore )
 
( Things had been always well and safe
at Stanmore )
 
( They would find George at Stanmore )
 
(

It was with the greatest difficulty they could get her down to the beach )
 
(
where presently my brother succeeded in attracting the attention of some men
on a paddle steamer from the Thames  )
 
(They sent a boat and drove a bargain
for thirty-six pounds for the three )
 
( The steamer was going  these men said 
to Ostend )
 
(

It was about two o clock when my brother )
 
( having paid their fares at the
gangway  )
 
(found himself safely aboard the steamboat with his charges  )
 
(There
was food aboard  albeit at exorbitant prices )
 
( and the three of them
contrived to eat a meal on one of the seats forward )
 
(

There were already a couple of score of passengers aboard )
 
( some of whom had
expended their last money in securing a passage )
 
( but the captain lay off the
Blackwater until five in the afternoon )
 
( picking up passengers until the
seated decks were even dangerously crowded  )
 
(He would probably have remained
longer )
 
(had it not been for the sound of guns that began about that hour in
the south )
 
( As if in answer  the ironclad seaward fired a small gun )
 
(and
hoisted a string of flags  A jet of smoke sprang out of her funnels )
 
(

Some of the passengers were of opinion that this firing came from
Shoeburyness  )
 
(until it was noticed that it was growing louder  )
 
(At the same
time  far away in the southeast the masts )
 
(and upperworks of three ironclads
rose one after the other out of the sea )
 
( beneath clouds of black smoke  )
 
(But
my brother s attention speedily reverted to the distant firing in the south )
 
(
He fancied he saw a column of smoke rising out of the distant grey haze )
 
(

The little steamer was already flapping her way eastward of the big crescent
of shipping )
 
( and the low Essex coast was growing blue and hazy  )
 
(when a
Martian appeared  )
 
(small and faint in the remote distance  )
 
(advancing along
the muddy coast from the direction of Foulness )
 
( At that the captain on the
bridge swore )
 
(at the top of his voice with fear and anger at his own delay 
and the paddles seemed infected with his terror  )
 
(Every soul aboard stood at
the bulwarks )
 
(or on the seats of the steamer and stared at that distant
shape  )
 
(higher than the trees or church towers inland )
 
( and advancing with a
leisurely parody of a human stride )
 
(

It was the first Martian my brother had seen  )
 
(and he stood  more amazed than
terrified )
 
( watching this Titan advancing deliberately )
 
(towards the shipping 
wading farther and farther into the water )
 
(as the coast fell away  )
 
(Then  far
away beyond the Crouch )
 
( came another  striding over some stunted trees  and
then yet another  )
 
(still farther off  )
 
(wading deeply through a shiny mudflat
that seemed to hang halfway up between sea and sky  )
 
(They were all stalking
seaward )
 
( as if to intercept the escape of the multitudinous vessels that
were crowded between Foulness and the Naze )
 
( In spite of the throbbing
exertions of the engines of the little paddleboat  )
 
(and the pouring foam that
her wheels )
 
(flung behind her )
 
( she receded with terrifying slowness from this
ominous advance )
 
(

;;;;


Glancing northwestward )
 
( my brother saw the large crescent of shipping)
 
(
already writhing with the approaching terror )
 
( one ship passing behind
another  another coming round from broadside to end on )
 
( steamships whistling
and giving off volumes of steam  )
 
(sails being let out  launches rushing
hither and thither  )
 
(He was so fascinated by this )
 
(and by the creeping danger
away to the left that he had no eyes for anything seaward  )
 
(And then a swift
movement of the steamboat )
 
(she had suddenly come round to avoid being run
down)
 
( flung him headlong from the seat upon which he was standing  There was
a shouting all about him  )
 
(a trampling of feet  and a cheer that seemed to be
answered faintly  )
 
(The steamboat lurched and rolled him over upon his hands )
 
(

He sprang to his feet and saw to starboard )
 
( and not a hundred yards from
their heeling  pitching boat )
 
( a vast iron bulk like the blade of a plough
tearing through the water )
 
( tossing it on either side in huge waves of foam
that leaped towards the steamer  )
 
(flinging her paddles helplessly in the air )
 
(
and then sucking her deck down almost to the waterline )
 
(

A douche of spray blinded my brother for a moment  )
 
(When his eyes were clear
again )
 
(he saw the monster had passed and was rushing landward )
 
( Big iron
upperworks rose out of this headlong structure )
 
( and from that twin funnels
projected and spat a smoking blast shot with fire  )
 
(It was the torpedo ram 
Thunder Child )
 
( steaming headlong  coming to the rescue of the threatened
shipping )
 
(

Keeping his footing on the heaving deck by clutching the bulwarks )
 
( my
brother looked past this charging leviathan at the Martians again  )
 
(and he
saw the three of them now close together )
 
( and standing so far out to sea
that their tripod supports were almost entirely submerged )
 
( Thus sunken  and
seen in remote perspective  )
 
(they appeared far less formidable than the huge
iron bulk in whose wake the steamer was pitching so helplessly  )
 
(It would
seem they were regarding this new antagonist with astonishment  )
 
(To their
intelligence  )
 
(it may be  the giant was even such another as themselves  )
 
(The
Thunder Child fired no gun )
 
( but simply drove full speed towards them  )
 
(It was
probably her not firing that enabled her to get so near the enemy as she
did  )
 
(They did not know what to make of her )
 
( One shell  and they would have
sent her to the bottom forthwith with the Heat-Ray 

She was steaming at such a pace )
 
(that in a minute she seemed halfway between
the steamboat )
 
(and the Martians a diminishing black bulk against the
receding horizontal expanse of the Essex coast )
 
(

Suddenly the foremost Martian lowered his tube and discharged a canister of
the black gas at the ironclad )
 
( It hit her larboard side and glanced off in
an inky jet that rolled away to seaward )
 
( an unfolding torrent of Black
Smoke )
 
( from which the ironclad drove clear )
 
( To the watchers from the
steamer  low in the water and with the sun in their eyes )
 
( it seemed as
though she were already among the Martians )
 
(

They saw the gaunt figures separating)
 
( and rising out of the water as they
retreated shoreward )
 
( and one of them raised the camera-like generator of the
Heat-Ray  )
 
(He held it pointing obliquely downward )
 
( and a bank of steam sprang
from the water at its touch )
 
( It must have driven through the iron of the
ship s side )
 
(like a white-hot iron rod through paper )
 
(

A flicker of flame went up through the rising steam  )
 
(and then the Martian
reeled and staggered  )
 
(In another moment he was cut down  )
 
(and a great body of
water and steam shot high in the air )
 
( The guns of the Thunder Child sounded
through the reek)
 
(  going off one after the other )
 
( and one shot splashed the
water high close by the steamer  )
 
(ricocheted towards the other flying ships
to the north )
 
( and smashed a smack to matchwood )
 
(

But no one heeded that very much  )
 
(At the sight of the Martian s collapse the
captain on the bridge yelled inarticulately )
 
( and all the crowding passengers
on the steamer s stern shouted together )
 
( And then they yelled again  For 
surging out beyond the white tumult  )
 
(drove something long and black )
 
( the
flames streaming from its middle parts )
 
( its ventilators and funnels spouting
fire )
 
(

She was alive still  the steering gear )
 
( it seems  was intact and her engines
working )
 
( She headed straight for a second Martian )
 
( and was within a hundred
yards of him when the Heat-Ray came to bear  )
 
(Then with a violent thud  a
blinding flash )
 
( her decks  her funnels  leaped upward  )
 
(The Martian staggered
with the violence of her explosion )
 
( and in another moment the flaming
wreckage )
 
( still driving forward with the impetus of its pace  )
 
(had struck him
and crumpled him up )
 
(like a thing of cardboard )
 
( My brother shouted
involuntarily )
 
( A boiling tumult of steam hid everything again )
 
(

 Two    yelled the captain )
 
(

Everyone was shouting  )
 
(The whole steamer from end to end rang with frantic
cheering )
 
(that was taken up first by one )
 
(and then by all in the crowding
multitude of ships and boats that was driving out to sea )
 
(

The steam hung upon the water for many minutes  )
 
(hiding the third Martian and
the coast altogether  )
 
(And all this time the boat was paddling steadily out
to sea and away from the fight )
 
( and when at last the confusion cleared  the
drifting bank of black vapour intervened  )
 
(and nothing of the Thunder Child
could be made out )
 
( nor could the third Martian be seen)
 
(  But the ironclads to
seaward were now quite close )
 
(and standing in towards shore past the
steamboat )
 
(

The little vessel continued to beat its way seaward  )
 
(and the ironclads
receded slowly towards the coast )
 
( which was hidden still by a marbled bank
of vapour  part steam  )
 
(part black gas  eddying and combining in the
strangest way )
 
(The fleet of refugees was scattering to the northeast 
several smacks were sailing )
 
(between the ironclads and the steamboat  )
 
(After a
time  and before they reached the sinking cloud bank)
 
(  the warships turned
northward )
 
( and then abruptly went about and passed into the thickening haze
of evening southward )
 
( The coast grew faint  )
 
(and at last indistinguishable
amid the low banks of clouds that were gathering )
 
(about the sinking sun )
 
(

Then suddenly out of the golden haze of the sunset )
 
(came the vibration of
guns )
 
( and a form of black shadows moving )
 
( Everyone struggled to the rail of
the steamer and peered into the blinding furnace of the west )
 
( but nothing
was to be distinguished clearly  )
 
(A mass of smoke rose slanting and barred
the face of the sun  )
 
(The steamboat throbbed on its way through an
interminable suspense )
 
(

The sun sank into grey clouds  )
 
(the sky flushed and darkened  )
 
(the evening
star trembled into sight  )
 
(It was deep twilight when the captain cried out
and pointed  )
 
(My brother strained his eyes  )
 
(Something rushed up into the sky)
 
(
out of the greyness rushed slantingly upward )
 
(and very swiftly into the
luminous clearness above the clouds in the western sky )
 
( something flat and
broad )
 
( and very large  that swept round in a vast curve  grew smaller  )
 
(sank
slowly  and vanished again into the grey mystery of the night )
 
( And as it
flew it rained down darkness upon the land )))

