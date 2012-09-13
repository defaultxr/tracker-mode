tracker.el
==========

an osc music tracker/sequencer for emacs

inspired by Chun Lee's etracker ( http://www.youtube.com/watch?v=9YOigs1lYRY ) whose source i was unable to find.

my plan for this is to make a quick & dirty sequencer for emacs modelled after traditional trackers, but allowing for much more programmability in the sequence: each step of the tracker will be elisp code that is executed to perform side-effects and/or return OSC events. emacs itself will only host the tracker; you will need an OSC-compatible synthesizer (i plan to use SuperCollider) in order to actually generate sound.

currently multiple patterns are supported, although the tracker will only loop through the one it started on (doesn't automatically progress to the next like it should). i haven't started doing anything with OSC yet but i'll probably do that soon. in the future i will store valid elisp read from each step into an invisible part of the text so that if you edit a step and it makes it invalid, the tracker will automatically fall-back on the last-known valid elisp for that step.

you can use C-c s to start/stop the tracker, C-c i to insert a new pattern, and C-c n or C-c p to view the next or previous patterns, respectively. M-up and M-down will increase or decrease the number under the point.

this is the first major bit of emacs code i've written so expect some (many) things to be awful. i'm aware that emacs is not the best environment to write a tracker in since long calculations could cause delays in the sequence but i've wanted to write something like this for a long time. i'm ok with having less-than-exact timing if it means i will be able to compose sequences out of arbitrary computations.

i'm very open to suggestions for ways to improve the code or use more "standard" elisp practices.

right now tracker.el is usable and fairly bug-free but can't really be used to sequence music yet, as i haven't begun to add the OSC features. there are a lot of things i need to change before i'd recommend it to anybody.

TODO
====

* tracker-back-to-indent (C-a)
* detect commented steps (don't treat them as "errors")
* tracker-comment-step (C-') - should also be able to comment out only a specific line of the step (auto-detect the "correct" thing to do)
* automatically save "good" elisp for each step in invisible text beneath the "real" step
* make latching functional (progress to next pattern automatically when it's off)
* tracker-repeat-song variable (shows up as "R" in the status field); loops song after last pattern; t by default
* fix syntax coloring (should inherit from emacs-lisp-mode)
* M-n and M-p to go through fields
* change status line (only the BPM field should be editable by the user)
* user should not be able to delete step numbers or "Pattern 0:" in the patterns
* add OSC features
* make sure that if tracker-mode is started from a file that is already a tracker song, it parses the file correctly
* color each 4th line's numbers (000, 004, 008, 012, 016, etc)
* "init" code (after header but before patterns; elisp that is run whenever the tracker starts to initialize variables and all that jazz)
* make it possible to "mix" tracker songs (i.e. make tracker-mode able to be invoked upon multiple buffers simultaneously)
* "goto" keyboard shortcuts
 * C-c g b to go to BPM
 * C-c g s to go to a step
 * C-c g p to go to a pattern
 * C-c g t to go to the title
 * etc
* convenience macro that is applied to the elisp from each step before it is evaluated
 * vars prefixed with % are interpreted as pattern- and step-local vars.
 * functions to make OSC messaging easier
 * functions to make controlling SuperCollider easier