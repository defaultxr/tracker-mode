tracker.el
==========

an osc music tracker/sequencer for emacs

inspired by Chun Lee's etracker ( http://www.youtube.com/watch?v=9YOigs1lYRY ) whose source i was unable to find.

my plan for this is to make a quick & dirty sequencer for emacs modelled after traditional trackers, but allowing for much more programmability in the sequence. each step of the tracker will be elisp code that is executed to perform side-effects or to return OSC events. emacs itself will only host the tracker; you will need an OSC-compatible synthesizer (i plan to use SuperCollider) in order to actually generate sound. there will be multiple patterns that are changable with keybindings in the tracker-mode, as well as other functions to make quick changes in the patterns easier (an example of this is the tracker-increase-number and tracker-decrease-number functions, bound to <M-up> and <M-down> respectively, which increase or decrease the number under the point). when a step cannot be read as valid elisp or returns an error upon execution, the step is marked as causing an error and the last known "good" elisp code for that step is reused.

this is the first major bit of emacs code i've written so expect some (many) things to be awful. i'm aware that emacs is not the best environment to write a tracker in since long calculations could cause delays in the sequence but i've wanted to write something like this for a long time and i'm ok with having less-than-precise timing.

i'm very open to suggestions for ways to improve the code or use more "standard" elisp practices.

right now tracker.el is far too immature for me to recommend it to anyone but hopefully i will have something more complete and useful soon.