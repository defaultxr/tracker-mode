tracker.el
==========

a music tracker/sequencer for emacs

inspired by Chun Lee's etracker ( http://www.youtube.com/watch?v=9YOigs1lYRY ) whose source i was unable to find.

tracker.el is a "quick & dirty" sequencer for emacs modelled after traditional trackers, but allowing for much more programmability in the sequence by using emacs lisp to notate events. Because of this, any emacs lisp command can be used to generate a sequence or perform an action, or possibly even edit another part of the sequence! Emacs itself only hosts the tracker; you will need another program (i plan to use SuperCollider) in order to actually generate sound.

Right now, the tracker reads each step and evaluates it immediately. This means that if you make an error while writing out the step, tracker.el simply marks the step as having an error, but otherwise ignores it and progresses through the sequence as if the step was empty. In the future, i plan to add features so that modifications to a step aren't read until they are confirmed with a key combination (likely to be C-c C-c).

You can use C-c C-s to start/stop the tracker, C-c i to insert a new pattern, and C-c C-n or C-c C-p to view the next or previous patterns, respectively. M-up and M-down will increase or decrease the number under the point.

This is the first major bit of emacs code i've written so expect some (many) things to be awful. I'm aware that emacs is not the best environment to write a tracker in since long calculations could cause delays in the sequence but i've wanted to write something like this for a long time. I'm ok with having less-than-exact timing if it means i will be able to compose sequences out of arbitrary code. At the moment I can only recommend this tracker if you feel the same way.

Included with tracker.el are a few SuperCollider "convenience functions" i've made that make controlling SC via the `sclang-eval-string' function easier. The main one right now is the `synth' function, which plays a synth, sending the provided arguments to it. I have plans to add more convenience functions like this, and of course the whole _point_ of tracker.el is that you can make your own to suit your needs.

I'm very open to suggestions for ways to improve this, especially if those improvements bring it more in-line with the "standard" elisp way to do them.

Notes
=====

The current state of tracker.el is that it is usable but has a few rough edges. There are a few minor bugs that cause it to stop playing. For instance, the sequence will stop playing if it cannot find the next step. If you accidentally hold backspace for too long and delete text that you weren't supposed to, the tracker will stop. In the future i plan to add more safeguards against stuff like this, but for now, you will just have to be careful.

Originally i was hoping to make this into a tracker that would send OSC messages but i can't get osc.el to work, even after contacting the author. Using osc.el could possibly fix (or lessen) the timing issues, although i'm not sure how much it would help. When the tracker "plays" a step, all the code in it is executed "as soon as possible". In other words, i haven't added any code to make sure the actual sound events occur at their "proper" time. This is the cause of the aforementioned "less-than-exact" timing.

TODO
====

* use C-c C-c to confirm the edits to a step
* get the tab key to complete
* detect commented steps (don't treat them as "errors")
* tracker-comment-step (C-') - should also be able to comment out only a specific line of the step (auto-detect the "correct" thing to do)
* "tracker repeat song" function (shows up as "R" in the status field); loops song after last pattern; t by default
* "tracker follow" function (shows up as "F" in the status field); automatically switches currently viewed pattern when the tracker is playing
* fix syntax coloring (should inherit from emacs-lisp-mode)
* M-n and M-p to go through fields
* make clone- & delete-pattern functions
* make pattern resizing function
* when switching patterns, keep the point on the same step (or the closest one to it)
* update status line (only the BPM field should be editable by the user)
* user should not be able to delete step numbers or "Pattern 0:" in the patterns
* make sure that if tracker-mode is started from a file that is already a tracker song, it parses the file correctly
* color each 4th line's numbers (000, 004, 008, 012, 016, etc)
* "init" code: elisp that is run whenever the tracker starts to initialize variables/"loads" a song (after header but before patterns)
* make it possible to "mix" tracker songs (i.e. make tracker-mode able to be invoked upon multiple buffers simultaneously)
* fix the overlay (don't use timers to delete it; just update it in tracker-loop)
* add more SuperCollider convenience functions
 * `retrig' - re-executes the code a certain number of times with a delay.
* add functions to make modifying other parts of the sequence easier
* convenience macro that is applied to the elisp from each step before it is evaluated
 * vars prefixed with % are interpreted as pattern- and/or step-local vars.
