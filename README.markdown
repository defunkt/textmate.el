TextMate Minor Mode
===================

    ;; This minor mode exists to mimick TextMate's awesome
    ;; features. 
    
    ;;    ⌘T - Go to File
    ;;  ⇧⌘T - Go to Symbol
    ;;    ⌘L - Go to Line
    ;;  ⇧⌘L - Select Line (or expand Selection to select lines)
    ;;    ⌘/ - Comment Line (or Selection/Region)
    ;;    ⌘] - Shift Right (currently indents region)
    ;;    ⌘[ - Shift Left  (not yet implemented)
    ;;  ⌥⌘] - Align Assignments
    ;;  ⌥⌘[ - Indent Line
    ;;    ⌥↑ - Column Up
    ;;    ⌥↓ - Column Down
    ;;  ⌘RET - Insert Newline at Line's End
    ;;  ⌥⌘T - Reset File Cache (for Go to File)
    
    ;; A "project" in textmate-mode is determined by the presence of
    ;; a .git directory, an .hg directory, a Rakefile, or a Makefile.

    ;; You can configure what makes a project root by appending a file
    ;; or directory name onto the `*textmate-project-roots*' list.

    ;; If no project root indicator is found in your current directory,
    ;; textmate-mode will traverse upwards until one (or none) is found.
    ;; The directory housing the project root indicator (e.g. a .git or .hg
    ;; directory) is presumed to be the project's root.

    ;; In other words, calling Go to File from
    ;; ~/Projects/fieldrunners/app/views/towers/show.html.erb will use
    ;; ~/Projects/fieldrunners/ as the root if ~/Projects/fieldrunners/.git
    ;; exists.

Installation
============

    $ cd ~/.emacs.d/vendor
    $ git clone git://github.com/defunkt/textmate.el.git

In your emacs config:

    (add-to-list 'load-path "~/.emacs.d/vendor/textmate.el")
    (require 'textmate)
    (textmate-mode)

Rave Reviews
============

![DaysAreMade](http://img.skitch.com/20081125-mgwafnkj3cku5dwqukqns57eus.png)
![SoFarSoAwesome](http://img.skitch.com/20081125-m2snw6s36eh7aifc3dh6acxk72.png)
![ReallyGreat](http://img.skitch.com/20081126-pk4qt8itb1482y7kg963af2aj1.png)
![MadeMyWeek](http://img.skitch.com/20081128-k6gxswy6hxm5y3airagt8t8d1d.png)
![ItRules](http://img.skitch.com/20081203-bbeenfrq885wy6xkxapirgujk3.png)
![Baberaham](http://img.skitch.com/20081203-gi3w2adibmadrjtxqe6txw95eb.png)
![TextMade](http://img.skitch.com/20081203-cm6j9n9drcdkhx4sr36b287jbt.png)
![KeepOnTruckin](http://img.skitch.com/20081210-uby8x9uqfrsccy5jm96rhnkkt.png)
![DontMissIt](http://img.skitch.com/20081127-g77d1iy86nu1wdi7y8amhd6ixy.png)

Author
======

    Chris Wanstrath // chris@ozmm.org
