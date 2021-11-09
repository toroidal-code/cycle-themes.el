cycle-themes.el
===============

Because switching between your themes shouldn't be so damn hard

How it works
------------

Define a list of themes you want to be able to switch between. Then, enable the
global minor mode. If you don't list any themes, we default to switching between
all the ones emacs knows about.

```elisp
(setq cycle-themes-theme-list
      '(leuven monokai solarized-dark))

(require 'cycle-themes)
(cycle-themes-mode)

```

That's it! Now, you can switch between your themes by calling `M-x
cycle-themes`, or with the (easily re-definable) binding of `C-c C-t`.

If you use [use-package](https://github.com/jwiegley/use-package), it'll look
something like this:

```elisp
(use-package cycle-themes
  :ensure t
  :init (setq cycle-themes-theme-list
          '(leuven monokai solarized-dark))
  :config (cycle-themes-mode))
```

Hooks
-----

Additionally, there is a custom variable for attaching your own hooks to be run
after switching themes. For example, I don't like having my fringes be a
different color than the background, so I simply use this:

```elisp
(add-hook 'cycle-themes-after-cycle-hook
          #'(lambda ()
              (dolist (frame (frame-list))
                (set-face-attribute 'fringe frame
                   :background (face-background 'default)))))
```
If you add any hooks and call `cycle-themes-mode` anywhere in your init file, it will automatically enable the first theme in the list and run all of your hooks.

Caveats
-------

If you find that it's skipping themes in your list, make sure that all of your
themes are installed and loaded properly. This is especially important for users
of `use-package`, as it will `require` the theme, but not actually `load` it
into emacs' list of valid themes (which makes `(custom-theme-p my-theme)` return
`nil`).

```elisp
use-package solarized-theme
:ensure t
```

Thanks
------

This package was inspired by a [stackoverflow
answer](http://stackoverflow.com/a/18796138). I thought that snippet was great,
and that it really deserved to be made into a true package. So I did.
