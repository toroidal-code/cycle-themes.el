# cycle-themes.el
Because switching between your themes shouldn't be so damn hard

How it works
------------

Define a list of themes you want to be able to switch between. This should ideally happen before `require`ing cycle-themes (see Hooks section). Then, enable the global minor mode.

```elisp
(setq cycle-themes-theme-list
      '(leuven monokai solarized-dark))

(require 'cycle-themes)
(cycle-themes-mode)

```

That's it! Now, you can switch between your themes by calling `M-x cycle-themes`, or with the (easily re-definable) binding of `C-c C-t`. That's it!

If you use [use-package](https://github.com/jwiegley/use-package), it'll look something like this:

```elisp
(use-package cycle-themes
  :ensure t
  :init (setq cycle-themes-theme-list
          '(leuven monokai solarized-dark))
  :config (cycle-themes-mode))
```
  
Hooks
-----

Additionally, there is a custom variable for attaching your own hooks to be run after switching themes. For example, I don't like having my fringes be a different color than the background, so I simply use this:

```elisp
(add-hook 'cycle-themes-after-cycle-hook
          #'(lambda ()
              (dolist (frame (frame-list))
                (set-face-attribute 'fringe frame :background (face-background 'default)))))
```
If you set the theme list and add all of your hooks before calling `cycle-themes-mode`, it will automatically enable the first theme in the list and run all of your hooks.

Thanks
------

Originally this idea came from a [stackoverflow answer](http://stackoverflow.com/a/18796138). I thought that snippet was great, but thought it really deserved to be made into a true package. So I did.
