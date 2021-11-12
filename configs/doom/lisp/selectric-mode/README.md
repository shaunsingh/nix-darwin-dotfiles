# selectric-mode

![MELPA](https://melpa.org/packages/selectric-mode-badge.svg)
![MELPA Stable](https://stable.melpa.org/packages/selectric-mode-badge.svg)

![IBM Selectric](https://upload.wikimedia.org/wikipedia/commons/9/9f/IBM_Selectric.jpg)

*Photo By Oliver Kurmis - Self-photographed, CC BY 2.5, https://commons.wikimedia.org/w/index.php?curid=104015*

Make your Emacs sound like a proper typewriter. Extremely useful if you
have a puny, silent, rubberish, non-clicky keyboard.

The sound of the typewriter was recorded by a person nicknamed
"secretmojo" and is available on
https://www.freesound.org/people/secretmojo/sounds/224012/ and the bell
was extracted from https://freesound.org/people/knufds/sounds/345955/,
recorded by "knufds", both available under a Creative Commons license.

To install it, simply add it to your load-path, require it:

```elisp
(add-to-list 'load-path "~/.emacs.d/plugins/selectric-mode")
(require 'selectric-mode)
```

And then activate/deactivate with M-x `selectric-mode`. When it's
activated, you'll hear a typing sound for confirmation. When it
deactivates, you'll hear a carriage movement sound instead.

Alternatively, you can install it from [MELPA](https://melpa.org).To
install it, type <kbd>M-x package-install [ret] selectric-mode</kbd>

![selectric-mode on MELPA](https://raw.githubusercontent.com/wiki/rbanffy/selectric-mode/melpa.png)
