emacs
=====

Emacs configuration that sets up Erlang environment: erlang-mode, flymake, wrangler.

Currently the flymake's configuration is already quite helpful as it handles not only erlang source files (.erl), but also supports erlang header files (.hrl) and erlang terms (.config, ...). I hope this repo will act as a starting poing for kind of `erlang-flymake2' mode. :).

Check sintax scripts in bin folder are generic and don't depend on Emacs in any way, so
Vim erlang users might also find those scripts helpful for them and use them in tools like
https://github.com/scrooloose/syntastic.
