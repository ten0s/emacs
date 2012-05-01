emacs
=====

Emacs configuration that sets up Erlang environment: erlang-mode, flymake, wrangler.

Currently the flymake's configuration is already quite helpful as it handles not only erlang source files (.erl), but also supports erlang header files (.hrl) and erlang terms (.config, ...). I hope this repo will act as a starting poing for kind of `erlang-flymake2' mode. :).

The syntax check scripts in the bin folder are generic and don't depend on Emacs in any way, so Vim erlang users might also find these scripts helpful and use in tools like
https://github.com/scrooloose/syntastic.
