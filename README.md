
TIS Kernel is the open-source kernel of TIS Analyzer, based on a
modified version of Frama-C. It includes TIS Interpreter, an
interpreter for C programs that detects undefined behavior. TIS
Interpreter can be launched by invoking the provided `tis-interpreter`
shell script inside the directory [tis-interpreter](tis-interpreter).

A strict aliasing violation analysis is provided. It can be enabled by
adding the `-sa` option to an already working TIS Interpreter invocation.

INSTALLATION

To install, it is recommended to follow the instructions in the
[.travis.yml](.travis.yml) file. IF you already have some version of OCaml
installed, be aware that TIS Kernel is only guaranteed to compile with
one version. Typing the commands in the .travis.yml file as they are
will install a separate OCaml compiler of the right version in
$HOME/tis-kernel-opam so as not to pollute or depend on any other
OCaml installation.
