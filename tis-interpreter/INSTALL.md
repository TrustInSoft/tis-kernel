This is tis-interpreter, an interpreter of C for detecting undefined behavior.

tis-interpreter depends on TrustInSoft Kernel and needs TrustInSoft
Kernel to be compiled and installed.

For the usage of tis-interpreter, it is recommended:

- to have with following dependencies:
  - version 4.03.0 of the OCaml compiler,
  - the Zarith library of OCaml bindings for GMP
  - findlib.
  As of this writing (April 2016), this means that the OCaml package
  from your Linux distribution is likely to be too old. Your best bet
  is Opam, which is likely to be available as a package in your
  distribution and can make the installation of recent OCaml and OCaml
  dependencies a breeze.

- to configure TrustInSoft Kernel with:
  ```
  INSTALL_PATH=~/tis-interpreter  # change the path at your ease

  pushd ..
  autoconf -f
  ./configure "--prefix=$INSTALL_PATH" --disable-from_analysis --disable-gui --disable-impact --disable-inout --disable-metrics --disable-occurrence --disable-pdg --disable-postdominators --enable-rtegen --disable-scope --disable-slicing --disable-sparecode --enable-users --disable-aorai --disable-obfuscator --disable-report --disable-security_slicing --disable-wp --disable-wp-coq --disable-wp-why3 --disable-print_api --with-all-static
  popd
  ```

- then to continue the compilation of TrustInSoft Kernel with
  ```
  pushd ..
  make depend
  make
  make install
  popd
  ```

- and finally to install tis-interpreter with
  ```
  make install  # in this directory
  ```

Once tis-interpreter installed, it can be used with:
```
$INSTALL_PATH/bin/tis-interpreter
```

Note that `$INSTALL_PATH` is the variable set during the TrustInSoft
Kernel configuration. If this step has been skipped, this variable
should be replace by the path to the TrustInSoft Kernel installation.

It is also recommended to modify the `PATH` environment variable:
```
export PATH=$INSTALL_PATH/bin:$PATH
```


If you are using Ubuntu trusty, then you can use the `.travis.yml`
file in this directory as an explicit, step-by-step installation guide
for all the necessary dependencies.
