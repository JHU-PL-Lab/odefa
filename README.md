Sato
=====

_"I am so embarrassed. My name's Asami. Let me make this up to you somehow. Uh...how about I treat you to dinner?"_ - Asami Sato, _The Legend of Korra_

**Sato**, the **S**ymbolic **A**nalysis **T**ypechecker for **O**defa, dynamically locates type errors during demand-driven symbolic execution.

Install
-------

For Ubuntu install as follows.

```
sudo apt upgrade opam
opam update
opam switch create 4.09.0+flambda
```


Run this to produce libraries needed
```
# dune external-lib-deps --missing @@default
```

Here are the libraries needed:
```
opam install shexp core batteries gmap jhupllib monadlib ocaml-monadic pds-reachability ppx_deriving ppx_deriving_yojson -y
opam pin z3 4.8.1 -y
```

For Z3:
```
export LD_LIBRARY_PATH=`opam config var z3:lib`
```

Run
---

```
make
make test
make benchmark
```
