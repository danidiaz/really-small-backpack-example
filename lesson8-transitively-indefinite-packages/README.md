# lesson 8 - transitively indefinite packages

(This lesson was previously in a [separate repository](https://twitter.com/DiazCarrete/status/1379126087637143556).)

We call a package "indefinite" if it has an unfilled module signature. 

A package can be indefinite either because it directly imports a module signature, or because it has a dependency on another indefinite package (and doesn't fill the other package's signatures). In that case, the unfilled signatures are "carried over".





