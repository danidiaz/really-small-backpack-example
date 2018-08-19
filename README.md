# really-small-backpack-example

This is a small tutorial on the very basics of the Backpack module system.

It requires [cabal-install 2.2](https://www.haskell.org/cabal/download.html). 

It is a [multi-package project](https://www.haskell.org/cabal/users-guide/nix-local-build.html#developing-multiple-packages). You can build all the packages from this folder using:

```
cabal new-build all
```

# Other resources

Edward Z. Yang's [thesis](https://github.com/ezyang/thesis/releases) is quite
readable and gives a good account of the motivations for Backpack.

Edward Z. Yang's [blog](http://blog.ezyang.com/category/haskell/backpack/).

The [Backpack](https://wiki.haskell.org/Backpack) and [Module
Signature](https://wiki.haskell.org/Module_signature) entries in the
Haskellwiki.

[Example of an abstract package on Hackage which uses
signatures](http://hackage.haskell.org/package/unpacked-containers). Discussed
[here](https://www.reddit.com/r/haskell/comments/8a5w1n/new_package_unpackedcontainers/).

[A signature for streaming libraries](https://github.com/danidiaz/streamy)

I wrote a few Backpack tips & tricks
[here](https://medium.com/@danidiaz/backpacking-tips-3adb727bb8f7) and
[here](https://medium.com/@danidiaz/backpacking-tips-ii-47fa86e5bf2).

[The Cabal user
guide](https://www.haskell.org/cabal/users-guide/nix-local-build-overview.html).
At the moment, the 'signatures' and 'mixins' sections doesn't seem to be
documented there.

[picnic: put containers into a backpack](https://kowainik.github.io/posts/2018-08-19-picnic-put-containers-into-a-backpack) [reddit](https://www.reddit.com/r/haskell/comments/98jegn/blog_post_picnic_put_containers_into_a_backpack/) Features an interesting example of abstracting over classes.

