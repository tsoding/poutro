[![Tsoding](https://img.shields.io/badge/twitch.tv-tsoding-purple?logo=twitch&style=for-the-badge)](https://www.twitch.tv/tsoding)
[![Build Status](https://travis-ci.org/tsoding/poutro.svg?branch=master)](https://travis-ci.org/tsoding/poutro)

# Poutro

## Quick Start

## NixOS

```console
$ nix-shell
$ cabal configure
$ cabal build
$ cabal exec hlint .
$ cabal run out/ patrons.csv aliases.json
$ ./render-video.sh
```

## Stack

Install [stack](https://docs.haskellstack.org/en/stable/README/) if you're not using [NixOS]

```console
$ stack build             # to build the project
$ stack exec hlint src/   # to check the code with HLint
$ stack exec poutro out/ patrons.csv aliases.json
$ ./render-video.sh
```

## Preview

Final video rendering is slow, so to preview a video open
`preview.html` in your favorite browser. The video will be replayed
and looped automatically.

## Support

You can support my work via

- Twitch channel: https://www.twitch.tv/subs/tsoding
- Patreon: https://www.patreon.com/tsoding

[tsoder]: http://github.com/tsoding/tsoder
[tsoding]: https://www.twitch.tv/tsoding
[NixOS]: https://nixos.org/
