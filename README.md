# dnf-repo

A wrapper of the dnf package manager for fine control of
enabled/disabled yum repos (eg coprs).

DNF is not terribly fast at handling repos partly because it likes
to refresh the cached repodata frequently. So it can be advantageous
to disable smaller repos by default and only enable them as needed
(eg periodically).

This tool can temporarily enable/disable repos selected by substring.
Repo states can also be saved or their cache individually expired.

There are options to enable/disable testing/modular repos,
and also to create a repo file for a Copr or Koji repo.

## Help

```shellsession
$ dnf-repo --version
0.5
$ dnf-repo --help
DNF wrapper repo tool

Usage: dnf-repo [--version] [-n|--dryrun] [-D|--debug] [-l|--list] [--exact]
                [-s|--save] [(-w|--weak-deps) | (-W|--no-weak-deps)]
                [
                  [(-c|--add-copr COPR) | (-k|--add-koji REPO) |
                    (-d|--disable REPOPAT) | (-e|--enable REPOPAT) |
                    (-x|--expire REPOPAT) | (-X|--clear-expires) |
                    (-E|--delete-repofile REPOPAT) | (-t|--enable-testing) |
                    (-T|--disable-testing) | (-m|--enable-modular) |
                    (-M|--disable-modular) | --enable-debuginfo |
                    --disable-debuginfo | --enable-source | --disable-source] |
                  --enable-defaults | --disable-defaults] [DNFARGS]
  see https://github.com/juhp/dnf-repo#readme

Available options:
  -h,--help                Show this help text
  --version                Show version
  -n,--dryrun              Dry run
  -D,--debug               Debug output
  -l,--list                List all repos
  --exact                  Match repo names exactly
  -s,--save                Save the repo enable/disable state
  -w,--weak-deps           Use weak dependencies
  -W,--no-weak-deps        Disable weak dependencies
  -c,--add-copr COPR       Create repo file for copr repo
  -k,--add-koji REPO       Create repo file for koji repo (f37-build, rawhide,
                           epel9-build, etc)
  -d,--disable REPOPAT     Disable repos
  -e,--enable REPOPAT      Enable repos
  -x,--expire REPOPAT      Expire repo cache
  -X,--clear-expires       Undo cache expirations
  -E,--delete-repofile REPOPAT
                           Remove unwanted .repo file
  -t,--enable-testing      Enable testing repos
  -T,--disable-testing     Disable testing repos
  -m,--enable-modular      Enable modular repos
  -M,--disable-modular     Disable modular repos
  --enable-debuginfo       Enable debuginfo repos
  --disable-debuginfo      Disable debuginfo repos
  --enable-source          Enable source repos
  --disable-source         Disable source repos
  --enable-defaults        Enable modular and Cisco h264 repos
  --disable-defaults       Disable modular and Cisco h264 repos
```

## Usage examples
List repos:
```shellsession
$ dnf-repo
```

Update with testing repos:
```shellsession
$ dnf-repo -t update
```

Note that sudo is used implicitly when needed:
there is no need to run dnf-repo with sudo.

### Copr
List disabled copr repos (actually shows "copr" repos that would be enabled):
```shellsession
$ dnf-repo -e 'copr*'
```

Disable all copr repos for update:
```shellsession
$ dnf-repo -d 'copr*' update
```

Install a package directly from a new copr:
```shellsession
$ dnf-repo -c varlad/helix install helix
```
(note the copr repo is not permanently enabled).

Later update with the copr:
```shellsession
$ dnf-repo -e helix update
```

### Changing system repo config
Disable fedora modular and cisco h264 repos permanently:
```shellsession
$ dnf-repo --disable-defaults --save
```

To only disable modular you can use: `dnf-repo -M -s`.

### Switch system from rawhide
Switch a system from Rawhide to F37:
```shellsession
$ dnf-repo --exact -d rawhide -e fedora distrosync --releasever 37 fedora\*
```

### Repo patterns
By default repo patterns are matched as infix substrings
(unless you use `--exact`).

But you can also prepend `^`/append `$` (or both) to match a repo name
from its beginning/end (or exactly).

You can also use glob patterns to match repo names:
see https://hackage.haskell.org/package/Glob/docs/System-FilePath-Glob.html#v:compile for the supported syntax.

## Installation
A copr repo is available:
<https://copr.fedorainfracloud.org/coprs/petersen/dnf-repo/>

## Building
Use {cabal,stack,cabal-rpm} install.

## Contributing
The source repository is https://github.com/juhp/dnf-repo/

dnf-repo is currently distributed under a BSD license.

Contributions including suggestions for improvement are welcome.
