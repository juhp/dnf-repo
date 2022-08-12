# dnf-repo

A wrapper of the  dnf package manager for fine control of enabled/disabled yum repos (eg coprs).

DNF is not terribly fast at handling repos partly because it likes
to refresh the cached repodata frequently. So it can be advantageous
to disable smaller repos by default and only enable them as needed.

This tool can temporarily enable/disable repos selected by substring.
Repo states can also be saved or their cache individually expired.

There are options to enable/disable testing/modular repos,
and also to create a repo file for a Copr or Koji repo.

## Help

```shellsession
$ dnf-repo --version
0.3
$ dnf-repo --help
DNF wrapper repo tool

Usage: dnf-repo [--version] [-n|--dryrun] [-D|--debug] [--exact] [-s|--save]
                [(-c|--add-copr COPR) | (-k|--add-koji REPO) |
                  (-d|--disable REPOPAT) | (-e|--enable REPOPAT) |
                  (-x|--expire REPOPAT) | (-E|--delete-repofile REPOPAT) |
                  (-t|--enable-testing) | (-T|--disable-testing) |
                  (-m|--enable-modular) | (-M|--disable-modular) |
                  --enable-debuginfo | --disable-debuginfo | --enable-source |
                  --disable-source] [DNFARGS]
  see https://github.com/juhp/dnf-repo#readme

Available options:
  -h,--help                Show this help text
  --version                Show version
  -n,--dryrun              Dry run
  -D,--debug               Debug output
  --exact                  Match repo names exactly
  -s,--save                Save the repo enable/disable state
  -c,--add-copr COPR       Create repo file for copr repo
  -k,--add-koji REPO       Create repo file for koji repo
  -d,--disable REPOPAT     Disable repos
  -e,--enable REPOPAT      Enable repos
  -x,--expire REPOPAT      Expire repo cache
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

List disabled copr repos (actually shows "copr" repos that would be enabled):
```shellsession
$ dnf-repo -e copr
```

Disable all copr repos for update:
```shellsession
$ dnf-repo -d copr update
```

Disable modular repos permanently:
```shellsession
$ dnf-repo --disable-modular --save
```
(or equivalently `dnf-repo -M -s`).

Switch a system from Rawhide to F37:
```shellsession
$ dnf-repo --exact -d rawhide -e fedora distrosync --releasever 37 fedora\*
```

Note that sudo is used implicitly when needed:
there is no need to run dnf-repo with sudo.

## Installation

A copr repo is available:
<https://copr.fedorainfracloud.org/coprs/petersen/dnf-repo/>

## Building

Use {cabal,stack,cabal-rpm} install.
