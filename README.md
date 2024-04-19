# dnf-repo

A wrapper of the dnf package manager for fine control of
enabled/disabled yum repos (eg Copr repos).

dnf is the package manager used by Fedora Linux, Centos Stream, and RHEL.

DNF can get slower with many repos enabled because it attempts
to refresh its cached repodata frequently. So it can be advantageous
to disable some small repos by default and only enable them periodically
as needed.

This tool can temporarily enable/disable repo(s) selected by substring(s).
Changes to repos' enabled states can be saved too.
It is also possible to expire repo caches individually.

There are also smart options to enable/disable testing/modular repos
(and even source/debuginfo repos), and also to a Copr repo or Koji repo file.

## Help

`$ dnf-repo --version`
```
0.5.6
```
`$ dnf-repo --help`
```
DNF wrapper repo tool

Usage: dnf-repo [--version] [-n|--dryrun] [-q|--quiet] [-D|--debug] [-l|--list]
                [-s|--save] [(-w|--weak-deps) | (-W|--no-weak-deps)] [--exact]
                [(-d|--disable REPOPAT) | (-e|--enable REPOPAT) |
                  (-x|--expire REPOPAT) | (-X|--clear-expires) |
                  (-E|--delete-repofile REPOPAT) | (-t|--enable-testing) |
                  (-T|--disable-testing) | (-m|--enable-modular) |
                  (-M|--disable-modular) | --enable-debuginfo |
                  --disable-debuginfo | --enable-source | --disable-source |
                  (-c|--add-copr COPR) [--osname OSNAME]
                  [--releasever RELEASEVER] |
                  (-k|--add-koji REPO) | (-u|--repourl URL)] [DNFARGS]

  see https://github.com/juhp/dnf-repo#readme

Available options:
  -h,--help                Show this help text
  --version                Show version
  -n,--dryrun              Dry run
  -q,--quiet               Suppress necessary output
  -D,--debug               Debug output
  -l,--list                List all repos
  -s,--save                Save the repo enable/disable state
  -w,--weak-deps           Use weak dependencies
  -W,--no-weak-deps        Disable weak dependencies
  --exact                  Match repo names exactly
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
  -c,--add-copr COPR       Create repo file for copr repo
  --osname OSNAME          Specify OS Name to override (eg epel)
  --releasever RELEASEVER  Specify OS Release Version to override (eg rawhide)
  -k,--add-koji REPO       Create repo file for koji repo (f40-build, rawhide,
                           epel9-build, etc)
  -u,--repourl URL         Use temporary repo from a baseurl
```

## Usage examples
List repos:
```shellsession
$ dnf-repo [--list]
```

Update with testing repos enabled:
```shellsession
$ dnf-repo -t update
```

Note that sudo is used implicitly when needed:
there is no need to run dnf-repo with sudo.

### Copr
List disabled copr repos (ie lists copr repos that would be enabled):
```shellsession
$ dnf-repo -e copr
```

Disable active copr repos for update:
```shellsession
$ dnf-repo -d copr update
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
Disable fedora modular and cisco openh264 repos permanently:
```shellsession
$ dnf-repo --M -d h264 --save
```

### Use only source repos
```shellsession
$ dnf-repo -d \* --enable-source
with enabled 'fedora-source'
with enabled 'updates-source'
with disabled 'fedora'
with disabled 'updates'
```

### Switch system from rawhide
Switch a system from Rawhide to F40:
```shellsession
$ dnf-repo -d rawhide -e fedora distrosync --releasever 40 fedora-\*
with disabled 'rawhide'
with enabled 'fedora'

:
```

### Repo patterns
By default repo patterns are matched as infix substrings
(unless you use `--exact`).

But you can also prepend `^`/append `$` (or both) to match a repo name
from its beginning/end (or exactly).

You can also use glob patterns to match one or more repo names:
see the [supported Glob syntax](https://hackage.haskell.org/package/Glob/docs/System-FilePath-Glob.html#v:compile).

Without a glob (eg '*') a shortest common repo match will be sought,
otherwise all matching repos will be considered.

An initial `^` (final `$`) in a glob pattern prevents
the automatic prepending (appending) of `*` before (after) the pattern.

Repo actions expand to a sequence of `--enablerepo=`, `--disablerepo=`,
`--repo=`, etc, so as usual later settings will overrule conflicting
earlier settings.

## Installation
A copr repo is available:
<https://copr.fedorainfracloud.org/coprs/petersen/dnf-repo/>

## Building
Use {cabal,stack,cabal-rpm} install.

## Contributing
dnf-repo is distributed under the GPL license version 3 or later.

The source repository is https://github.com/juhp/dnf-repo/

Contributions including reports and suggestions for improvement are welcome.
