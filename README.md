# dnf-repo

A wrapper over dnf to use extra disabled yum repos (eg coprs).

DNF is not terribly fast at handling repos partly because it likes
to refresh the cached repodata frequently. So it can be advantageous
to disable smaller repos by default and only enable them periodically
as needed.

Basically this tool can temporarily enable/disable repos selected by substring.
Repo enabled states can also be saved or cache expired.

There is an option to create a repo file for a copr.

Also options to enable/disable testing/modular repos.

## Help

```shellsession
$ dnf-repo
DNF wrapper repo tool

Usage: dnf-repo [--version] [-n|--dryrun] [-s|--save]
                [(-c|--add-copr) | (-d|--disable) | (-x|--expire)]
                [(-t|--enable-testing) | (-T|--disable-testing)]
                [(-m|--enable-modular) | (-M|--disable-modular)] REPO [ARGS]
  see https://github.com/juhp/dnf-repo#readme

Available options:
  -h,--help                Show this help text
  --version                Show version
  -n,--dryrun              Dry run
  -s,--save                Save enabled state
  -c,--add-copr            Create repo file for copr repo
  -d,--disable             Disable repos
  -x,--expire              Expire repo cache
  -t,--enable-testing      Include testing repos
  -T,--disable-testing     Exclude testing repos
  -m,--enable-modular      Include modular repos
  -M,--disable-modular     Exclude modular repos
```

## Usage
List disabled copr repos:
```shellsession
$ dnf-tool -n copr
```

Disable copr repos for update:
```shellsession
$ dnf-tool -d copr update
```

Disable modular repos permanently:
```shellsession
$ dnf-tool --disable-modular --save
