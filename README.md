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
$ dnf-repo --help
DNF wrapper repo tool

Usage: dnf-repo [--version] [-n|--dryrun] [-D|--debug] [-s|--save]
                [(-c|--add-copr COPR) | (-d|--disable REPOPAT) |
                  (-e|--enable REPOPAT) | (-x|--expire REPOPAT)]
                [(-t|--include-testing) | (-T|--exclude-testing)]
                [(-m|--include-modular) | (-M|--exclude-modular)]
                [[REPOPAT] ARGS]
  see https://github.com/juhp/dnf-repo#readme

Available options:
  -h,--help                Show this help text
  --version                Show version
  -n,--dryrun              Dry run
  -D,--debug               Debug output
  -s,--save                Save the repo enable/disable state
  -c,--add-copr COPR       Create repo file for copr repo
  -d,--disable REPOPAT     Disable repos
  -e,--enable REPOPAT      Enable repos
  -x,--expire REPOPAT      Expire repo cache
  -t,--include-testing     Enable testing repos
  -T,--exclude-testing     Disable testing repos
  -m,--include-modular     Enable modular repos
  -M,--exclude-modular     Disable modular repos
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
