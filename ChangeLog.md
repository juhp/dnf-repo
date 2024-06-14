# dnf-repo releases

## 0.6.1 (2024-06-14)
- add `--add-repofile URL` action
- `--add-copr` can now also take an url
- `--add-copr owner/pkg install` will now expand to "install pkg"
- allow `-add-copr` and `--add-koji` to continue even if the .repo exists
- get 1s speedup by dropping sleep 1!

## 0.6 (2024-04-22)
- switch license from BSD to GPLv3+
- add --only option to only --repo enable certain repos
- add --dnf4 switch to avoid dnf5
- --add-copr: copr is now available while being added
- selectRepo: use left fold to not reverse order of repo actions
- normalize copr name internally
- error if /etc/yum.repo.d doesn't exist
- fix 0.5.6 regression: error again when no matching repo
- --list output now takes --only into account properly

## 0.5.6 (2024-04-18)
- --add-copr now supports non-fedora copr servers and downloads .repo with curl
- with --add-copr can override --osname OS and --releasever VERSION
- use dnf5 if available
- --expire now enables the repo for the invocation
- --add-koji now uses rpm system arch

## 0.5.5 (2023-07-02)
- for dnf5 test for dnf-3 (for config-manager) and also dnf
- YumRepoFile: fix parsing of "enable = 1" (for UBI)
- sudo debug: print cmd
- update simple-prompt to 0.2

## 0.5.4 (2023-05-06)
- Revert "don't printAction's if no args"
- use simple-prompt
- just check for SUDO_USER instead of euid

## 0.5.3 (2022-12-31)
- for copr template file, detect fedora distro otherwise assume epel
- don't print actions if no args
- skip euid check if proot session

## 0.5.2 (2022-11-28)
- --releasever now induces using a separate dnf cache subdir
- YumRepoFile: do not sort modes
- silence "already enabled/disabled" warnings when there are actions
- improve --save: use yesno prompt and only act if changes
- --clear-expires: error if no repos set to expire
- warning when run as sudo

## 0.5.1 (2022-11-08)
- check if new copr or koji repo exists with http-directory
- remove initial/trailing / or : from reponames
- add --quiet option: limits output to dnf --quiet and permanent changes
- add --repourl to use a repo baseurl
- output state info to stderr
- expire: don't print action and tweak prompt

## 0.5 (2022-10-23)
- support repo Glob patterns
- prefer shortest common prefix repo match if not glob
- add --clear-expires command; expire commands now prompt
- abort if no match for repopat action
- accumulate repostate unchanged warnings
- drop overspecific --disable/--enable-defaults
- more consistent output, particularly newlines
- simple testsuite

## 0.4 (2022-10-20)
- only list all repos if --list or no repo actions
- repo patterns can now use a ^ prefix and $ suffix
- add --disable/enable-defaults for Fedora modular and cisco h264 repos
- add --(no-)weak-deps options (-W/-w)
- transform / to : in any given repo pattern

## 0.3 (2022-08-12)
- fold over multiple changes (eg -m -t now combine correctly)
- handle debuginfo and source repos

## 0.2 (2022-08-12)
- support multiple repos per repo file (eg needed for eln)
- support multiple repo actions (eg "-d rawhide -e fedora")
- --exact repo match option (eg for 'fedora')
- --debug now shows what sudo does, like --dryrun

## 0.1 (2022-06-20)
- initial version with basic functionality: --add-copr, --add-koji,
  --disable repo, --enable repo, --enable/disable-{testing,modular}
  and --save & --expire
