# dnf-repo releases

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
