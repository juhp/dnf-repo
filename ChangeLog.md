# dnf-repo releases

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
