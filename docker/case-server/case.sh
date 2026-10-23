# Nothing in the homes can be written: no history file, no less history,
# no Git locks in the read-only repository.
HISTFILE=
LESSHISTFILE=-
GIT_OPTIONAL_LOCKS=0
export HISTFILE LESSHISTFILE GIT_OPTIONAL_LOCKS

# Where you are, at a glance: the prompt starts with the server's name in
# colour, [museum] amber, [alarmanlage] red. The prompt of the Workspace has
# no colour, so after exit it is plain again.
case "$(hostname)" in
    museum*) case_colour='1;33' ;;
    alarmanlage*) case_colour='1;31' ;;
    *) case_colour='1;36' ;;
esac
PS1="\[\e[${case_colour}m\][\h]\[\e[0m\] \u:\w\$ "
unset case_colour
