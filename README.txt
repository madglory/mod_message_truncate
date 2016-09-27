The 'mod_message_truncate' ejabberd module aims to truncate message body text to
a certain length (currently 1000 characters). If a message body is too long,
it will be truncated to the max length - 3, and '...' will be added to the end.
(Making it a max of max length)

To install in ejabberd:

cd ~/.ejabberd-modules/sources
clone the git repo

make sure ejabberd is running
run: ejabberdctl module_install mod_message_truncate
run: ejabberdctl restart
module will be installed in: ~/.ejabberd-modules/mod_message_truncate
