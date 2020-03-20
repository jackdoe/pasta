# pasta

service to share my clipboard in the tty and emacs.

it is an http server that picks random key on start and only stores
the latest value in memory.

# install

build the cmd/ commands, and copy them to /usr/bin
then:

  $ cp pasta-server.service ~/.config/systemd/uesr
  $ systemctl enable --user pasta-server

# setup

make emacs use pasta-paste and pasta-copy:

  (defun copy-from-linux ()
    (shell-command-to-string "pasta-paste"))
  
  (defun paste-to-linux (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pasta-copy" "*Messages*" "pasta-copy")))
        (process-send-string proc text)
        (process-send-eof proc))))
  
  (setq interprogram-cut-function 'paste-to-linux)
  (setq interprogram-paste-function 'copy-from-linux


Also in zsh:
  pasta_paste() {
      BUFFER=$(pasta-paste)
      CURSOR=${#BUFFER}
  }
  zle -N pasta_paste
  bindkey "^y" pasta_paste

  alias pbcopy=pasta-copy
  alias pbpaste=pasta-paste

so now i can do `echo -n a | pbcopy` and then in emacs just C-y

pretty cool.

PS: it is just a http server you can use it with curl:

  # put aaa in the clipboard
  echo -n 'aaa' | curl --data-binary @- http://127.0.0.1:8111/copy

  # get aaa from the clipboard
  curl http://127.0.0.1:8111/paste


