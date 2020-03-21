# pasta

service to share my clipboard in the tty and emacs.

it is an http server that picks random key on start and only stores
the latest value in memory.

# install

build the cmd/ commands, and copy them to /usr/bin
then:

  $ cp pasta-server.service ~/.config/systemd/user
  $ systemctl enable --user pasta-server

or run install.sh which does those steps

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
  (setq interprogram-paste-function 'copy-from-linux)


Also in zsh:
  pasta_paste() {
      p=$(pasta-paste)
      cp=$LBUFFER$p

      BUFFER=$LBUFFER$p$RBUFFER
      CURSOR=${#cp}
  }

  zle -N pasta_paste
  bindkey "^y" pasta_paste

  alias pbcopy=pasta-copy
  alias pbpaste=pasta-paste

so now i can do `echo -n a | pbcopy` and then in emacs just C-y

pretty cool.

PS: it is just a http server you can use it with curl:

  # put aaa in the clipboard
  echo -n 'aaa' | curl --unix-socket $HOME/.pasta/sock --data-binary @- http://unix/copy

  # get aaa from the clipboard
  curl --unix-socket $HOME/.pasta/sock  http://unix/paste


If you want to add support for programatically getting the mouse
selection you have to apply linux-5.6.0-patch/add_copy_selection_to_user.diff
(dont use in production, it is just a hack to get the current
selection)

then you can run `pasta-copy -sel` which will do

	fd, _ := os.Open("/dev/tty")

	value := make([]byte, 1024*10) // 10k should be enough

	value[0] = byte(TIOCL_GETSEL)
	nativeEndian.PutUint32(value[1:], uint32(len(value)-5))

	err = ioctl(int(fd.Fd()), TIOCLINUX, uintptr(unsafe.Pointer(&value[0])))
	if err != nil {
		panic(err)
	}

	size := nativeEndian.Uint32(value[1:])
	clipboard = bytes.NewReader(value[5 : size+5])

and send it to the pasta server

pasta_mouse_copy() {
    pasta-copy -sel
}

zle -N pasta_mouse_copy
bindkey "^[w" pasta_mouse_copy
