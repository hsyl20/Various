. $HOME/.zshrc

if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
  nohup startx > .xorg_log & vlock 
  echo "Halt in 3 seconds..."
  sleep 3s
  systemctl poweroff
fi
