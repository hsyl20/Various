. /etc/profile.d/maven.sh
. $HOME/.bashrc

if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
  nohup startx > .xorg_log & vlock 
  echo "Halt in 3 seconds..."
  sleep 3s
  sudo halt
fi
