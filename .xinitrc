export GDK_USE_XFT=1
setxkbmap -option terminate:ctrl_alt_bksp

DEFAULT_SESSION=i3
export TERMINAL='urxvt'
[[ -f ~/.Xdefaults ]] && xrdb -merge ~/.Xdefaults
xmodmap ~/.Xmodmap

case $1 in
i3)
  #xcompmgr -c &
  exec i3
  ;;
mate)
  xcompmgr -c &
  exec mate-session
  ;;
*)
  #xcompmgr -c &
  exec $DEFAULT_SESSION
esac
