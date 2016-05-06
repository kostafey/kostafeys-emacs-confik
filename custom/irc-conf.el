(eval-after-load 'circe
  '(progn
     (setq circe-reduce-lurker-spam t)
     (enable-circe-color-nicks)
     (setq circe-color-nicks-everywhere t)))

(provide 'irc-conf)
