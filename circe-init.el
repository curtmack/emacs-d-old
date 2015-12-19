(use-package circe
  :config
  (setq circe-network-options
        '(("Foonetic"
           :host "irc.foonetic.net"
           :port (6667 . 6697)
           :nick "curtmack"
           :channels ("#xkcd")))))
