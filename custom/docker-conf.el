;;; -*- lexical-binding: t -*-
;;-------------------------------------------------------------------
;; docker.el -- manage containers, images, volumes, networks and
;; compose projects from Emacs instead of the Docker Desktop app.
;;
;; It drives the `docker' CLI, so it talks to whatever the current
;; docker context points at (`docker context ls'); switch contexts with
;; `docker context use NAME' or the transient's -H/--host option.
;; Commands that need a live terminal (attach, exec, run -it) pick the
;; terminal backend on their own: ghostel, set up in `shell-conf'.
;;
(use-package docker
  :straight (docker :type git :host github
                    :repo "Silex/docker.el" :branch "main")
  :bind ("C-c D" . docker))

;;-------------------------------------------------------------------
;; Dockerfile editing
;;
(use-package dockerfile-mode
  :straight '(dockerfile-mode :type git :host github
			                        :repo "spotify/dockerfile-mode"
                              :branch "master"))

(provide 'docker-conf)
