;;; init-mcp.el --- init mcp packages                -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'mcp-hub)

(require 'gptel)

(setq mcp-hub-servers
      `(,@(when-let* ((key (lizqwer/api-key-from-auth-source "api.github.com" "lizqwerscott^mcp")))
            `(("github" . (
                           :command "docker"
                           :args ("run" "--rm" "-i" "-e" "GITHUB_PERSONAL_ACCESS_TOKEN" "ghcr.io/github/github-mcp-server")
                           :env (:GITHUB_PERSONAL_ACCESS_TOKEN ,key)))))
        ("ddg-search" . (:command "uvx" :args ("duckduckgo-mcp-server")))
        ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
        ("sequential-thinking" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-sequential-thinking")))
        ("context7" . (:command "npx" :args ("-y" "@upstash/context7-mcp@latest")))))

(require 'gptel-integrations)

(provide 'init-mcp)
;;; init-mcp.el ends here
