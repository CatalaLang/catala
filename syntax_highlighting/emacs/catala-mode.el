(defvar catala-code-mode-hook nil)

(defun catala-set-syntax-table()
  (let ((st (syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?% "_" st)

    (modify-syntax-entry ?- "." st)
    (modify-syntax-entry ?\; "." st)
    (modify-syntax-entry ?. "." st)
    (modify-syntax-entry ?, "." st)
    (modify-syntax-entry ?: "." st)

    (modify-syntax-entry ?$ "_" st)
    (modify-syntax-entry ?@ "_" st)
    (modify-syntax-entry ?€ "_" st)
    (modify-syntax-entry ?^ "_" st)

    (modify-syntax-entry ?| "$" st)
))

(define-generic-mode 'catala-mode-fr
  '("#")
  '("contexte" "entrée" "résultat" "interne"
    "champ d'application" "si et seulement si" "dépend de" "déclaration" "inclus" "collection" "contenu" "optionnel" "structure" "énumération" "contexte" "entrée" "résultat" "interne" "règle" "sous condition" "condition" "donnée" "conséquence" "rempli" "égal à" "assertion" "définition" "état" "étiquette" "exception" "soit")
  '(("\\<\\(selon\\|sous\s+forme\\|fixé\\|par\\|décroissante\\|croissante\\|varie\\|avec\\|on\s+a\\|soit\\|dans\\|tel\s+que\\|existe\\|pour\\|tout\\|de\\|si\\|alors\\|sinon\\|initial\\)\\>" . font-lock-builtin-face)
    ("\\<\\(vrai\\|faux\\)\\>" . font-lock-constant-face)
    ("\\<\\([0-9][0-9 ]*\\(,[0-9]*\\|\\)\\)\\>" . font-lock-constant-face)
    ("\\(->\\|+.\\|+@\\|+^\\|+€\\|+\\|-.\\|-@\\|-^\\|-€\\|-\\|*.\\|*@\\|*^\\|*€\\|*\\|/.\\|/@\\|/€\\|/\\|!\\|>.\\|>=.\\|<=.\\|<.\\|>@\\|>=@\\|<=@\\|<@\\|>€\\|>=€\\|<=€\\|<€\\|>^\\|>=^\\|<=^\\|<^\\|>\\|>=\\|<=\\|<\\|=\\)" . font-lock-keyword-face)
    ("\\<\\(\\|non\\|ou\s+bien\\|ou\\|et\\|€\\|%\\|an\\|mois\\|jour\\)\\>" . font-lock-keyword-face)
    ("\\<\\(entier\\|booléen\\|date\\|durée\\|argent\\|texte\\|décimal\\|décret\\|loi\\|nombre\\|somme\\)\\>" . font-lock-type-face)
    ("\\<[a-zéèàâùîôêœç][a-zéèàâùîôêœçA-ZÉÈÀÂÙÎÔÊŒÇ0-9_']*\\>" . font-lock-variable-name-face)
    ("\\<[A-ZÉÈÀÂÙÎÔÊŒÇ][a-zéèàâùîôêœçA-ZÉÈÀÂÙÎÔÊŒÇ0-9_']*\\>" . font-lock-function-name-face)
    ("\\<\\(|[0-9]\\+-[0-9]\\+-[0-9]\\+|\\)\\>" . font-lock-constant-face)
  )
  '("\\.catala_fr_raw$")
  '(catala-set-syntax-table)
  "A basic generic major mode for catala files (fr version)"
)

(define-generic-mode 'catala-mode-en
  '("#")
  '("context" "input" "output" "internal"
    "scope" "depends on" "declaration" "includes" "collection" "content" "optional" "structure" "enumeration" "context" "input" "output" "internal" "rule" "under condition" "condition" "data" "consequence" "fulfilled" "equals" "assertion" "definition" "state" "label" "exception" "let")
  '(("\\<\\(match\\|with\s+pattern\\|fixed\\|by\\|decreasing\\|increasing\\|varies\\|with\\|we\s+have\\|let\\|in\\|such\s+that\\|exists\\|for\\|all\\|of\\|if\\|then\\|else\\|initial\\)\\>" . font-lock-builtin-face)
    ("|[0-9]\\+-[0-9]\\+-[0-9]\\+|" . font-lock-constant-face)
    ("\\<\\(true\\|false\\)\\>" . font-lock-constant-face)
    ("\\<\\([0-9][0-9,]*\\(\\.[0-9]*\\|\\)\\)\\>" . font-lock-constant-face)
    ("\\(->\\|+.\\|+@\\|+^\\|+$\\|+\\|-.\\|-@\\|-^\\|-$\\|-\\|*.\\|*@\\|*^\\|*$\\|*\\|/.\\|/@\\|/$\\|/\\|!\\|>.\\|>=.\\|<=.\\|<.\\|>@\\|>=@\\|<=@\\|<@\\|>$\\|>=$\\|<=$\\|<$\\|>^\\|>=^\\|<=^\\|<^\\|>\\|>=\\|<=\\|<\\|=\\)" . font-lock-keyword-face)
    ("\\<\\(not\\|or\\|xor\\|and\\|\\$\\|%\\|year\\|month\\|day\\)\\>" . font-lock-keyword-face)
    ("\\<\\(integer\\|boolean\\|date\\|duration\\|money\\|text\\|decimal\\|number\\|sum\\)\\>" . font-lock-type-face)
    ("\\<[a-zéèàâùîôêœç][a-zéèàâùîôêœçA-ZÉÈÀÂÙÎÔÊŒÇ0-9_']*\\>" . font-lock-variable-name-face)
    ("\\<[A-ZÉÈÀÂÙÎÔÊŒÇ][a-zéèàâùîôêœçA-ZÉÈÀÂÙÎÔÊŒÇ0-9_']*\\>" . font-lock-function-name-face))
  '("\\.catala_en_raw$")
  '(catala-set-syntax-table)
  "A basic generic major mode for catala files (en version)"
)

(defun catala-code-mode()
  "Major mode for catala code (inside of ```catala blocks)"
  (interactive)
  (let* ((buf (window-buffer (minibuffer-selected-window)))
         (name (buffer-file-name buf)))
    (if (and name (string-match-p "\\.catala_fr" name))
        (catala-mode-fr)
      (catala-mode-en)))
  (run-mode-hooks 'catala-code-mode-hook))
;; needed due to <https://github.com/jrblevin/markdown-mode/pull/764>
(add-to-list 'auto-mode-alist '("\\.catala" . catala-code-mode))

(provide 'catala-code-mode)

;;;###autoload
(define-derived-mode catala-mode markdown-mode "Catala"
  "Catala major mode based on markdown-mode"
  (setq-local markdown-fontify-code-blocks-natively t)
  (setq-local markdown-code-lang-modes
   '(("catala-metadata" . catala-code-mode)
     ("catala" . catala-code-mode))))

(provide 'catala-mode)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.catala_\\(fr\\|en\\|pl\\)" . catala-mode))

;; To use, add the following lines to ~/.emacs (without the leading ';') :
; (autoload 'catala-mode "catala-mode" "Catala major mode based on markdown-mode." t)
; (add-to-list 'auto-mode-alist '("\\.catala_\\(fr\\|en\\|pl\\)" . catala-mode))
;; You may additionally need the following if you don't already have any emacs setup in opam:
; (add-to-list 'load-path (concat (string-trim (shell-command-to-string "opam var prefix")) "/share/emacs/site-lisp"))
