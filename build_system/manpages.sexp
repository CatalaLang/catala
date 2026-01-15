(rule (alias man) (action (with-stdout-to clerk.1 (run %{bin:clerk}  --help=groff))))
