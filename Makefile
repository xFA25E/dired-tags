test: dired-tags-test.el dired-tags.el
	emacs -Q -module-assertions -batch -L . -l dired-tags-test.el -f ert-run-tests-batch-and-exit
