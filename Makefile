.RECIPEPREFIX = >

EMACS_FLAGS=-l init.el
EMACS=emacs --quick --batch $(EMACS_FLAGS)

.PHONY: install autoload autoremove update clean doc help update-moon

# install have to be in the front
# otherwise use-package will not be avaliable
# on fresh install
# all: | install autoload autoremove
all: autoload autoremove install
>@$(EMACS) --eval '(progn (message "\nInstalling packages...\n") (moon/install-package) (message "\nGenerating autoload file...\n") (moon/generate-autoload-file) (message "\nRemoving unused packages...\n") (moon/remove-unused-package))'

# commands
install: init.el
>@echo "Installing packages" ;\
$(EMACS) -f moon/install-package

autoload: init.el
>@echo "Generating autoload files" ;\
$(EMACS) -f moon/generate-autoload-file ;\
# rm -f .local/autoloads.el~

autoremove: init.el
>@echo "Removing unused packages" ;\
$(EMACS) -f moon/remove-unused-package

update: init.el
>@echo "Updating packages" ;\
$(EMACS) -f moon/update-package -f moon/generate-autoload-file

update-moon:
>git pull --rebase

clean:
>@echo "Removing compiled files" ;\
find . -type f -name *.elc -delete

test:
>emacs --eval "(add-hook 'moon-post-init-hook #'moon/run-test t)"
