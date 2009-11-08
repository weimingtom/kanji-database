TARGETS = .htaccess index.html

all: $(TARGETS)

install:: all
	rsync -av -e ssh --delete \
	--exclude=CVS --exclude='Makefile*' --exclude='*.in' --exclude='*~' \
	* .[a-z]* kawabata@kanji-database.sourceforge.net:/home/groups/k/ka/kanji-database/htdocs
