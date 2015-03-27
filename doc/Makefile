s-expand/index.html: s-expand-manual.xml
	mkdir -p s-expand
	xmlto -o s-expand xhtml $<

s-expand-manual.xml: s-expand-manual.lisp
	sbcl --noinform --load $< --eval "(expand)" --eval "(quit)"

clean:
	rm -f *.html *.xml *.fasl s-exand
