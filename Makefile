MODULE=httpheader
pydoc:	$(MODULE).html
	rm -f pydoc
	ln -s $(MODULE).html pydoc

$(MODULE).html:	$(MODULE).py
	pydoc -w $(MODULE)
	mv $(MODULE).html $(MODULE).html2
	sed -e 's|file:/home/dem/public_html|http://deron.meranda.us|g' -e 's|>/home/dem/public_html/python/$(MODULE)/|>|g' <$(MODULE).html2 >$(MODULE).html
