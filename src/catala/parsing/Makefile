parser.messages.raw: parser.mly
	menhir --list-errors $< > parser.messages

parser.messages: parser.mly
	menhir --update-errors $@ $< > $@.new
	cp -f $@.new $@
	rm $@.new

parser_errors.ml: parser.messages
	menhir parser.mly --compile-errors $< > $@
