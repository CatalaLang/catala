parser.messages.raw: parser.mly
	menhir --list-errors parser.mly > parser.messages

update_messages: parser.messages
	menhir --update-errors parser.messages parser.mly > parser.messages.new

parser_errors.ml: parser.mly parser.messages
	menhir parser.mly --compile-errors parser.messages > parser_errors.ml
