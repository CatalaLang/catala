import re
error_state_regex = re.compile("\#\# Ends in an error in state\: (\d+)\.")
report_regex= re.compile("To get a better error messsage, file an issue at https\:\/\/github.com\/CatalaLang\/catala\/issues with this parser error token\: \$")

new_file_content = ""

with open('parser.messages') as messages:
    error_state_number = 0
    for cnt, line in enumerate(messages):
        line = line.strip()
        new_line = line
        if error_state_regex.match(line):
            error_state_number = error_state_regex.search(line).group(1)
            continue
        if report_regex.match(line):
            new_line = "To get a better error messsage, file an issue at https://github.com/CatalaLang/catala/issues with this parser error token: ERROR#{}".format(error_state_number)
        new_file_content += new_line +"\n"

writing_file = open("parser.messages", "w")
writing_file.write(new_file_content)
writing_file.close()
