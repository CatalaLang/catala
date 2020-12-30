/*
* To try in Ace editor, copy and paste into the mode creator
* here : http://ace.c9.io/tool/mode_creator.html
*/

ace.define("ace/mode/catala_en_highlighting_rules", ["require", "exports", "module", "ace/lib/oop", "ace/mode/text_highlight_rules"], function (require, exports, module) {
    "use strict";
    var oop = require("../lib/oop");
    var TextHighlightRules = require("./text_highlight_rules").TextHighlightRules;
    /* --------------------- START ----------------------------- */
    var CatalaEnHighlightRules = function () {
        this.$rules = {
            "start": [
                {
                    "token": "markup.heading.title",
                    "regex": "(@@)",
                    "push": "main__1"
                },
                {
                    "token": "markup.heading.subtitle",
                    "regex": "(@)",
                    "push": "main__2"
                },
                {
                    "token": "entity.law",
                    "regex": "([^\\/])"
                },
                {
                    "token": "comment.block.documentation",
                    "regex": "(\\/\\*)",
                    "push": "code"
                },
                {
                    defaultToken: "text",
                }
            ],
            "code": [
                {
                    "token": "comment.block.documentation",
                    "regex": "(\\*\\/)",
                    "next": "pop"
                },
                {
                    "token": "comment.line",
                    "regex": "(\\s*\\#.*$)"
                },
                {
                    "token": ["keyword.other", "text", "entity.name.function"],
                    "regex": "(context)(\\s+)([a-z\u00e9\u00e8\u00e0\u00e2\u00f9\u00ee\u00ea\u0153\u00e7][a-z\u00e9\u00e8\u00e0\u00e2\u00f9\u00ee\u00ea\u0153\u00e7A-Z\u00c9\u00c8\u00c0\u00c2\u00d9\u00ce\u00ca\u0152\u00c70-9_\\']*)"
                },
                {
                    "token": "keyword.control",
                    "regex": "\\b(match|with\\s+pattern|fixed|by|decreasing|increasing|varies|with|we\\s+have|in|such\\s+that|exists|for|all|of|if|then|else)\\b"
                },
                {
                    "token": "keyword.other",
                    "regex": "\\b(scope|depends\\s+on|declaration|includes|collection|content|optional|structure|enumeration|context|rule|under\\s+condition|condition|data|consequence|fulfilled|equals|assertion|definition|label|exception)\\b"
                },
                {
                    "token": "constant.numeric",
                    "regex": "(\\|[0-9]+/[0-9]+/[0-9]+\\|)"
                },
                {
                    "token": "constant",
                    "regex": "\\b(true|false)\\b"
                },
                {
                    "token": "constant.numeric",
                    "regex": "\\b([0-9]+(,[0.9]*|))\\b"
                },
                {
                    "token": "punctuation",
                    "regex": "(\\-\\-|\\;|\\.|\\,|\\:|\\(|\\)|\\[|\\]|\\{|\\})"
                },
                {
                    "token": "keyword.operator",
                    "regex": "(\\-\\>|\\+|\\-|\\*|/|\\!|not|or|and|=|>|<|\\$|%|year|month|day)"
                },
                {
                    "token": "support.type",
                    "regex": "\\b(integer|boolean|date|money|text|decimal|number|sum)\\b"
                },
                {
                    "token": ["entity.name.class", "punctuation", "entity.name.function"],
                    "regex": "\\b([A-Z\u00c9\u00c8\u00c0\u00c2\u00d9\u00ce\u00ca\u0152\u00c7][a-z\u00e9\u00e8\u00e0\u00e2\u00f9\u00ee\u00ea\u0153\u00e7A-Z\u00c9\u00c8\u00c0\u00c2\u00d9\u00ce\u00ca\u0152\u00c70-9_\\']*)(\\.)([a-z\u00e9\u00e8\u00e0\u00e2\u00f9\u00ee\u00ea\u0153\u00e7][a-z\u00e9\u00e8\u00e0\u00e2\u00f9\u00ee\u00ea\u0153\u00e7A-Z\u00c9\u00c8\u00c0\u00c2\u00d9\u00ce\u00ca\u0152\u00c70-9_\\']*)\\b"
                },
                {
                    "token": ["entity.name.function", "punctuation", "meta.variable_id"],
                    "regex": "\\b([a-z\u00e9\u00e8\u00e0\u00e2\u00f9\u00ee\u00ea\u0153\u00e7][a-z\u00e9\u00e8\u00e0\u00e2\u00f9\u00ee\u00ea\u0153\u00e7A-Z\u00c9\u00c8\u00c0\u00c2\u00d9\u00ce\u00ca\u0152\u00c70-9_\\']*)(\\.)([a-z\u00e9\u00e8\u00e0\u00e2\u00f9\u00ee\u00ea\u0153\u00e7][a-z\u00e9\u00e8\u00e0\u00e2\u00f9\u00ee\u00ea\u0153\u00e7A-Z\u00c9\u00c8\u00c0\u00c2\u00d9\u00ce\u00ca\u0152\u00c70-9_\\'\\.]*)\\b"
                },
                {
                    "token": "entity.name.function",
                    "regex": "\\b([a-z\u00e9\u00e8\u00e0\u00e2\u00f9\u00ee\u00ea\u0153\u00e7][a-z\u00e9\u00e8\u00e0\u00e2\u00f9\u00ee\u00ea\u0153\u00e7A-Z\u00c9\u00c8\u00c0\u00c2\u00d9\u00ce\u00ca\u0152\u00c70-9_\\']*)\\b"
                },
                {
                    "token": "entity.name.class",
                    "regex": "\\b([A-Z\u00c9\u00c8\u00c0\u00c2\u00d9\u00ce\u00ca\u0152\u00c7][a-z\u00e9\u00e8\u00e0\u00e2\u00f9\u00ee\u00ea\u0153\u00e7A-Z\u00c9\u00c8\u00c0\u00c2\u00d9\u00ce\u00ca\u0152\u00c70-9_\\']*)\\b"
                },
                {
                    defaultToken: "text",
                }
            ],
            "main__1": [
                {
                    "token": "markup.heading.title",
                    "regex": "(@@[\\+]*)",
                    "next": "pop"
                },
                {
                    "token": "markup.heading.title",
                    "regex": "(.)"
                },
                {
                    defaultToken: "text",
                }
            ],
            "main__2": [
                {
                    "token": "markup.heading.subtitle",
                    "regex": "(@)",
                    "next": "pop"
                },
                {
                    "token": "markup.heading.subtitle",
                    "regex": "(.)"
                },
                {
                    defaultToken: "text",
                }
            ]
        };
        this.normalizeRules();
    };
    /* ------------------------ END ------------------------------ */
    oop.inherits(CatalaEnHighlightRules, TextHighlightRules);
    exports.CatalaEnHighlightRules = CatalaEnHighlightRules;
});

ace.define("ace/mode/catala_en", ["require", "exports", "module", "ace/lib/oop", "ace/mode/text", "ace/mode/catala_en_highlighting_rules"], function (require, exports, module) {
    "use strict";

    var oop = require("../lib/oop");
    var TextMode = require("./text").Mode;
    var CatalaEnHighlightRules = require("./catala_en_highlighting_rules").CatalaEnHighlightRules;

    var Mode = function () {
        this.HighlightRules = CatalaEnHighlightRules;
        this.$behaviour = this.$defaultBehaviour;
    };
    oop.inherits(Mode, TextMode);

    (function () {

        this.lineCommentStart = "#";

        this.$id = "ace/mode/catala_en";
        this.snippetFileId = "ace/snippets/catala_en";
    }).call(Mode.prototype);

    exports.Mode = Mode;

}); (function () {
    ace.require(["ace/mode/catala_en"], function (m) {
        if (typeof module == "object" && typeof exports == "object" && module) {
            module.exports = m;
        }
    });
})();
