/*
* To try in Ace editor, copy and paste into the mode creator
* here : http://ace.c9.io/tool/mode_creator.html
*/

ace.define("ace/mode/catala_fr_highlighting_rules", ["require", "exports", "module", "ace/lib/oop", "ace/mode/text_highlight_rules"], function (require, exports, module) {
    "use strict";
    var oop = require("../lib/oop");
    var TextHighlightRules = require("./text_highlight_rules").TextHighlightRules;
    /* --------------------- START ----------------------------- */
    var CatalaFrHighlightRules = function () {
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
                    "regex": "(contexte)(\\s+)([a-z\u00e9\u00e8\u00e0\u00e2\u00f9\u00ee\u00f4\u00ea\u0153\u00e7][a-z\u00e9\u00e8\u00e0\u00e2\u00f9\u00ee\u00f4\u00ea\u0153\u00e7A-Z\u00c9\u00c8\u00c0\u00c2\u00d9\u00ce\u00d4\u00ca\u0152\u00c70-9_\\']*)"
                },
                {
                    "token": "keyword.control",
                    "regex": "\\b(selon|sous\\s+forme|fix\u00e9|par|d\u00e9croissante|croissante|varie|avec|on\\s+a|dans|tel\\s+que|existe|pour|tout|de|si|alors|sinon)\\b"
                },
                {
                    "token": "keyword.other",
                    "regex": "\\b(champ\\s+d'application|si\\s+et\\s+seulement\\s+si|d\u00e9pend\\s+de|d\u00e9claration|inclus|collection|contenu|optionnel|structure|\u00e9num\u00e9ration|contexte|r\u00e8gle|sous\\s+condition|condition|donn\u00e9e|cons\u00e9quence|rempli|\u00e9gal\\s+\u00e0|assertion|d\u00e9finition|\u00e9tiquette|exception)\\b"
                },
                {
                    "token": "constant.numeric",
                    "regex": "(\\|[0-9]+/[0-9]+/[0-9]+\\|)"
                },
                {
                    "token": "constant",
                    "regex": "\\b(vrai|faux)\\b"
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
                    "regex": "(\\-\\>|\\+|\\-|\\*|/|\\!|non|ou|et|=|>|<|\\u20ac|%|an|mois|jour)"
                },
                {
                    "token": "support.type",
                    "regex": "\\b(entier|bool\u00e9en|date|argent|texte|d\u00e9cimal|d\u00e9cret|loi|nombre|somme|date_aujourd_hui)\\b"
                },
                {
                    "token": ["entity.name.class", "punctuation", "entity.name.function"],
                    "regex": "\\b([A-Z\u00c9\u00c8\u00c0\u00c2\u00d9\u00ce\u00d4\u00ca\u0152\u00c7][a-z\u00e9\u00e8\u00e0\u00e2\u00f9\u00ee\u00f4\u00ea\u0153\u00e7A-Z\u00c9\u00c8\u00c0\u00c2\u00d9\u00ce\u00d4\u00ca\u0152\u00c70-9_\\']*)(\\.)([a-z\u00e9\u00e8\u00e0\u00e2\u00f9\u00ee\u00f4\u00ea\u0153\u00e7][a-z\u00e9\u00e8\u00e0\u00e2\u00f9\u00ee\u00f4\u00ea\u0153\u00e7A-Z\u00c9\u00c8\u00c0\u00c2\u00d9\u00ce\u00d4\u00ca\u0152\u00c70-9_\\']*)\\b"
                },
                {
                    "token": ["entity.name.function", "punctuation", "meta.variable_id"],
                    "regex": "\\b([a-z\u00e9\u00e8\u00e0\u00e2\u00f9\u00ee\u00f4\u00ea\u0153\u00e7][a-z\u00e9\u00e8\u00e0\u00e2\u00f9\u00ee\u00f4\u00ea\u0153\u00e7A-Z\u00c9\u00c8\u00c0\u00c2\u00d9\u00ce\u00d4\u00ca\u0152\u00c70-9_\\']*)(\\.)([a-z\u00e9\u00e8\u00e0\u00e2\u00f9\u00ee\u00f4\u00ea\u0153\u00e7][a-z\u00e9\u00e8\u00e0\u00e2\u00f9\u00ee\u00f4\u00ea\u0153\u00e7A-Z\u00c9\u00c8\u00c0\u00c2\u00d9\u00ce\u00d4\u00ca\u0152\u00c70-9_\\'\\.]*)\\b"
                },
                {
                    "token": "entity.name.function",
                    "regex": "\\b([a-z\u00e9\u00e8\u00e0\u00e2\u00f9\u00ee\u00f4\u00ea\u0153\u00e7][a-z\u00e9\u00e8\u00e0\u00e2\u00f9\u00ee\u00f4\u00ea\u0153\u00e7A-Z\u00c9\u00c8\u00c0\u00c2\u00d9\u00ce\u00d4\u00ca\u0152\u00c70-9_\\']*)\\b"
                },
                {
                    "token": "entity.name.class",
                    "regex": "\\b([A-Z\u00c9\u00c8\u00c0\u00c2\u00d9\u00ce\u00d4\u00ca\u0152\u00c7][a-z\u00e9\u00e8\u00e0\u00e2\u00f9\u00ee\u00f4\u00ea\u0153\u00e7A-Z\u00c9\u00c8\u00c0\u00c2\u00d9\u00ce\u00d4\u00ca\u0152\u00c70-9_\\']*)\\b"
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
    oop.inherits(CatalaFrHighlightRules, TextHighlightRules);
    exports.CatalaFrHighlightRules = CatalaFrHighlightRules;
});


ace.define("ace/mode/catala_fr", ["require", "exports", "module", "ace/lib/oop", "ace/mode/text", "ace/mode/catala_fr_highlighting_rules"], function (require, exports, module) {
    "use strict";

    var oop = require("../lib/oop");
    var TextMode = require("./text").Mode;
    var CatalaFrHighlightRules = require("./catala_fr_highlighting_rules").CatalaFrHighlightRules;

    var Mode = function () {
        this.HighlightRules = CatalaFrHighlightRules;
        this.$behaviour = this.$defaultBehaviour;
    };
    oop.inherits(Mode, TextMode);

    (function () {

        this.lineCommentStart = "#";

        this.$id = "ace/mode/catala_fr";
        this.snippetFileId = "ace/snippets/catala_fr";
    }).call(Mode.prototype);

    exports.Mode = Mode;

}); (function () {
    ace.require(["ace/mode/catala_fr"], function (m) {
        if (typeof module == "object" && typeof exports == "object" && module) {
            module.exports = m;
        }
    });
})();
