#let palette = (
    title: (fill:luma(80), weight: "bold"),
    directive: (fill: rgb(80,80,200), weight: "bold"),
    fence: (fill: rgb(210,130,80), weight: "bold"),
    struct_keyword: (fill: rgb(70,70,180), weight: "bold"),
    keyword: (fill: rgb(60,60,160)),
    literal: (fill: rgb(20,130,50), weight: "bold"),
    type: (fill: purple.darken(10%)),
    operator: (fill: orange.darken(20%), weight: "bold"),
    punctuation: (fill: luma(100)),
    lid: (fill: black, weight: "bold"),
    uid: (fill: rgb(20,140,140), weight: "bold"),
    field: (style: "oblique"),
    comment: (fill: rgb(150,90,60), style: "oblique"),
    state: (fill: rgb(50,130,80), style: "oblique")
)

#let show-catala-fr-code(txt) = {
    set text(font: "DejaVu Sans Mono", style: "normal")
    let override(style,txt) = {
        show text: text.with(..style)
        txt
    }
    show regex("```.*"): txt => {
        box(width: 100%, clip: true,
            $#text(..palette.fence, txt)
            #line(length: 100%, stroke: 0.35pt + luma(140))$)
    }
    show regex("\b(déclaration|règle|définition|étiquette|exception|type|assertion)\b"): override.with(palette.struct_keyword)
    show regex("\b(champ\s+d'application|structure|énumération)\b"): override.with(palette.struct_keyword)
    show regex("\b(si|alors|sinon|selon|sous\s+forme|mais\s+en\s+remplaçant|n'importe\s+quel)\b"): override.with(palette.keyword)
    show regex("\b(non|et|ou|bien|accès_\w+|arrondi|(premier|dernier)_jour_du_mois|pour\s+tout|on\s+a|parmi|transforme|en|chaque|contient|existe|tel\s+que|somme|nombre|maximum|minimum|avec|initialement)\b"): text.with(..palette.keyword)
    show regex("\b(jour|mois|an)\b"): text.with(..palette.literal)
    show regex("\b(booléen|entier|décimal|argent|date|durée|liste)\b"): text.with(..palette.type)
    show regex("\b\p{Lu}[\pL\d_']*\b"): text.with(..palette.uid)
    show regex("\b(-?[0-9]+(,[0-9]+)?[%€]?|vrai|faux)"): override.with(palette.literal)
    show regex("[-=><+*/!]+[!.€^]?|\bde\b"): text.with(..palette.operator)
    show regex("\|[0-9]{4}-[0-9]{2}-[0-9]{2}\|"): override.with(palette.literal)
    show regex("[:;,\[\](){}.]"): text.with(..palette.punctuation)
    show regex("--\s+(n'importe\s+quel|[\pL\d_']+)"): txt => {
        show "--": override.with(palette.punctuation + (style: "normal", weight: "medium"))
        show regex("\b\p{Ll}[\pL\d_']*"): text.with(..palette.field)
        show regex("n'importe\s+quel"): override.with(palette.keyword + (style: "normal"))
        txt
    }
    show regex("\b(contexte|entrée|résultat|interne|état|date\s+arrondi\s+(décroissant|croissant)|soit|dans|dépend\s+de|contenu|sous\s+condition|condition|donnée|conséquence|rempli|égal\s+à)\b"): override.with(palette.keyword + (style: "normal", weight: "medium"))
    show regex("\bdonnée\s+\w+\b"): txt => {
        show regex("\s+.*"): text.with(..palette.field)
        txt
    }
    show regex("\b((liste\s+de)|(ou\s+si\s+liste\s+vide\s+alors)|(combine\s+tout))\b"): override.with(palette.keyword + (weight: "medium"))
    show regex("\bétat\s+\w+\b"): text.with(..palette.state)
    show regex("\b\p{Ll}[\pL\d_']*[.]\p{Ll}[\pL\d_']*\b"): txt => {
        show regex("[.].*"): text.with(..palette.field); txt
    }
    show regex("#.*"): override.with(palette.comment)
    txt
}

#let show-catala-en-code(txt) = {
    set text(font: "DejaVu Sans Mono", style: "normal")
    let override(style,txt) = {
        show text: text.with(..style)
        txt
    }
    show regex("```.*"): txt => {
        box(width: 100%, clip: true,
            $#text(..palette.fence, txt)
            #line(length: 100%, stroke: 0.35pt + luma(140))$)
    }
    show regex("\b(declaration|rule|definition|label|exception|type|assertion)\b"): override.with(palette.struct_keyword)
    show regex("\b(scope|structure|enumeration)\b"): override.with(palette.struct_keyword)
    show regex("\b(not|and|or|xor|get_\w+|round|(first|last)_day_of_month|for\s+all|we\s+have|among|map\s+each|to|contains|exists|such\s+that|sum|number|maximum|minimum|with|initially)\b"): text.with(..palette.keyword)
    show regex("\b(if|then|else|match|(with\s+pattern)|but\s+replace|anything)\b"): override.with(palette.keyword)
    show regex("\b(year|month|day)\b"): text.with(..palette.literal)
    show regex("\b(boolean|integer|decimal|money|date|duration|list)\b"): text.with(..palette.type)
    show regex("\b\p{Lu}[\pL\d_']*\b"): text.with(..palette.uid)
    show regex("\b(\$?-?[0-9,]+(.[0-9,]+)?%?|true|false)"): override.with(palette.literal)
    show regex("[-=><+*/!]+[!.$^]?|\bof\b"): text.with(..palette.operator)
    show regex("\|[0-9]{4}-[0-9]{2}-[0-9]{2}\|"): override.with(palette.literal)
    show regex("[:;,\[\](){}.]"): text.with(..palette.punctuation)
    show regex("--\s+[\pL\d_']+"): txt => {
        show "--": override.with(palette.punctuation + (style: "normal", weight: "medium"))
        show regex("\b\p{Ll}[\pL\d_']*"): text.with(..palette.field)
        show "anything": override.with(palette.keyword + (style: "normal"))
        txt
    }
    show regex("\b(context|input|output|internal|state|date\s+round\s+(decreasing|increasing)|let|in|depends\s+on|content|under\s+condition|condition|data|consequence|fulfilled|equals)\b"): override.with(palette.keyword + (style: "normal", weight: "medium"))
    show regex("\bdata\s+\w+\b"): txt => {
        show regex("\s+.*"): text.with(..palette.field)
        txt
    }
    show regex("\b((list\s+of)|(or\s+if\s+list\s+empty\s+then)|(combine\s+all))\b"): override.with(palette.keyword + (weight: "medium"))
    show regex("\bstate\s+\w+\b"): text.with(..palette.state)
    show regex("\b\p{Ll}[\pL\d_']*[.]\p{Ll}[\pL\d_']*\b"): txt => {
        show regex("[.].*"): text.with(..palette.field); txt
    }
    show regex("#.*"): override.with(palette.comment)
    txt
}

#let setup(it) = {
     show raw.where(lang: "catala-fr"): txt => {
        show regex("^#+.*(\n|$)"): txt => {
            show regex("\| \p{Lu}{8}\d{12}"): text.with(size: 0.8em, fill: luma(100))
            text(..palette.title, txt)
        }
        show regex("^> *.*(\n|$)"): text.with(..palette.directive, weight: "bold")
        show regex("```catala(.|\n)*?```"): show-catala-fr-code
        txt
    }
    show raw.where(lang: "catala-fr-code"): show-catala-fr-code

    show raw.where(lang: "catala-en"): txt => {
        show regex("^#+.*(\n|$)"): text.with(..palette.title)
        show regex("^> *.*(\n|$)"): text.with(..palette.directive, weight: "bold")
        show regex("```catala(.|\n)*?```"): show-catala-en-code
        txt
    }
    show raw.where(lang: "catala-fr-code"): show-catala-fr-code
    show raw.where(lang: "catala-en-code"): show-catala-en-code

    it
}
