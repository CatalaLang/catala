#let palette = (
    // 12-bit color palette https://iamkate.com/data/12-bit-rainbow/
    plum: rgb("#817"),
    purple: rgb("#a35"),
    brick: rgb("#c66").darken(10%),
    orange: rgb("#e94").darken(20%),
    yellow: rgb("#ed0").darken(30%),
    lime: rgb("#9d5").darken(30%),
    bluegreen: rgb("#4d8").darken(25%),
    sea: rgb("#2cb").darken(20%),
    azur: rgb("#0bc").darken(20%),
    blue: rgb("#09c").darken(10%),
    marine: rgb("#36b"),
    violet: rgb("#639"),

    grey: rgb("#666"),
    black: rgb("#000")
)

#let bold = "extrabold"
#let normal = "medium"

#let style = (
    title: (fill: palette.violet, weight: bold),
    directive: (fill: palette.violet),
    fence: (fill: palette.grey),
    struct_keyword: (fill: palette.marine, weight: bold),
    keyword: (fill: palette.marine),
    literal: (fill: palette.azur),
    type: (fill: palette.plum),
    operator: (fill: palette.orange, weight: bold),
    punctuation: (fill: palette.grey),
    lid: (fill: palette.black, weight: bold),
    uid: (fill: palette.bluegreen, weight: bold),
    field: (style: "oblique"),
    comment: (fill: palette.brick, style: "oblique"),
    annotation: (fill: palette.brick, weight: bold),
    state: (fill: palette.lime, style: "oblique"),
)

#let show-catala-fr-code(txt) = {
    set text(style: "normal")
    let override(style,txt) = {
        show text: text.with(..style)
        txt
    }
    show regex("```.*"): txt => {
        box(width: 100%, clip: true,
            $#text(..style.fence, txt)
            #line(length: 100%, stroke: 0.35pt + luma(140))$)
    }
    show regex("\b(déclaration|règle|définition|étiquette|exception|type|assertion)\b"): override.with(style.struct_keyword)
    show regex("\b(champ\s+d'application|structure|énumération)\b"): override.with(style.struct_keyword)
    show regex("\b(si|alors|sinon|selon|sous\s+forme|mais\s+en\s+remplaçant|n'importe\s+quel)\b"): override.with(style.keyword)
    show regex("\b(non|et|ou|bien|accès_\w+|arrondi|(premier|dernier)_jour_du_mois|pour\s+tout|on\s+a|parmi|transforme|en|chaque|contient|existe|tel\s+que|somme|nombre|maximum|minimum|avec|initialement)\b"): text.with(..style.keyword)
    show regex("\b(booléen|entier|décimal|argent|date|durée|liste)\b"): text.with(..style.type)
    show regex("\b\p{Lu}[\pL\d_']*\b"): text.with(..style.uid)
    show regex("[-=><+*/!]+[!.€^]?|\bde\b"): override.with(style.operator)
    show regex("[:;,\[\](){}.]"): text.with(..style.punctuation)
    show regex("(\b|-)([0-9]+(,[0-9]*)?[%€]?)"): txt => {
        show text: text.with(..style.literal)
        show regex("[^-0-9,]"): text.with(weight: bold)
        txt
    }
    show regex("\b(vrai|faux|jour|mois|an)\b"): text.with(..style.literal, weight: bold)
    show regex("\|[0-9]{4}-[0-9]{2}-[0-9]{2}\|"): override.with(style.literal + (weight: normal))
    show regex("--\s+(n'importe\s+quel|[\pL\d_']+)"): txt => {
        show "--": override.with(style.punctuation + (style: "normal", weight: normal))
        show regex("\b\p{Ll}[\pL\d_']*"): text.with(..style.field)
        show regex("n'importe\s+quel"): override.with(style.keyword + (style: "normal"))
        txt
    }
    show regex("\b(contexte|entrée|résultat|interne|état|date\s+arrondi(\s+dé)?|(dé)?croissant|soit|dans|dépend\s+de|contenu|sous\s+condition|condition|donnée|conséquence|rempli|égal\s+à)\b"): override.with(style.keyword + (style: "normal", weight: normal))
    show regex("\bdonnée\s+\w+\b"): txt => {
        show regex("\s+.*"): text.with(..style.field)
        txt
    }
    show regex("\b((liste\s+de)|(ou\s+si\s+liste\s+vide\s+alors)|(combine\s+tout))\b"): override.with(style.keyword + (weight: normal))
    show regex("\bétat\s+\w+\b"): text.with(..style.state)
    show regex("\b\p{Ll}[\pL\d_']*[.]\p{Ll}[\pL\d_']*\b"): txt => {
        show regex("[.].*"): text.with(..style.field); txt
    }
    show regex("#[^\[].*"): override.with(style.comment)
    show regex("#\[[^\]]+\]"): override.with(style.annotation)
    txt
}

#let show-catala-en-code(txt) = {
    set text(style: "normal")
    let override(style,txt) = {
        show text: text.with(..style)
        txt
    }
    show regex("```.*"): txt => {
        box(width: 100%, clip: true,
            $#text(..style.fence, txt)
            #line(length: 100%, stroke: 0.35pt + luma(140))$)
    }
    show regex("\b(declaration|rule|definition|label|exception|type|assertion)\b"): override.with(style.struct_keyword)
    show regex("\b(scope|structure|enumeration)\b"): override.with(style.struct_keyword)
    show regex("\b(not|and|or|xor|get_\w+|round|(first|last)_day_of_month|for\s+all|we\s+have|among|map\s+each|to|contains|exists|such\s+that|sum|number|maximum|minimum|with|initially)\b"): text.with(..style.keyword)
    show regex("\b(if|then|else|match|(with\s+pattern)|but\s+replace|anything)\b"): override.with(style.keyword)
    show regex("\b(boolean|integer|decimal|money|date|duration|list)\b"): text.with(..style.type)
    show regex("\b\p{Lu}[\pL\d_']*\b"): text.with(..style.uid)
    show regex("[-=><+*/!]+[!.$^]?|\bof\b"): text.with(..style.operator)
    show regex("[:;,\[\](){}.]"): text.with(..style.punctuation)
    show regex("(\b|\$-?|-)[0-9][0-9,]*(\.[0-9,]*[0-9])?%?"): txt => {
        show text: text.with(..style.literal)
        show regex("[^-0-9,]"): text.with(weight: bold)
        txt
    }
    show regex("\b(true|false|year|month|day)\b"): text.with(..style.literal, weight: bold)
    show regex("\|[0-9]{4}-[0-9]{2}-[0-9]{2}\|"): override.with(style.literal + (weight: normal))
    show regex("--\s+[\pL\d_']+"): txt => {
        show "--": override.with(style.punctuation + (style: "normal", weight: normal))
        show regex("\b\p{Ll}[\pL\d_']*"): text.with(..style.field)
        show "anything": override.with(style.keyword + (style: "normal"))
        txt
    }
    show regex("\b(context|input|output|internal|state|date\s+round|(de|in)?creasing|let|in|depends\s+on|content|under\s+condition|condition|data|consequence|fulfilled|equals)\b"): override.with(style.keyword + (style: "normal", weight: normal))
    show regex("\bdata\s+\w+\b"): txt => {
        show regex("\s+.*"): text.with(..style.field)
        txt
    }
    show regex("\b((list\s+of)|(or\s+if\s+list\s+empty\s+then)|(combine\s+all))\b"): override.with(style.keyword + (weight: normal))
    show regex("\bstate\s+\w+\b"): text.with(..style.state)
    show regex("\b\p{Ll}[\pL\d_']*[.]\p{Ll}[\pL\d_']*\b"): txt => {
        show regex("[.].*"): text.with(..style.field); txt
    }
    show regex("#.*"): override.with(style.comment)
    txt
}

#let setup(it) = {
     show raw.where(lang: "catala-fr"): txt => {
        show regex("^#+.*(\n|$)"): txt => {
            show regex("\| \p{Lu}{8}\d{12}"): text.with(font: "DejaVu Sans Mono", weight: "semibold", size: 0.75em, fill: luma(130))
            text(..style.title, txt)
        }
        show regex("^> *.*(\n|$)"): text.with(..style.directive)
        show regex("```catala(.|\n)*?```"): show-catala-fr-code
        txt
    }
    show raw.where(lang: "catala-fr-code"): show-catala-fr-code

    show raw.where(lang: "catala-en"): txt => {
        show regex("^#+.*(\n|$)"): text.with(..style.title)
        show regex("^> *.*(\n|$)"): text.with(..style.directive)
        show regex("```catala(.|\n)*?```"): show-catala-en-code
        txt
    }
    show raw.where(lang: "catala-fr-code"): show-catala-fr-code
    show raw.where(lang: "catala-en-code"): show-catala-en-code

    it
}
