#show heading: set text(size: 9pt)

#let render_title(title, subtitle) = {
    place(top+left, image("logo.svg", width: 30pt))
    place(top+right)[v1.0.1 pre · Révision \#1 · ⓒ #datetime.today().year()]
    box(height:30pt, width:100%, align(horizon+center,{
        upper(text(size:15pt, title))
        h(30pt)
        text(size: 9pt, subtitle)
    }))
    v(1em)
}

#let syntax-doc(title, ..args) = {
  let lines = args.pos().chunks(2).map(x => (
      grid.cell(align: horizon, x.at(0)),
      grid.cell(stroke: (left: 1pt + luma(200)),
                         inset: (left: 0.5em),
                         align: horizon,
          text(style:"oblique",x.at(1)))))
  [= #title]
  v(0.8em)
  grid(columns: (65%, 35%),
      row-gutter: 0.8em,
      ..lines.flatten())
}

#let layout(title, subtitle, ..columns) = {
    set page(paper: "a4", flipped: true, margin: 1cm)
    set text(font: "Inter 18pt", size: 7pt)
    show raw: set text(font: "Annotation Mono", weight: "medium", size: 7pt)

    render_title(title, subtitle)
    grid(
        columns: (1fr, 1fr, 1fr),
        gutter: 0pt,
        stroke: (x, y) => if x > 0 { (left: 0.1pt + black) },
        inset: (x, y) => if x > 0 { (left: 6pt) } + if x < 2 { (right: 6pt) },
        ..columns.pos().map(col => col.join(v(1fr)))
    )
}
