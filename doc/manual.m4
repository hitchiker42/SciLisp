dnl m4 macro definitions for use in the SciLisp Manual
m4_changequote(`[[',`]]')
m4_changecom
m4_define([[cindices]],
[[m4_ifelse([[$#]],[[1]],
[[@cindex]] [[$1]],
[[@cindex]] [[$1]]
[[cindices(m4_shift($@))]])]])
m4_define([[node_and_section]],
[[@node $1]]
[[@section $1]])
m4_define([[node_and_chapter]],
[[@node $1]]
[[@chapter $1]])

