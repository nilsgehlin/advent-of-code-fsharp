namespace Day4

type Section = Section of int
type SectionRange = SectionRange of Section Set
type Pair = Pair of (SectionRange * SectionRange)
