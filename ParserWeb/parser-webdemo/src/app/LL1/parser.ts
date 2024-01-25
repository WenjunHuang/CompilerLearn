export interface Production {
    lhs: string
    rhs: string[]
}

export interface Grammar {
    rules: Production[]
    terminals: Set<string>
    nonTerminals: Set<string>
    start: string
}

export const SYNTHETIC_START = "S"
export const SENTINEL = "$"
export const EPSILON = "ε"
export const parseGrammar = (input: string): Grammar => {
    const rules: Production[] = []
    const nonTerminals = new Set<string>()
    nonTerminals.add(SYNTHETIC_START)
    const terminals = new Set<string>()
    for (const rule of input.split('\n')) {
        const [left, right] = rule.split('->')
        if (right !== undefined) {
            const lhs = left.trim()
            const rhs = right.trim().split(/\s+/).filter((x) => !(x === EPSILON))
            rules.push({lhs, rhs})
            nonTerminals.add(lhs)
        }
    }
    for (const rule of rules) {
        for (const symbol of rule.rhs) {
            if (!nonTerminals.has(symbol)) {
                terminals.add(symbol)
            }
        }
    }
    const start = rules[0].lhs

    terminals.add(SENTINEL)
    rules.push({lhs: SYNTHETIC_START, rhs: [start, SENTINEL]})

    return {rules, terminals, nonTerminals, start}
}

export const computeNullable = (grammar: Grammar): Set<string> => {
    const nullable = new Set<string>()
    let changed = true
    while (changed) {
        changed = false
        for (const rule of grammar.rules) {
            if (rule.rhs.every((x) => nullable.has(x))) {
                if (!nullable.has(rule.lhs)) {
                    changed = true
                    nullable.add(rule.lhs)
                }
            }
        }
    }
    return nullable
}

export const computeFirst = (grammar: Grammar, nullable: Set<string>): Map<string, Set<string>> => {
    const first = new Map<string, Set<string>>()
    grammar.terminals.forEach((terminal) => {
        first.set(terminal, new Set<string>([terminal]))
    })
    grammar.nonTerminals.forEach((nt) => {
        first.set(nt, new Set<string>())
    })

    let changed = true
    while (changed) {
        changed = false
        for (const rule of grammar.rules) {
            const firstOfRhs = first.get(rule.lhs)!
            reachableTerminals(rule.rhs, first, nullable).forEach((x) => {
                if (!firstOfRhs.has(x)) {
                    changed = true
                    firstOfRhs.add(x)
                }
            })
        }
    }

    return first
}

const reachableTerminals = (rhs: string[], first: Map<string, Set<string>>, nullable: Set<string>): Set<string> => {
    const idx = rhs.findIndex((sym) => !nullable.has(sym))
    return rhs
        .slice(0, idx < 0 ? rhs.length : idx + 1)
        .map((sym) => first.get(sym)!)
        .reduce((prev, cur) => {
            const r = new Set<string>()
            addToSet(prev, r)
            addToSet(cur, r)
            return r
        }, new Set<string>())
}

export const computeFollow = (grammar: Grammar, nullable: Set<string>, first: Map<string, Set<string>>): Map<string, Set<string>> => {
    const last = new Map<string, Set<string>>()
    grammar.nonTerminals.forEach((nt) => {
        last.set(nt, new Set<string>())
    })
    let changed = true
    while (changed) {
        changed = false
        for (const rule of grammar.rules) {
            let temp = new Set<string>(last.get(rule.lhs)!)
            rule.rhs.toReversed().forEach((sym) => {
                if (grammar.terminals.has(sym)) {
                    temp = new Set<string>([sym])
                } else if (grammar.nonTerminals.has(sym)) {
                    const lastOfSym = last.get(sym)!
                    changed = addToSet(temp, lastOfSym)

                    if (!nullable.has(sym)) {
                        temp = new Set<string>(first.get(sym)!)
                    } else {
                        addToSet(first.get(sym)!, temp)
                    }
                }
            })
        }
    }
    return last
}

export const computeLL1Tables = (grammar: Grammar) => {
    const nullable = computeNullable(grammar)
    const first = computeFirst(grammar, nullable)
    const follow = computeFollow(grammar, nullable, first)

    const transition = new Map<string, Map<string, Array<Production>>>()
    grammar.nonTerminals.forEach((nt) => {
        const row = new Map<string, Array<Production>>()
        grammar.terminals.forEach((t) => {
            row.set(t, [])
        })
        transition.set(nt, row)
    })

    grammar.rules.forEach((rule) => {
        reachableTerminals(rule.rhs, first, nullable).forEach((t) => {
            transition.get(rule.lhs)!.get(t)!.push(rule)
        })
        if (rule.rhs.every(x => nullable.has(x))) {
            follow.get(rule.lhs)!.forEach((x) => {
                transition.get(rule.lhs)!.get(x)!.push(rule)
            })
        }
    })
    return {nullable, first, follow, transition}
}
const addToSet = (source: Set<string>, target: Set<string>): boolean => {
    let changed = false
    source.forEach((x) => {
        if (!target.has(x)) {
            target.add(x)
            changed = true
        }
    })
    return changed
}
