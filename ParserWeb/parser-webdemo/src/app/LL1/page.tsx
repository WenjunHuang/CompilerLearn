"use client"

import {useState} from "react";

interface Production {
    lhs: string
    rhs: string[]
}

interface Grammar {
    rules: Production[]
    terminals: Set<string>
    nonTerminals: Set<string>
    start: string
}

const SYNTHETIC_START = "S"
const SENTINEL = "$"
const EPSILON = "''"
const parseGrammar = (input: string): Grammar => {
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
    return {rules, terminals, nonTerminals, start}
}

const computeNullable = (grammar: Grammar): Set<string> => {
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

const computeFirst = (grammar: Grammar, nullable: Set<string>): Map<string, Set<string>> => {
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
            const idx = rule.rhs.findIndex((sym) => !nullable.has(sym))
            const firstOfRhs = first.get(rule.lhs)!
            rule.rhs
                .slice(0, idx < 0 ? rule.rhs.length : idx + 1)
                .map((sym) => first.get(sym)!)
                .reduce((prev, cur) => {
                    const r = new Set<string>()
                    prev.forEach((x) => r.add(x))
                    cur.forEach((x) => r.add(x))
                    return r
                }, new Set<string>())
                .forEach((x) => {
                    if (!firstOfRhs.has(x)) {
                        changed = true
                        firstOfRhs.add(x)
                    }
                })
        }
    }

    return first
}
export default function LL1Page() {


    const [grammarInput, setGrammarInput] = useState<string>(
        "E -> T E'\nE' -> + T E'\nE' -> ''\nT -> F T'\nT' -> * F T'\nT' -> ''\nF -> ( E )\nF -> id ")
    return (
        <main className={"px-6"}>
            <section className={"py-4 px-6 max-w-[768px] mx-auto border-gray-400 border-b-[1px]"}>
                <h1 className={"text-center text-5xl"}>LL(1) Parser Visualization</h1>
                <h2 className={"mt-4 text-center text-xl"}>Write your own context-free grammar and see an LL(1) parser
                    in
                    action!</h2>
            </section>
            <section className={"mt-8"}>
                <h3 className={"text-2xl"}>1. Write your LL(1) grammar (empty string &apos;&apos; represents ε):</h3>
                <div className={"my-4"}>
                    <textarea rows={15} className={"w-full border-black border-2"}
                              onChange={e => setGrammarInput(e.target.value)} value={grammarInput}></textarea>
                </div>
                <button className={"rounded-full bg-blue-500 hover:bg-blue-600 py-2 px-5 font-bold text-white"}
                        onClick={() => {
                            const grammar = parseGrammar(grammarInput)
                            console.log("Parsed grammar:", grammar)
                            const nullable = computeNullable(grammar)
                            console.log("Computed nullable:", nullable)
                            const first = computeFirst(grammar, nullable)
                            console.log("Computed first:", first)
                        }}
                >Generate tables
                </button>
            </section>
            <section className={"mt-8"}>
                <h3 className={"text-2xl"}>2. Nullable/First/Follow Table and Transition Table</h3>
                <div className={"flex"}>
                    <Example/>
                    <Example/>
                </div>
            </section>
            <section className={"mt-8"}>
                <h3 className={"text-2xl"}>3. Parsing</h3>
            </section>
        </main>
    )
}

const people = [
    {name: 'Lindsay Walton', title: 'Front-end Developer', email: 'lindsay.walton@example.com', role: 'Member'},
    // More people...
]

function Example() {
    return (
        <div className="px-4 sm:px-6 lg:px-8">
            <div className="mt-8 flow-root">
                <div className="-mx-4 -my-2 overflow-x-auto sm:-mx-6 lg:-mx-8">
                    <div className="inline-block min-w-full py-2 align-middle sm:px-6 lg:px-8">
                        <table className="min-w-full divide-y divide-gray-300 border-gray-300 border-[1px]">
                            <thead>
                            <tr className="divide-x divide-gray-200">
                                <th scope="col"
                                    className="py-3.5 pl-4 pr-4 text-left text-sm font-semibold text-gray-900 sm:pl-0">
                                    Nonterminal
                                </th>
                                <th scope="col" className="px-4 py-3.5 text-left text-sm font-semibold text-gray-900">
                                    Nullable
                                </th>
                                <th scope="col" className="px-4 py-3.5 text-left text-sm font-semibold text-gray-900">
                                    First
                                </th>
                                <th scope="col"
                                    className="py-3.5 pl-4 pr-4 text-left text-sm font-semibold text-gray-900 sm:pr-0">
                                    Follow
                                </th>
                            </tr>
                            </thead>
                            <tbody className="divide-y divide-gray-200 bg-white">
                            {people.map((person) => (
                                <tr key={person.email} className="divide-x divide-gray-200">
                                    <td className="whitespace-nowrap py-4 pl-4 pr-4 text-sm font-medium text-gray-900 sm:pl-0">
                                        {person.name}
                                    </td>
                                    <td className="whitespace-nowrap p-4 text-sm text-gray-500">{person.title}</td>
                                    <td className="whitespace-nowrap p-4 text-sm text-gray-500">{person.email}</td>
                                    <td className="whitespace-nowrap py-4 pl-4 pr-4 text-sm text-gray-500 sm:pr-0">{person.role}</td>
                                </tr>
                            ))}
                            </tbody>
                        </table>
                    </div>
                </div>
            </div>
        </div>
    )
}


