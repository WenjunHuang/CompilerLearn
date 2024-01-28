"use client";

import * as Either from "@/Data.Either";
import * as List from "@/Data.List";
import * as A from "@/Data.Array";
import {
  LL1Table,
  ParseContext,
  ParseStep,
  ParseStepState,
  createParser,
  firstOf,
  followOf,
  grammarNonTerminals,
  grammarTerminals,
  hasNullable,
  nextStep,
  productionOf,
  sentinel,
  startParse, showProduction,
} from "@/LL1.Parser";
import { useState } from "react";

export default function LL1Page() {
  const [grammarInput, setGrammarInput] = useState<string>(
    "E -> T E'\nE' -> + T E'\nE' -> ε\nT -> F T'\nT' -> * F T'\nT' -> ε\nF -> ( E )\nF -> id ",
  );
  const [ll1Table, setLL1Table] = useState<LL1Table | null>(null);

  return (
    <main className={"px-6"}>
      <section className={"mx-auto max-w-[768px] border-b-[1px] border-gray-400 px-6 py-4"}>
        <h1 className={"text-center text-5xl"}>LL(1) Parser Visualization</h1>
        <h2 className={"mt-4 text-center text-xl"}>
          Write your own context-free grammar and see an LL(1) parser in action!
        </h2>
      </section>
      <section className={"mt-8"}>
        <h3 className={"text-2xl"}>1. Write your LL(1) grammar (empty string is ε):</h3>
        <div className={"my-4"}>
          <textarea
            rows={15}
            className={"w-full border-2 border-black"}
            onChange={e => setGrammarInput(e.target.value)}
            value={grammarInput}></textarea>
        </div>
        <button
          className={"rounded-full bg-blue-500 px-5 py-2 font-bold text-white hover:bg-blue-600"}
          onClick={() => {
            const result = createParser(grammarInput);
            if (result instanceof Either.Left) {
            } else if (result instanceof Either.Right) {
              setLL1Table(result.value0);
            }
            // const grammar = parseGrammar(grammarInput);
            // const { nullable, first, follow, transition } = computeLL1Tables(grammar);
            // console.log(transition);
            // setLL1Table({ grammar, nullable, first, follow, transition });
            // console.log("Parsed grammar:", grammar)
            // const nullable = computeNullable(grammar)
            // console.log("Computed nullable:", nullable)
            // const first = computeFirst(grammar, nullable)
            // console.log("Computed first:", first)
            // const last = computeFollow(grammar, nullable, first)
            // console.log("Computed last:", last)
          }}>
          Generate tables
        </button>
      </section>
      <section className={"mt-8"}>
        <h3 className={"text-2xl"}>2. Nullable/First/Follow Table and Transition Table</h3>
        <div className={"flex gap-4 divide-x divide-gray-200 border-2 border-gray-200 p-4"}>
          <div>
            <NullableFirstFollowTable ll1Table={ll1Table} />
          </div>
          <div className={"flex-1"}>
            <TransitionTable ll1Table={ll1Table} />
          </div>
        </div>
      </section>
      <section className={"mt-8"}>
        <h3 className={"text-2xl"}>3. Parsing</h3>
        <Interpreter ll1Table={ll1Table} />
      </section>
    </main>
  );
}

function NullableFirstFollowTable(props: { ll1Table: LL1Table | null }) {
  const { ll1Table } = props;
  return (
    <table className="min-w-full divide-gray-300 border-[1px] border-gray-300">
      <thead>
        <tr className="divide-x divide-gray-300">
          <>
            {["Nonterminal", "Nullable", "First", "Follow"].map(title => (
              <th
                key={title}
                scope="col"
                className="bg-gray-200 py-3.5 pl-4 pr-4 text-center text-xl font-semibold text-gray-600">
                {title}
              </th>
            ))}
          </>
        </tr>
      </thead>
      <tbody className="divide-y divide-gray-200 bg-white">
        {ll1Table &&
          grammarNonTerminals(ll1Table).map(nonterminal => (
            <tr key={nonterminal} className="divide-x divide-gray-200">
              {[
                nonterminal,
                hasNullable(nonterminal)(ll1Table) ? "Yes" : "No",
                firstOf(nonterminal)(ll1Table).join(", "),
                followOf(nonterminal)(ll1Table).join(", "),
              ].map((content, index) => (
                <td
                  key={nonterminal + index.toString}
                  className="whitespace-nowrap p-4 text-lg text-gray-500">
                  {content}
                </td>
              ))}
            </tr>
          ))}
      </tbody>
    </table>
  );
}

function Interpreter(props: { ll1Table: LL1Table | null }) {
  const { ll1Table } = props;
  const [tokenStream, setTokenStream] = useState<string>("id + id * id");
  const [parseState, setParseState] = useState<ParseStepState | null>(null);
  const start = (input: string, ll1Table: LL1Table) => {
    const state = startParse([...input.split(/\s/), sentinel])(ll1Table);
    setParseState(state);
  };

  const forward = (state: ParseStepState) => {
    const n = nextStep(state);
    setParseState(state);
  };

  return (
    <div>
      <div className={"flex items-center"}>
        <div className="py-4 align-middle text-lg font-medium text-gray-900">
          Token stream separated by spaces:
        </div>
        <input
          type="text"
          className={
            "ml-4 rounded-md border-2 border-gray-300 text-emerald-600 shadow-inner focus:ring-blue-200"
          }
          value={tokenStream}
          onChange={e => setTokenStream(e.target.value)}
        />
      </div>
      <div className={"mt-4 flex gap-2"}>
        <button
          className={
            "rounded-full bg-blue-500 px-5 py-2 font-bold text-white hover:bg-blue-600 disabled:bg-gray-300"
          }
          onClick={() => ll1Table && tokenStream && start(tokenStream, ll1Table)}
          disabled={!(ll1Table && tokenStream)}>
          Start/Reset
        </button>
        <button
          className={
            "rounded-full bg-blue-500 px-5 py-2 font-bold text-white hover:bg-blue-600 disabled:bg-gray-300"
          }
          disabled={!(ll1Table && tokenStream)}
          onClick={() => parseState && forward(parseState)}>
          Step Forward
        </button>
      </div>
      <div className={"mt-4 flex"}>
        <div className={"flex-col space-y-3"}>
          <div>
            <label htmlFor="email" className="block text-sm font-medium leading-6 text-gray-900">
              Stack
            </label>
            <div className="mt-2">
              <input
                type="text"
                name="stack"
                disabled={true}
                className="block w-full rounded-md border-0 py-1.5 text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 placeholder:text-gray-400 focus:ring-2 focus:ring-inset focus:ring-indigo-600 sm:text-sm sm:leading-6"
                value={parseState instanceof ParseStep ? parseState.value0.stack.join(" ") : ""}
              />
            </div>
          </div>
          <div>
            <label htmlFor="remainingInput" className="block text-sm font-medium leading-6 text-gray-900">
              Remaining Input
            </label>
            <div className="mt-2">
              <input
                type="text"
                name="remainingInput"
                className="block w-full rounded-md border-0 py-1.5 text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 placeholder:text-gray-400 focus:ring-2 focus:ring-inset focus:ring-indigo-600 sm:text-sm sm:leading-6"
                disabled={true}
                value={parseState instanceof ParseStep ? parseState.value0.remainTokenStream.join(" ") : ""}
              />
            </div>
          </div>

          <div>
            <label htmlFor="rule" className="block text-sm font-medium leading-6 text-gray-900">
              Rule
            </label>
            <div className="mt-2">
              <input
                type="text"
                name="rule"
                disabled={true}
                className="block w-full rounded-md border-0 py-1.5 text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 placeholder:text-gray-400 focus:ring-2 focus:ring-inset focus:ring-indigo-600 sm:text-sm sm:leading-6"
                value={parseState instanceof ParseStep ? parseState.value0.rule : ""}
              />
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}

function TransitionTable(props: { ll1Table: LL1Table | null }) {
  const { ll1Table } = props;
  if (ll1Table == null) return <></>;
  else
    return (
      <table className="min-w-full divide-gray-300 border-[1px] border-gray-300">
        <thead>
          <tr className="divide-x divide-gray-300">
            <>
              {["", ...grammarTerminals(ll1Table)].map(terminal => (
                <th
                  key={terminal}
                  scope="col"
                  className="bg-gray-200 py-3.5 pl-4 pr-4 text-center text-xl font-semibold text-gray-600">
                  {terminal}
                </th>
              ))}
            </>
          </tr>
        </thead>
        <tbody className="divide-y divide-gray-200 bg-white">
          {grammarNonTerminals(ll1Table).map(nonterminal => (
            <tr key={nonterminal} className="divide-x divide-gray-200">
              <td className="whitespace-nowrap p-4 text-xl text-gray-500">{nonterminal}</td>
              {grammarTerminals(ll1Table).map(terminal => (
                <td key={terminal} className="whitespace-nowrap p-4 text-lg text-gray-500">
                  {productionOf(nonterminal)(terminal)(ll1Table).map(production => (
                    <>
                      {showProduction(production)}
                      <br />
                    </>
                  ))}
                </td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>
    );
}
