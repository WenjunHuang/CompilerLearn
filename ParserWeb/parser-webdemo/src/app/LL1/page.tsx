"use client"
export default function LL1Page() {
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
                <textarea rows={15} className={"w-full border-black border-2"}>
                </textarea>
                </div>
                <button className={"rounded-full bg-blue-500 hover:bg-blue-600 py-2 px-5 font-bold text-white"}>Generate
                    tables
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

