import { EPSILON, State } from "./State";

export class NFA {
    inState: State;
    outState: State;
    constructor(inState: State, outState: State) {
        this.inState = inState;
        this.outState = outState;
    }

    test(str: string) {
        return this.inState.test(str);
    }

    /**
     * Returns transition table.
     */
    getTransitionTable() {
        const table: { [state: string]: { [symbol: string]: string[] } } = {};

        const states: State[] = [];
        const visited: Set<State> = new Set();

        const addState = (state: State) => {
            if (!visited.has(state)) {
                visited.add(state);
                states.push(state);
            }
        };

        addState(this.inState);

        while (states.length > 0) {
            const currentState = states.pop()!;
            table[currentState.id] = {};

            for (const transition of currentState.getTransitions()) {
                const [ symbol, toStates ] = transition;
                table[currentState.id][symbol] = toStates.map((state) => state.id);
                toStates.forEach(addState);
            }
        }

        return table;
    }
}

export const char = (symbol: string): NFA => {
    const inState = new State();
    const outState = new State()

    outState.accepting = true;

    inState.addTransitionForSymbol(symbol, outState);

    return new NFA(inState, outState);
}


/**
 * Concatenation factory: single pair `ab`
 */
export function concatPair(first: NFA, second: NFA): NFA {
    first.outState.accepting = false;
    second.outState.accepting = true;

    first.outState.addTransitionForSymbol(
        EPSILON,
        second.inState,
    );
    return new NFA(first.inState, second.outState);
}

export function concat(first: NFA, ...rest: Array<NFA>): NFA {
    for (let fragment of rest) {
        first = concatPair(first, fragment);
    }
    return first;
}

/**
 * Union factory: single pair `a|b`
 */
export function orPair(first: NFA, second: NFA): NFA {
    first.outState.accepting = false;
    second.outState.accepting = false;

    const newStart = new State(false);
    const newEnd = new State(true);

    newStart.addTransitionForSymbol(EPSILON, first.inState);
    newStart.addTransitionForSymbol(EPSILON, second.inState);

    first.outState.addTransitionForSymbol(EPSILON, newEnd);
    second.outState.addTransitionForSymbol(EPSILON, newEnd);

    return new NFA(newStart, newEnd);
}

export function or(first: NFA, ...rest: Array<NFA>): NFA {
    for (let fragment of rest) {
        first = orPair(first, fragment);
    }
    return first;
}

/**
 * Repetion factory aka "Kleene closure" : `a*`
 */
export function rep(fragment: NFA): NFA {
    fragment.outState.accepting = false;
    const newStart = new State(false);
    const newEnd = new State(true);

    newStart.addTransitionForSymbol(EPSILON, fragment.inState);
    newStart.addTransitionForSymbol(EPSILON, newEnd);
    newEnd.addTransitionForSymbol(EPSILON, fragment.inState);
    fragment.outState.addTransitionForSymbol(EPSILON, newEnd);

    return new NFA(newStart, newEnd);
}