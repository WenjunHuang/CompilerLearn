
export const EPSILON = "ɛ";
let _stateId = 1;
/**
 * FA State class
 */
export class State {
    accepting: boolean;
    transitionsMap: Map<string, State[]>;
    id: string = (_stateId++).toString();

    constructor(accepting = false) {
        this.accepting = accepting;
        this.transitionsMap = new Map<string, Array<State>>();
    }

    addTransitionForSymbol(symbol: string, state: State) {
        const transitions = this.transitionsMap.get(symbol) || [];
        this.transitionsMap.set(symbol, transitions.concat(state));
    }

    getTransitionForSymbol(symbol: string) {
        return this.transitionsMap.get(symbol) || [];
    }

    getTransitions(): Array<[string, State[]]> {
        return Array.from(this.transitionsMap.entries());
    }


    test(str: string, visited = new Set<State>()): boolean {
        if (visited.has(this)) {
            return false;
        }
        visited.add(this);

        if (str.length === 0) {
            if (this.accepting) {
                return true;
            }
            for (const nextState of this.getTransitionForSymbol(EPSILON)) {
                if (nextState.test('', visited)) {
                    return true;
                }
            }
            return false;
        }

        const symbol = str[0];
        const rest = str.slice(1);

        const symbolTransitions = this.getTransitionForSymbol(symbol);
        for (const nextState of symbolTransitions) {
            if (nextState.test(rest)) {
                return true;
            }
        }

        for (const nextState of this.getTransitionForSymbol(EPSILON)) {
            if (nextState.test(str, visited)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Returns the ɛ-closure for this state:
     * set + all states following ɛ-transitions
     */
    getEpsilonClosure(): Array<State> {
        const closure = new Set<State>();
        const stack: [State] = [this];
        while (stack.length > 0) {
            const state = stack.pop()!!;
            closure.add(state);
            for (const nextState of state.getTransitionForSymbol(EPSILON)) {
                if (!closure.has(nextState)) {
                    stack.push(nextState);
                }
            }
        }
        return Array.from(closure);
    }
}